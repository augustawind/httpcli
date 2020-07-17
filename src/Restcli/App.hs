{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Restcli.App where

import           Control.Exception              ( displayException )
import           Control.Monad.Catch            ( catch
                                                , throwM
                                                , try
                                                )
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as B
import qualified Data.CaseInsensitive          as CI
import           Data.Char                      ( isSpace )
import qualified Data.HashMap.Strict.InsOrd    as OrdMap
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Yaml                     as Yaml
import           Options.Applicative            ( handleParseResult )
import           System.Console.Haskeline       ( InputT(..)
                                                , Settings(..)
                                                , defaultSettings
                                                , getInputLine
                                                , runInputT
                                                )
import           System.Directory               ( XdgDirectory(..)
                                                , canonicalizePath
                                                , createDirectoryIfMissing
                                                , getXdgDirectory
                                                )
import           System.Environment             ( getEnvironment )
import           System.Exit                    ( ExitCode )
import           System.FilePath                ( (</>) )
import           Text.Mustache                  ( Template )
import qualified Text.Pretty.Simple            as PP

import           Restcli.Data
import           Restcli.CLI
import           Restcli.Data.Encoding
import           Restcli.Error
import           Restcli.Requests
import           Restcli.Scripting              ( runScript )
import           Restcli.Types
import           Restcli.Utils                  ( tokenize
                                                , unsnoc
                                                , whenMaybe
                                                )

type App = ReaderT Options (StateT AppState IO)

data AppState = AppState
    { appAPI :: API
    , appEnv :: Env
    , appAPITemplate :: Template
    } deriving (Show)

replPrompt :: String
replPrompt = "> "

-- | Run the main program.
-- This is a convenience wrapper that runs `dispatch` and prints the result.
run :: IO ()
run = runApp dispatch >>= liftIO . B.putStrLn

-- | Run the given App with context obtained from the commandline.
--
-- 1. Obtains Options from the commandline.
-- 2. Loads initial AppState.
-- 3. Calls `runAppWith` with the Options and AppState.
runApp :: App ByteString -> IO ByteString
runApp app = do
    opts <- getEnvironment >>= parseCLI
    initAppState opts >>= \case
        Left  err      -> return $ renderError err
        Right appState -> runAppWith app opts appState

-- | Run the given App with the given Options and AppState.
runAppWith :: App ByteString -> Options -> AppState -> IO ByteString
runAppWith app opts appState =
    evalStateT (runReaderT app opts) appState `catch` (return . renderError)

-- | Create an initial AppState by reading Template & Env files and compiling
-- an API object from them.
initAppState :: Options -> IO (Either Error AppState)
initAppState opts = do
    let apiFile = optAPIFile opts
    tmpl <- readAPITemplate apiFile
    env  <- case optEnvFile opts of
        Just filePath -> readEnv filePath
        Nothing       -> return $ Env OrdMap.empty
    return $ do
        api <- parseAPI tmpl env apiFile
        Right AppState { appAPI = api, appEnv = env, appAPITemplate = tmpl }

-- | Execute the App's command, found in its Options.
dispatch :: App ByteString
dispatch = ask >>= \opts -> case optCommand opts of
    CmdRun  path      -> cmdRun (map T.pack path) (optSave opts)
    CmdView path      -> cmdView (map T.pack path)
    CmdEnv path value -> cmdEnv (fmap T.pack path) (fmap B.pack value)
    CmdRepl histfile  -> cmdRepl histfile

-- | Execute the `run` command.
cmdRun :: [Text] -> Bool -> App ByteString
cmdRun path save = do
    api <- gets appAPI
    let (groupKeys, reqKey) = unsnoc path
    case getAPIRequest groupKeys reqKey api of
        Left  err -> throwM . WithMsg err =<< asks optAPIFile
        Right req -> do
            res <- liftIO $ sendRequest req
            whenMaybe (reqScript req) $ \script -> do
                execScript script req res
                when save $ asks optEnvFile >>= \case
                    Nothing -> return ()
                    Just fp -> gets appEnv >>= liftIO . saveEnv fp
            return $ Yaml.encode res

execScript :: Text -> HttpRequest -> HttpResponse -> App ()
execScript script req res =
    gets appEnv >>= liftIO . runScript script req res >>= \case
        Nothing  -> return ()
        Just env -> modify' $ \appState -> appState { appEnv = env }

-- | Execute the `view` command.
cmdView :: [Text] -> App ByteString
cmdView path = do
    api <- gets appAPI
    case getAPIComponent' path api of
        Right (APIGroup       group) -> return $ Yaml.encode group
        Right (APIRequest     req  ) -> return $ Yaml.encode req
        Right (APIRequestAttr attr ) -> return $ Yaml.encode attr
        Left  err                    -> throwM . WithMsg err =<< asks optAPIFile


-- | Execute the `env` command.
cmdEnv :: Maybe Text -> Maybe ByteString -> App ByteString
cmdEnv Nothing    _       = Yaml.encode <$> gets appEnv
cmdEnv (Just key) Nothing = do
    env <- gets appEnv
    case lookupEnv key env of
        Right value -> return $ Yaml.encode value
        Left  err   -> throwM =<< maybe err (WithMsg err) <$> asks optEnvFile
cmdEnv (Just key) (Just text) = do
    env   <- gets appEnv
    value <- Yaml.decodeThrow text
    let env' = insertEnv key value env
    modify' $ \appState -> appState { appEnv = env' }
    asks optEnvFile >>= \case
        Nothing -> return ()
        Just fp -> liftIO $ saveEnv fp env'
    return $ Yaml.encode env'

-- | Execute the `repl` command.
cmdRepl :: Maybe FilePath -> App ByteString
cmdRepl histfileArg = do
    histfile <- liftIO $ case histfileArg of
        Just path
            | null path || all isSpace path -> do
                dir <- getXdgDirectory XdgCache progName
                createDirectoryIfMissing True dir
                return . Just $ dir </> "history"
            | otherwise -> Just <$> canonicalizePath path
        Nothing -> return Nothing
    let settings = defaultSettings { autoAddHistory = True
                                   , historyFile    = histfile
                                   }
    liftIO $ runInputT settings repl
    return B.empty

-- TODO: allow initial options to be specified, and options to persist between commands
repl :: InputT IO ()
repl = getInputLine replPrompt >>= \case
    Nothing -> return ()
    Just s  -> do
        sysenv <- liftIO getEnvironment
        let argv   = tokenize s
            result = handleParseResult $ parseCLICommand argv sysenv
        liftIO $ (try result :: IO (Either ExitCode Options)) >>= \case
            Left _ -> return ()
            Right opts ->
                initAppState opts
                    >>= either (return . renderError) (runAppWith dispatch opts)
                    >>= B.putStrLn
        repl

-- | Reload the App's Env.
reloadEnv :: App Env
reloadEnv = do
    filePath <- asks optEnvFile
    case filePath of
        Nothing       -> gets appEnv
        Just filePath -> do
            env <- liftIO $ readEnv filePath
            modify' $ \appState -> appState { appEnv = env }
            return env

-- | Reload the App's API, compiling it from its Template and Env.
reloadAPI :: App API
reloadAPI = do
    tmpl    <- reloadAPITemplate
    env     <- gets appEnv
    apiFile <- asks optAPIFile
    case parseAPI tmpl env apiFile of
        Left  err -> throwM err
        Right api -> do
            modify' $ \appState -> appState { appAPI = api }
            return api

-- | Reload the App's API Template.
reloadAPITemplate :: App Template
reloadAPITemplate = do
    tmpl <- asks optAPIFile >>= liftIO . readAPITemplate
    modify' $ \appState -> appState { appAPITemplate = tmpl }
    return tmpl

renderError :: Error -> ByteString
renderError = B.pack . displayException . (`WithMsg` (progName ++ ": error"))

pprint :: Show a => a -> IO ()
pprint = PP.pPrintOpt PP.NoCheckColorTty prettyOptions

pshow :: Show a => a -> String
pshow = LT.unpack . PP.pShowOpt prettyOptions

prettyOptions :: PP.OutputOptions
prettyOptions =
    PP.defaultOutputOptionsNoColor { PP.outputOptionsIndentAmount = 2 }
