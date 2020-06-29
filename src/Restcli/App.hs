{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restcli.App where

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson              hiding ( Options )
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as LB
import qualified Data.CaseInsensitive          as CI
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as OrdMap
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import qualified Data.Yaml                     as Yaml
import qualified Foreign.Lua                   as Lua
import           Foreign.Lua.Aeson
import           Text.Mustache                  ( Template )
import qualified Text.Pretty.Simple            as PP

import           Restcli.Api
import           Restcli.Cli
import           Restcli.Data.Encoding
import           Restcli.Error
import           Restcli.Requests
import           Restcli.Types
import           Restcli.Utils                  ( unsnoc )

type App = ReaderT Options (StateT AppState IO)

data AppState = AppState
    { appAPI :: API
    , appEnv :: Env
    , appAPITemplate :: Template
    } deriving (Show)

-- | Run the main program.
run :: IO ()
run = runApp $ dispatch >>= liftIO . B.putStrLn

-- | Run the main program with custom Options and AppState, ignoring the
-- commandline.
runWith :: Options -> AppState -> IO ()
runWith = runAppWith $ dispatch >>= liftIO . B.putStrLn

-- | Run the given App with context obtained from the commandline.
--
-- 1. Obtains Options by parsing the commandline.
-- 2. Creates an initial AppState by reading Template & Env files and compiling
--    an API object from them.
-- 3. Calls `runAppWith` with the Options and AppState.
runApp :: App a -> IO a
runApp app = do
    opts <- parseCli
    tmpl <- readApiTemplate $ optApiFile opts
    env  <- case optEnvFile opts of
        Just filePath -> readEnv filePath
        Nothing       -> return $ Env OrdMap.empty

    case parseAPI tmpl env of
        Left  err -> fail $ displayException err
        Right api -> runAppWith
            app
            opts
            AppState { appAPI = api, appEnv = env, appAPITemplate = tmpl }

-- | Run the given App with the given Options and AppState.
runAppWith :: App a -> Options -> AppState -> IO a
runAppWith app opts = evalStateT (runReaderT app opts)

-- | Execute the App's command, found in its Options.
dispatch :: App ByteString
dispatch = ask >>= \opts -> case optCommand opts of
    CmdRun path save -> do
        ret <- cmdRun (toText path) save
        -- <DEBUG>
        AppState { appAPI = api, appEnv = env, ..} <- get
        let section name = liftIO $ putStrLn $ unlines [replicate 25 '-', name]
        section "API"
        liftIO $ B.putStrLn (Yaml.encode api)
        section "ENV"
        liftIO $ B.putStrLn (Yaml.encode env)
        section "PROGRAM OUTPUT"
        -- </DEBUG>
        return ret
    CmdView path      -> cmdView (toText path)
    CmdEnv path value -> cmdEnv (fmap T.pack path) (fmap B.pack value)
    where toText = map T.pack

-- | Execute the `run` command.
cmdRun :: [Text] -> Bool -> App ByteString
cmdRun path save = do
    api <- gets appAPI
    let (groupKeys, reqKey) = unsnoc path
    case getApiRequest groupKeys reqKey api of
        Left  err -> fail $ displayException err
        Right req -> do
            res <- liftIO $ sendRequest req
            case reqScript req of
                Nothing     -> return ()
                Just script -> do
                    execScript script res
                    when save $ asks optEnvFile >>= maybe
                        (return ())
                        (\fp -> gets appEnv >>= liftIO . saveEnv fp)
            return $ (B.pack . pshow) res

execScript :: Text -> HttpResponse -> App ()
execScript script res = do
    env  <- gets appEnv
    env' <- liftIO . Lua.run $ do
        Lua.openlibs
        Lua.push (mkScriptContext res env) *> Lua.setglobal' "ctx"

        result <- Lua.dostring (encodeUtf8 script)
        when (result /= Lua.OK) $ Lua.peek 1 >>= liftIO . fail

        Lua.getglobal "ctx"
        ctx <- Lua.peek =<< Lua.gettop :: Lua.Lua (HashMap String Value)
        return $ Map.lookup "env" ctx
    case env' of
        Nothing          -> return ()
        Just (Object hm) -> modify
            $ \appState -> appState { appEnv = Env (OrdMap.fromHashMap hm) }

mkScriptContext :: HttpResponse -> Env -> HashMap String Value
mkScriptContext HttpResponse {..} (Env env) = Map.fromList
    [ ( "response"
      , object
          [ "version" .= (String . T.pack . show $ resHttpVersion)
          , "status_code" .= (Number . fromIntegral $ resStatusCode)
          , "status" .= String
              (T.unwords
                  [ T.pack . show $ resStatusCode
                  , decodeUtf8 . LB.toStrict $ resStatusText
                  ]
              )
          , "headers"
              .= toJSON
                     (map
                         (\(k, v) -> (B.unpack $ CI.foldedCase k, B.unpack v))
                         resHeaders
                     )
          -- TODO: add error handling (change func sig to App, or maybe just Either)
          , "body" .= (either error id (eitherDecode' resBody) :: Value)
          ]
      )
    , ("env", Object $ OrdMap.toHashMap env)
    ]

-- | Execute the `view` command.
cmdView :: [Text] -> App ByteString
cmdView path = do
    api <- gets appAPI
    case getApiComponent' path api of
        Right (APIGroup       group) -> return $ Yaml.encode group
        Right (APIRequest     req  ) -> return $ Yaml.encode req
        Right (APIRequestAttr attr ) -> return $ Yaml.encode attr
        Left  err                    -> fail $ displayException err

cmdEnv :: Maybe Text -> Maybe ByteString -> App ByteString
cmdEnv Nothing    _       = Yaml.encode <$> gets appEnv
cmdEnv (Just key) Nothing = do
    env <- gets appEnv
    case getEnvItem key env of
        Right value -> return $ Yaml.encode value
        Left  err   -> fail $ displayException err
cmdEnv (Just key) (Just text) = do
    env   <- gets appEnv
    value <- Yaml.decodeThrow text
    case setEnvItem key value env of
        Right env' -> do
            modify $ \st -> st { appEnv = env' }
            return $ Yaml.encode env'
        Left err -> fail $ displayException err

-- | Reload the App's Env.
reloadEnv :: App Env
reloadEnv = do
    filePath <- asks optEnvFile
    case filePath of
        Nothing       -> gets appEnv
        Just filePath -> do
            env <- liftIO $ readEnv filePath
            modify $ \appState -> appState { appEnv = env }
            return env

-- | Reload the App's API Template.
reloadAPITemplate :: App Template
reloadAPITemplate = do
    tmpl <- asks optApiFile >>= liftIO . readApiTemplate
    modify $ \appState -> appState { appAPITemplate = tmpl }
    return tmpl

-- | Reload the App's API, compiling it from its Template and Env.
reloadAPI :: App API
reloadAPI = do
    AppState { appEnv = env, appAPITemplate = tmpl } <- get
    case parseAPI tmpl env of
        Left  err -> fail $ displayException err
        Right api -> do
            modify $ \appState -> appState { appAPI = api }
            return api

pprint :: Show a => a -> IO ()
pprint = PP.pPrintOpt PP.NoCheckColorTty prettyOptions

pshow :: Show a => a -> String
pshow = LT.unpack . PP.pShowOpt prettyOptions

prettyOptions :: PP.OutputOptions
prettyOptions =
    PP.defaultOutputOptionsNoColor { PP.outputOptionsIndentAmount = 2 }
