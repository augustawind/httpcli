module Restcli.App where

import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Char8         as B
import qualified Data.HashMap.Strict           as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml

import           Restcli.Api
import           Restcli.Cli
import           Restcli.Data.Encoding
import           Restcli.Error
import           Restcli.Types

type App = ReaderT Options (StateT AppState IO)

data AppState = AppState
    { appAPI :: API
    , appEnv :: Env
    } deriving (Eq, Show)

run :: IO ()
run = runApp dispatch

runWith :: Options -> API -> Env -> IO ()
runWith = runAppWith dispatch

runApp :: App a -> IO a
runApp app = do
    opts <- parseCli
    tmpl <- readApiTemplate $ optApiFile opts
    env  <- case optEnvFile opts of
        Just filePath -> readEnv filePath
        Nothing       -> return Map.empty

    case parseAPI tmpl env of
        Left err -> fail $ displayException err
        Right api ->
            -- <DEBUG>
            putStrLn ("\n" ++ replicate 25 '-' ++ "\nTEMPLATE\n")
                >> print tmpl
                >> putStrLn ("\n" ++ replicate 25 '-' ++ "\nENV\n")
                >> print env
                >>
            -- </DEBUG>
                   runAppWith app opts api env

runAppWith :: App a -> Options -> API -> Env -> IO a
runAppWith app opts api env =
    evalStateT (runReaderT app opts) AppState { appAPI = api, appEnv = env }

dispatch :: App ()
dispatch = dispatchS >>= liftIO . putStrLn

dispatchS :: App String
dispatchS = ask >>= \opts -> case optCommand opts of
    Run  path -> cmdRunS $ toText path
    View path -> cmdViewS $ toText path
    where toText = map T.pack

cmdRunS :: [Text] -> App String
cmdRunS = undefined

cmdViewS :: [Text] -> App String
cmdViewS path = do
    api <- gets appAPI
    case getApiComponent' path api of
        Right (APIGroup       group) -> return . B.unpack $ Yaml.encode group
        Right (APIRequest     req  ) -> return . B.unpack $ Yaml.encode req
        Right (APIRequestAttr attr ) -> return . B.unpack $ Yaml.encode attr
        Left  err                    -> fail $ displayException err

