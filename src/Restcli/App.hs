module Restcli.App where

import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Char8         as B
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

-- appOptions :: App Options
-- appOptions = asks fst

-- appInitialState :: App AppState
-- appInitialState = asks snd

-- appState :: App AppState
-- appState = get

run :: Options -> API -> Env -> IO ()
run = runApp dispatch

runApp :: App a -> Options -> API -> Env -> IO a
runApp app opts api env =
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

