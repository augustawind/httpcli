module Restcli.App where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State

import           Restcli.Api
import           Restcli.Cli
import           Restcli.Error
import           Restcli.Types

type App = ReaderT (Options, AppState) (StateT AppState IO)

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

runS :: App String
runS = asks fst >>= \opts -> case optCommand opts of
    Run  path -> cmdRunS path
    View path -> cmdViewS path

cmdRunS :: [String] -> App String
cmdRunS = undefined

cmdViewS :: [String] -> App String
cmdViewS path = do
    api <- gets appAPI
    undefined


