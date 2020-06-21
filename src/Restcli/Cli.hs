module Restcli.Cli where

import           Control.Applicative            ( optional )
import           Data.Semigroup                 ( (<>) )
import           Options.Applicative

data Options = Options
    { optCommand :: Command
    , optApiFile :: FilePath
    , optEnvFile :: Maybe FilePath
    } deriving (Eq, Show)

data Command
    = Run { optRunPath :: String }
    | View { optViewPath :: String }
    deriving (Eq, Show)

runCli :: IO Options
runCli = execParser cli

cli :: ParserInfo Options
cli = info
    (cliOptions <**> helper)
    (fullDesc <> progDesc "run httpcli" <> header "testing the header out")

cliOptions :: Parser Options
cliOptions =
    Options
        <$> (subparser . foldMap mkCommand)
                [ ("run", "run a request", Run <$> argDataPath)
                , ( "view"
                  , "inspect a group, request, or request attribute"
                  , View <$> argDataPath
                  )
                ]
        <*> option str (long "api")  -- apiFile
        <*> optional (option str (long "env"))  -- envFile
  where
    mkCommand (name, desc, parser) = command name (info parser (progDesc desc))
    argDataPath = argument str (metavar "PATH")
