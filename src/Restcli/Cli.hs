module Restcli.Cli where

import           Control.Applicative            ( optional )
import           Data.Semigroup                 ( (<>) )
import           Options.Applicative

data Options = Options
    { apiFile :: FilePath, envFile :: Maybe FilePath } deriving (Show)

runCli :: IO Options
runCli = execParser cli

cli :: ParserInfo Options
cli = info
    (cliOptions <**> helper)
    (fullDesc <> progDesc "run httpcli" <> header "testing the header out")

cliOptions :: Parser Options
cliOptions =
    Options
        <$> strOption (long "api")  -- apiFile
        <*> optional (strOption (long "env"))  -- envFile
