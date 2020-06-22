{-# LANGUAGE LambdaCase #-}

module Restcli.Cli where

import           Control.Applicative            ( optional )
import           Data.List.Split                ( splitOn )
import           Data.Semigroup                 ( (<>) )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Options.Applicative

data Options = Options
    { optCommand :: Command
    , optApiFile :: FilePath
    , optEnvFile :: Maybe FilePath
    } deriving (Eq, Show)

data Command
    = Run { optRunPath :: [String] }
    | View { optViewPath :: [String] }
    deriving (Eq, Show)

parseCli :: IO Options
parseCli = execParser cli

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
    argDataPath = splitOn "." <$> argument nonEmptyStr (metavar "PATH")

nonEmptyStr :: IsString s => ReadM s
nonEmptyStr = eitherReader $ \case
    "" -> Left "invalid argument: empty string"
    x  -> Right $ fromString x
