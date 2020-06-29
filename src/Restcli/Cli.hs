{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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
    = CmdRun { cmdRunPath :: [String] }
    | CmdView { cmdViewPath :: [String] }
    | CmdEnv { cmdEnvPath :: Maybe String, cmdEnvValue :: Maybe String }
    deriving (Eq, Show)

parseCli :: IO Options
parseCli = execParser cli

cli :: ParserInfo Options
cli = info
    (cliOptions <**> helper)
    (fullDesc <> progDesc "run httpcli" <> header "testing the header out")

cliOptions :: Parser Options
cliOptions = do
    optCommand <- (subparser . foldMap mkCommand)
        [ ("run", "run a request", CmdRun <$> argDataPath)
        , ( "view"
          , "view a group, request, or request attribute"
          , CmdView <$> argDataPath
          )
        , ( "env"
          , "view or update an Env value"
          , CmdEnv
          <$> optional (argument nonEmptyStr (metavar "KEY"))
          <*> optional (argument str (metavar "VALUE"))
          )
        ]
    optApiFile <- option str (long "api")
    optEnvFile <- optional (option str (long "env"))
    pure Options { .. }
  where
    mkCommand (name, desc, parser) = command name (info parser (progDesc desc))
    argDataPath = splitOn "." <$> argument nonEmptyStr (metavar "PATH")

nonEmptyStr :: IsString s => ReadM s
nonEmptyStr = eitherReader $ \case
    "" -> Left "invalid argument: empty string"
    x  -> Right $ fromString x
