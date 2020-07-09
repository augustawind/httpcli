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
    , optSave :: Bool
    } deriving (Eq, Show)

data Command
    = CmdRun { cmdRunPath :: [String] }
    | CmdView { cmdViewPath :: [String] }
    | CmdEnv { cmdEnvPath :: Maybe String, cmdEnvValue :: Maybe String }
    | CmdRepl
    deriving (Eq, Show)

parseCli :: IO Options
parseCli = execParser cli

parseCliCommand :: [String] -> ParserResult Options
parseCliCommand = execParserPure defaultPrefs cli

cli :: ParserInfo Options
cli = info
  (cliOptions <**> helper)
  (fullDesc <> progDesc "run httpcli" <> header "testing the header out")

cliOptions :: Parser Options
cliOptions = do
  optCommand <- (subparser . foldMap mkCommand)
    [ ( "run"
      , "run a request"
      , CmdRun
        <$> (splitOn "." <$> argument
              nonEmptyStr
              (metavar "REQUEST" <> help "name of the request to run in the API"
              )
            )
      )
    , ( "view"
      , "inspect a group, request, or request attribute"
      , CmdView
        <$> (splitOn "." <$> argument
              nonEmptyStr
              (metavar "ITEM" <> help
                "the group, request, or request attribute to view in the API"
              )
            )
      )
    , ( "env"
      , "view or update the Env"
      , CmdEnv
      <$> optional
            (argument nonEmptyStr (metavar "KEY" <> help "a key in the Env"))
      <*> optional
            (argument str
                      (metavar "VALUE" <> help "a value to assign to the key")
            )
      )
    , ("repl", "start an interactive prompt", pure CmdRepl)
    ]
  optApiFile <- option str (long "api")
  optEnvFile <- optional $ option str (long "env")
  optSave    <- switch
    (long "save" <> short 's' <> help
      "persist any changes to the Env, writing them to disk"
    )
  pure Options { .. }
 where
  mkCommand (name, desc, parser) =
    command name (info (parser <**> helper) (progDesc desc))

nonEmptyStr :: IsString s => ReadM s
nonEmptyStr = eitherReader $ \case
  "" -> Left "invalid argument: empty string"
  x  -> Right $ fromString x
