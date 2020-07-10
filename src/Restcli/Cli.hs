{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Restcli.Cli where

import           Control.Applicative            ( optional )
import qualified Data.ByteString.Lazy.Char8    as LB
import           Data.List.Split                ( splitOn )
import           Data.Semigroup                 ( (<>) )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Options.Applicative
import           Options.Applicative.Types      ( readerAsk )
import           System.Environment
import           System.FilePath                ( joinPath )

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
    | CmdRepl { cmdReplHistFile :: Maybe FilePath }
    deriving (Eq, Show)

progName :: String
progName = "httpcli"

parseCli :: [(String, String)] -> IO Options
parseCli = execParser . cli

parseCliCommand :: [String] -> [(String, String)] -> ParserResult Options
parseCliCommand argv environ = execParserPure parserPrefs (cli environ) argv
  where parserPrefs = prefs $ showHelpOnEmpty <> showHelpOnError

cli :: [(String, String)] -> ParserInfo Options
cli environ = info
  (cliOptions environ <**> helper)
  (fullDesc <> header
    (progName ++ " - a command-line HTTP client for API development")
  )

cliOptions :: [(String, String)] -> Parser Options
cliOptions environ = do
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
              (  metavar "ITEM"
              <> help
                   "the name of a group, request, or request attribute in the API spec"
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
    , ( "repl"
      , "start an interactive prompt"
      , CmdRepl <$> option
        maybeStr
        (  long "histfile"
        <> short 'H'
        <> value (Just "")
        <> help
             "File where command history is saved.\
            \ Pass an empty value (e.g. \"\") to disable this feature."
        <> showDefaultWith
             (const $ joinPath ["$XDG_CACHE_HOME", progName, "history"])
        )
      )
    ]
  optApiFile <- option
    nonEmptyStr
    (long "api" <> short 'a' <> envvar "HTTPCLI_API" environ <> help
      "path to an API spec file"
    )
  optEnvFile <- optional $ option
    nonEmptyStr
    (long "env" <> short 'e' <> envvar "HTTPCLI_ENV" environ <> help
      "path to an Environment file"
    )
  optSave <- switch
    (long "save" <> short 's' <> help
      "save changes made to the Environment from request scripts"
    )
  pure Options { .. }
 where
  mkCommand (name, desc, parser) =
    command name (info (parser <**> helper) (progDesc desc))

nonEmptyStr :: IsString s => ReadM s
nonEmptyStr = eitherReader $ \case
  "" -> Left "invalid argument: empty string"
  s  -> Right $ fromString s

maybeStr :: IsString s => ReadM (Maybe s)
maybeStr = strToMaybe <$> readerAsk
 where
  strToMaybe "" = Nothing
  strToMaybe s  = Just (fromString s)

envvar :: (HasValue f, IsString a) => String -> [(String, String)] -> Mod f a
envvar key environ = maybe idm (value . fromString) $ lookup key environ
