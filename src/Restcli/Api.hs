module Restcli.Api where

import           Data.Map                       ( Map )
import qualified Data.Yaml                     as Yaml
import           System.FilePath                ( splitFileName )
import           Text.Mustache
import           Text.Parsec.Error              ( ParseError )

readApiFile :: FilePath -> IO Template
readApiFile path = do
    let (apiDir, apiFileName) = splitFileName path
    compiled <- automaticCompile [apiDir] apiFileName
    case compiled of
        Left  err  -> error (show err)
        Right tmpl -> return tmpl

type Env = Map String Yaml.Value

readEnvFile :: FilePath -> IO Env
readEnvFile path = do
    decoded <- Yaml.decodeFileEither path
    case decoded of
        Left  err -> error (show err)
        Right env -> return env
