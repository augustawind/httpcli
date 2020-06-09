{-# LANGUAGE GADTs #-}
module Restcli.Api where

import           Data.Aeson
import           Data.Map                       ( Map )
import qualified Data.Yaml                     as Yaml
import           Network.HTTP.Req               ( HttpBody )
import           Network.HTTP.Types
import           System.FilePath                ( splitFileName )
import           Text.Mustache
import           Text.Parsec.Error              ( ParseError )
import           Text.URI                       ( URI(..) )

readApiTemplate :: FilePath -> IO Template
readApiTemplate path = do
    let (apiDir, apiFileName) = splitFileName path
    compiled <- automaticCompile [apiDir] apiFileName
    case compiled of
        Left  err  -> error (show err)
        Right tmpl -> return tmpl

readEnv :: FilePath -> IO Env
readEnv path = do
    decoded <- Yaml.decodeFileEither path
    case decoded of
        Left  err -> error (show err)
        Right env -> return env
