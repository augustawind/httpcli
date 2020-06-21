{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Restcli.Api where

import           Data.Aeson
import           Data.Either
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.List                      ( intercalate )
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.Yaml                     as Yaml
import           Network.HTTP.Req               ( HttpBody(..) )
import qualified Network.HTTP.Req              as Req
import           System.FilePath                ( splitFileName )
import           Text.Mustache
import           Text.Parsec.Error              ( ParseError )

import           Restcli.Internal.Decodings
import           Restcli.Types

parseAPI :: Template -> Env -> Either String API
parseAPI tmpl env =
    let rendered = encodeUtf8 $ substitute tmpl env
        parsed   = Yaml.decodeEither' rendered :: YamlParser API
    in  either (Left . show) return parsed

instance HttpBody (Maybe RequestBody) where
    getRequestBody Nothing                = getRequestBody Req.NoReqBody
    getRequestBody (Just (ReqBodyJson v)) = getRequestBody $ Req.ReqBodyJson v

    getRequestContentType Nothing = getRequestContentType Req.NoReqBody
    getRequestContentType (Just (ReqBodyJson v)) =
        getRequestContentType $ Req.ReqBodyJson v

readApiTemplate :: FilePath -> IO Template
readApiTemplate path = do
    let (apiDir, apiFileName) = splitFileName path
    compiled <- automaticCompile [apiDir] apiFileName
    either (error . show) return compiled

readEnv :: FilePath -> IO Env
readEnv path = do
    decoded <- Yaml.decodeFileEither path
    either (error . show) return decoded
