{-# LANGUAGE RecordWildCards #-}

module Restcli.Requests where

import           Control.Lens
import           Data.Aeson                     ( toJSON )
import qualified Data.ByteString.Lazy.Char8    as LB
import           Data.Maybe                     ( maybeToList )
import           Network.HTTP.Client            ( responseVersion )
import qualified Network.HTTP.Types            as HTTP
import           Network.Wreq
import           Text.URI                       ( URI )
import qualified Text.URI                      as URI

import           Restcli.Error
import           Restcli.Types

sendRequest :: HttpRequest -> IO HttpResponse
sendRequest r = toHttpResponse <$> case body of
  Just payload -> customPayloadMethodWith method options url payload
  Nothing      -> customMethodWith method options url
 where
  body    = runRequestBody <$> reqBody r
  method  = show $ reqMethod r
  url     = URI.renderStr $ reqUrl r
  options = defaults & query & headers'
  query   = case reqQuery r of
    Nothing         -> id
    Just (Query qs) -> \opts -> foldl f opts qs
    where f opts (k, v) = opts & param k .~ maybeToList v
  headers' = case reqHeaders r of
    Nothing           -> id
    Just (Headers hs) -> \opts -> foldl f opts hs
    where f opts (k, v) = opts & header k .~ [v]

toHttpResponse :: Response LB.ByteString -> HttpResponse
toHttpResponse r = HttpResponse { .. }
 where
  resHttpVersion = responseVersion r
  resStatusCode  = r ^. responseStatus . statusCode
  resStatusText  = LB.fromStrict $ r ^. responseStatus . statusMessage
  resHeaders     = r ^. responseHeaders
  resBody        = r ^. responseBody
