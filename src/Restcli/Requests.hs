{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restcli.Requests where

import           Control.Lens
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as LB
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP
import           Network.Wreq
import           Text.URI                       ( URI )
import qualified Text.URI                      as URI

import           Restcli.Error
import           Restcli.Types

sendRequest :: HttpRequest -> IO HttpResponse
sendRequest HttpRequest {..} = toHttpResponse <$> case body of
  Just payload -> customPayloadMethodWith method options url payload
  Nothing      -> customMethodWith method options url
 where
  body    = unRequestBody <$> reqBody
  method  = show reqMethod
  url     = URI.renderStr reqUrl
  options = defaults & query & headers'
  query   = case reqQuery of
    Nothing                -> id
    Just (RequestQuery qs) -> \opts -> foldl f opts qs
    where f opts (k, v) = opts & param k .~ maybeToList v
  headers' = case reqHeaders of
    Nothing                  -> id
    Just (RequestHeaders hs) -> \opts -> foldl f opts hs
    where f opts (k, v) = opts & header k .~ [v]

toHttpResponse :: Response LB.ByteString -> HttpResponse
toHttpResponse r = HttpResponse { .. }
 where
  resHttpVersion = HTTP.responseVersion r
  resStatusCode  = r ^. responseStatus . statusCode
  resStatusText  = decodeUtf8 $ r ^. responseStatus . statusMessage
  resHeaders     = r ^. responseHeaders
  resBody        = r ^. responseBody
  resJSON
    | hasContentTypeJSONHeader resHeaders = Just $ do
      r' <- asValue r
      Right $ r' ^. responseBody
    | otherwise = Nothing

hasContentTypeJSONHeader :: [HTTP.Header] -> Bool
hasContentTypeJSONHeader headers =
  let contentType =
          fst
            . B.break (== ';')
            . fromMaybe "unknown"
            . lookup "Content-Type"
            $ headers
  in  ("application/json" `B.isPrefixOf` contentType)
        || (              "application/"
           `B.isPrefixOf` contentType
           &&             "+json"
           `B.isSuffixOf` contentType
           )
