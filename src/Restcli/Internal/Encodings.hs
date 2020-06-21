{-# LANGUAGE OverloadedStrings #-}

module Restcli.Internal.Encodings where

import           Data.Aeson
import           Data.Aeson.Types               ( Parser )
import qualified Data.ByteString.Char8         as C
import qualified Data.CaseInsensitive          as CI
import           Data.Char                      ( toLower
                                                , toTitle
                                                )
import qualified Data.HashMap.Strict           as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding
import qualified Data.Yaml                     as Yaml
import qualified Network.HTTP.Types            as HTTP
import qualified Text.URI                      as URI

import           Restcli.Types

instance ToJSON API where
    toJSON (API api) = toJSON . ReqGroup $ api

instance ToJSON ReqNode where
    toJSON (Req      req  ) = toJSON req
    toJSON (ReqGroup group) = object . Map.toList . Map.map toJSON $ group

instance ToJSON Request where
    toJSON r = object
        [ "method" .= show (reqMethod r)
        , "url" .= URI.render (reqUrl r)
        , "query" .= encodeQuery (reqQuery r)
        , "headers" .= encodeHeaders (reqHeaders r)
        , "json" .= toJSON (reqBody r)
        ]

encodeQuery :: HTTP.QueryText -> Value
encodeQuery = toJSONList . map (uncurry Map.singleton)

encodeHeaders :: HTTP.RequestHeaders -> Text
encodeHeaders = T.unlines . map encodeHeader

encodeHeader :: HTTP.Header -> Text
encodeHeader (name, value) = T.concat [name', ": ", decodeUtf8 value]
  where
    nameParts = T.split (== '-') . decodeUtf8 $ CI.original name
    name'     = T.intercalate "-" $ map toTitleCase nameParts
    toTitleCase word = case T.uncons word of
        Just (first, rest) -> toTitle first `T.cons` T.map toLower rest
        Nothing            -> word

instance ToJSON RequestBody where
    toJSON NoReqBody          = Null
    toJSON (ReqBodyJson body) = String . decodeUtf8 . Yaml.encode $ body

