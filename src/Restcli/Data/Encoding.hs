{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restcli.Data.Encoding where

import           Data.Aeson
import           Data.Aeson.Types               ( Parser )
import qualified Data.CaseInsensitive          as CI
import           Data.Char                      ( toLower
                                                , toTitle
                                                )
import qualified Data.HashMap.Strict           as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as OrdMap
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Text.Lazy.Encoding       as TL
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.Yaml                     as Yaml
import qualified Network.HTTP.Types            as HTTP
import           Text.URI                       ( URI )
import qualified Text.URI                      as URI

import           Restcli.Data.Common
import           Restcli.Types

instance ToJSON API where
    toJSON (API api) = toJSON . ReqGroup $ api

instance ToJSON ReqNode where
    toJSON (ReqGroup group) = ordMapToJSON group
    toJSON (Req      req  ) = toJSON req

instance ToJSON Env where
    toJSON (Env env) = ordMapToJSON env

ordMapToJSON :: (ToJSON v) => InsOrdHashMap Text v -> Value
ordMapToJSON = Array . OrdMap.foldrWithKey f V.empty
    where f k v = V.cons $ Object (Map.singleton k (toJSON v))

instance ToJSON HttpRequest where
    toJSON     = genericToJSON aesonRequestOptions
    toEncoding = genericToEncoding aesonRequestOptions

instance ToJSON HTTP.StdMethod where
    toJSON = toJSON . show

instance ToJSON URI where
    toJSON = toJSON . URI.render

instance ToJSON RequestQuery where
    toJSON (RequestQuery query) =
        toJSONList $ map (uncurry OrdMap.singleton) query

instance ToJSON RequestHeaders where
    toJSON (RequestHeaders headers) = String . encodeHeaders $ headers

instance ToJSON RequestBody where
    toJSON (RequestBody body) = String . decodeUtf8 . Yaml.encode $ body

instance ToJSON RequestAttr where
    toJSON (ReqMethod  method ) = toJSON method
    toJSON (ReqUrl     url    ) = toJSON url
    toJSON (ReqQuery   query  ) = toJSON query
    toJSON (ReqHeaders headers) = toJSON headers
    toJSON (ReqBody    body   ) = toJSON body

instance ToJSON HttpResponse where
    toJSON HttpResponse {..} = object
        [ "http_version" .= show resHttpVersion
        , "status" .= T.unwords [T.pack . show $ resStatusCode, resStatusText]
        , "status_code" .= resStatusCode
        , "headers" .= encodeHeaders resHeaders
        , "body" .= TL.decodeUtf8 resBody
        ]

encodeHeaders :: [HTTP.Header] -> Text
encodeHeaders = T.unlines . map encodeHeader
  where
    encodeHeader (name, value) =
        let nameParts = T.split (== '-') . decodeUtf8 $ CI.original name
            name'     = T.intercalate "-" $ map toTitleCase nameParts
        in  T.concat [name', ": ", decodeUtf8 value]
    toTitleCase word = case T.uncons word of
        Just (x, xs) -> toTitle x `T.cons` T.map toLower xs
        Nothing      -> word
