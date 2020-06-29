{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Text.Encoding
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
    toJSON (Query query) = toJSONList . map (uncurry OrdMap.singleton) $ query

instance ToJSON RequestHeaders where
    toJSON (Headers headers) = String . T.unlines . map encodeHeader $ headers
      where
        encodeHeader (name, value) =
            let nameParts = T.split (== '-') . decodeUtf8 $ CI.original name
                name'     = T.intercalate "-" $ map toTitleCase nameParts
            in  T.concat [name', ": ", decodeUtf8 value]

instance ToJSON RequestBody where
    toJSON (RequestBody body) = String . decodeUtf8 . Yaml.encode $ body

instance ToJSON RequestAttr where
    toJSON (ReqMethod  method ) = toJSON method
    toJSON (ReqUrl     url    ) = toJSON url
    toJSON (ReqQuery   query  ) = toJSON query
    toJSON (ReqHeaders headers) = toJSON headers
    toJSON (ReqBody    body   ) = toJSON body

toTitleCase :: Text -> Text
toTitleCase word = case T.uncons word of
    Just (first, rest) -> toTitle first `T.cons` T.map toLower rest
    Nothing            -> word
