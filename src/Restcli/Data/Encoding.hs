{-# LANGUAGE OverloadedStrings #-}

module Restcli.Data.Encoding where

import           Data.Aeson
import           Data.Aeson.Types               ( Parser )
import qualified Data.CaseInsensitive          as CI
import           Data.Char                      ( toLower
                                                , toTitle
                                                )
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding
import qualified Data.Yaml                     as Yaml
import qualified Network.HTTP.Types            as HTTP
import           Text.URI                       ( URI )
import qualified Text.URI                      as URI

import           Restcli.Data.Common
import           Restcli.Types

instance ToJSON API where
    toJSON (API api) = toJSON . ReqGroup $ api

instance ToJSON ReqNode where
    toJSON (Req      req  ) = toJSON req
    toJSON (ReqGroup group) = object . Map.toList . Map.map toJSON $ group

instance ToJSON Request where
    toJSON     = genericToJSON aesonRequestOptions
    toEncoding = genericToEncoding aesonRequestOptions

instance ToJSON HTTP.StdMethod where
    toJSON = toJSON . show

instance ToJSON URI where
    toJSON = toJSON . URI.render

instance ToJSON RequestQuery where
    toJSON (Query query) = toJSONList . map (uncurry Map.singleton) $ query

instance ToJSON RequestHeaders where
    toJSON (Headers headers) = String . T.unlines . map encodeHeader $ headers
      where
        encodeHeader (name, value) =
            let nameParts = T.split (== '-') . decodeUtf8 $ CI.original name
                name'     = T.intercalate "-" $ map toTitleCase nameParts
            in  T.concat [name', ": ", decodeUtf8 value]

instance ToJSON RequestBody where
    toJSON (ReqBodyJson body) = String . decodeUtf8 . Yaml.encode $ body

toTitleCase :: Text -> Text
toTitleCase word = case T.uncons word of
    Just (first, rest) -> toTitle first `T.cons` T.map toLower rest
    Nothing            -> word