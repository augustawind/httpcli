module Restcli.Internal.Common where

import           Data.Aeson.Types
import           Data.Maybe                     ( fromJust )

aesonRequestOptions :: Options
aesonRequestOptions = defaultOptions
    { fieldLabelModifier = fromJust . (`lookup` fieldNames)
    , omitNothingFields  = True
    }
  where
    fieldNames =
        [ ("reqMethod" , "method")
        , ("reqUrl"    , "url")
        , ("reqQuery"  , "query")
        , ("reqHeaders", "headers")
        , ("reqBody"   , "json")
        ]
