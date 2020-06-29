{-# LANGUAGE OverloadedStrings #-}

module Restcli.Data.Common where

import           Data.Aeson.Types
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )

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
    , ("reqScript" , "script")
    ]

reqKeys :: [Text]
reqKeys = requiredReqKeys ++ ["query", "headers", "json", "script"]

requiredReqKeys :: [Text]
requiredReqKeys = ["method", "url"]
