{-# LANGUAGE DeriveGeneric #-}

module Restcli.Types where

import qualified Data.Aeson                    as Aeson
import           Data.HashMap.Strict            ( HashMap )
import           Data.Text                      ( Text )
import qualified Data.Yaml                     as Yaml
import           GHC.Generics                   ( Generic )
import qualified Network.HTTP.Types            as HTTP
import           Text.URI                       ( URI(..) )

newtype API = API (HashMap Text ReqNode)
    deriving (Generic, Eq, Show)

data ReqNode = Req Request | ReqGroup (HashMap Text ReqNode)
    deriving (Eq, Show)

data Request = Request
    { reqMethod :: HTTP.StdMethod
    , reqUrl :: URI
    , reqQuery :: Maybe RequestQuery
    , reqHeaders :: Maybe RequestHeaders
    , reqBody :: Maybe RequestBody
    } deriving (Generic, Eq, Show)

newtype RequestQuery = Query HTTP.QueryText deriving (Eq, Show)

newtype RequestHeaders = Headers HTTP.RequestHeaders deriving (Eq, Show)

newtype RequestBody = ReqBodyJson Aeson.Value
    deriving (Eq, Show)

type YamlParser = Either Yaml.ParseException

type Env = HashMap Text Yaml.Value
