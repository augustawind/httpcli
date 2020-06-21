{-# LANGUAGE DeriveGeneric #-}

module Restcli.Types where

import qualified Data.Aeson                    as Aeson
import           Data.HashMap.Strict            ( HashMap )
import           Data.Text                      ( Text )
import qualified Data.Yaml                     as Yaml
import           GHC.Generics                   ( Generic )
import qualified Network.HTTP.Types            as HTTP
import           Text.URI                       ( URI(..) )

newtype API = API ReqGroup
    deriving (Generic, Eq, Show)

data ReqNode = Req Request | ReqGroup ReqGroup
    deriving (Eq, Show)

type ReqGroup = HashMap Text ReqNode

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

data RequestAttr
    = ReqMethod HTTP.StdMethod
    | ReqUrl URI
    | ReqQuery (Maybe RequestQuery)
    | ReqHeaders (Maybe RequestHeaders)
    | ReqBody (Maybe RequestBody)
    deriving (Eq, Show)

data APIComponent
    = GroupT
    | RequestT
    | RequestAttrT RequestComponent
    deriving (Eq)

data RequestComponent
    = ReqMethodT
    | ReqUrlT
    | ReqQueryT
    | ReqHeadersT
    | ReqBodyT
    deriving (Eq)

instance Show APIComponent where
    show GroupT               = "Group"
    show RequestT             = "Request"
    show (RequestAttrT attrT) = "Request '" ++ show attrT ++ "'"

instance Show RequestComponent where
    show ReqMethodT  = "method"
    show ReqUrlT     = "url"
    show ReqQueryT   = "query"
    show ReqHeadersT = "headers"
    show ReqBodyT    = "json"

type Env = HashMap Text Yaml.Value

type YamlParser = Either Yaml.ParseException
