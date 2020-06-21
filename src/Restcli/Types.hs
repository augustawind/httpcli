module Restcli.Types where

import qualified Data.Aeson                    as Aeson
import           Data.HashMap.Strict            ( HashMap )
import           Data.Text                      ( Text )
import qualified Data.Yaml                     as Yaml
import qualified Network.HTTP.Types            as HTTP
import           Text.URI                       ( URI(..) )

newtype API = API (HashMap Text ReqNode)
    deriving (Eq, Show)

data ReqNode = Req Request | ReqGroup (HashMap Text ReqNode)
    deriving (Eq, Show)

data Request = Request
    { reqMethod :: HTTP.StdMethod
    , reqUrl :: URI
    , reqQuery :: HTTP.QueryText
    , reqHeaders :: HTTP.RequestHeaders
    , reqBody :: RequestBody
    } deriving (Eq, Show)



data RequestBody
    = NoReqBody
    | ReqBodyJson Aeson.Value
    deriving (Eq, Show)

type YamlParser = Either Yaml.ParseException
