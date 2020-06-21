module Restcli.Types where

import qualified Data.Aeson                    as Aeson
import           Data.HashMap.Strict            ( HashMap )
import           Data.Text                      ( Text )
import qualified Network.HTTP.Types            as HTTP
import           Text.URI                       ( URI(..) )

newtype API = API (HashMap Text ReqNode)
    deriving (Eq, Show)

data ReqNode = Req Request | ReqGroup (HashMap Text ReqNode)
    deriving (Eq, Show)

data Request = Request
    { reqUrl :: URI
    , reqMethod :: HTTP.StdMethod
    , reqHeaders :: HTTP.RequestHeaders
    , reqQuery :: HTTP.QueryText
    , reqBody :: RequestBody
    } deriving (Eq, Show)

data RequestBody
    = NoReqBody
    | ReqBodyJson Aeson.Value
    | ReqBodyFile FilePath
    deriving (Eq, Show)
