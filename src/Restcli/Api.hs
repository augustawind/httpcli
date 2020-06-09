{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Restcli.Api where

import           Data.Aeson
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.List                      ( intercalate )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Yaml                     as Yaml
import           Network.HTTP.Req               ( HttpBody(..) )
import qualified Network.HTTP.Req              as Req
import           Network.HTTP.Types
import           System.FilePath                ( splitFileName )
import           Text.Mustache
import           Text.Parsec.Error              ( ParseError )
import           Text.URI                       ( URI(..) )

newtype API = API (HashMap Text ReqNode)
    deriving (Eq, Show)

data ReqNode = Req Request | ReqGroup (HashMap Text ReqNode)
    deriving (Eq, Show)

data Request = Request
    { reqUrl :: URI
    , reqMethod :: Method
    , reqHeaders :: RequestHeaders
    , reqQuery :: QueryText
    , reqBody :: RequestBody
    } deriving (Eq, Show)

data RequestBody
    = NoReqBody
    | ReqBodyJson String
    | ReqBodyFile FilePath
    deriving (Eq, Show)

instance HttpBody RequestBody where
    getRequestBody NoReqBody          = getRequestBody Req.NoReqBody
    getRequestBody (ReqBodyJson v   ) = getRequestBody $ Req.ReqBodyJson v
    getRequestBody (ReqBodyFile path) = getRequestBody $ Req.ReqBodyFile path

    getRequestContentType NoReqBody = getRequestContentType Req.NoReqBody
    getRequestContentType (ReqBodyJson v) =
        getRequestContentType $ Req.ReqBodyJson v
    getRequestContentType (ReqBodyFile path) =
        getRequestContentType $ Req.ReqBodyFile path

readApiTemplate :: FilePath -> IO Template
readApiTemplate path = do
    let (apiDir, apiFileName) = splitFileName path
    compiled <- automaticCompile [apiDir] apiFileName
    case compiled of
        Left  err  -> error (show err)
        Right tmpl -> return tmpl

parseAPI :: Template -> Env -> API
parseAPI tmpl env =
    let rendered = encodeUtf8 $ substitute tmpl env
        decoded =
                Yaml.decodeEither' rendered :: Either
                        Yaml.ParseException
                        (HashMap Text Yaml.Value)
    in  case decoded of
            Left  err -> error (show err)
            Right val -> API $ Map.map parseNode val

requestKeys :: [Text]
requestKeys = ["url", "method", "headers", "query", "json", "file"]

parseNode :: Value -> ReqNode
parseNode (Object kvs)
    | any (`Map.member` kvs) requestKeys = Req $ parseRequest kvs
    | otherwise                          = ReqGroup $ Map.map parseNode kvs
parseNode value = error "this should never happen"

parseRequest :: HashMap Text Value -> Request
parseRequest = undefined

type Env = HashMap Text Yaml.Value

readEnv :: FilePath -> IO Env
readEnv path = do
    decoded <- Yaml.decodeFileEither path
    case decoded of
        Left  err -> error (show err)
        Right env -> return env
