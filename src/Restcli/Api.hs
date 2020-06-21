{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Restcli.Api where

import           Data.Aeson
import           Data.Either
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.List                      ( intercalate )
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.Yaml                     as Yaml
import           Network.HTTP.Req               ( HttpBody(..) )
import qualified Network.HTTP.Req              as Req
import           System.FilePath                ( splitFileName )
import           Text.Mustache
import           Text.Parsec.Error              ( ParseError )

import           Restcli.Internal.Decodings
import           Restcli.Types

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

parseAPI :: Template -> Env -> Either String API
parseAPI tmpl env =
    let rendered = encodeUtf8 $ substitute tmpl env
        decoded =
                Yaml.decodeEither' rendered :: Either
                        Yaml.ParseException
                        (HashMap Text Yaml.Value)
    in  case decoded of
            Left  err -> Left $ show err
            Right val -> API <$> mapM parseNode val

parseNode :: Value -> Either String ReqNode
parseNode (Object kvs)
    | -- If any keys in the object are reqKeys, treat it as a Request.
      any (`Map.member` kvs) reqKeys = Req <$> parseRequest kvs
    | -- Otherwise, it's treated as another ReqGroup.
      otherwise                      = ReqGroup <$> mapM parseNode kvs
parseNode value =
    error $ "unexpected node found while parsing API file: " ++ show value

parseRequest :: HashMap Text Value -> Either String Request
parseRequest kvs
    | not . null $ missingKeys
    = Left $ "required keys missing from request: " ++ show missingKeys
    | not . null $ extraKeys
    = Left $ "invalid keys found in request: " ++ show extraKeys
    | otherwise
    -- = Right . fromJust . decode . encode $ kvs
    = let encoded = Yaml.encode kvs
          decoded =
                  Yaml.decodeEither' encoded :: Either Yaml.ParseException Request
      in  case decoded of
              Left  err -> Left $ show err
              Right req -> Right req
  where
    missingKeys = filter (not . (`Map.member` kvs)) requiredReqKeys
    extraKeys   = filter (not . (`elem` reqKeys)) (Map.keys kvs)

reqKeys :: [Text]
reqKeys = requiredReqKeys ++ ["headers", "query", "json", "file"]

requiredReqKeys :: [Text]
requiredReqKeys = ["url", "method"]

type Env = HashMap Text Yaml.Value

readEnv :: FilePath -> IO Env
readEnv path = do
    decoded <- Yaml.decodeFileEither path
    case decoded of
        Left  err -> error (show err)
        Right env -> return env
