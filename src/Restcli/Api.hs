{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Restcli.Api where

import           Control.Monad                  ( foldM )
import           Data.Aeson
import qualified Data.HashMap.Strict.InsOrd    as Map
import           Data.List                      ( intercalate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.Yaml                     as Yaml
import           System.FilePath                ( splitFileName )
import           Text.Mustache
import           Text.Parsec.Error              ( ParseError )
import           Text.Read                      ( readMaybe )

import           Restcli.Data.Decoding
import           Restcli.Data.Encoding
import           Restcli.Error
import           Restcli.Types
import           Restcli.Utils                  ( snoc
                                                , unsnoc
                                                )

parseAPI :: Template -> Env -> Either Error API
parseAPI tmpl (Env env) =
    let rendered = encodeUtf8 . substitute tmpl . Map.toHashMap $ env
        parsed   = Yaml.decodeEither' rendered :: YamlParser API
    in  case parsed of
            Left  err -> Left $ YamlError err `WithMsg` "Error parsing API"
            Right api -> return api

getApiComponent
    :: [Text] -> APIComponentKind -> API -> Either Error APIComponent
getApiComponent keys GroupKind api = APIGroup <$> getApiGroup keys api
getApiComponent keys kind api
    | length keys < 2
    = Left $ APILookupError keys kind Nothing
    | otherwise
    = let (groupKeys, reqKey) = unsnoc keys
      in  case kind of
              RequestKind -> APIRequest <$> getApiRequest groupKeys reqKey api
              RequestAttrKind attr ->
                  APIRequestAttr <$> getApiRequestAttr groupKeys reqKey attr api

getApiComponent' :: [Text] -> API -> Either Error APIComponent
getApiComponent' keys (API api) = fst <$> foldM f (APIGroup api, []) keys
  where
    f (APIGroup reqGroup, ks) k =
        let ks' = ks `snoc` k
        in  case Map.lookup k reqGroup of
                Just (ReqGroup group) -> Right (APIGroup group, ks')
                Just (Req      req  ) -> Right (APIRequest req, ks')
                Nothing               -> error' ks' GroupKind
    f (APIRequest req, ks) k =
        let ks' = ks ++ [k]
        in  case readMaybe (T.unpack k) :: Maybe RequestAttrKind of
                Just attr ->
                    Right (APIRequestAttr $ getRequestAttr attr req, ks')
                -- TODO: find a way to represent a not-found RequestAttr
                Nothing -> error' ks' RequestKind
    error' ks kind = Left $ APILookupError ks kind Nothing


getApiGroup :: [Text] -> API -> Either Error ReqGroup
getApiGroup []   (API api) = Right api
getApiGroup keys (API api) = fst <$> foldM f (api, []) keys
  where
    f (reqGroup, ks) k =
        let ks'    = ks `snoc` k
            error' = Left . APILookupError ks' GroupKind
        in  case Map.lookup k reqGroup of
                Just (ReqGroup group) -> Right (group, ks')
                Just (Req      req  ) -> error' $ Just RequestKind
                Nothing               -> error' Nothing

getApiRequest :: [Text] -> Text -> API -> Either Error Request
getApiRequest groupKeys reqKey api = case getApiGroup groupKeys api of
    Right group -> case Map.lookup reqKey group of
        Just (Req      req  ) -> Right req
        Just (ReqGroup group) -> error' $ Just GroupKind
        Nothing               -> error' Nothing
    Left APILookupError{} -> error' Nothing
  where
    error' = Left . APILookupError keys RequestKind
    keys   = groupKeys `snoc` reqKey

getApiRequestAttr
    :: [Text] -> Text -> RequestAttrKind -> API -> Either Error RequestAttr
getApiRequestAttr groupKeys reqKey attr api =
    case getApiRequest groupKeys reqKey api of
        Right req -> Right . getRequestAttr attr $ req
        Left APILookupError{} ->
            Left $ APILookupError keys (RequestAttrKind attr) Nothing
    where keys = groupKeys ++ [reqKey, T.pack $ show attr]

readApiTemplate :: FilePath -> IO Template
readApiTemplate path = do
    let (apiDir, apiFileName) = splitFileName path
    compiled <- automaticCompile [apiDir] apiFileName
    case compiled of
        Left err ->
            errorFail $ TemplateError err `WithMsg` "failed to compile API"
        Right tmpl -> return tmpl

lookupEnv :: Text -> Env -> Either Error Value
lookupEnv key (Env env) = case Map.lookup key env of
    Just val -> Right val
    Nothing  -> Left $ EnvLookupError key

insertEnv :: Text -> Value -> Env -> Either Error Env
insertEnv key value (Env env)
    | Map.member key env = Right . Env $ Map.insert key value env
    | otherwise          = Left $ EnvLookupError key

readEnv :: FilePath -> IO Env
readEnv path = do
    decoded <- Yaml.decodeFileEither path
    case decoded of
        Left  err -> errorFail $ YamlError err `WithMsg` "failed to parse Env"
        Right env -> return env

saveEnv :: FilePath -> Env -> IO ()
saveEnv = Yaml.encodeFile
