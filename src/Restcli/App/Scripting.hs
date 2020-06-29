{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restcli.App.Scripting where

import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson              hiding ( Options )
import           Data.Bifunctor                 ( second )
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as LB
import qualified Data.CaseInsensitive          as CI
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as OrdMap
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Foreign.Lua                    ( Lua
                                                , Pushable
                                                , push
                                                )
import qualified Foreign.Lua                   as Lua
import           Foreign.Lua.Aeson
import qualified Network.HTTP.Types            as HTTP
import qualified Text.URI                      as URI

import           Restcli.Types

runScript :: Text -> HttpRequest -> HttpResponse -> Env -> IO (Maybe Env)
runScript script req res env = liftIO . Lua.run $ do
    Lua.openlibs
    Lua.push (Context req res env) *> Lua.setglobal' "ctx"

    result <- Lua.dostring (encodeUtf8 script)
    when (result /= Lua.OK) $ Lua.peek 1 >>= liftIO . fail

    Lua.getglobal "ctx"
    ctx <- Lua.peek =<< Lua.gettop :: Lua.Lua (HashMap String Value)
    return $ case Map.lookup "env" ctx of
        Just (Object hm) -> Just $ Env (OrdMap.fromHashMap hm)
        _                -> Nothing

data Context = Context
    { ctxRequest :: HttpRequest
    , ctxResponse :: HttpResponse
    , ctxEnv :: Env
    } deriving (Eq, Show)

instance Pushable Context where
    push Context {..} = withTable $ do
        "request" ~> ctxRequest
        "response" ~> ctxResponse
        "env" ~> ctxEnv

instance Pushable HttpRequest where
    push HttpRequest {..} = withTable $ do
        "method" ~> show reqMethod
        "url" ~> URI.render reqUrl
        "query" ~> case reqQuery of
            Nothing -> Map.empty
            Just (Query query) ->
                Map.fromListWith (++) . map (second maybeToList) $ query
        "headers" ~> case reqHeaders of
            Nothing                -> Map.empty
            Just (Headers headers) -> mkHeaderMap headers
        "body" ~> case reqBody of
            Nothing                 -> Null
            Just (RequestBody body) -> body
        "script" ~> fromMaybe "" reqScript

instance Pushable HttpResponse where
    push HttpResponse {..} = withTable $ do
        "version" ~> show resHttpVersion
        "status_code" ~> resStatusCode
        "status" ~> join " " (show resStatusCode) (LB.unpack resStatusText)
        "headers" ~> mkHeaderMap resHeaders
        "body" ~> (either error id (eitherDecode' resBody) :: Value)
        where join sep a b = a <> sep <> b

mkHeaderMap :: HTTP.RequestHeaders -> HashMap String String
mkHeaderMap = Map.fromListWith (\a b -> b <> ", " <> a) . map unpackHeader
    where unpackHeader (k, v) = (B.unpack (CI.foldedCase k), B.unpack v)

instance Pushable Env where
    push (Env hm) = withTable $ mapM_ (uncurry (~>)) (OrdMap.toList hm)

withTable :: Lua a -> Lua a
withTable = (Lua.newtable >>)

(~>) :: Pushable a => Text -> a -> Lua ()
k ~> v = push k *> push v *> Lua.rawset (-3)
infixr 2 ~>
