{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restcli.App.Scripting where

import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson              hiding ( Options )
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as LB
import qualified Data.CaseInsensitive          as CI
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as OrdMap
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

import           Restcli.Types

runScript :: Text -> HttpResponse -> Env -> IO (Maybe Env)
runScript script resp env = liftIO . Lua.run $ do
    Lua.openlibs
    Lua.push (Context resp env) *> Lua.setglobal' "ctx"

    result <- Lua.dostring (encodeUtf8 script)
    when (result /= Lua.OK) $ Lua.peek 1 >>= liftIO . fail

    Lua.getglobal "ctx"
    ctx <- Lua.peek =<< Lua.gettop :: Lua.Lua (HashMap String Value)
    return $ case Map.lookup "env" ctx of
        Just (Object hm) -> Just $ Env (OrdMap.fromHashMap hm)
        _                -> Nothing

data Context = Context
    { ctxResponse :: HttpResponse
    , ctxEnv :: Env
    } deriving (Eq, Show)

instance Pushable Context where
    push Context {..} = withTable $ do
        "response" ~> ctxResponse
        "env" ~> ctxEnv

instance Pushable HttpResponse where
    push HttpResponse {..} = withTable $ do
        "version" ~> show resHttpVersion
        "status_code" ~> resStatusCode
        "status" ~> show resStatusCode ++ LB.unpack resStatusText
        "headers"
            ~> map (\(k, v) -> (B.unpack $ CI.foldedCase k, B.unpack v))
                   resHeaders
        "body" ~> (either error id (eitherDecode' resBody) :: Value)

instance Pushable Env where
    push (Env hm) = withTable $ mapM_ (uncurry (~>)) (OrdMap.toList hm)

withTable :: Lua a -> Lua a
withTable = (Lua.newtable >>)

(~>) :: Pushable a => Text -> a -> Lua ()
k ~> v = push k *> push v *> Lua.rawset (-3)
infixr 2 ~>
