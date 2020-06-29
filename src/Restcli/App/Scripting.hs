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
import qualified Foreign.Lua                   as Lua
import           Foreign.Lua.Aeson

import           Restcli.Types

runScript :: Text -> HttpResponse -> Env -> IO (Maybe Env)
runScript script resp env = liftIO . Lua.run $ do
    Lua.openlibs
    Lua.push (mkScriptContext resp env) *> Lua.setglobal' "ctx"

    result <- Lua.dostring (encodeUtf8 script)
    when (result /= Lua.OK) $ Lua.peek 1 >>= liftIO . fail

    Lua.getglobal "ctx"
    ctx <- Lua.peek =<< Lua.gettop :: Lua.Lua (HashMap String Value)
    return $ case Map.lookup "env" ctx of
        Just (Object hm) -> Just $ Env (OrdMap.fromHashMap hm)
        _                -> Nothing

mkScriptContext :: HttpResponse -> Env -> HashMap String Value
mkScriptContext HttpResponse {..} (Env env) = Map.fromList
    [ ( "response"
      , object
          [ "version" .= (String . T.pack . show $ resHttpVersion)
          , "status_code" .= (Number . fromIntegral $ resStatusCode)
          , "status" .= String
              (T.unwords
                  [ T.pack . show $ resStatusCode
                  , decodeUtf8 . LB.toStrict $ resStatusText
                  ]
              )
          , "headers"
              .= toJSON
                     (map
                         (\(k, v) -> (B.unpack $ CI.foldedCase k, B.unpack v))
                         resHeaders
                     )
          -- TODO: add error handling (change func sig to App, or maybe just Either)
          , "body" .= (either error id (eitherDecode' resBody) :: Value)
          ]
      )
    , ("env", Object $ OrdMap.toHashMap env)
    ]
