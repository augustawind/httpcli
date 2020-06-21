{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restcli.Internal.Decodings
    ()
where

import           Data.Aeson
import           Data.Aeson.Types               ( Parser )
import qualified Data.ByteString.Char8         as C
import           Data.Char                      ( toUpper )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Void
import qualified Data.Yaml                     as Yaml
import           Text.Read                      ( readEither )
import           Text.URI                       ( URI )
import qualified Text.URI                      as URI
import           Text.Megaparsec                ( Parsec
                                                , runParser
                                                )

import           Restcli.Internal.ParseHeaders  ( parseHeaders )
import           Restcli.Types

instance FromJSON Request where
    parseJSON (Object v) = do
        reqUrl <- do
            src <- v .: "url" :: Parser Text
            case runParser (URI.parser :: Parsec Void Text URI) "" src of
                Left  err -> errReqField (T.unpack src) "url" (show err)
                Right val -> return val

        reqMethod <- do
            -- v .: "method" >>= either fail return . readEither . T.unpack
            src <- map toUpper <$> v .: "method" :: Parser String
            case readEither src of
                Left  err    -> errReqField src "method" ""
                Right method -> return method

        reqHeaders <- do
            src <- v .:? "headers" .!= "" :: Parser String
            -- TODO: write a new, much more lenient parser that auto-escapes bad chars,
            -- auto-quotes header values, and allows multiple newlines between headers.
            case runParser parseHeaders "" $ C.pack src of
                Left  err -> errReqField src "headers" (show err)
                Right val -> return val

        -- TODO: allow for no-value keys
        reqQuery <- do
            src <- v .:? "query" .!= [] :: Parser [HashMap Text (Maybe Text)]
            return . concatMap Map.toList $ src

        -- TODO: allow for other body types
        reqBody <- do
            src <- v .:? "json" .!= "" :: Parser String
            case Yaml.decodeEither' (C.pack src) :: YamlParser Value of
                Left  err -> errReqField src "json" (show err)
                Right val -> return . ReqBodyJson $ val

        return Request { .. }

errReqField :: String -> String -> String -> Parser a
errReqField actual field msg = fail $ before ++ base ++ after
  where
    base   = "invalid value for request `" ++ field ++ "`"
    before = if null actual then "" else "'" ++ actual ++ "': "
    after  = if null msg then "" else ": " ++ msg
