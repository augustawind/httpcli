{-# LANGUAGE OverloadedStrings #-}

module Restcli.Data.Decoding.Headers where

import qualified Data.ByteString.Char8         as B
import qualified Data.CaseInsensitive          as CI
import           Data.Char                      ( ord )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Data.Word                      ( Word8 )
import qualified Network.HTTP.Types            as HTTP
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void Text

parseHeaders :: Parser HTTP.RequestHeaders
parseHeaders = sepEndBy parseHeader eol

parseHeader :: Parser HTTP.Header
parseHeader = do
  name  <- parseHeaderName
  value <- char ':' >> many nbsp >> parseHeaderValue
  return (name, value)

parseHeaderName :: Parser HTTP.HeaderName
parseHeaderName = CI.mk . B.pack <$> some fieldChar <?> "a header field name"

parseHeaderValue :: Parser B.ByteString
parseHeaderValue = B.pack <$> (quoted <|> unquoted) <?> "a header value"
 where
  unquoted =
    let p = (:) <$> some valueChar <*> many (try $ some nbsp >> some valueChar)
    in  concat <$> p
  quoted = do
    lq    <- char '"'
    inner <- concat <$> many quotedChar
    rq    <- T.unpack <$> string "\""
    return $ lq : inner ++ rq
   where
    quotedChar     = try escapeSequence <|> ((: []) <$> anySingleBut '"')
    escapeSequence = sequence [char '\\', anySingle]

fieldChar :: Parser Char
fieldChar = alphaNumChar <|> oneOf ("!#$%&'*+-.^_`|~" :: String)

valueChar :: Parser Char
valueChar = fieldChar <|> oneOf delimiters

delimiters :: String
delimiters = "\"(),/:;<=>?@[\\]{}"

nbsp :: Parser Char
nbsp = oneOf (" \t" :: String)
