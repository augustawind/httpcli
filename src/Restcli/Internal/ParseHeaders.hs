{-# LANGUAGE OverloadedStrings #-}

module Restcli.Internal.ParseHeaders where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.CaseInsensitive          as CI
import           Data.Char                      ( ord )
import           Data.Void
import           Data.Word                      ( Word8 )
import qualified Network.HTTP.Types            as HTTP
import           Text.Megaparsec
import           Text.Megaparsec.Byte

type Parser = Parsec Void B.ByteString

parseHeaders :: Parser HTTP.RequestHeaders
parseHeaders = sepEndBy parseHeader eol

parseHeader :: Parser HTTP.Header
parseHeader = do
    name  <- parseHeaderName
    value <- char (word8 ':') >> many nbsp >> parseHeaderValue
    return (name, value)

parseHeaderName :: Parser HTTP.HeaderName
parseHeaderName = CI.mk . B.pack <$> some fieldChar <?> "a header field name"

parseHeaderValue :: Parser B.ByteString
parseHeaderValue = (quoted <|> unquoted) <?> "a header value"
  where
    unquoted =
        let
            p = (:) <$> some valueChar <*> many
                (try $ some nbsp >> some valueChar)
        in  B.concat . map B.pack <$> p
    quoted = do
        lq    <- char (word8 '"')
        inner <- B.concat <$> many quotedChar
        rq    <- string "\""
        return $ lq `B.cons` inner `B.append` rq
      where
        quotedChar =
            try escapeSequence <|> (B.singleton <$> anySingleBut (word8 '"'))
        escapeSequence = B.pack <$> sequence [char (word8 '\\'), anySingle]

fieldChar :: Parser Word8
fieldChar = alphaNumChar <|> oneOf (B.unpack "!#$%&'*+-.^_`|~")

valueChar :: Parser Word8
valueChar = fieldChar <|> oneOf (B.unpack delimiters)

delimiters :: B.ByteString
delimiters = "\"(),/:;<=>?@[\\]{}"

nbsp :: Parser Word8
nbsp = oneOf (B.unpack " \t")

word8 = fromIntegral . ord
