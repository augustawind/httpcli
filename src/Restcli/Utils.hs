{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Restcli.Utils
    ( snoc
    , unsnoc
    , between
    , tokenize
    )
where

import           Data.List                      ( intercalate )
import           Text.Regex.PCRE
import           Text.Regex.PCRE.String
import           Text.RawString.QQ

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

unsnoc :: [a] -> ([a], a)
unsnoc [] = error "empty list"
unsnoc xs = (init xs, last xs)

between :: a -> a -> [a] -> [a]
between l r xs = l : xs ++ [r]

tokenize :: String -> [String]
tokenize s = getAllTextMatches (s =~ tokenRegex)
  where
    tokenRegex  = intercalate "|" . map (between '(' ')') $ parts
    parts       = [unquotedTok, quotedTok, quotedTok']
    unquotedTok = [r|(?:[^"'\s\\]|\\.)(?:[^\s\\]|\\.)*|]
    quotedTok   = [r|"(?:[^"\\]|\\.)*"|]
    quotedTok'  = [r|'(?:[^'\\]|\\.)*'|]
