{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Restcli.Utils
  ( snoc
  , unsnoc
  , tokenize
  , whenMaybe
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

tokenize :: String -> [String]
tokenize s = getAllTextMatches (s =~ tokenRegex)
 where
  tokenRegex   = intercalate "|" [word, doubleQuoted, singleQuoted]
  word         = [r|((?:[^"'\s\\]|\\.)(?:[^\s\\]|\\.)*)|]
  doubleQuoted = [r|("(?:[^"\\]|\\.)*")|]
  singleQuoted = [r|('(?:[^'\\]|\\.)*')|]

whenMaybe :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenMaybe Nothing  _ = pure ()
whenMaybe (Just x) f = f x
