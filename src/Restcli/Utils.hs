module Restcli.Utils where

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

unsnoc :: [a] -> ([a], a)
unsnoc [] = error "empty list"
unsnoc xs = (init xs, last xs)
