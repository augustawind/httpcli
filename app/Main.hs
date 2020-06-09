{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson                    as Aeson
import qualified Data.Map                      as Map
import           Data.Text                      ( unpack )
import           System.FilePath                ( (</>) )
import           Text.Mustache

import           Restcli.Api
import           Restcli.Cli

main :: IO ()
main = do
        -- Parse command line options.
        opts <- runCli

        -- Compile API template.
        tmpl <- readApiTemplate $ apiFile opts
        putStrLn $ "\n" ++ replicate 25 '-' ++ "\nTEMPLATE\n"
        print tmpl

        -- Parse Env file if given, otherwise use a blank env.
        env <- case envFile opts of
                Just path -> readEnv path
                Nothing   -> return Map.empty
        putStrLn $ "\n" ++ replicate 25 '-' ++ "\nENV\n"
        print env

        let text = substitute tmpl env
        putStrLn $ "\n" ++ replicate 25 '-' ++ "\nTEXT\n"
        putStrLn $ unpack text

        return ()

test :: IO ()
test = do
        let     searchSpace  = [".", "./examples"]
                templateName = "example.mustache"

        compiled <- automaticCompile searchSpace templateName

        let tmpl = case compiled of
                    Left  err -> error (show err)
                    Right t   -> t

        print tmpl

        let text = substitute tmpl $ object
                    [ "name" ~> ("Chris" :: String)
                    , "value" ~> (1000 :: Int)
                    , "taxed_value" ~> (1000 - (1000 * 0.4) :: Double)
                    , "in_ca" ~> True
                    ]

        print text

        return ()
