{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception              ( displayException )
import qualified Data.ByteString.Char8         as C
import qualified Data.HashMap.Strict           as Map
import qualified Data.Yaml                     as Yaml
import           Text.Pretty.Simple

import           Restcli.Api
import           Restcli.App
import           Restcli.Cli
import           Restcli.Data.Encoding
import           Restcli.Error
import           Restcli.Types

main :: IO ()
main = do
        -- Parse command line options.
        opts <- runCli

        -- Compile API template.
        tmpl <- readApiTemplate $ optApiFile opts
        putStrLn $ "\n" ++ replicate 25 '-' ++ "\nTEMPLATE\n"
        print tmpl

        -- Parse Env file if given, otherwise use a blank env.
        env <- case optEnvFile opts of
                Just path -> readEnv path
                Nothing   -> return Map.empty
        putStrLn $ "\n" ++ replicate 25 '-' ++ "\nENV\n"
        print env

        -- Parse API file.
        let api = parseAPI tmpl env
        putStrLn $ "\n" ++ replicate 25 '-' ++ "\nAPI\n"
        case api of
                Left err -> putStr "ERROR: " >> print err >> putStrLn
                        ("\n" ++ displayException err)
                Right val -> runApp dispatchS opts val env >>= pprint

        return ()

pprint :: Show a => a -> IO ()
pprint = pPrintOpt
        CheckColorTty
        defaultOutputOptionsDarkBg { outputOptionsIndentAmount = 2 }

printAPI :: API -> IO ()
printAPI val = do
        pprint val
        putStrLn $ "\n" ++ replicate 25 '-' ++ "\n...AS YAML:\n"
        putStrLn . C.unpack $ Yaml.encode val
