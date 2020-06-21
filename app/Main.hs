{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8         as C
import qualified Data.HashMap.Strict           as Map
import           Data.Text                      ( unpack )
import qualified Data.Yaml                     as Yaml
import           System.FilePath                ( (</>) )
import           Text.Mustache
import           Text.Pretty.Simple

import           Restcli.Api
import           Restcli.Cli
import           Restcli.Internal.Encodings

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

        -- TEST: template substitution.
        let text = substitute tmpl env
        putStrLn $ "\n" ++ replicate 25 '-' ++ "\nTEXT\n"
        putStrLn $ unpack text

        -- Parse API file.
        let api = parseAPI tmpl env
        putStrLn $ "\n" ++ replicate 25 '-' ++ "\nAPI\n"
        case api of
                Left  err -> putStr "ERROR :: " >> putStrLn err
                Right val -> do
                        pPrintOpt
                                CheckColorTty
                                defaultOutputOptionsDarkBg
                                        { outputOptionsIndentAmount = 2
                                        }
                                val
                        putStrLn $ "\n" ++ replicate 25 '-' ++ "\n...AS YAML:\n"
                        putStrLn . C.unpack $ Yaml.encode val

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
