{-# LANGUAGE OverloadedStrings #-}

module Restcli.Error
    ( DataPath
    , Error(..)
    )
where

import           Control.Exception
import           Control.Monad.Writer           ( execWriter
                                                , tell
                                                )
import           Data.ByteString.Char8          ( ByteString )
import           Data.List                      ( intercalate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import qualified Data.Yaml                     as Yaml
import           Text.Megaparsec.Error
import qualified Text.Parsec.Error             as Parsec.Error

import           Restcli.Types
import           Restcli.Utils                  ( whenMaybe )

type DataPath = [Text]

data Error
    = APISpecError APIComponentKind String
    | APILookupError DataPath APIComponentKind (Maybe APIComponentKind)
    | EnvLookupError Text
    | TemplateError Parsec.Error.ParseError
    | ParsecError (ParseErrorBundle Text Void)
    | YamlError Yaml.ParseException
    | WithMsg Error String
    | WithCause { getError :: Error, getCause :: Error }
    deriving (Show)

instance Exception Error where
    displayException (APISpecError kind msg) =
        chain ["invalid " ++ show kind, msg]
    displayException (APILookupError keys expected found) =
        chain . execWriter $ do
            tell ["'" ++ fmtDataPath keys ++ "'", show expected ++ " not found"]
            whenMaybe found
                $ \actual -> tell ["found " ++ show actual ++ " instead"]
    displayException (EnvLookupError key) =
        "key '" ++ T.unpack key ++ "' not found"
    displayException (TemplateError err) = show err
    displayException (ParsecError   err) = errorBundlePretty err
    displayException (YamlError     err) = case err of
        Yaml.AesonException s -> s
        _ ->
            T.unpack
                . T.replace "\n" " "
                . T.pack
                . Yaml.prettyPrintParseException
                $ err
    displayException (WithMsg err msg) =
        let parts = [msg, displayException err]
        in  case err of
                YamlError _ -> unlines parts
                _           -> chain parts
    displayException (WithCause err cause) =
        displayException err ++ "\n\ncaused by: " ++ displayException cause

fmtDataPath :: DataPath -> String
fmtDataPath = T.unpack . T.intercalate "."

chain :: [String] -> String
chain = intercalate ": "
