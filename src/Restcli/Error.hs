{-# LANGUAGE OverloadedStrings #-}

module Restcli.Error
    ( DataPath
    , Error(..)
    , errorFail
    , withFilePath
    )
where

import           Control.Exception
import           Data.ByteString.Char8          ( ByteString )
import           Data.List                      ( intercalate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import qualified Data.Yaml                     as Yaml
import           Text.Megaparsec.Error
import qualified Text.Parsec.Error             as Parsec.Error

import           Restcli.Types
import           Restcli.Utils                  ( between )

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
        let expectedMsg = unwords ["no", show expected, "at", fmtDataPath keys]
        in
            case found of
                Nothing     -> expectedMsg
                Just actual -> chain
                    [expectedMsg, unwords ["found", show actual, "instead"]]
    displayException (EnvLookupError key) =
        "key '" ++ T.unpack key ++ "' not found in Environment"
    displayException (TemplateError err) = show err
    displayException (ParsecError   err) = errorBundlePretty err
    displayException (YamlError     err) = Yaml.prettyPrintParseException err
    displayException (WithMsg err msg  ) = chain [msg, displayException err]
    displayException (WithCause err cause) =
        displayException err ++ "\n\ncaused by: " ++ displayException cause

errorFail :: (MonadFail m) => Error -> m a
errorFail = fail . displayException

withFilePath :: Error -> FilePath -> Error
err `withFilePath` fp = err `WithMsg` between '`' '`' fp

fmtDataPath :: DataPath -> String
fmtDataPath = T.unpack . T.intercalate "."

chain :: [String] -> String
chain = intercalate ": "
