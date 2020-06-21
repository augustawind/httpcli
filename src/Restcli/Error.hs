{-# LANGUAGE OverloadedStrings #-}

module Restcli.Error where

import           Control.Exception
import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text
                                                , intercalate
                                                )
import           Data.Void                      ( Void )
import qualified Data.Yaml                     as Yaml
import           Text.Megaparsec.Error
import qualified Text.Parsec.Error             as Parsec.Error

data Error
    = APIError String String
    | TemplateError Parsec.Error.ParseError
    | ParsecError (ParseErrorBundle Text Void)
    | YamlError Yaml.ParseException
    | WithMsg Error String
    | WithCause { getError :: Error, getCause :: Error }
    deriving (Show)

instance Exception Error where
    displayException (APIError subject msg) =
        "invalid " ++ subject ++ ": " ++ msg
    displayException (TemplateError err) = show err
    displayException (ParsecError   err) = errorBundlePretty err
    displayException (YamlError     err) = Yaml.prettyPrintParseException err
    displayException (WithMsg err msg  ) = msg ++ ": " ++ displayException err
    displayException (WithCause err cause) =
        displayException err ++ "\n\ncaused by: " ++ displayException cause

errorFail :: (MonadFail m) => Error -> m a
errorFail = fail . displayException
