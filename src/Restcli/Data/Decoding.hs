{-# LANGUAGE OverloadedStrings #-}

module Restcli.Data.Decoding
    ()
where

import           Data.Aeson
import           Data.Aeson.Types               ( JSONPathElement(..)
                                                , Parser
                                                , emptyArray
                                                , prependFailure
                                                , typeMismatch
                                                )
import qualified Data.ByteString.Char8         as C
import           Data.Char                      ( toUpper )
import qualified Data.HashMap.Strict           as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as OrdMap
import           Data.List                      ( (\\)
                                                , intercalate
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Void
import qualified Data.Yaml                     as Yaml
import qualified Network.HTTP.Types            as HTTP
import           Text.Read                      ( readEither )
import           Text.URI                       ( URI )
import qualified Text.URI                      as URI
import           Text.Megaparsec                ( Parsec
                                                , runParser
                                                )

import           Restcli.Error
import           Restcli.Data.Common
import           Restcli.Data.Decoding.Headers  ( parseHeaders )
import           Restcli.Types

instance FromJSON API where
    parseJSON = withArray "API" $ fmap API . parseOrdMap

instance FromJSON ReqNode where
    parseJSON (Array  node) = ReqGroup <$> parseOrdMap node
    parseJSON (Object node) = Req <$> parseRequest node
    parseJSON invalid       = prependFailure
        "parsing API node failed: "
        (typeMismatch "Array or Object" invalid)
    -- more concise:
    -- parseJSON = withArray "node" (fmap ReqGroup . parseOrdMap)
    --     <> withObject "node" (fmap Req . parseRequest)

parseRequest :: Object -> Parser HttpRequest
parseRequest obj
    | not . null $ missingKeys
    = errorFail
        .  APISpecError RequestKind []
        $  "missing required key(s) "
        ++ unitems missingKeys
    | not . null $ unknownKeys
    = errorFail
        .  APISpecError RequestKind []
        $  "extra key(s) "
        ++ unitems unknownKeys
    | otherwise
    = let encoded = Yaml.encode obj
          decoded = Yaml.decodeEither' encoded :: YamlParser HttpRequest
      in  either (fail . show) return decoded
  where
    missingKeys = requiredReqKeys \\ Map.keys obj
    unknownKeys = Map.keys obj \\ reqKeys
    unitems     = T.unpack . T.intercalate ", " . map (surround "'")
    surround x = (x `T.append`) . (`T.append` x)

instance FromJSON HttpRequest where
    parseJSON = genericParseJSON aesonRequestOptions

instance FromJSON HTTP.StdMethod where
    parseJSON = withText "method" $ \method ->
        case readEither . map toUpper $ T.unpack method of
            Left err -> errorFail $ errReqAttr ReqMethodT
                                               ["method"]
                                               method
                                               "method not recognized"
            Right method -> return method

instance FromJSON URI where
    parseJSON = withText "uri" $ \uri ->
        case runParser (URI.parser :: Parsec Void Text URI) "" uri of
            Left err ->
                errorFail
                    $           errReqAttr ReqUrlT ["url"] uri ""
                    `WithCause` ParsecError err
            Right val -> return val

instance FromJSON RequestQuery where
    -- TODO: allow for no-value keys
    parseJSON =
        withArray "query"
            $ fmap (RequestQuery . concatMap Map.toList . V.toList)
            . mapM parseQueryItems
      where
        parseQueryItems = withObject "query item" $ mapM parseQueryVal
        parseQueryVal   = withText "query item value" (return . Just)

instance FromJSON RequestHeaders where
    parseJSON = withText "headers" $ \headers ->
        -- TODO: write a new, much more lenient parser that auto-escapes bad chars,
        -- auto-quotes header values, and allows multiple newlines between headers.
        case runParser parseHeaders "" headers of
            Left err ->
                errorFail
                    $           errReqAttr ReqHeadersT ["headers"] headers ""
                    `WithCause` ParsecError err
            Right val -> return . RequestHeaders $ val

instance FromJSON RequestBody where
    -- TODO: allow for other body types
    parseJSON = withText "json" $ \body ->
        case Yaml.decodeEither' $ encodeUtf8 body :: YamlParser Value of
            Left err ->
                errorFail
                    $           errReqAttr ReqBodyT ["json"] body ""
                    `WithCause` YamlError err
            Right val -> return . RequestBody $ val

errReqAttr :: RequestAttrKind -> DataPath -> Text -> String -> Error
errReqAttr kind path actual msg =
    APISpecError (RequestAttrKind kind) path
        . intercalate ": "
        . catMaybes
        $ [actual', msg']
  where
    actual' | T.null actual = Nothing
            | otherwise     = Just $ "'" ++ T.unpack actual ++ "'"
    msg' | null msg  = Nothing
         | otherwise = Just msg

instance FromJSON Env where
    parseJSON = withArray "Env" $ fmap Env . parseOrdMap

parseOrdMap :: (FromJSON v) => Vector Value -> Parser (InsOrdHashMap Text v)
parseOrdMap vec =
    OrdMap.fromList . concatMap Map.toList . V.toList <$> V.mapM parseJSON vec
