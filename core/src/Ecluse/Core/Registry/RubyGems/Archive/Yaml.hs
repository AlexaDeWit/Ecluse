-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Inert YAML events with bounded aliases and explicit Ruby data tags.
module Ecluse.Core.Registry.RubyGems.Archive.Yaml (
    YamlValue (..),
    decodeYaml,
    yamlMapping,
    yamlSequence,
    yamlText,
    yamlNativeText,
    yamlNativeString,
    yamlField,
    yamlTag,
) where

import Conduit
import Control.Monad.Except (throwError)
import Data.ByteString qualified as BS
import Data.Char (isAlpha, isDigit, isHexDigit, isSpace)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Calendar (fromGregorianValid)
import Text.Libyaml qualified as Y
import UnliftIO.Exception (try)

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.RubyGems.Marshal (inertNodeBudget)
import Ecluse.Core.Security.Limits (Limits (..))

-- | Tags identify data schema only. No class lookup or object construction occurs.
data YamlValue = YamlScalar Y.Tag Y.Style Text | YamlSequence Y.Tag [YamlValue] | YamlMapping Y.Tag (Map Text YamlValue)
    deriving stock (Eq, Show)

data YamlState = YamlState
    { ysLimits :: Limits
    , ysNodes :: Int
    , ysAliases :: Map Y.AnchorName (YamlValue, Int, Int)
    }

type Decoder = StateT YamlState (ConduitT Y.Event Void (ExceptT ParseError (ResourceT IO)))

-- | Consume one complete document, rejecting cycles, duplicate keys and unsupported tags.
decodeYaml :: Limits -> (Int -> IO ()) -> ByteString -> IO (Either ParseError YamlValue)
decodeYaml limits charge bytes
    | BS.length bytes > maxMetadataBytes limits = pure (Left (ParseError "YAML exceeds its byte bound"))
    | otherwise = do
        charge (65536 + 64 * BS.length bytes)
        result <- try (runResourceT . runExceptT . runConduit $ Y.decode bytes .| evalStateT document (YamlState limits 0 Map.empty))
        pure $ case result of
            Left (_ :: Y.YamlException) -> Left (ParseError "malformed YAML stream")
            Right decoded -> decoded
  where
    document = do
        expect Y.EventStreamStart
        expect Y.EventDocumentStart
        value <- next >>= node 0
        expect Y.EventDocumentEnd
        expect Y.EventStreamEnd
        lift await >>= \case Nothing -> pure (); Just _ -> refuse "YAML has trailing events"
        pure value

node :: Int -> Y.Event -> Decoder YamlValue
node depth event = do
    limits <- gets ysLimits
    when (depth > maxNestingDepth limits) (refuse "YAML nesting exceeds its bound")
    consume 1
    case event of
        Y.EventAlias anchor -> do
            references <- gets ysAliases
            case Map.lookup anchor references of
                Nothing -> refuse "invalid or cyclic YAML alias"
                Just (value, cost, height) -> do
                    when (depth + height > maxNestingDepth limits) (refuse "YAML referenced depth exceeds its bound")
                    consume cost
                    pure value
        Y.EventScalar bytes tag style anchor -> do
            checkTag tag
            value <- either (const (refuse "YAML scalar is not UTF-8")) pure (TE.decodeUtf8' bytes)
            retain anchor 1 (YamlScalar tag style value)
        Y.EventSequenceStart tag _ anchor -> do
            checkTag tag
            before <- gets ysNodes
            values <- sequenceValues (depth + 1)
            after <- gets ysNodes
            retain anchor (after - before + 1) (YamlSequence tag values)
        Y.EventMappingStart tag _ anchor -> do
            checkTag tag
            before <- gets ysNodes
            values <- mappingValues (depth + 1) Map.empty
            after <- gets ysNodes
            retain anchor (after - before + 1) (YamlMapping tag values)
        _ -> refuse "unexpected YAML framing event"

sequenceValues :: Int -> Decoder [YamlValue]
sequenceValues depth =
    next >>= \case
        Y.EventSequenceEnd -> pure []
        event -> (:) <$> node depth event <*> sequenceValues depth

mappingValues :: Int -> Map Text YamlValue -> Decoder (Map Text YamlValue)
mappingValues depth held =
    next >>= \case
        Y.EventMappingEnd -> pure held
        event -> do
            keyValue <- node depth event
            key <- checked (yamlText keyValue)
            when (Map.member key held || key == "<<") (refuse "duplicate YAML key or merge key")
            value <- next >>= node depth
            mappingValues depth (Map.insert key value held)

retain :: Y.Anchor -> Int -> YamlValue -> Decoder YamlValue
retain anchor cost value = do
    for_ anchor $ \name -> do
        references <- gets ysAliases
        when (Map.member name references) (refuse "duplicate YAML anchor")
        modify' (\cursor -> cursor{ysAliases = Map.insert name (value, cost, valueDepth value) references})
    pure value

valueDepth :: YamlValue -> Int
valueDepth = \case
    YamlScalar{} -> 0
    YamlSequence _ values -> descendants values
    YamlMapping _ fields -> descendants (Map.elems fields)
  where
    descendants values = foldl' max 0 (map ((1 +) . valueDepth) values)

consume :: Int -> Decoder ()
consume amount = do
    cursor <- get
    when (amount > inertNodeBudget (ysLimits cursor) - ysNodes cursor) (refuse "YAML node or alias work exceeds its bound")
    modify' (\current -> current{ysNodes = ysNodes current + amount})

checkTag :: Y.Tag -> Decoder ()
checkTag = \case
    Y.UriTag tag | toText tag `elem` ["!ruby/object:Gem::Specification", "!ruby/object:Gem::Version", "!ruby/object:Gem::Requirement", "!ruby/object:Gem::Dependency", "!ruby/object:Gem::Platform", "tag:yaml.org,2002:timestamp", "!ruby/symbol"] -> pure ()
    Y.UriTag _ -> refuse "unsupported YAML tag"
    Y.SetTag -> refuse "unsupported YAML set"
    _ -> pure ()

next :: Decoder Y.Event
next = lift await >>= maybe (refuse "truncated YAML document") pure

expect :: Y.Event -> Decoder ()
expect wanted = next >>= \actual -> unless (actual == wanted) (refuse "unexpected YAML framing")

checked :: Either ParseError a -> Decoder a
checked = either (lift . lift . throwError) pure

refuse :: Text -> Decoder a
refuse = checked . Left . ParseError

-- | Read a mapping without discarding its checked tag.
yamlMapping :: YamlValue -> Either ParseError (Map Text YamlValue)
yamlMapping (YamlMapping _ values) = Right values
yamlMapping _ = Left (ParseError "expected YAML mapping")

-- | Read a sequence without constructing Ruby objects.
yamlSequence :: YamlValue -> Either ParseError [YamlValue]
yamlSequence (YamlSequence _ values) = Right values
yamlSequence _ = Left (ParseError "expected YAML sequence")

-- | Read the exact decoded scalar spelling.
yamlText :: YamlValue -> Either ParseError Text
yamlText (YamlScalar _ _ value) = Right value
yamlText _ = Left (ParseError "expected YAML scalar")

-- | The String Psych loads from a name, version or platform scalar, refusing nil and other types.
yamlNativeText :: YamlValue -> Either ParseError Text
yamlNativeText value = yamlNativeString value >>= maybeToRight notNativeString

{- | The String Psych loads from a scalar, or 'Nothing' for its nil, refusing a scalar it types
otherwise. Text outside @[A-Za-z0-9._-]@ is left to the caller's name grammar.
-}
yamlNativeString :: YamlValue -> Either ParseError (Maybe Text)
yamlNativeString = \case
    YamlScalar tag style value
        | tag == Y.StrTag || (tag == Y.NoTag && style `elem` [Y.SingleQuoted, Y.DoubleQuoted, Y.Literal, Y.Folded]) -> Right (Just value)
        | tag == Y.FloatTag || tag == Y.UriTag "!ruby/symbol" -> Left notNativeString
        -- Psych re-scans the text of a plain scalar and of every other tag it accepts.
        | otherwise -> plainNativeString value
    _ -> Left (ParseError "expected YAML scalar")

-- Psych's ScalarScanner#tokenize, deciding only between a String, nil and another type.
plainNativeString :: Text -> Either ParseError (Maybe Text)
plainNativeString value
    | T.null value = Right Nothing
    | wordLike || T.any (== '\n') value = keyword
    | gregorianDate || T.toLower value `elem` [".inf", "+.inf", "-.inf", ".nan"] || psychNumber value = Left notNativeString
    | otherwise = Right (Just value)
  where
    lower = T.toLower value
    wordClass c = isAlpha c || isSpace c || c `elem` ("_!@#$%^&*(){}<>|/\\~;=" :: String)
    wordLike = case T.unpack (T.take 2 value) of
        c0 : rest -> wordClass c0 || (c0 `notElem` ("0123456789.:-" :: String) && any wordClass rest)
        [] -> False
    keyword
        | T.length value > 5 || T.take 1 lower `notElem` ["y", "t", "o", "n", "f", "~"] = Right (Just value)
        | value == "~" || lower == "null" = Right Nothing
        | lower `elem` ["yes", "true", "on", "no", "false", "off"] = Left notNativeString
        | otherwise = Right (Just value)
    gregorianDate = case T.splitOn "-" value of
        [year, month, day]
            | T.length year == 4 && all (\part -> not (T.null part) && T.all isDigit part) [year, month, day] && T.length month <= 2 && T.length day <= 2 ->
                isJust (fromGregorianValid (readDigits year) (fromInteger (readDigits month)) (fromInteger (readDigits day)))
        _ -> False
    readDigits = T.foldl' (\total digit -> total * 10 + toInteger (fromEnum digit - fromEnum '0')) 0

-- Psych's legacy YAML 1.1 integer and float spellings, optionally signed.
psychNumber :: Text -> Bool
psychNumber signed = integer || float
  where
    text = fromMaybe signed (T.stripPrefix "-" signed <|> T.stripPrefix "+" signed)
    separated = (`elem` ['_', ','])
    radix prefix digit = case T.stripPrefix prefix text of
        Just rest -> case T.uncons (T.dropWhile separated rest) of
            Just (leading, others) -> digit leading && T.all (\c -> digit c || separated c) others
            Nothing -> False
        Nothing -> False
    decimal = case T.uncons text of
        Just ('0', rest) -> T.null rest
        Just (leading, rest) -> leading `elem` ['1' .. '9'] && grouped (T.unpack rest)
        Nothing -> False
    grouped = \case
        [] -> True
        c : cs | isDigit c -> grouped cs
        c : d : cs | separated c && isDigit d -> grouped cs
        _ -> False
    integer = radix "0b" (`elem` ['0', '1']) || radix "0x" isHexDigit || radix "0" (`elem` ['0' .. '7']) || decimal
    float = case T.breakOn "." text of
        (whole, point)
            | Just after <- T.stripPrefix "." point ->
                let (_, scale) = T.span isDigit after
                 in (T.null whole || (T.all isDigit (T.take 1 whole) && T.all (\c -> isDigit c || separated c) whole))
                        && not (T.null whole && T.null after)
                        && (T.null scale || validScale scale)
        _ -> False
    validScale scale = case T.unpack scale of
        e : sign : digits@(_ : _) -> e `elem` ['e', 'E'] && sign `elem` ['-', '+'] && all isDigit digits
        _ -> False

notNativeString :: ParseError
notNativeString = ParseError "YAML scalar does not load as a native string"

-- | Require one named field of a native mapping.
yamlField :: Text -> Map Text YamlValue -> Either ParseError YamlValue
yamlField key = maybeToRight (ParseError ("missing YAML field: " <> key)) . Map.lookup key

-- | Return the explicit native tag for schema checks.
yamlTag :: YamlValue -> Y.Tag
yamlTag (YamlScalar tag _ _) = tag
yamlTag (YamlSequence tag _) = tag
yamlTag (YamlMapping tag _) = tag
