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
    yamlField,
    yamlTag,
) where

import Conduit
import Control.Monad.Except (throwError)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as TE
import Text.Libyaml qualified as Y
import UnliftIO.Exception (try)

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Security.Limits (Limits (..))

-- | Tags identify data schema only. No class lookup or object construction occurs.
data YamlValue = YamlScalar Y.Tag Text | YamlSequence Y.Tag [YamlValue] | YamlMapping Y.Tag (Map Text YamlValue)
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
        Y.EventScalar bytes tag _ anchor -> do
            checkTag tag
            value <- either (const (refuse "YAML scalar is not UTF-8")) pure (TE.decodeUtf8' bytes)
            retain anchor 1 (YamlScalar tag value)
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
    YamlScalar _ _ -> 0
    YamlSequence _ values -> descendants values
    YamlMapping _ fields -> descendants (Map.elems fields)
  where
    descendants values = foldl' max 0 (map ((1 +) . valueDepth) values)

consume :: Int -> Decoder ()
consume amount = do
    cursor <- get
    let cap = min (maxArtifactCount (ysLimits cursor)) (maxMetadataBytes (ysLimits cursor) `div` 8)
    when (amount > cap - ysNodes cursor) (refuse "YAML node or alias work exceeds its bound")
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
yamlText (YamlScalar _ value) = Right value
yamlText _ = Left (ParseError "expected YAML scalar")

-- | Require one named field of a native mapping.
yamlField :: Text -> Map Text YamlValue -> Either ParseError YamlValue
yamlField key = maybeToRight (ParseError ("missing YAML field: " <> key)) . Map.lookup key

-- | Return the explicit native tag for schema checks.
yamlTag :: YamlValue -> Y.Tag
yamlTag (YamlScalar tag _) = tag
yamlTag (YamlSequence tag _) = tag
yamlTag (YamlMapping tag _) = tag
