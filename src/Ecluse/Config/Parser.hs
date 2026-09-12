-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared configuration decoders. Key declarations define both accepted keys and reads.
Aeson paths retain the location of type errors through nested groups.
-}
module Ecluse.Config.Parser (
    -- * Group decoding
    GroupDecoder,
    decodeGroup,
    decodeBareGroup,
    requiredKey,
    optionalKey,
    optionalKeyOr,
    plainKey,
    optionalPlainKey,
    optionalPlainKeyOr,
    nestedKey,
    unreadKey,

    -- * Tagged targets
    TagCase (..),
    taggedTarget,

    -- * Value shapes
    expectString,
    commaSeparated,
    valueKind,
    rejectSecretKeys,

    -- * Leaf parsers
    parseRegistryUrl,
    parseEnum,
    parseHttpUrl,
    parseQueueUrl,
    parseAdvisoryStoreUrl,
    parsePort,
    parseCodeArtifactDuration,
) where

import Data.Aeson (FromJSON, Value (..), parseJSON, (.!=), (.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (JSONPathElement (Key), Parser, modifyFailure, (<?>))
import Data.Text qualified as T

import Ecluse.Config.AdvisoryStore (mkAdvisoryStoreUrl)
import Ecluse.Config.QueueTarget (mkQueueUrl)
import Ecluse.Config.Types (AdvisoryStoreUrl, QueueUrl, Url, mkUrl)
import Ecluse.Core.Json.Lenient (valueKind)
import Ecluse.Core.Security (hostPortAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, mkConfiguredRegistryUrl)
import Ecluse.Core.Text (nonBlank, readDecimalText)

-- The object a group decodes, with the prefix its value refusals write before each key.
data GroupInput = GroupInput
    { giPrefix :: String
    , giObject :: KeyMap.KeyMap Value
    }

-- | A group whose key declarations define both accepted keys and reads.
data GroupDecoder a = GroupDecoder
    { gdKeys :: [Key.Key]
    , gdRead :: GroupInput -> Parser a
    }

instance Functor GroupDecoder where
    fmap f decoder = decoder{gdRead = fmap f . gdRead decoder}

instance Applicative GroupDecoder where
    pure a = GroupDecoder [] (const (pure a))
    lhs <*> rhs =
        GroupDecoder
            (gdKeys lhs <> gdKeys rhs)
            (\input -> gdRead lhs input <*> gdRead rhs input)

-- | Refuse unknown keys before reading values. Refinement labels use @noun.key@.
decodeGroup :: String -> GroupDecoder a -> KeyMap.KeyMap Value -> Parser a
decodeGroup noun = runGroupDecoder noun (noun <> ".")

-- | Decode with bare refinement labels when the enclosing parser supplies the group context.
decodeBareGroup :: String -> GroupDecoder a -> KeyMap.KeyMap Value -> Parser a
decodeBareGroup noun = runGroupDecoder noun ""

runGroupDecoder :: String -> String -> GroupDecoder a -> KeyMap.KeyMap Value -> Parser a
runGroupDecoder noun prefix decoder o = do
    rejectUnknownKeys noun (gdKeys decoder) o
    gdRead decoder (GroupInput{giPrefix = prefix, giObject = o})

-- | Decode and refine a required key. Refinements receive its group-qualified label.
requiredKey :: (FromJSON b) => Key.Key -> (String -> b -> Parser a) -> GroupDecoder a
requiredKey k parse = GroupDecoder [k] present
  where
    present input = case KeyMap.lookup k (giObject input) of
        Nothing -> fail (labelOf input k <> " is required")
        Just v -> (parseAt (labelOf input k) v >>= parse (labelOf input k)) <?> Key k

-- | 'requiredKey' for an optional key: an absent or @null@ one yields 'Nothing'.
optionalKey :: (FromJSON b) => Key.Key -> (String -> b -> Parser a) -> GroupDecoder (Maybe a)
optionalKey k parse =
    GroupDecoder [k] (\input -> readOptionalKey input k >>= traverse (parse (labelOf input k)))

-- | 'requiredKey' for an optional key whose absence reads as @fallback@ before @parse@ sees it.
optionalKeyOr :: (FromJSON b) => Key.Key -> b -> (String -> b -> Parser a) -> GroupDecoder a
optionalKeyOr k fallback parse =
    GroupDecoder [k] (\input -> readOptionalKey input k .!= fallback >>= parse (labelOf input k))

-- | A required key its own 'FromJSON' instance decodes whole, with no further refusal.
plainKey :: (FromJSON a) => Key.Key -> GroupDecoder a
plainKey k = GroupDecoder [k] present
  where
    present input = case KeyMap.lookup k (giObject input) of
        Nothing -> giObject input .: k
        Just v -> parseAt (labelOf input k) v <?> Key k

-- | 'plainKey' for an optional key.
optionalPlainKey :: (FromJSON a) => Key.Key -> GroupDecoder (Maybe a)
optionalPlainKey k = optionalKey k (const pure)

-- | 'plainKey' for an optional key, with the value an absent one reads as.
optionalPlainKeyOr :: (FromJSON a) => Key.Key -> a -> GroupDecoder a
optionalPlainKeyOr k fallback = optionalKeyOr k fallback (const pure)

-- | Decode an absent group as an empty object so its required keys determine the refusal.
nestedKey :: Key.Key -> (KeyMap.KeyMap Value -> Parser a) -> GroupDecoder a
nestedKey k parse = GroupDecoder [k] (\input -> nested (giObject input) <?> Key k)
  where
    nested o = case KeyMap.lookup k o of
        Nothing -> parse KeyMap.empty
        Just (Object inner) -> parse inner
        Just other -> fail (Key.toString k <> " must be an object, but encountered " <> valueKind other)

-- | A key the group accepts and no field reads.
unreadKey :: Key.Key -> GroupDecoder ()
unreadKey k = GroupDecoder [k] (const (pure ()))

-- | One tag a target key admits: the tag as an operator writes it, and the group under it.
data TagCase a = TagCase Key.Key (GroupDecoder a)

-- | Admit exactly one store tag and only the keys its decoder declares.
taggedTarget :: [TagCase a] -> String -> Value -> Parser a
taggedTarget cases field = \case
    Object o -> case KeyMap.toList o of
        [(tag, inner)] | Just (TagCase _ decoder) <- caseFor tag -> tagGroup tag decoder inner
        written -> fail (field <> " must name " <> admitted <> ", got: " <> writtenTags (map fst written))
    other -> fail (field <> " must be an object naming " <> admitted <> ", but encountered " <> valueKind other)
  where
    caseFor tag = find (\(TagCase k _) -> k == tag) cases

    admitted = "exactly one store tag (" <> intercalate ", " (map (\(TagCase k _) -> Key.toString k) cases) <> ")"

    writtenTags = \case
        [] -> "no tag"
        tags -> intercalate ", " (map (show . Key.toText) tags)

    tagGroup tag decoder = \case
        Object inner -> decodeGroup (field <> "." <> Key.toString tag) decoder inner
        other -> fail (field <> "." <> Key.toString tag <> " must be an object, but encountered " <> valueKind other)

parseAt :: (FromJSON a) => String -> Value -> Parser a
parseAt label = modifyFailure ((label <> ": ") <>) . parseJSON

readOptionalKey :: (FromJSON a) => GroupInput -> Key.Key -> Parser (Maybe a)
readOptionalKey input k =
    giObject input .:? k >>= traverse (\v -> parseAt (labelOf input k) v <?> Key k)

labelOf :: GroupInput -> Key.Key -> String
labelOf input k = giPrefix input <> Key.toString k

rejectUnknownKeys :: String -> [Key.Key] -> KeyMap.KeyMap Value -> Parser ()
rejectUnknownKeys context accepted o =
    let isUnknown k = k `notElem` accepted
     in case filter isUnknown (KeyMap.keys o) of
            [] -> pure ()
            unknown ->
                fail
                    ( "unexpected "
                        <> context
                        <> " key(s): "
                        <> intercalate ", " (map (show . Key.toText) unknown)
                    )

-- | Refuse document credentials without including their values in the error.
rejectSecretKeys :: KeyMap.KeyMap Value -> Parser ()
rejectSecretKeys o =
    case filter (`KeyMap.member` o) secretKeys of
        [] -> pure ()
        present ->
            fail
                ( "secret key(s) are not allowed in the config document (use environment variables): "
                    <> intercalate ", " (map (show . Key.toText) present)
                )
  where
    secretKeys :: [Key.Key]
    secretKeys = ["token", "authToken", "password", "secret", "credentialToken"]

-- | Refuse a non-string with its setting label and JSON kind, without quoting its value.
expectString :: String -> (Text -> Parser a) -> Value -> Parser a
expectString field parse = \case
    String t -> parse t
    other -> fail (field <> " expected a string, but encountered " <> valueKind other)

-- | A blank string gives no entries. Empty comma-separated entries still reach @parseEntry@.
commaSeparated :: String -> (Text -> Parser a) -> Value -> Parser [a]
commaSeparated field parseEntry =
    expectString field (maybe (pure []) (traverse (parseEntry . T.strip) . T.splitOn ",") . nonBlank)

-- | Refuse credentials before any URL refusal can quote the input.
parseRegistryUrl :: String -> Value -> Parser RegistryUrl
parseRegistryUrl field = expectString field $ \t -> case mkConfiguredRegistryUrl t of
    Left reason -> fail (field <> ": " <> T.unpack reason)
    Right url
        | isNothing (hostPortAddress t) ->
            fail
                ( field
                    <> ": registry URL must carry a host and, when a port is written, a decimal port in 1..65535 (got "
                    <> T.unpack t
                    <> ")"
                )
        | otherwise -> pure url

-- | Decode a named enum and retain the setting label on refusal.
parseEnum :: (Text -> Either Text a) -> String -> Value -> Parser a
parseEnum parser field =
    expectString field (either (\e -> fail (field <> ": " <> T.unpack e)) pure . parser)

-- | Parse an HTTP(S) URL without credentials. Plain HTTP remains legal for loopback deployments.
parseHttpUrl :: String -> Value -> Parser Url
parseHttpUrl field = expectString field (refined (mkUrl (T.pack field)))

-- | Parse a queue destination whose shape determines its provider.
parseQueueUrl :: String -> Value -> Parser QueueUrl
parseQueueUrl field = expectString field (refined (mkQueueUrl (T.pack field)))

-- | Parse an advisory object store whose scheme determines its provider.
parseAdvisoryStoreUrl :: String -> Value -> Parser AdvisoryStoreUrl
parseAdvisoryStoreUrl field = expectString field (refined (mkAdvisoryStoreUrl (T.pack field)))

-- A smart constructor's refusal, which already names the key, raised as the key's parse failure.
refined :: (Text -> Either Text a) -> Text -> Parser a
refined parse = either (fail . T.unpack) pure . parse

-- | A listener port: 0..65535, where 0 asks the OS for an ephemeral port.
parsePort :: String -> Int -> Parser Int
parsePort field value
    | value >= 0 && value <= 65535 = pure value
    | otherwise = fail (field <> " must be a port in 0..65535 (0 = OS-assigned), got " <> show value)

-- | Accept seconds in CodeArtifact's 900..43200 range before the first token mint.
parseCodeArtifactDuration :: String -> Value -> Parser Natural
parseCodeArtifactDuration field v = do
    n <- case v of
        String t -> case readDecimalText t :: Maybe Natural of
            Just parsed -> pure parsed
            Nothing -> fail (field <> ": invalid duration: " <> T.unpack t)
        other -> parseAt field other
    if n >= 900 && n <= 43200
        then pure n
        else fail (field <> " must be a duration in seconds within 900..43200, got " <> show n)
