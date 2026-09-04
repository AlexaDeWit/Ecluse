-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared refusals every configuration key is decoded through: secret and unknown keys,
enumerations, ports, durations, and the URL shapes.

A malformed key fails at load with the key named, never at its first use. A group accepts exactly
the keys its 'GroupDecoder' declares, and 'taggedTarget' nests one group per store tag under an
endpoint. A URL-valued key is refined by the smart constructor of the type it resolves to:
'parseHttpUrl', 'parseQueueUrl', and 'parseAdvisoryStoreUrl' pass it the key so the refusal names
it, while 'parseRegistryUrl' prefixes the key itself and adds the host refusal 'RegistryUrl' omits.
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
import Data.Aeson.Types (Parser)
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

{- | One config group's decoder. The key helpers below are its only builders, so the accepted
set and the reads come from one declaration and a read key is always an accepted key.
-}
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

{- | Decode a group under @noun@. An undeclared key refuses before any value parses, so a typo
is reported even when a required key is missing too. A value refusal names @noun.key@.
-}
decodeGroup :: String -> GroupDecoder a -> KeyMap.KeyMap Value -> Parser a
decodeGroup noun = runGroupDecoder noun (noun <> ".")

{- | 'decodeGroup' where a value refusal names the bare key, for a group an enclosing error
already places (a mount's keys, which "Ecluse.Config" reports under its ecosystem).
-}
decodeBareGroup :: String -> GroupDecoder a -> KeyMap.KeyMap Value -> Parser a
decodeBareGroup noun = runGroupDecoder noun ""

runGroupDecoder :: String -> String -> GroupDecoder a -> KeyMap.KeyMap Value -> Parser a
runGroupDecoder noun prefix decoder o = do
    rejectUnknownKeys noun (gdKeys decoder) o
    gdRead decoder (GroupInput{giPrefix = prefix, giObject = o})

{- | A required key, decoded by its own 'FromJSON' instance then refined by @parse@, which is
handed the key's label so every refusal it raises names the key, an absent key included.
-}
requiredKey :: (FromJSON b) => Key.Key -> (String -> b -> Parser a) -> GroupDecoder a
requiredKey k parse = GroupDecoder [k] present
  where
    present input = case KeyMap.lookup k (giObject input) of
        Nothing -> fail (labelOf input k <> " is required")
        Just v -> parseJSON v >>= parse (labelOf input k)

-- | 'requiredKey' for an optional key: an absent or @null@ one yields 'Nothing'.
optionalKey :: (FromJSON b) => Key.Key -> (String -> b -> Parser a) -> GroupDecoder (Maybe a)
optionalKey k parse =
    GroupDecoder [k] (\input -> giObject input .:? k >>= traverse (parse (labelOf input k)))

-- | 'requiredKey' for an optional key whose absence reads as @fallback@ before @parse@ sees it.
optionalKeyOr :: (FromJSON b) => Key.Key -> b -> (String -> b -> Parser a) -> GroupDecoder a
optionalKeyOr k fallback parse =
    GroupDecoder [k] (\input -> giObject input .:? k .!= fallback >>= parse (labelOf input k))

-- | A required key its own 'FromJSON' instance decodes whole, with no further refusal.
plainKey :: (FromJSON a) => Key.Key -> GroupDecoder a
plainKey k = GroupDecoder [k] ((.: k) . giObject)

-- | 'plainKey' for an optional key.
optionalPlainKey :: (FromJSON a) => Key.Key -> GroupDecoder (Maybe a)
optionalPlainKey k = GroupDecoder [k] ((.:? k) . giObject)

-- | 'plainKey' for an optional key, with the value an absent one reads as.
optionalPlainKeyOr :: (FromJSON a) => Key.Key -> a -> GroupDecoder a
optionalPlainKeyOr k fallback = GroupDecoder [k] ((.!= fallback) . (.:? k) . giObject)

{- | A key holding a nested group. An absent one decodes as an empty object, so the nested
decoder reports its own required keys as missing.
-}
nestedKey :: Key.Key -> (KeyMap.KeyMap Value -> Parser a) -> GroupDecoder a
nestedKey k parse = GroupDecoder [k] (nested . giObject)
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

{- | Read a target: an object naming exactly one of the tags this endpoint admits, and under it
exactly the keys that tag admits. Two tags is what a layer earns for overriding a document's tag.
-}
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

{- | Read a string-valued key, naming the key and the JSON kind found on anything else. It is
the one string-shape refusal the configuration decoders share.
-}
expectString :: String -> (Text -> Parser a) -> Value -> Parser a
expectString field parse = \case
    String t -> parse t
    other -> fail (field <> " expected a string, but encountered " <> valueKind other)

{- | Read a comma-separated string value as its trimmed entries, a blank value being the empty
list. An empty entry still reaches @parseEntry@, so @a,,b@ fails rather than silently losing one.
-}
commaSeparated :: String -> (Text -> Parser a) -> Value -> Parser [a]
commaSeparated field parseEntry =
    expectString field (maybe (pure []) (traverse (parseEntry . T.strip) . T.splitOn ",") . nonBlank)

-- mkConfiguredRegistryUrl runs first, because the refusal below it quotes the value. An authority
-- the egress gate cannot extract could only build a mount that refuses every fetch.
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

parseEnum :: (Text -> Either Text a) -> String -> Value -> Parser a
parseEnum parser field =
    expectString field (either (\e -> fail (field <> ": " <> T.unpack e)) pure . parser)

{- | An @http(s)@ URL Écluse serves, rewrites against, or fetches from. Plain http stays legal for
loopback, and 'mkUrl' carries the scheme, authority, and credential refusals.
-}
parseHttpUrl :: String -> Value -> Parser Url
parseHttpUrl field = expectString field (refined (mkUrl (T.pack field)))

{- | The mirror-queue destination Écluse hands to a cloud SDK rather than dialling itself. It keeps
its provider's shape, so 'mkQueueUrl' derives the backend instead of checking a scheme or a host.
-}
parseQueueUrl :: String -> Value -> Parser QueueUrl
parseQueueUrl field = expectString field (refined (mkQueueUrl (T.pack field)))

{- | The object store the compiled advisory databases sync from. Its scheme names the provider,
so 'mkAdvisoryStoreUrl' derives it rather than reading a separate selector.
-}
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

{- | A CodeArtifact token duration in seconds, bounded to the 900..43200 the service accepts. An
out-of-range value would otherwise fail at the first mint, with the queue already accepting work.
-}
parseCodeArtifactDuration :: String -> Value -> Parser Natural
parseCodeArtifactDuration field v = do
    n <- case v of
        String t -> case readDecimalText t :: Maybe Natural of
            Just parsed -> pure parsed
            Nothing -> fail (field <> ": invalid duration: " <> T.unpack t)
        other -> parseJSON other
    if n >= 900 && n <= 43200
        then pure n
        else fail (field <> " must be a duration in seconds within 900..43200, got " <> show n)
