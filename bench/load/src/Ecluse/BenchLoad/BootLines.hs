-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | The runtime, admission, compression, and rule decisions a proxy logged at boot, read back from
its JSON log. The report quotes the lines as the build under test wrote them, and derives the limits
from them where they parse, so a build that renames or removes a control shows its own lines and no
figure.
-}
module Ecluse.BenchLoad.BootLines (
    logMessages,
    bootMessages,
    BootLimits (..),
    bootLimits,

    -- * Native metadata compression
    metadataCompression,
    renderMetadataCompression,

    -- * The rule policy
    ruleMessages,
    LoggedRule (..),
    loggedRules,
    ruleBootOrders,
) where

import Data.Aeson (FromJSON, ToJSON, Value (Object, String), decodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isDigit)
import Data.Text qualified as T

import Ecluse.Core.Server.Encoding (MetadataCompression (GzipWhenAccepted, IdentityOnly))

-- | The @message@ of every JSON log line that states a runtime, admission, or compression decision.
bootMessages :: [ByteString] -> [Text]
bootMessages = filter decision . logMessages
  where
    decision m = any (`T.isPrefixOf` m) ["runtime:", "memory plan:", metadataGzipKey]

-- The resolved key's provenance line, up to its value.
metadataGzipKey :: Text
metadataGzipKey = "config: server.metadataGzip = "

-- | The native metadata compression a proxy resolved. 'Nothing' for a log that does not state it.
metadataCompression :: [Text] -> Maybe MetadataCompression
metadataCompression messages = listToMaybe (mapMaybe (posture <=< listToMaybe . words <=< T.stripPrefix metadataGzipKey) messages)
  where
    posture = \case
        "true" -> Just GzipWhenAccepted
        "false" -> Just IdentityOnly
        _ -> Nothing

-- | The posture as the report names it. A log from a build before the setting reads as unreported.
renderMetadataCompression :: Maybe MetadataCompression -> Text
renderMetadataCompression = \case
    Just GzipWhenAccepted -> "enabled"
    Just IdentityOnly -> "disabled"
    Nothing -> "unreported"

-- | The @message@ of every JSON log line that states a rule's configuration or a mount's rule order.
ruleMessages :: [ByteString] -> [Text]
ruleMessages = filter ruleLine . logMessages
  where
    ruleLine m = "rule " `T.isPrefixOf` m || maybe False (elem "rules" . keySegments) (T.stripPrefix "config: " m)
    keySegments = T.splitOn "." . T.takeWhile (/= ' ')

-- | The @message@ of every JSON log line.
logMessages :: [ByteString] -> [Text]
logMessages = mapMaybe $ \line -> case decodeStrict line of
    Just (Object o) | Just (String m) <- KeyMap.lookup "message" o -> Just m
    _ -> Nothing

-- | The limits a proxy resolved, in bytes or requests. Each is 'Nothing' when no line states it.
data BootLimits = BootLimits
    { blCpuAdmission :: Maybe Int
    -- ^ Serve-admission limit in requests, explicit or derived from GHC capabilities.
    , blMemoryBudgetBytes :: Maybe Int
    -- ^ The metadata memory budget at boot. The proxy's sampler moves it at run time.
    , blCacheBytes :: Maybe Int
    , blCacheEntries :: Maybe Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Read the limits from the boot messages.
bootLimits :: [Text] -> BootLimits
bootLimits messages =
    BootLimits
        { blCpuAdmission = after "runtime: serve admission "
        , blMemoryBudgetBytes = after "memory plan: transient budget "
        , blCacheBytes = after "memory plan: cache byte bound "
        , blCacheEntries = after "memory plan: cache entry bound "
        }
  where
    after prefix = listToMaybe (mapMaybe (firstNumber <=< T.stripPrefix prefix) messages)

firstNumber :: Text -> Maybe Int
firstNumber = readMaybe . toString <=< listToMaybe . words

-- | One configured rule, from the lines the proxy logged for its resolved configuration keys.
data LoggedRule = LoggedRule
    { lrName :: Text
    -- ^ The key under @rules@, or the whole key path of a rule a mount configures.
    , lrType :: Maybe Text
    , lrSettings :: [(Text, Text)]
    -- ^ Every other key the rule sets, with its value.
    , lrLayers :: [Text]
    -- ^ The layers the keys came from: default, document, or environment.
    }
    deriving stock (Eq, Show)

-- | Group the @config: <key> = <value> (<layer>)@ lines by rule, in the order the log names each rule.
loggedRules :: [Text] -> [LoggedRule]
loggedRules messages = map collect (ordNub (map fst keyed))
  where
    keyed = mapMaybe ruleKey messages
    collect name =
        let fields = [field | (n, field) <- keyed, n == name]
         in LoggedRule
                { lrName = fromMaybe name (T.stripPrefix "rules." name)
                , lrType = listToMaybe [value | (key, value, _) <- fields, key == "type"]
                , lrSettings = [(key, value) | (key, value, _) <- fields, key /= "type"]
                , lrLayers = ordNub [layer | (_, _, layer) <- fields]
                }

-- A rule key's rule path and its field: the key, the value, and the layer.
ruleKey :: Text -> Maybe (Text, (Text, Text, Text))
ruleKey message = do
    entry <- T.stripPrefix "config: " message
    let (path, assigned) = T.breakOn " = " entry
    (valued, layer) <- T.stripSuffix ")" (T.drop 3 assigned) >>= splitLast " ("
    (rule, key) <- splitLast "." path
    guard ("rules" `elem` T.splitOn "." rule && not (T.null key))
    pure (rule, (key, valued, layer))
  where
    splitLast separator text = case T.breakOnEnd separator text of
        (before, after) | not (T.null before) -> Just (T.dropEnd (T.length separator) before, after)
        _ -> Nothing

-- | Each mount's label and its rules in evaluation order, each with its type, precedence, and phases.
ruleBootOrders :: [Text] -> [(Text, [Text])]
ruleBootOrders = \case
    [] -> []
    message : rest -> case T.stripSuffix ":" =<< T.stripPrefix "rule boot order for mount " message of
        Just mount ->
            let (ordered, more) = span (isJust . orderedRule) rest
             in (mount, mapMaybe orderedRule ordered) : ruleBootOrders more
        Nothing -> ruleBootOrders rest
  where
    orderedRule m = do
        numbered <- T.stripPrefix "rule " m
        let (position, rule) = T.breakOn ": " numbered
        guard (not (T.null position) && T.all isDigit position)
        T.stripPrefix ": " rule
