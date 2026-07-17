-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ecluse.Config.Aeson () where

import Data.Aeson (FromJSON (..), Value (..), withObject, withText, (.!=), (.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.IP (IPRange)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)

import Ecluse.Config.Parser
import Ecluse.Config.Rule
import Ecluse.Config.Types

import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem, parseEcosystem)
import Ecluse.Core.Package (Scope, mkScope)
import Ecluse.Core.Package.Integrity (parseMinIntegrity, parseMinTrustedIntegrity)
import Ecluse.Core.Package.Merge (parseDivergencePolicy)
import Ecluse.Core.Security (parseBlockedRange)
import Ecluse.Runtime.Log (parseLogFormat)
import Ecluse.Runtime.Telemetry (parseTelemetrySwitch)

instance FromJSON MountConfig where
    parseJSON = withObject "MountConfig" mountConfigParser

mountConfigParser :: KeyMap.KeyMap Value -> Parser MountConfig
mountConfigParser o = do
    rejectUnknownKeys "mount" acceptedMountKeys o
    MountConfig
        <$> o .:? "enabled"
        <*> (o .:? "privateUpstream" >>= traverse parseRegistryUrl)
        <*> (o .: "publicUpstream" >>= parseRegistryUrl)
        <*> (o .:? "mirrorTarget" >>= traverse parseRegistryUrl)
        <*> (o .:? "mirrorTargetToken" >>= traverse parseSecret)
        <*> (o .:? "mirrorCodeArtifactTokenDuration" >>= traverse parseDuration)
        <*> (o .:? "publicationTarget" >>= traverse parseRegistryUrl)
        <*> (o .:? "publicationTargetToken" >>= traverse parseSecret)
        <*> (o .:? "publishAllow" .!= String "" >>= parseScopes)
        <*> (o .:? "minTrustedIntegrity" >>= traverse (parseEnum parseMinTrustedIntegrity "minTrustedIntegrity"))
        <*> (o .:? "divergencePolicy" >>= traverse (parseEnum parseDivergencePolicy "divergencePolicy"))
        <*> o .:? "rules" .!= RulePatch Map.empty

acceptedMountKeys :: [Key.Key]
acceptedMountKeys =
    [ "enabled"
    , "privateUpstream"
    , "publicUpstream"
    , "mirrorTarget"
    , "mirrorTargetToken"
    , "mirrorCodeArtifactTokenDuration"
    , "publicationTarget"
    , "publicationTargetToken"
    , "publishAllow"
    , "minTrustedIntegrity"
    , "divergencePolicy"
    , "rules"
    ]

parseDuration :: Value -> Parser Natural
parseDuration (String t) = case readMaybe (T.unpack t) :: Maybe Natural of
    Just n -> pure n
    Nothing -> fail ("invalid duration: " <> T.unpack t)
parseDuration v = parseJSON v

parseScopes :: Value -> Parser [Scope]
parseScopes = withText "Scopes" $ \t ->
    if T.null (T.strip t)
        then pure []
        else pure (map (mkScope . T.strip) (T.splitOn "," t))

instance FromJSON AppConfig where
    parseJSON = withObject "AppConfig" appConfigParser

appConfigParser :: KeyMap.KeyMap Value -> Parser AppConfig
appConfigParser o = do
    rejectUnknownKeys "document" acceptedDocumentKeys o
    AppConfig
        <$> (groupOf "server" o >>= serverParser)
        <*> (groupOf "queue" o >>= queueParser)
        <*> (groupOf "limits" o >>= limitsParser)
        <*> (groupOf "cache" o >>= cacheParser)
        <*> (groupOf "integrity" o >>= integrityParser)
        <*> (groupOf "egress" o >>= egressParser)
        <*> (groupOf "advisories" o >>= advisoriesParser)
        <*> (groupOf "runtime" o >>= runtimeParser)
        <*> (groupOf "observability" o >>= observabilityParser)
        <*> (o .:? "mounts" .!= mempty >>= parseMounts)

-- An absent group reads as empty (its required keys, pinned in the embedded
-- defaults, are then reported missing by its own parser).
groupOf :: Key.Key -> KeyMap.KeyMap Value -> Parser (KeyMap.KeyMap Value)
groupOf key o = case KeyMap.lookup key o of
    Nothing -> pure KeyMap.empty
    Just v -> case v of
        Object g -> pure g
        other -> fail (Key.toString key <> " must be an object, but encountered " <> valueKind other)

serverParser :: KeyMap.KeyMap Value -> Parser ServerSettings
serverParser o = do
    rejectUnknownKeys "server" ["port", "publicUrl", "authToken", "helpMessage", "shutdownDrainTimeout"] o
    ServerSettings
        <$> o .: "port"
        <*> (o .:? "publicUrl" >>= traverse parseUrl)
        <*> (o .:? "authToken" >>= traverse parseSecret)
        <*> o .:? "helpMessage"
        <*> o .: "shutdownDrainTimeout"

queueParser :: KeyMap.KeyMap Value -> Parser QueueSettings
queueParser o = do
    rejectUnknownKeys "queue" ["url", "memoryMaxDepth"] o
    QueueSettings
        <$> (o .:? "url" >>= traverse parseUrl)
        <*> (o .:? "memoryMaxDepth" >>= traverse (parsePositiveInt "queue.memoryMaxDepth"))

limitsParser :: KeyMap.KeyMap Value -> Parser LimitsSettings
limitsParser o = do
    rejectUnknownKeys "limits" ["maxResponseBytes", "maxVersionCount", "maxNestingDepth", "maxRequestBytes"] o
    LimitsSettings
        <$> (o .:? "maxResponseBytes" >>= traverse (parsePositiveInt "limits.maxResponseBytes"))
        <*> o .: "maxVersionCount"
        <*> o .: "maxNestingDepth"
        <*> (o .:? "maxRequestBytes" >>= traverse (parsePositiveInt "limits.maxRequestBytes"))

cacheParser :: KeyMap.KeyMap Value -> Parser CacheSettings
cacheParser o = do
    rejectUnknownKeys "cache" ["ttl", "maxEntries", "maxBytes"] o
    CacheSettings
        <$> (o .: "ttl" >>= parseSeconds)
        <*> (o .:? "maxEntries" >>= traverse (parsePositiveInt "cache.maxEntries"))
        <*> (o .:? "maxBytes" >>= traverse (parsePositiveInt "cache.maxBytes"))

integrityParser :: KeyMap.KeyMap Value -> Parser IntegritySettings
integrityParser o = do
    rejectUnknownKeys "integrity" ["minPublic", "minTrusted", "divergencePolicy"] o
    IntegritySettings
        <$> (o .: "minPublic" >>= parseEnum parseMinIntegrity "integrity.minPublic")
        <*> (o .: "minTrusted" >>= parseEnum parseMinTrustedIntegrity "integrity.minTrusted")
        <*> (o .: "divergencePolicy" >>= parseEnum parseDivergencePolicy "integrity.divergencePolicy")

egressParser :: KeyMap.KeyMap Value -> Parser EgressSettings
egressParser o = do
    rejectUnknownKeys "egress" ["additionalBlockedRanges"] o
    EgressSettings
        <$> (o .:? "additionalBlockedRanges" .!= String "" >>= parseAdditionalBlockedRanges)

advisoriesParser :: KeyMap.KeyMap Value -> Parser AdvisoriesSettings
advisoriesParser o = do
    rejectUnknownKeys "advisories" ["bucket", "pollInterval", "compileInterval", "dataDir", "osvExportBaseUrl", "maxDatabaseBytes"] o
    AdvisoriesSettings
        <$> o .:? "bucket"
        <*> (o .: "pollInterval" >>= parseDelaySeconds "advisories.pollInterval")
        <*> (o .: "compileInterval" >>= parseDelaySeconds "advisories.compileInterval")
        <*> o .: "dataDir"
        <*> o .: "osvExportBaseUrl"
        <*> (o .: "maxDatabaseBytes" >>= parsePositiveInt "advisories.maxDatabaseBytes")

runtimeParser :: KeyMap.KeyMap Value -> Parser RuntimeSettings
runtimeParser o = do
    rejectUnknownKeys "runtime" ["cores", "maxHeapBytes", "serveMaxInFlight", "publicConnectionsPerHost", "privateConnectionsPerHost"] o
    RuntimeSettings
        <$> (o .:? "cores" >>= traverse (parsePositiveInt "runtime.cores"))
        <*> (o .:? "maxHeapBytes" >>= traverse (parsePositiveInt "runtime.maxHeapBytes"))
        <*> (o .:? "serveMaxInFlight" >>= traverse (parsePositiveInt "runtime.serveMaxInFlight"))
        <*> (o .:? "publicConnectionsPerHost" >>= traverse (parsePositiveInt "runtime.publicConnectionsPerHost"))
        <*> (o .:? "privateConnectionsPerHost" >>= traverse (parsePositiveInt "runtime.privateConnectionsPerHost"))

observabilityParser :: KeyMap.KeyMap Value -> Parser ObservabilitySettings
observabilityParser o = do
    rejectUnknownKeys "observability" ["logFormat", "telemetry"] o
    ObservabilitySettings
        <$> (o .: "logFormat" >>= parseEnum parseLogFormat "observability.logFormat")
        <*> (o .: "telemetry" >>= parseEnum parseTelemetrySwitch "observability.telemetry")

acceptedDocumentKeys :: [Key.Key]
acceptedDocumentKeys =
    [ "server"
    , "queue"
    , "limits"
    , "cache"
    , "integrity"
    , "egress"
    , "advisories"
    , "runtime"
    , "observability"
    , "mounts"
    , "rules"
    ]

{- | Parse every mount in the merged @mounts@ object, the shipped per-ecosystem
templates included. Which of them are /active/ (operator-declared, served, and
required to be complete) is decided against the operator overlay in
"Ecluse.Config"; this parser stays a faithful projection of the merged document.
-}
parseMounts :: KeyMap.KeyMap Value -> Parser (Map.Map Ecosystem MountConfig)
parseMounts km = Map.fromList <$> traverse parseMountEntry (KeyMap.toList km)

parseMountEntry :: (Key.Key, Value) -> Parser (Ecosystem, MountConfig)
parseMountEntry (k, v) = do
    eco <- case parseEcosystem (Key.toText k) of
        Just e -> pure e
        Nothing -> fail ("Invalid ecosystem: " <> T.unpack (Key.toText k))
    mcfg <- parseJSON v
    pure (eco, mcfg)

parseSecret :: Value -> Parser Secret
parseSecret = withText "Secret" (pure . mkSecret)

parseAdditionalBlockedRanges :: Value -> Parser [IPRange]
parseAdditionalBlockedRanges = withText "IPRange list" $ \t ->
    if T.null (T.strip t)
        then pure []
        else traverse parseBlockedRangeEntry (T.splitOn "," t)

parseBlockedRangeEntry :: Text -> Parser IPRange
parseBlockedRangeEntry entry =
    let trimmed = T.strip entry
     in case parseBlockedRange trimmed of
            Just range -> pure range
            Nothing -> fail ("invalid CIDR range in additionalBlockedRanges: " <> T.unpack trimmed)

parseSeconds :: Value -> Parser NominalDiffTime
parseSeconds (String t) = case readMaybe (T.unpack t) :: Maybe Integer of
    Just n | n >= 0 -> pure (fromInteger n)
    _ -> fail ("expected a non-negative integer count of seconds, got " <> show t)
parseSeconds (Number n) =
    let val = truncate n :: Integer
     in if val >= 0 then pure (fromInteger val) else fail "expected a non-negative integer count of seconds"
parseSeconds _ = fail "expected a String or Number for Seconds"

parsePositiveInt :: String -> Int -> Parser Int
parsePositiveInt field value
    | value > 0 = pure value
    | otherwise = fail (field <> " must be a positive integer")

{- A recurring delay: positive (zero would spin the poll without
yielding) and bounded so its microsecond conversion fits 'Int'
rather than wrapping into an invalid negative delay.
-}
parseDelaySeconds :: String -> Value -> Parser NominalDiffTime
parseDelaySeconds field v = do
    secs <- parseSeconds v
    let n = truncate secs :: Integer
        maxDelay = toInteger (maxBound :: Int) `div` 1_000_000
    if n >= 1 && n <= maxDelay
        then pure secs
        else fail (field <> " must be a positive integer count of seconds, at most " <> show maxDelay)

instance FromJSON RulePatch where
    parseJSON = withObject "rules" $ \o ->
        RulePatch . Map.fromList <$> traverse decodeEntry (KeyMap.toList o)
      where
        decodeEntry (k, v) = (Key.toText k,) <$> parseJSON v

instance FromJSON RuleEntry where
    parseJSON = withObject "rule" $ \o -> do
        rejectSecretKeys o
        rejectUnknownKeys "rule" ["type", "precedence", "enabled", "ageSeconds", "scope", "identity", "minSeverity", "onUnavailable"] o
        RuleEntry
            <$> o .:? "type"
            <*> o .:? "precedence"
            <*> o .:? "enabled"
            <*> o .:? "ageSeconds"
            <*> o .:? "scope"
            <*> o .:? "identity"
            <*> o .:? "minSeverity"
            <*> o .:? "onUnavailable"
