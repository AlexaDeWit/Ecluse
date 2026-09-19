-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | The document decoders: one 'GroupDecoder' per configuration group, assembled from
"Ecluse.Config.Parser"'s key vocabulary, plus the @FromJSON@ instances that run them.

The instances are orphans because the types live in "Ecluse.Config.Types", which carries no Aeson
dependency. A group's decoder declares every key the group admits, so an unknown key refuses.
-}
module Ecluse.Config.Aeson () where

import Data.Aeson (FromJSON (..), Value (..), withObject)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.IP (IPRange)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific, base10Exponent, toBoundedInteger)
import Data.Text qualified as T
import Data.Time (NominalDiffTime)

import Ecluse.Config.Parser
import Ecluse.Config.Rule
import Ecluse.Config.Types

import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (..), ecosystemName, parseEcosystem)
import Ecluse.Core.Package (Scope)
import Ecluse.Core.Package.Integrity (parseMinIntegrity, parseMinTrustedIntegrity)
import Ecluse.Core.Registry.Maintenance.Budget (parseQuotaDimension, parseRequestKind)
import Ecluse.Core.Registry.Npm.Project (projectScope)
import Ecluse.Core.Registry.PyPI.FirstParty (PyPIFirstParty, projectFirstPartyEntry)
import Ecluse.Core.Security (parseBlockedRange)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Text (readDecimalText)
import Ecluse.Runtime.Log (parseLogFormat, parseLogLevel)
import Ecluse.Runtime.Telemetry (parseTelemetrySwitch)

-- A mount's refusals name the bare key, because "Ecluse.Config" reports the mount around them.
-- The ecosystem comes from the mounts key, so a per-ecosystem value reads in its own shape.
mountDecoder :: Ecosystem -> GroupDecoder MountConfig
mountDecoder eco =
    MountConfig
        <$> optionalPlainKey "enabled"
        <*> optionalKey "privateUpstream" parseReadTarget
        <*> requiredKey "publicUpstream" parsePublicUpstream
        <*> optionalKey "mirrorTarget" parseMirrorTarget
        <*> optionalKey "publicationTarget" parsePublicationTarget
        <*> optionalKey "firstParty" (parseFirstParty eco)
        <*> nestedKey "integrity" (decodeGroup "integrity" mountIntegrityDecoder)
        <*> optionalPlainKeyOr "rules" (RulePatch Map.empty)

-- Each endpoint admits exactly the tags and keys its cell of the store admission matrix names,
-- so a shape outside the cell has nothing to parse into and refuses with the key path.
parsePublicUpstream :: String -> Value -> Parser RegistryUrl
parsePublicUpstream = taggedTarget [TagCase (tagKey TagRegistry) targetUrl]

parseReadTarget :: String -> Value -> Parser PrivateEndpoint
parseReadTarget = taggedTarget [readCase TagRegistry, readCase TagCodeArtifact, verdaccio]
  where
    readCase tag = TagCase (tagKey tag) (PrivateEndpoint . Target tag <$> targetUrl <*> pure Nothing <*> pure DeletionWithheld)
    verdaccio = TagCase (tagKey TagVerdaccio) (PrivateEndpoint . Target TagVerdaccio <$> targetUrl <*> optionalKey "token" parseSecret <*> deletionConsent)

parseMirrorTarget :: String -> Value -> Parser MirrorEndpoint
parseMirrorTarget =
    taggedTarget
        [ TagCase (tagKey TagRegistry) (MirrorEndpoint <$> targetUrl <*> (WriteRegistry <$> writeToken))
        , TagCase (tagKey TagCodeArtifact) (MirrorEndpoint <$> targetUrl <*> (WriteCodeArtifact <$> mintLifetime))
        , TagCase (tagKey TagVerdaccio) (MirrorEndpoint <$> targetUrl <*> (WriteVerdaccio <$> writeToken <*> deletionConsent))
        ]
  where
    writeToken = requiredKey "token" parseSecret
    mintLifetime = optionalKey "tokenDuration" parseCodeArtifactDuration

parsePublicationTarget :: String -> Value -> Parser PublicationEndpoint
parsePublicationTarget = taggedTarget (map publishCase [TagRegistry, TagCodeArtifact, TagVerdaccio])
  where
    publishCase tag =
        TagCase (tagKey tag) (PublicationEndpoint . Target tag <$> targetUrl <*> optionalKey "token" parseSecret)

-- The one key every tag admits, refined by the egress boundary's own constructor.
targetUrl :: GroupDecoder RegistryUrl
targetUrl = requiredKey "url" parseRegistryUrl

-- Unwritten, the Dredger holds no consent to delete from the store.
deletionConsent :: GroupDecoder DeletionConsent
deletionConsent = optionalKeyOr "permitDeletion" False (const (pure . consentOf))
  where
    consentOf granted = if granted then DeletionPermitted else DeletionWithheld

tagKey :: StoreTag -> Key.Key
tagKey = Key.fromText . storeTagName

-- An absent mount integrity object inherits the global trusted floor.
mountIntegrityDecoder :: GroupDecoder MountIntegrity
mountIntegrityDecoder =
    MountIntegrity
        <$> optionalKey "minTrusted" (parseEnum parseMinTrustedIntegrity)
        <* optionalKey "divergencePolicy" parseLegacyDivergencePolicy

instance FromJSON AppConfig where
    parseJSON = withObject "AppConfig" (decodeGroup "document" documentDecoder)

-- @rules@ is accepted here and read by "Ecluse.Config" out of the merged document, never
-- through this decoder.
documentDecoder :: GroupDecoder AppConfig
documentDecoder =
    AppConfig
        <$> nestedKey "server" (decodeGroup "server" serverDecoder)
        <*> nestedKey "queue" (decodeGroup "queue" queueDecoder)
        <*> nestedKey "limits" (decodeGroup "limits" limitsDecoder)
        <*> nestedKey "cache" (decodeGroup "cache" cacheDecoder)
        <*> nestedKey "integrity" (decodeGroup "integrity" integrityDecoder)
        <*> nestedKey "egress" (decodeGroup "egress" egressDecoder)
        <*> nestedKey "advisories" (decodeGroup "advisories" advisoriesDecoder)
        <*> nestedKey "runtime" (decodeGroup "runtime" runtimeDecoder)
        <*> nestedKey "observability" (decodeGroup "observability" observabilityDecoder)
        <*> nestedKey "dredger" (decodeGroup "dredger" dredgerDecoder)
        <*> optionalKeyOr "mounts" mempty (const parseMounts)
        <* unreadKey "rules"

serverDecoder :: GroupDecoder ServerSettings
serverDecoder =
    ServerSettings
        <$> requiredKey "port" parsePort
        <*> optionalKey "publicUrl" parseHttpUrl
        <*> optionalKey "authToken" parseSecret
        <*> optionalPlainKey "helpMessage"
        <*> requiredKey "shutdownDrainTimeout" parsePositiveInt

queueDecoder :: GroupDecoder QueueSettings
queueDecoder =
    QueueSettings
        <$> optionalKey "url" parseQueueUrl
        <*> optionalKey "maxMemoryDepth" parsePositiveInt
        <*> requiredKey "maxReceiveCount" parsePositiveInt

limitsDecoder :: GroupDecoder LimitsSettings
limitsDecoder =
    LimitsSettings
        <$> optionalKey "maxResponseBytes" parsePositiveInt
        <*> requiredKey "maxVersionCount" parsePositiveInt
        <*> requiredKey "maxArtifactCount" parsePositiveInt
        <*> requiredKey "maxNestingDepth" parsePositiveInt
        <*> requiredKey "maxAdvisoryDatabaseBytes" parsePositiveInt
        <*> optionalKey "maxRequestBytes" parsePositiveInt
        <*> optionalKey "maxArtifactBytes" parsePositiveInt

cacheDecoder :: GroupDecoder CacheSettings
cacheDecoder =
    CacheSettings
        <$> requiredKey "ttl" parseSeconds
        <*> optionalKey "maxEntries" parsePositiveInt
        <*> optionalKey "maxBytes" parsePositiveInt

integrityDecoder :: GroupDecoder IntegritySettings
integrityDecoder =
    IntegritySettings
        <$> requiredKey "minPublic" (parseEnum parseMinIntegrity)
        <*> requiredKey "minTrusted" (parseEnum parseMinTrustedIntegrity)
        <* optionalKey "divergencePolicy" parseLegacyDivergencePolicy

-- A removed refusal must fail at boot instead of silently becoming private preference.
parseLegacyDivergencePolicy :: String -> Value -> Parser ()
parseLegacyDivergencePolicy field = expectString field $ \value ->
    case T.toLower (T.strip value) of
        "warn" -> pure ()
        removed
            | removed `elem` ["fail-closed", "fail_closed", "failclosed"] ->
                fail (field <> ": fail-closed was removed. Remove this setting after accepting private preference and divergence alarms.")
        _ -> fail (field <> ": expected deprecated warn, or remove this setting")

egressDecoder :: GroupDecoder EgressSettings
egressDecoder =
    EgressSettings
        <$> optionalKeyOr "additionalBlockedRanges" (String "") parseBlockedRanges

advisoriesDecoder :: GroupDecoder AdvisoriesSettings
advisoriesDecoder =
    AdvisoriesSettings
        <$> optionalKey "url" parseAdvisoryStoreUrl
        <*> requiredKey "pollInterval" parseDelaySeconds
        <*> requiredKey "compileInterval" parseDelaySeconds
        <*> plainKey "dataDir"
        <*> requiredKey "osvExportBaseUrl" parseHttpUrl
        <*> requiredKey "epssFeedUrl" parseHttpUrl
        <*> nestedKey "quietTime" parseQuietTimes
        <*> requiredKey "epssQuietTime" parseDelaySeconds
        <*> optionalKey "maxAgeSeconds" parseDelaySeconds

runtimeDecoder :: GroupDecoder RuntimeSettings
runtimeDecoder =
    RuntimeSettings
        <$> optionalKey "cores" parsePositiveInt
        <*> optionalKey "coresCeiling" parsePositiveInt
        <*> optionalKey "maxHeapBytes" parsePositiveInt
        <*> optionalKey "serveMaxInFlight" parsePositiveInt
        <*> optionalKey "publicConnectionsPerHost" parsePositiveInt
        <*> optionalKey "privateConnectionsPerHost" parsePositiveInt

observabilityDecoder :: GroupDecoder ObservabilitySettings
observabilityDecoder =
    ObservabilitySettings
        <$> requiredKey "logFormat" (parseEnum parseLogFormat)
        <*> requiredKey "logLevel" (parseEnum parseLogLevel)
        <*> requiredKey "telemetry" (parseEnum parseTelemetrySwitch)

dredgerDecoder :: GroupDecoder DredgerSettings
dredgerDecoder =
    DredgerSettings
        <$> requiredKey "chunkSize" parsePositiveInt
        <*> requiredKey "chunkPause" parseDelaySeconds
        <*> requiredKey "cyclePause" parseDelaySeconds
        <*> optionalKey "targetCycleWindow" parseDelaySeconds
        <*> optionalKey "requestBudgetFraction" parseUnitFraction
        <*> nestedKey "quotaOverrides" parseQuotaOverrides
        <*> optionalKey "deletionCap" parsePositiveInt
        <*> plainKey "fullWalk"

-- The key is the store URL the entry describes, kept as written so the boot can match it against
-- the endpoints the mounts declare.
parseQuotaOverrides :: KeyMap.KeyMap Value -> Parser (Map.Map Text QuotaOverride)
parseQuotaOverrides km = Map.fromList <$> traverse parseQuotaOverrideEntry (KeyMap.toList km)

parseQuotaOverrideEntry :: (Key.Key, Value) -> Parser (Text, QuotaOverride)
parseQuotaOverrideEntry (k, v) =
    (Key.toText k,) <$> withObject "QuotaOverride" (decodeBareGroup entryPath (quotaOverrideDecoder entryPath)) v
  where
    entryPath = "dredger.quotaOverrides." <> Key.toString k

quotaOverrideDecoder :: String -> GroupDecoder QuotaOverride
quotaOverrideDecoder entryPath =
    QuotaOverride
        <$> optionalKey "scope" (`expectString` pure)
        <*> nestedKey "quotas" (parseRates entryPath parseQuotaDimension "quota dimension")
        <*> nestedKey "requestWeights" (parseRates entryPath parseRequestKind "request kind")

{- Every entry under one map of positive rates, keyed by a name this build meters under. An
unknown name fails the load, because a rate nothing reads would silently pace nothing. -}
parseRates :: (Ord key) => String -> (Text -> Maybe key) -> String -> KeyMap.KeyMap Value -> Parser (Map.Map key Rational)
parseRates entryPath readKey subject km = Map.fromList <$> traverse rate (KeyMap.toList km)
  where
    rate (k, v) = case readKey (Key.toText k) of
        Nothing -> fail (entryPath <> ": " <> Key.toString k <> " is not a " <> subject <> " this build meters")
        Just metered -> (metered,) <$> parsePositiveRate (entryPath <> "." <> Key.toString k) v

parseQuietTimes :: KeyMap.KeyMap Value -> Parser (Map.Map Ecosystem NominalDiffTime)
parseQuietTimes km = Map.fromList <$> traverse parseQuietTimeEntry (KeyMap.toList km)

parseQuietTimeEntry :: (Key.Key, Value) -> Parser (Ecosystem, NominalDiffTime)
parseQuietTimeEntry (k, v) = do
    eco <- ecosystemKey " in advisories.quietTime" k
    (eco,) <$> parseDelaySeconds ("advisories.quietTime." <> T.unpack (Key.toText k)) v

-- Every mount in the merged object, the shipped per-ecosystem templates included.
-- "Ecluse.Config" decides which of them are active and must be complete.
parseMounts :: KeyMap.KeyMap Value -> Parser (Map.Map Ecosystem MountConfig)
parseMounts km = Map.fromList <$> traverse parseMountEntry (KeyMap.toList km)

parseMountEntry :: (Key.Key, Value) -> Parser (Ecosystem, MountConfig)
parseMountEntry (k, v) = do
    eco <- ecosystemKey "" k
    mcfg <- withObject "MountConfig" (decodeBareGroup "mount" (mountDecoder eco)) v
    pure (eco, mcfg)

-- A map key spelled as a mounts key is, so an unknown ecosystem fails the load rather than
-- configuring nothing. The location is appended to the refusal that names the key.
ecosystemKey :: String -> Key.Key -> Parser Ecosystem
ecosystemKey location k = maybe (fail refusal) pure (parseEcosystem (Key.toText k))
  where
    refusal = "Invalid ecosystem" <> location <> ": " <> T.unpack (Key.toText k)

parseSecret :: String -> Value -> Parser Secret
parseSecret field = expectString field (pure . mkSecret)

-- Every arm is explicit, so an added 'Ecosystem' surfaces here as a compiler error. One with no
-- namespace shape yet refuses the key rather than parsing it as another ecosystem's.
parseFirstParty :: Ecosystem -> String -> Value -> Parser FirstParty
parseFirstParty eco field v = case eco of
    Npm -> FirstPartyNpmScopes <$> parseNpmScopes field v
    PyPI -> FirstPartyPyPI <$> parsePyPIFirstParty field v
    RubyGems -> fail (field <> " is not supported for " <> toString (ecosystemName eco) <> " yet")

-- A configured list that names nothing privileges nothing, so it fails the load instead.
parseNpmScopes :: String -> Value -> Parser (NonEmpty Scope)
parseNpmScopes field v = do
    scopes <- commaSeparated field parseScopeEntry v
    maybe (fail (field <> " must name at least one scope")) pure (nonEmpty scopes)

-- Reject a firstParty segment no scope can equal, an empty one or a wrong separator, so a typo
-- fails the load instead of seeding a privilege that covers nothing.
parseScopeEntry :: Text -> Parser Scope
parseScopeEntry entry =
    either (const (fail ("invalid scope in firstParty: " <> show entry))) pure (projectScope entry)

-- A configured list that names nothing privileges nothing, so it fails the load instead.
parsePyPIFirstParty :: String -> Value -> Parser (NonEmpty PyPIFirstParty)
parsePyPIFirstParty field v = do
    entries <- commaSeparated field (parsePyPIEntry field) v
    maybe (fail (field <> " must name at least one distribution or prefix")) pure (nonEmpty entries)

-- Reject a firstParty segment no distribution name or prefix can equal, so a typo fails the load
-- instead of seeding a privilege that covers nothing.
parsePyPIEntry :: String -> Text -> Parser PyPIFirstParty
parsePyPIEntry field entry =
    either (const (fail ("invalid entry in " <> field <> ": " <> show entry))) pure (projectFirstPartyEntry entry)

parseBlockedRanges :: String -> Value -> Parser [IPRange]
parseBlockedRanges field = commaSeparated field parseBlockedRangeEntry

parseBlockedRangeEntry :: Text -> Parser IPRange
parseBlockedRangeEntry entry =
    case parseBlockedRange entry of
        Just range -> pure range
        Nothing -> fail ("invalid CIDR range in additionalBlockedRanges: " <> T.unpack entry)

-- A rate of zero admits no request at all, so it is refused rather than read as a store that may
-- never be swept.
parsePositiveRate :: String -> Value -> Parser Rational
parsePositiveRate field value = case value of
    Number n | Just rate <- boundedRational n, rate > 0 -> pure rate
    _ -> fail (field <> " must be a positive number written within nine decimal places")

-- Neither end of the range is a share, so both are refused rather than read as a sweep that
-- stops or one that takes the whole store.
parseUnitFraction :: String -> Value -> Parser Rational
parseUnitFraction field value = case value of
    Number n | Just share <- boundedRational n, share > 0, share < 1 -> pure share
    _ -> fail (field <> " must be a number above 0 and below 1")

-- The exponent is bounded before the rational is realised, so a 1e999999999 is refused rather
-- than expanded.
boundedRational :: Scientific -> Maybe Rational
boundedRational n
    | base10Exponent n > rationalExponentBound || base10Exponent n < negate rationalExponentBound = Nothing
    | otherwise = Just (toRational n)

rationalExponentBound :: Int
rationalExponentBound = 9

parseSeconds :: String -> Value -> Parser NominalDiffTime
parseSeconds field = \case
    String t -> case readDecimalText t :: Maybe Integer of
        Just n -> boundedSeconds field n
        Nothing -> secondsFailure field (show t)
    -- 'toBoundedInteger' refuses a fractional or out-of-'Int64' value, and its exponent guard
    -- rejects a pathological 1e999999999999 without ever realising the integer.
    Number n -> case toBoundedInteger n :: Maybe Int64 of
        Just val -> boundedSeconds field (toInteger val)
        Nothing -> secondsFailure field (show n)
    other -> fail (field <> " must be a non-negative integer count of seconds, but encountered " <> valueKind other)

boundedSeconds :: String -> Integer -> Parser NominalDiffTime
boundedSeconds field n
    | n >= 0 && n <= toInteger (maxBound :: Int64) = pure (fromInteger n)
    | otherwise = secondsFailure field (show n)

secondsFailure :: String -> String -> Parser a
secondsFailure field got =
    fail (field <> " must be a non-negative integer count of seconds, got " <> got)

parsePositiveInt :: String -> Int -> Parser Int
parsePositiveInt field value
    | value > 0 = pure value
    | otherwise = fail (field <> " must be a positive integer")

{- A recurring delay, positive and bounded. Zero would spin the poll without yielding, and the
bound keeps the microsecond conversion inside 'Int' rather than wrapping to a negative delay.
-}
parseDelaySeconds :: String -> Value -> Parser NominalDiffTime
parseDelaySeconds field v = do
    secs <- parseSeconds field v
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
        decodeGroup "rule" ruleEntryDecoder o

ruleEntryDecoder :: GroupDecoder RuleEntry
ruleEntryDecoder =
    RuleEntry
        <$> optionalPlainKey "type"
        <*> optionalPlainKey "precedence"
        <*> optionalPlainKey "enabled"
        <*> optionalPlainKey "ageSeconds"
        <*> optionalPlainKey "scope"
        <*> optionalPlainKey "identity"
        <*> optionalPlainKey "minCvss"
        <*> optionalPlainKey "minEpss"
        <*> optionalPlainKey "onUnavailable"
