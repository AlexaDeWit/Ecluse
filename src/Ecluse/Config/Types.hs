-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.Types (
    Url (..),
    mkUrl,
    unUrl,
    QueueBackend (..),
    parseQueueBackend,
    MirrorCredential (..),
    MountConfig (..),
    AppConfig (..),
    MountRegistries (..),
    MirrorTarget (..),
    Mount (..),
    MountMap,
    Config (..),
    ConfigError (..),
    renderConfigError,
) where

import Data.IP (IPRange)
import Data.Text qualified as T
import Data.Time (NominalDiffTime)

import Ecluse.Config.Rule (PolicyError, RulePatch, renderPolicyError)
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Package (Scope)
import Ecluse.Core.Package.Integrity (MinIntegrity, MinTrustedIntegrity)
import Ecluse.Core.Package.Merge (DivergencePolicy)
import Ecluse.Core.Rules.Types (PrecededRule)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Wire (WireVocab (..), parseWire)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig)
import Ecluse.Runtime.Log (LogFormat)
import Ecluse.Runtime.Telemetry (TelemetrySwitch)

newtype Url = Url Text
    deriving stock (Eq, Ord, Show)

mkUrl :: Text -> Either Text Url
mkUrl raw =
    let trimmed = T.strip raw
     in if T.null trimmed
            then Left "expected a non-empty URL"
            else Right (Url trimmed)

unUrl :: Url -> Text
unUrl (Url u) = u

data QueueBackend
    = SqsQueue
    | PubSubQueue
    | MemoryQueue
    deriving stock (Eq, Show)

instance WireVocab QueueBackend where
    wireKind = "queue provider"
    wireTable =
        (SqsQueue, "sqs")
            :| [ (PubSubQueue, "pubsub")
               , (MemoryQueue, "memory")
               ]

parseQueueBackend :: Text -> Either Text QueueBackend
parseQueueBackend = parseWire

{- | The mirror-write credential, __derived from the mirror-target URL__ so a token
can never be paired with an endpoint it was not minted for. A CodeArtifact endpoint
encodes its whole identity in its host, so that identity is parsed straight from the
URL; any other host is written with an operator-supplied static bearer. The choice is
made once, at config load ('Ecluse.Config.MirrorCredential.resolveMirrorCredential'),
and carried here so the pairing is correct by construction.
-}
data MirrorCredential
    = -- | A CodeArtifact mirror target: the mint identity parsed from its host.
      MirrorCodeArtifact CodeArtifactConfig
    | -- | Any other mirror target: an operator-supplied static write token.
      MirrorStatic Secret
    deriving stock (Eq, Show)

data MountConfig = MountConfig
    { mntPrivateUpstream :: Maybe RegistryUrl
    , mntPublicUpstream :: RegistryUrl
    , mntMirrorTarget :: Maybe RegistryUrl
    , mntMirrorTargetToken :: Maybe Secret
    , mntRespectUpstreamTarballHost :: Bool
    , mntMirrorCodeArtifactTokenDuration :: Maybe Natural
    , mntPublicationTarget :: Maybe RegistryUrl
    , mntPublicationTargetToken :: Maybe Secret
    , mntPublishScopes :: [Scope]
    , mntAdditionalRules :: RulePatch
    }
    deriving stock (Eq, Show)

data AppConfig = AppConfig
    { cfgPort :: Int
    , cfgMounts :: Map Ecosystem MountConfig
    , cfgQueueBackend :: QueueBackend
    , cfgQueueUrl :: Maybe Url
    , cfgQueueMemoryMaxDepth :: Int
    , cfgAwsRegion :: Maybe Text
    , cfgAwsEndpointUrlSqs :: Maybe Text
    , cfgAwsEndpointUrl :: Maybe Text
    , cfgGoogleProject :: Maybe Text
    , cfgAuthToken :: Maybe Secret
    , cfgHelpMessage :: Maybe Text
    , cfgCveSyncInterval :: NominalDiffTime
    , cfgShutdownDrainTimeout :: Int
    , cfgCores :: Maybe Int
    , cfgMaxHeapBytes :: Maybe Int
    , cfgServeMaxInFlight :: Maybe Int
    , cfgPublicConnectionsPerHost :: Maybe Int
    , cfgPrivateConnectionsPerHost :: Maybe Int
    , cfgCacheTtl :: NominalDiffTime
    , cfgCacheMaxEntries :: Int
    , cfgCacheMaxBytes :: Int
    , cfgMaxResponseBytes :: Int
    , cfgMaxVersionCount :: Int
    , cfgMaxNestingDepth :: Int
    , cfgLogFormat :: LogFormat
    , cfgTelemetry :: TelemetrySwitch
    , cfgPublicUrl :: Maybe Url
    , cfgMinPublicIntegrity :: MinIntegrity
    , cfgMinTrustedIntegrity :: MinTrustedIntegrity
    , cfgDivergencePolicy :: DivergencePolicy
    , cfgAdditionalBlockedRanges :: [IPRange]
    , cfgOsvDataDir :: FilePath
    , cfgOsvExportBaseUrl :: Text
    , cfgVulnerabilityDatabaseBucket :: Maybe Text
    , cfgCveDbPollInterval :: NominalDiffTime
    , cfgMaxOsvDbBytes :: Int
    }
    deriving stock (Eq, Show)

data MountRegistries = MountRegistries
    { regPrivateUpstream :: RegistryUrl
    , regPublicUpstream :: RegistryUrl
    , regMirrorTarget :: MirrorTarget
    }
    deriving stock (Eq, Show)

data MirrorTarget = MirrorTarget
    { mtUrl :: RegistryUrl
    , mtCredential :: MirrorCredential
    }
    deriving stock (Eq, Show)

data Mount = Mount
    { mountEcosystem :: Ecosystem
    , mountRegistries :: MountRegistries
    , mountPolicy :: [PrecededRule]
    }
    deriving stock (Eq, Show)

type MountMap = Map Ecosystem Mount

data Config = Config
    { configApp :: AppConfig
    , configMounts :: MountMap
    }
    deriving stock (Eq, Show)

data ConfigError
    = ParseError Text
    | PolicyErrors [PolicyError]
    | -- | An operator-declared (active) mount does not define its private upstream.
      MountMissingPrivateUpstream Ecosystem
    | {- | An operator-declared (active) mount does not declare its mirror target.
      The declaration is required even when the intended value equals the private
      upstream: activation implies a mirror write, and the target is never implied
      from another endpoint.
      -}
      MountMissingMirrorTarget Ecosystem
    | {- | An active mount's mirror target is not a CodeArtifact endpoint (whose write
      token would be minted), so it needs an explicit static write token, and none was
      supplied. Carries the mount's ecosystem.
      -}
      MirrorCredentialTokenMissing Ecosystem
    | {- | An active mount's mirror target is a CodeArtifact endpoint (its write token is
      minted automatically from the host identity) yet a static write token was also
      supplied. Refused so the two credential sources can never silently contend.
      Carries the mount's ecosystem.
      -}
      MirrorCredentialConflict Ecosystem
    deriving stock (Eq, Show)

renderConfigError :: ConfigError -> Text
renderConfigError (ParseError e) = e
renderConfigError (PolicyErrors es) = T.unlines (map renderPolicyError es)
renderConfigError (MountMissingPrivateUpstream eco) =
    let name = ecosystemName eco
        envKey = "ECLUSE_MOUNTS__" <> T.toUpper name <> "__PRIVATE_UPSTREAM"
     in "mount \""
            <> name
            <> "\" is declared in the configuration, so it must define its private upstream: set mounts."
            <> name
            <> ".privateUpstream in the config document (or "
            <> envKey
            <> "), or remove the mount's keys to leave it unmounted"
renderConfigError (MountMissingMirrorTarget eco) =
    let name = ecosystemName eco
        envKey = "ECLUSE_MOUNTS__" <> T.toUpper name <> "__MIRROR_TARGET"
     in "mount \""
            <> name
            <> "\" is declared in the configuration, so it must declare its mirror target explicitly (even when it equals the private upstream): set mounts."
            <> name
            <> ".mirrorTarget in the config document (or "
            <> envKey
            <> "), or remove the mount's keys to leave it unmounted"
renderConfigError (MirrorCredentialTokenMissing eco) =
    let name = ecosystemName eco
        envKey = "ECLUSE_MOUNTS__" <> T.toUpper name <> "__MIRROR_TARGET_TOKEN"
     in "mount \""
            <> name
            <> "\" mirror target is not a CodeArtifact endpoint, so its write credential is not minted: set a static write token with mounts."
            <> name
            <> ".mirrorTargetToken (or "
            <> envKey
            <> ")"
renderConfigError (MirrorCredentialConflict eco) =
    let name = ecosystemName eco
        envKey = "ECLUSE_MOUNTS__" <> T.toUpper name <> "__MIRROR_TARGET_TOKEN"
     in "mount \""
            <> name
            <> "\" mirror target is a CodeArtifact endpoint (its write token is minted from the host identity), so a static write token must not also be set: remove mounts."
            <> name
            <> ".mirrorTargetToken (or "
            <> envKey
            <> ")"
