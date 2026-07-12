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
    CredentialBackend (..),
    parseCredentialBackend,
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
import Ecluse.Core.Rules.Policy (PrecededRule)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Wire (WireVocab (..), parseWire)
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

data CredentialBackend
    = CodeArtifactCredential
    | StaticCredential
    | GcpArtifactRegistryCredential
    deriving stock (Eq, Ord, Show)

instance WireVocab CredentialBackend where
    wireKind = "credential provider"
    wireTable =
        (CodeArtifactCredential, "codeartifact")
            :| [ (StaticCredential, "static")
               , (GcpArtifactRegistryCredential, "gcp-artifact-registry")
               ]

parseCredentialBackend :: Text -> Either Text CredentialBackend
parseCredentialBackend = parseWire

data MountConfig = MountConfig
    { mntPrivateUpstream :: Maybe RegistryUrl
    , mntPublicUpstream :: RegistryUrl
    , mntMirrorTarget :: Maybe RegistryUrl
    , mntMirrorTargetToken :: Maybe Secret
    , mntCredentialProvider :: CredentialBackend
    , mntRespectUpstreamTarballHost :: Bool
    , mntMirrorCodeArtifactDomain :: Maybe Text
    , mntMirrorCodeArtifactDomainOwner :: Maybe Text
    , mntMirrorCodeArtifactRegion :: Maybe Text
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
    , mtCredential :: CredentialBackend
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
