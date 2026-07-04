{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.Types (
    Url (..),
    mkUrl,
    unUrl,
    QueueBackend (..),
    parseQueueBackend,
    renderQueueBackend,
    CredentialBackend (..),
    parseCredentialBackend,
    renderCredentialBackend,
    MirrorCredentialProvider (..),
    parseMirrorCredentialProvider,
    renderMirrorCredentialProvider,
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

import Data.Text qualified as T
import Data.Time (NominalDiffTime)

import Ecluse.Config.Rule (PolicyError, RulePatch, renderPolicyError)
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (Scope)
import Ecluse.Core.Package.Integrity (MinIntegrity, MinTrustedIntegrity)
import Ecluse.Core.Rules.Types (PrecededRule)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Wire (WireVocab (..), parseWire, renderWire)
import Ecluse.Log (LogFormat)
import Ecluse.Telemetry (TelemetrySwitch)

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

renderQueueBackend :: QueueBackend -> Text
renderQueueBackend = renderWire

data CredentialBackend
    = CodeArtifactCredential
    | StaticCredential
    | AdcCredential
    deriving stock (Eq, Ord, Show)

instance WireVocab CredentialBackend where
    wireKind = "credential provider"
    wireTable =
        (CodeArtifactCredential, "codeartifact")
            :| [ (StaticCredential, "static")
               , (AdcCredential, "adc")
               ]

parseCredentialBackend :: Text -> Either Text CredentialBackend
parseCredentialBackend = parseWire

renderCredentialBackend :: CredentialBackend -> Text
renderCredentialBackend = renderWire

newtype MirrorCredentialProvider = MirrorCredentialProvider CredentialBackend
    deriving stock (Eq)

instance WireVocab MirrorCredentialProvider where
    wireKind = "mirror-target credential provider"
    wireTable =
        (MirrorCredentialProvider StaticCredential, "static")
            :| [ (MirrorCredentialProvider CodeArtifactCredential, "codeartifact")
               , (MirrorCredentialProvider AdcCredential, "gcp-artifact-registry")
               ]

parseMirrorCredentialProvider :: Text -> Either Text CredentialBackend
parseMirrorCredentialProvider raw =
    (\(MirrorCredentialProvider backend) -> backend) <$> parseWire raw

renderMirrorCredentialProvider :: CredentialBackend -> Text
renderMirrorCredentialProvider = renderWire . MirrorCredentialProvider

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
    , cfgOsvDataDir :: FilePath
    , cfgOsvExportBaseUrl :: Text
    , cfgVulnerabilityDatabaseBucket :: Maybe Text
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
    , mtQueue :: QueueBackend
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
    deriving stock (Eq, Show)

renderConfigError :: ConfigError -> Text
renderConfigError (ParseError e) = e
renderConfigError (PolicyErrors es) = T.unlines (map renderPolicyError es)
