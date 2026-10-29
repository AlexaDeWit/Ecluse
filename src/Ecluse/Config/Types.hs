-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Settings and load refusals shared by the decoders and composition root.
"Ecluse.Config" resolves the settings into a 'Config'.
-}
module Ecluse.Config.Types (
    Url,
    mkUrl,
    unUrl,
    HttpScheme (..),
    splitHttpScheme,
    StoreTag (..),
    storeTagName,
    Target (..),
    PrivateEndpoint (..),
    DeletionConsent (..),
    MirrorWrite (..),
    MirrorEndpoint (..),
    meTarget,
    PublicationEndpoint (..),
    MintPlan (..),
    ControlPlane (..),
    StoreBackend (..),
    sbTag,
    sbMint,
    sbControl,
    FirstParty (..),
    MountIntegrity (..),
    MountConfig (..),
    AppConfig (..),
    ServerSettings (..),
    QueueTarget (..),
    QueueUrl,
    queueUrlText,
    queueUrlTarget,
    QueueSettings (..),
    AdvisoryStoreTarget (..),
    AdvisoryStoreUrl,
    advisoryStoreUrlText,
    advisoryStoreTarget,
    LimitsSettings (..),
    CacheSettings (..),
    IntegritySettings (..),
    EgressSettings (..),
    AdvisoriesSettings (..),
    RuntimeSettings (..),
    ObservabilitySettings (..),
    DredgerSettings (..),
    QuotaOverride (..),
    MountRegistries (..),
    MountMode (..),
    MirroredLegs (..),
    regPrivateUpstream,
    regMirrorTarget,
    MirrorTarget (..),
    Mount (..),
    MountMap,
    RuleInactivity (..),
    Config (..),
    ConfigError (..),
    renderConfigError,
) where

import Data.IP (IPRange)
import Data.Text qualified as T
import Data.Time (NominalDiffTime)

import Ecluse.Config.Advisory.Internal (
    AdvisoryStoreTarget (..),
    AdvisoryStoreUrl,
    advisoryStoreTarget,
    advisoryStoreUrlText,
 )
import Ecluse.Config.Queue.Internal (QueueTarget (..), QueueUrl, queueUrlTarget, queueUrlText)
import Ecluse.Config.Resolve (mountDocRef, mountKeyRef)
import Ecluse.Config.Rule (PolicyError (SuppressUnknownRule), renderPolicyError)
import Ecluse.Core.BuildIdentity (OperatorContact)
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName)
import Ecluse.Core.Osv.Provenance (SourcePathRecording)
import Ecluse.Core.Osv.Publication (Percent)
import Ecluse.Core.Package (Scope)
import Ecluse.Core.Package.Integrity (MinIntegrity, MinTrustedIntegrity)
import Ecluse.Core.Registry.Cargo.FirstParty (CargoFirstParty)
import Ecluse.Core.Registry.Maintenance.Budget (QuotaDimension, RequestKind)
import Ecluse.Core.Registry.PyPI.FirstParty (PyPIFirstParty)
import Ecluse.Core.Registry.RubyGems.FirstParty (RubyGemsFirstParty)
import Ecluse.Core.Rules.Types (PrecededRule)
import Ecluse.Core.Security (hostPortAddress, refuseCredentialMaterial)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Server.Encoding (MetadataCompression)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig)
import Ecluse.Runtime.Log (LogFormat, LogLevel)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore)
import Ecluse.Runtime.Telemetry (TelemetrySwitch)

{- | An operator-configured @http(s)@ URL, whitespace-trimmed. 'mkUrl' is the only builder, so no
value exists carrying credential material, another scheme, or an authority the egress gate misses.
-}
newtype Url = Url Text
    deriving stock (Eq, Ord, Show)

{- | Build a 'Url' from a configuration key and the value written under it, the key naming every
refusal. The credential refusal runs first, because the two refusals below it quote the value.
-}
mkUrl :: Text -> Text -> Either Text Url
mkUrl key raw
    | Left reason <- refuseCredentialMaterial key trimmed = Left reason
    | isNothing (splitHttpScheme trimmed) =
        Left (key <> " must be an http:// or https:// URL (got " <> trimmed <> ")")
    | isNothing (hostPortAddress trimmed) =
        Left
            ( key
                <> " must carry a host and, when a port is written, a decimal port in 1..65535 (got "
                <> trimmed
                <> ")"
            )
    | otherwise = Right (Url trimmed)
  where
    trimmed = T.strip raw

-- | The stored URL text.
unUrl :: Url -> Text
unUrl (Url u) = u

-- | The scheme a configured @http(s)@ URL writes.
data HttpScheme = Http | Https
    deriving stock (Eq, Show)

{- | Split a URL into the scheme it writes and the text after the separator, or 'Nothing' for
neither @http@ nor @https@. It is the one scheme check the configuration layer shares.
-}
splitHttpScheme :: Text -> Maybe (HttpScheme, Text)
splitHttpScheme raw =
    ((Https,) <$> T.stripPrefix "https://" raw) <|> ((Http,) <$> T.stripPrefix "http://" raw)

-- | Which store backend an endpoint names: the one key under it, which the load validates the URL against, never a guess from the host.
data StoreTag
    = -- | Any host that speaks the ecosystem's protocol, authenticated by a static token.
      TagRegistry
    | -- | A CodeArtifact repository endpoint, which mints its own write token.
      TagCodeArtifact
    | -- | A Verdaccio development store, authenticated by a static token.
      TagVerdaccio
    deriving stock (Eq, Show)

-- | The tag as an operator writes it, and as a refusal names it.
storeTagName :: StoreTag -> Text
storeTagName = \case
    TagRegistry -> "registry"
    TagCodeArtifact -> "codeArtifact"
    TagVerdaccio -> "verdaccio"

-- | An endpoint as a mount declares it: the tag naming its store, and the URL under that tag.
data Target = Target
    { tgtTag :: StoreTag
    , tgtUrl :: RegistryUrl
    }
    deriving stock (Eq, Show)

-- | Whether the operator consented to @ecluse dredger@ deleting from a Verdaccio store. No other tag carries it.
data DeletionConsent
    = DeletionPermitted
    | DeletionWithheld
    deriving stock (Eq, Show)

-- | How the mirror write, Écluse's one standing credential, authenticates: a minting tag carries no static token, and any other requires one.
data MirrorWrite
    = -- | Any protocol-speaking host: the operator's static write token.
      WriteRegistry Secret
    | -- | A CodeArtifact repository: the requested lifetime of the token it mints.
      WriteCodeArtifact (Maybe Natural)
    | -- | A Verdaccio store: its static write token, and the operator's deletion consent.
      WriteVerdaccio Secret DeletionConsent
    deriving stock (Eq, Show)

-- | A declared @mirrorTarget@: where the mirror writes, and how that write authenticates.
data MirrorEndpoint = MirrorEndpoint
    { meUrl :: RegistryUrl
    , meWrite :: MirrorWrite
    , meUploadUrl :: Maybe RegistryUrl
    -- ^ Explicit native upload endpoint when it differs from the read root.
    , meUsername :: Maybe Text
    -- ^ Static Basic username. A managed backend supplies its own convention.
    }
    deriving stock (Eq, Show)

-- | A private read endpoint with maintenance authority used only by Dredger.
data PrivateEndpoint = PrivateEndpoint
    { preTarget :: Target
    , preToken :: Maybe Secret
    , preConsent :: DeletionConsent
    }
    deriving stock (Eq, Show)

-- | The mirror endpoint as the collision rules read it. The tag comes from 'meWrite'.
meTarget :: MirrorEndpoint -> Target
meTarget endpoint = Target (writeTag (meWrite endpoint)) (meUrl endpoint)
  where
    writeTag = \case
        WriteRegistry{} -> TagRegistry
        WriteCodeArtifact{} -> TagCodeArtifact
        WriteVerdaccio{} -> TagVerdaccio

-- | A publication endpoint and the credential that replaces an authenticated edge token.
data PublicationEndpoint = PublicationEndpoint
    { peTarget :: Target
    , peUploadUrl :: Maybe RegistryUrl
    -- ^ Approved API base, separate from the read/index root.
    , peToken :: Maybe Secret
    , peUsername :: Maybe Text
    -- ^ Username paired with the configured publication token.
    }
    deriving stock (Eq, Show)

-- | How a mount's mirror write authenticates, projected from its resolved 'StoreBackend'.
data MintPlan
    = -- | A CodeArtifact mirror target: the mint identity parsed from its host.
      MintCodeArtifact CodeArtifactConfig
    | -- | Any other mirror target: an operator-supplied static write token.
      MintStatic Secret
    deriving stock (Eq, Show)

-- | The control plane a mount's store offers, the face @ecluse dredger@ deletes through.
data ControlPlane
    = -- | The CodeArtifact repository the target addresses, which the load has vetted.
      ControlCodeArtifact CodeArtifactStore
    | {- | A store with no vendor control plane, swept through the ecosystem protocol's own
      verbs: its write token, and the operator's consent to delete from it.
      -}
      ControlProtocol Secret DeletionConsent
    | -- | The tag names no control plane this build implements.
      ControlNone
    deriving stock (Eq, Show)

{- | A mount's store backend, resolved once at load ("Ecluse.Config.Target"), so no two roles infer
a different one. The tag discriminates, so no arm pairs one store's mint with another's plane.
-}
data StoreBackend
    = -- | A protocol-speaking host: its static write token, and no control plane.
      BackendRegistry Secret
    | -- | A CodeArtifact repository: the identity it mints from, and the store a sweep deletes in.
      BackendCodeArtifact CodeArtifactConfig CodeArtifactStore
    | -- | A Verdaccio store: its static write token, and the operator's deletion consent.
      BackendVerdaccio Secret DeletionConsent
    deriving stock (Eq, Show)

-- | The tag a backend was declared under.
sbTag :: StoreBackend -> StoreTag
sbTag = \case
    BackendRegistry{} -> TagRegistry
    BackendCodeArtifact{} -> TagCodeArtifact
    BackendVerdaccio{} -> TagVerdaccio

-- | How the mirror write to this backend authenticates.
sbMint :: StoreBackend -> MintPlan
sbMint = \case
    BackendRegistry token -> MintStatic token
    BackendCodeArtifact caConfig _ -> MintCodeArtifact caConfig
    BackendVerdaccio token _ -> MintStatic token

-- | The control plane this build reaches for a backend.
sbControl :: StoreBackend -> ControlPlane
sbControl = \case
    BackendRegistry{} -> ControlNone
    BackendCodeArtifact _ store -> ControlCodeArtifact store
    BackendVerdaccio token consent -> ControlProtocol token consent

-- | The namespaces a mount's deployment owns, in its registry's naming shape. Every consumer of the privilege derives from this value.
data FirstParty
    = -- | The npm scopes the deployment owns, at least one.
      FirstPartyNpmScopes (NonEmpty Scope)
    | -- | The PyPI distributions and name prefixes the deployment owns, at least one.
      FirstPartyPyPI (NonEmpty PyPIFirstParty)
    | -- | The Cargo names and separator-delimited prefixes the deployment owns.
      FirstPartyCargo (NonEmpty CargoFirstParty)
    | -- | The exact RubyGems names and separator-delimited prefixes the deployment owns.
      FirstPartyRubyGems (NonEmpty RubyGemsFirstParty)
    deriving stock (Eq, Show)

-- | A mount's refinements of the global @integrity@ group, under its own @integrity@ key. Each is 'Nothing' at the global setting.
newtype MountIntegrity = MountIntegrity
    { miMinTrusted :: Maybe MinTrustedIntegrity
    -- ^ A per-mount trusted-integrity floor, for one legacy private registry whose loosening must not leak onto other mounts.
    }
    deriving stock (Eq, Show)

-- | Endpoint and activation settings for one mount before resolution.
data MountConfig = MountConfig
    { mntEnabled :: Maybe Bool
    -- ^ Any operator-declared key activates the mount, so @true@ serves a bare public gate and @false@ switches one off in place.
    , mntPrivateUpstream :: Maybe PrivateEndpoint
    , mntPublicUpstream :: RegistryUrl
    -- ^ Only the @registry@ tag is admitted here, so the URL is the whole declaration.
    , mntMirrorTarget :: Maybe MirrorEndpoint
    , mntPublicationTarget :: Maybe PublicationEndpoint
    , mntFirstParty :: Maybe FirstParty
    , mntIntegrity :: MountIntegrity
    , mntMaxRequestBytes :: Maybe Int
    -- ^ Optional per-mount upload bound. Absence inherits the resolved global cap.
    }
    deriving stock (Eq, Show)

-- | Application settings, before active mounts resolve their layered policies.
data AppConfig = AppConfig
    { cfgServer :: ServerSettings
    , cfgQueue :: QueueSettings
    , cfgLimits :: LimitsSettings
    , cfgCache :: CacheSettings
    , cfgIntegrity :: IntegritySettings
    , cfgEgress :: EgressSettings
    , cfgAdvisories :: AdvisoriesSettings
    , cfgMirrorState :: Maybe AdvisoryStoreUrl
    -- ^ Independent object-store location for expected files and retained source data.
    , cfgRuntime :: RuntimeSettings
    , cfgObservability :: ObservabilitySettings
    , cfgDredger :: DredgerSettings
    , cfgMounts :: Map Ecosystem MountConfig
    }
    deriving stock (Eq, Show)

-- | The @server@ group: the inbound edge Écluse itself presents.
data ServerSettings = ServerSettings
    { srvPort :: Int
    , srvPublicUrl :: Maybe Url
    -- ^ Required whenever a mount is active, or 'Ecluse.Config.loadConfig' refuses. Served artifact URLs are rewritten against it.
    , srvAuthToken :: Maybe Secret
    , srvHelpMessage :: Maybe Text
    , srvShutdownDrainTimeout :: Int
    , srvMetadataGzip :: MetadataCompression
    -- ^ @server.metadataGzip@: whether an admitted listing may be gzipped for a client that accepts it.
    }
    deriving stock (Eq, Show)

{- | The @queue@ group: the mirror queue's destination, depth cap, and redelivery budget. The
URL's shape decides the backend, and the load derives it once ("Ecluse.Config.QueueTarget").
-}
data QueueSettings = QueueSettings
    { qsUrl :: Maybe QueueUrl
    , qsMaxMemoryDepth :: Maybe Int
    -- ^ Computed from the runtime posture when unset. A configured value wins.
    , qsMaxReceiveCount :: Int
    {- ^ Deliveries one message gets before the worker retires it. A __floor__: the worker runs one above an
    attached dead-letter terminus's capture count, so the terminus captures first ("Ecluse.Core.Queue").
    -}
    }
    deriving stock (Eq, Show)

-- | The @limits@ group: the hostile-input bounds. The memory plan computes unset tenant-sized caps, and the rest stay pinned.
data LimitsSettings = LimitsSettings
    { limMaxResponseBytes :: Maybe Int
    , limMaxVersionCount :: Int
    , limMaxArtifactCount :: Int
    -- ^ The per-package artifact fan-out cap. It bounds document shape, not bytes, so it stays pinned.
    , limMaxNestingDepth :: Int
    , limMaxAdvisoryDatabaseBytes :: Int
    -- ^ The advisory-database download cap. It bounds a stream to disk, not a heap tenant, so it stays pinned.
    , limMaxRequestBytes :: Maybe Int
    , limMaxArtifactBytes :: Maybe Int
    -- ^ The mirror worker's per-artifact fetch byte cap, computed from the mirror-artifact tenant when unset.
    , limProgressWindow :: Int
    -- ^ Seconds of waiting within which an upstream body must deliver 'limMinProgressBytes'.
    , limMinProgressBytes :: Int
    -- ^ Body bytes an upstream exchange must receive within each progress window.
    }
    deriving stock (Eq, Show)

-- | The @cache@ group: the metadata cache's TTL and its computed-by-default bounds.
data CacheSettings = CacheSettings
    { csTtl :: NominalDiffTime
    , csMaxEntries :: Maybe Int
    -- ^ Computed from the runtime posture when unset. A configured value wins.
    , csMaxBytes :: Maybe Int
    -- ^ Computed from the runtime posture when unset. A configured value wins.
    }
    deriving stock (Eq, Show)

-- | Global integrity floors. A mount can refine the trusted floor through 'MountIntegrity'.
data IntegritySettings = IntegritySettings
    { intMinPublic :: MinIntegrity
    , intMinTrusted :: MinTrustedIntegrity
    }
    deriving stock (Eq, Show)

-- | Outbound identity and extra blocked target ranges.
data EgressSettings = EgressSettings
    { egrAdditionalBlockedRanges :: [IPRange]
    , egrOperatorContact :: Maybe OperatorContact
    -- ^ Optional contact included in registry and Pilot feed identities.
    }
    deriving stock (Eq, Show)

-- | The @advisories@ group: the OSV/CVE pipeline's store, cadences, and upstream feeds.
data AdvisoriesSettings = AdvisoriesSettings
    { advUrl :: Maybe AdvisoryStoreUrl
    -- ^ The store the compiled databases sync from, its provider taken from the scheme. 'Nothing' leaves the advisory stack off.
    , advPollInterval :: NominalDiffTime
    , advCompileInterval :: NominalDiffTime
    , advDataDir :: FilePath
    , advOsvExportBaseUrl :: Url
    , advEpssFeedUrl :: Url
    , advSourcePathRecording :: SourcePathRecording
    -- ^ Pilot only: @advisories.recordSourcePath@, whether an artifact records each source's path or only its SHA-256.
    , advQuietTime :: Map Ecosystem NominalDiffTime
    -- ^ Pilot only: how long one ecosystem's export may go unchanged before the quiet-time alarm. Unlisted, seven days.
    , advEpssQuietTime :: NominalDiffTime
    -- ^ Pilot only: the same threshold for the EPSS feed's declared score date.
    , advMaxAgeSeconds :: Maybe NominalDiffTime
    -- ^ How old a published advisory artifact may be before advisory denial refuses. Unset, each mount derives it from its quarantine rules.
    , advMaxShrink :: Percent
    -- ^ Pilot only: how far below its size baseline one publication may shrink the artifact.
    , advEpssLossCap :: Percent
    -- ^ Pilot only: the most an optional EPSS outage may credit, as a share of the previous artifact.
    }
    deriving stock (Eq, Show)

{- | The @runtime@ group: the process-sizing overrides. Unset, each is computed from the runtime
posture (cgroups, RTS, file-descriptor limit), with its provenance boot-logged.
-}
data RuntimeSettings = RuntimeSettings
    { rtCores :: Maybe Int
    , rtCoresCeiling :: Maybe Int
    , rtMaxHeapBytes :: Maybe Int
    , rtServeMaxInFlight :: Maybe Int
    , rtPublicConnectionsPerHost :: Maybe Int
    , rtPrivateConnectionsPerHost :: Maybe Int
    }
    deriving stock (Eq, Show)

-- | The @observability@ group: log shape, log level, and telemetry switch.
data ObservabilitySettings = ObservabilitySettings
    { obsLogFormat :: LogFormat
    , obsLogLevel :: LogLevel
    , obsTelemetry :: TelemetrySwitch
    }
    deriving stock (Eq, Show)

-- | The @dredger@ group: the sweep's pacing, its deletion cap, and the names it walks. Only @ecluse dredger@ reads it.
data DredgerSettings = DredgerSettings
    { drgChunkSize :: Int
    -- ^ Candidate packages one chunk examines before the sweep pauses.
    , drgChunkPause :: NominalDiffTime
    -- ^ Seconds between chunks, which is also the wait a fault advising no delay of its own takes.
    , drgCyclePause :: NominalDiffTime
    -- ^ Seconds between the end of one cycle and the start of the next.
    , drgTargetCycleWindow :: Maybe NominalDiffTime
    -- ^ Seconds within which an advisory is paced to reach every affected mirrored version. Unset, the cycle pause sets it.
    , drgRequestBudgetFraction :: Maybe Rational
    -- ^ The share of a store's request capacity one sweep may take, computed per pool when unset so the proxy is not starved.
    , drgQuotaOverrides :: Map Text QuotaOverride
    -- ^ Declared request capacity, keyed by the store URL it describes.
    , drgDeletionCap :: Maybe Int
    {- ^ Versions one cycle may hand over for deletion, computed per sweepable store when unset.
    Reaching it halts the sweep for the life of the process, so a poisoned generation stops there.
    -}
    , drgFullWalk :: Bool
    {- ^ Walk every package each cycle rather than the advisory and identity-deny candidates. It
    covers a rule-configuration change, and it writes one resumption marker to the store.
    -}
    }
    deriving stock (Eq, Show)

{- | One store's declared request capacity. It supplies the capacity a backend publishes none of,
and a shared scope joins two endpoints of one pool so the sweep paces them together.
-}
data QuotaOverride = QuotaOverride
    { qoScope :: Maybe Text
    -- ^ The capacity pool this store shares, which defaults to the store's own authority.
    , qoQuotas :: Map QuotaDimension Rational
    -- ^ Requests per second the pool admits, replacing the backend's own number per dimension.
    , qoWeights :: Map RequestKind Rational
    -- ^ What one request of a kind costs relative to the backend's own cost for it.
    }
    deriving stock (Eq, Show)

-- | A resolved mount's endpoints: the public upstream it gates, and what it does with the rest.
data MountRegistries = MountRegistries
    { regPublicUpstream :: RegistryUrl
    , regMode :: MountMode
    }
    deriving stock (Eq, Show)

-- | Whether a mount mirrors. A declared @mirrorTarget@ makes it 'Mirrored', which always carries the private upstream it reads back.
data MountMode
    = -- | The mount mirrors admitted public artifacts, and it needs both legs.
      Mirrored MirroredLegs
    | -- | The mount never writes. It still merges the optional private upstream when present.
      ServeOnly (Maybe RegistryUrl)
    deriving stock (Eq, Show)

-- | A mirrored mount's two required halves: the readable private upstream, and the mirror target with its derived write credential.
data MirroredLegs = MirroredLegs
    { mlPrivateUpstream :: RegistryUrl
    , mlMirrorTarget :: MirrorTarget
    }
    deriving stock (Eq, Show)

-- | The mount's private upstream, when it has one. It is total over both mount modes.
regPrivateUpstream :: MountRegistries -> Maybe RegistryUrl
regPrivateUpstream regs = case regMode regs of
    Mirrored legs -> Just (mlPrivateUpstream legs)
    ServeOnly mPrivate -> mPrivate

-- | The mount's mirror target (with its derived credential), when it mirrors.
regMirrorTarget :: MountRegistries -> Maybe MirrorTarget
regMirrorTarget regs = case regMode regs of
    Mirrored legs -> Just (mlMirrorTarget legs)
    ServeOnly _ -> Nothing

-- | A mirror target married to the backend resolved from the tag it was declared under.
data MirrorTarget = MirrorTarget
    { mtUrl :: RegistryUrl
    , mtBackend :: StoreBackend
    , mtUploadUrl :: RegistryUrl
    -- ^ Resolved native writer endpoint.
    , mtUsername :: Maybe Text
    -- ^ Resolved username, carried beside the refreshed secret.
    }
    deriving stock (Eq, Show)

-- | One resolved mount: its ecosystem, its endpoints, and the rules in precedence order.
data Mount = Mount
    { mountEcosystem :: Ecosystem
    , mountRegistries :: MountRegistries
    , mountPolicy :: [PrecededRule]
    , mountInactiveRules :: Map Text RuleInactivity
    -- ^ The source-activated shipped rules this mount leaves to an advisory source that is not configured, by name.
    }
    deriving stock (Eq, Show)

-- | The mounts a load resolved, keyed by the ecosystem each was declared under.
type MountMap = Map Ecosystem Mount

-- | Why a shipped rule an advisory source activates does not run, when no operator patch enabled or disabled it.
data RuleInactivity
    = -- | No @advisories.url@ is configured, and no patch decided the rule.
      NoAdvisorySource
    deriving stock (Eq, Show)

-- | A completed load: the document's settings and the mounts resolved against them.
data Config = Config
    { configApp :: AppConfig
    , configMounts :: MountMap
    , configInactiveRules :: Map Text RuleInactivity
    {- ^ The source-activated shipped rules the shared @rules@ map leaves to an unconfigured advisory
    source, where some mount leaves them to the source too and no mount runs them.
    -}
    }
    deriving stock (Eq, Show)

-- | Why a load was refused. 'renderConfigError' writes each one as the boot reports it.
data ConfigError
    = ParseError Text
    | PolicyErrors [PolicyError]
    | -- | Shared rule errors that include suppression of the shipped npm mount default.
      SharedPolicyErrors [PolicyError]
    | -- | A mount is active but @server.publicUrl@ is unset. Deriving it from a spoofable @Host@ header would poison the shared cache.
      PublicUrlRequired
    | {- | A mount declares a @mirrorTarget@ but no private upstream, through which the mirror
      write must be readable back. A serve-only mount never raises this.
      -}
      MountMissingPrivateUpstream Ecosystem
    | -- | A @codeArtifact@ endpoint whose URL is not a CodeArtifact endpoint. The tag names the store, so the URL would misdirect a write or read.
      CodeArtifactHostMismatch Ecosystem Text
    | -- | A @codeArtifact@ endpoint on an ecosystem CodeArtifact has no package format for. Carries its key.
      CodeArtifactFormatUnsupported Ecosystem Text
    | {- | A @codeArtifact@ endpoint whose path addresses no repository under the mount's own
      format, whose per-format endpoints are separate stores. Carries its key, then the format token.
      -}
      CodeArtifactRepositoryMissing Ecosystem Text Text
    deriving stock (Eq, Show)

-- | Render a load refusal, including the mount remedy for a misplaced shared suppression.
renderConfigError :: ConfigError -> Text
renderConfigError (ParseError e) = e
renderConfigError (PolicyErrors es) = T.unlines (map renderPolicyError es)
renderConfigError (SharedPolicyErrors es) = T.unlines (map renderSharedPolicyError es)
renderConfigError PublicUrlRequired =
    "a mount is active but server.publicUrl (ECLUSE_SERVER__PUBLIC_URL) is not set: "
        <> "served tarball URLs are rewritten against the proxy's own externally-reachable base URL, "
        <> "and without one the npm CLI reads the relative dist.tarball as a file: path and every install fails; "
        <> "set it to the URL clients reach this proxy on (e.g. https://registry.example.com)"
renderConfigError (MountMissingPrivateUpstream eco) =
    "mount \""
        <> ecosystemName eco
        <> "\" declares a mirror target, so it must also define the private upstream the mirror is read back through: set "
        <> keyRef eco "privateUpstream"
        <> " in the config document, or remove "
        <> mountDocRef eco "mirrorTarget"
        <> " for a serve-only mount that never mirrors"
renderConfigError (CodeArtifactHostMismatch eco path) =
    keyRef eco path
        <> " is declared under the codeArtifact tag, but its host is not a CodeArtifact endpoint "
        <> "({domain}-{account}.d.codeartifact.{region}.amazonaws.com): correct the URL, or declare this "
        <> "endpoint under the tag that names its store"
renderConfigError (CodeArtifactFormatUnsupported eco path) =
    keyRef eco path
        <> " names a CodeArtifact store, but CodeArtifact carries no package format for the "
        <> ecosystemName eco
        <> " ecosystem: point this endpoint at a store CodeArtifact serves"
renderConfigError (CodeArtifactRepositoryMissing eco path format) =
    keyRef eco path
        <> " is not a CodeArtifact repository endpoint for this mount: its path must be /"
        <> format
        <> "/{repository}/"

renderSharedPolicyError :: PolicyError -> Text
renderSharedPolicyError err@(SuppressUnknownRule "deny-scripts") =
    renderPolicyError err
        <> ". Disable the shipped npm default with enabled: false under "
        <> keyRef Npm "rules"
        <> " instead."
renderSharedPolicyError err = renderPolicyError err

-- A mount-scoped key in both spellings, the form every refusal above names it in.
keyRef :: Ecosystem -> Text -> Text
keyRef eco path = mountDocRef eco path <> " (" <> mountKeyRef eco path <> ")"
