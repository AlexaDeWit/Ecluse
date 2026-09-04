-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The configuration vocabulary: the settings records a load resolves to, the refusals their
URL-valued keys carry, and the errors a refused load reports.

Every type here is shared between the decoders ("Ecluse.Config.Parser", "Ecluse.Config.Aeson") and
the composition root that reads them, so neither side imports the other. "Ecluse.Config" assembles
them into a 'Config'.
-}
module Ecluse.Config.Types (
    Url,
    mkUrl,
    unUrl,
    HttpScheme (..),
    splitHttpScheme,
    MirrorCredential (..),
    FirstParty (..),
    PyPIFirstParty (..),
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
    MountRegistries (..),
    MountMode (..),
    MirroredLegs (..),
    regPrivateUpstream,
    regMirrorTarget,
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

import Ecluse.Config.Advisory.Internal (
    AdvisoryStoreTarget (..),
    AdvisoryStoreUrl,
    advisoryStoreTarget,
    advisoryStoreUrlText,
 )
import Ecluse.Config.Queue.Internal (QueueTarget (..), QueueUrl, queueUrlTarget, queueUrlText)
import Ecluse.Config.Resolve (mountKeyRef)
import Ecluse.Config.Rule (PolicyError, RulePatch, renderPolicyError)
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Package (PackageName, PyPIPrefix, Scope)
import Ecluse.Core.Package.Integrity (MinIntegrity, MinTrustedIntegrity)
import Ecluse.Core.Package.Merge (DivergencePolicy)
import Ecluse.Core.Rules.Types (PrecededRule)
import Ecluse.Core.Security (hostPortAddress, refuseCredentialMaterial)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig)
import Ecluse.Runtime.Log (LogFormat, LogLevel)
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

{- | The mirror-write credential, __derived from the mirror-target URL__ so a token can never pair
with an endpoint it was not minted for. Load resolves it once ("Ecluse.Config.MirrorCredential").
-}
data MirrorCredential
    = -- | A CodeArtifact mirror target: the mint identity parsed from its host.
      MirrorCodeArtifact CodeArtifactConfig
    | -- | Any other mirror target: an operator-supplied static write token.
      MirrorStatic Secret
    deriving stock (Eq, Show)

{- | The namespaces a mount's deployment owns, one arm per ecosystem, read only in that registry's own naming
shape. Every consumer of the privilege derives its predicate from this one value.
-}
data FirstParty
    = -- | The npm scopes the deployment owns, at least one.
      FirstPartyNpmScopes (NonEmpty Scope)
    | -- | The PyPI distributions and name prefixes the deployment owns, at least one.
      FirstPartyPyPI (NonEmpty PyPIFirstParty)
    deriving stock (Eq, Show)

{- | One PyPI first-party declaration. PyPI carries no structural namespace, so a deployment
either names a distribution it owns or the prefix its distributions share.
-}
data PyPIFirstParty
    = -- | A distribution the deployment owns, matched on its PEP 503 canonical name.
      PyPIOwnedName PackageName
    | -- | A prefix the deployment owns, matched at PEP 503's separator boundary.
      PyPIOwnedPrefix PyPIPrefix
    deriving stock (Eq, Show)

{- | A mount's refinements of the global @integrity@ group, under its own @integrity@ key so the
mount groups them exactly as the top level does. Each is 'Nothing' at the global setting.
-}
data MountIntegrity = MountIntegrity
    { miMinTrusted :: Maybe MinTrustedIntegrity
    {- ^ A per-mount refinement of the global trusted-integrity floor, for the one
    legacy private registry whose loosening must not leak onto other mounts.
    -}
    , miDivergencePolicy :: Maybe DivergencePolicy
    -- ^ A per-mount refinement of the global cross-upstream divergence policy.
    }
    deriving stock (Eq, Show)

data MountConfig = MountConfig
    { mntEnabled :: Maybe Bool
    {- ^ The mount's on\/off switch. Any operator-declared key already activates the mount, so
    @true@ serves the public gate that declares no other key and @false@ switches one off in place.
    -}
    , mntPrivateUpstream :: Maybe RegistryUrl
    , mntPublicUpstream :: RegistryUrl
    , mntMirrorTarget :: Maybe RegistryUrl
    , mntMirrorTargetToken :: Maybe Secret
    , mntMirrorTokenDuration :: Maybe Natural
    , mntPublicationTarget :: Maybe RegistryUrl
    , mntPublicationTargetToken :: Maybe Secret
    , mntFirstParty :: Maybe FirstParty
    , mntIntegrity :: MountIntegrity
    , mntAdditionalRules :: RulePatch
    }
    deriving stock (Eq, Show)

{- | The resolved application configuration, one sub-record per document group. The document
schema and this type mirror each other one to one.
-}
data AppConfig = AppConfig
    { cfgServer :: ServerSettings
    , cfgQueue :: QueueSettings
    , cfgLimits :: LimitsSettings
    , cfgCache :: CacheSettings
    , cfgIntegrity :: IntegritySettings
    , cfgEgress :: EgressSettings
    , cfgAdvisories :: AdvisoriesSettings
    , cfgRuntime :: RuntimeSettings
    , cfgObservability :: ObservabilitySettings
    , cfgMounts :: Map Ecosystem MountConfig
    }
    deriving stock (Eq, Show)

-- | The @server@ group: the inbound edge Écluse itself presents.
data ServerSettings = ServerSettings
    { srvPort :: Int
    , srvPublicUrl :: Maybe Url
    {- ^ Required whenever a mount is active, and 'Ecluse.Config.loadConfig' refuses
    otherwise. The proxy rewrites served artifact URLs against it.
    -}
    , srvAuthToken :: Maybe Secret
    , srvHelpMessage :: Maybe Text
    , srvShutdownDrainTimeout :: Int
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
    {- ^ Deliveries one message gets before the worker retires it. A __floor__: a queue with a
    dead-letter terminus runs one above its capture count, so it captures first ("Ecluse.Core.Queue").
    -}
    }
    deriving stock (Eq, Show)

{- | The @limits@ group: the hostile-input bounds. The memory plan computes the tenant-sized caps
when unset ("Ecluse.Composition.MemoryPlan"), a configured value winning, and the rest stay pinned.
-}
data LimitsSettings = LimitsSettings
    { limMaxResponseBytes :: Maybe Int
    , limMaxVersionCount :: Int
    , limMaxNestingDepth :: Int
    , limMaxAdvisoryDatabaseBytes :: Int
    {- ^ The advisory-database download cap. It bounds a stream to disk rather than a heap
    tenant, so the memory plan does not size it and it stays pinned.
    -}
    , limMaxRequestBytes :: Maybe Int
    , limMaxArtifactBytes :: Maybe Int
    {- ^ The mirror worker's per-artifact fetch byte cap. Computed from the memory
    plan's mirror-artifact tenant when unset, a configured value winning.
    -}
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

{- | The @integrity@ group: the global integrity floors and divergence policy. A mount refines
@minTrusted@ and @divergencePolicy@ under its own @integrity@ key ('MountIntegrity').
-}
data IntegritySettings = IntegritySettings
    { intMinPublic :: MinIntegrity
    , intMinTrusted :: MinTrustedIntegrity
    , intDivergencePolicy :: DivergencePolicy
    }
    deriving stock (Eq, Show)

-- | The @egress@ group: the operator's additions to the blocked target ranges.
newtype EgressSettings = EgressSettings
    { egrAdditionalBlockedRanges :: [IPRange]
    }
    deriving stock (Eq, Show)

-- | The @advisories@ group: the OSV/CVE pipeline's store, cadences, and upstream feeds.
data AdvisoriesSettings = AdvisoriesSettings
    { advUrl :: Maybe AdvisoryStoreUrl
    {- ^ The object store the compiled databases sync from, its provider derived from the URL's
    scheme ("Ecluse.Config.AdvisoryStore"). 'Nothing' leaves the advisory stack off.
    -}
    , advPollInterval :: NominalDiffTime
    , advCompileInterval :: NominalDiffTime
    , advDataDir :: FilePath
    , advOsvExportBaseUrl :: Url
    , advEpssFeedUrl :: Url
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

data MountRegistries = MountRegistries
    { regPublicUpstream :: RegistryUrl
    , regMode :: MountMode
    }
    deriving stock (Eq, Show)

{- | Whether a mount mirrors, derived from its declared endpoints. A declared @mirrorTarget@ makes
it 'Mirrored', which carries the private upstream the mirror is read back through, never without.
-}
data MountMode
    = -- | The mount mirrors admitted public artifacts, and it needs both legs.
      Mirrored MirroredLegs
    | -- | The mount never writes. It still merges the optional private upstream when present.
      ServeOnly (Maybe RegistryUrl)
    deriving stock (Eq, Show)

{- | A mirrored mount's two required halves: the readable private upstream and the
mirror target married to its derived write credential.
-}
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
    | {- | A mount is active but @server.publicUrl@ is unset. The proxy must rewrite
      served artifact URLs against its own externally-reachable base URL. The npm CLI
      reads a relative @dist.tarball@ as a @file:@ path and every install fails, so
      the omission is refused at boot rather than discovered client by client.
      Host-header derivation is deliberately not offered: a spoofed header would
      poison every shared-cache entry with an attacker-chosen artifact URL.
      -}
      PublicUrlRequired
    | {- | A __mirrored__ mount (one that declares a @mirrorTarget@) does not define
      its private upstream. The mirror write must be readable back through the
      private leg, so a mirrored mount without one is refused. A serve-only mount
      (no @mirrorTarget@) never raises this.
      -}
      MountMissingPrivateUpstream Ecosystem
    | {- | A serve-only mount (no @mirrorTarget@ declared) carries a mirror-write
      setting anyway. A write credential or token duration on a mount that never
      writes signals a misunderstanding (most likely a missing @mirrorTarget@), so
      it is refused per offending key rather than silently ignored. Carries the
      mount's ecosystem and the offending document key.
      -}
      MirrorSettingWithoutWrite Ecosystem Text
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
renderConfigError PublicUrlRequired =
    "a mount is active but server.publicUrl (ECLUSE_SERVER__PUBLIC_URL) is not set: "
        <> "served tarball URLs are rewritten against the proxy's own externally-reachable base URL, "
        <> "and without one the npm CLI reads the relative dist.tarball as a file: path and every install fails; "
        <> "set it to the URL clients reach this proxy on (e.g. https://registry.example.com)"
renderConfigError (MountMissingPrivateUpstream eco) =
    let name = ecosystemName eco
        envKey = mountKeyRef eco "privateUpstream"
     in "mount \""
            <> name
            <> "\" declares a mirror target, so it must also define the private upstream the mirror is read back through: set mounts."
            <> name
            <> ".privateUpstream in the config document (or "
            <> envKey
            <> "), or remove mounts."
            <> name
            <> ".mirrorTarget for a serve-only mount that never mirrors"
renderConfigError (MirrorSettingWithoutWrite eco key) =
    let name = ecosystemName eco
        envKey = mountKeyRef eco key
     in "mount \""
            <> name
            <> "\" declares no mirror target, so mounts."
            <> name
            <> "."
            <> key
            <> " ("
            <> envKey
            <> ") has nothing to write with: set mounts."
            <> name
            <> ".mirrorTarget to mirror, or remove the setting for a serve-only mount"
renderConfigError (MirrorCredentialTokenMissing eco) =
    let name = ecosystemName eco
        envKey = mountKeyRef eco "mirrorTargetToken"
     in "mount \""
            <> name
            <> "\" mirror target is not a CodeArtifact endpoint, so its write credential is not minted: set a static write token with mounts."
            <> name
            <> ".mirrorTargetToken (or "
            <> envKey
            <> ")"
renderConfigError (MirrorCredentialConflict eco) =
    let name = ecosystemName eco
        envKey = mountKeyRef eco "mirrorTargetToken"
     in "mount \""
            <> name
            <> "\" mirror target is a CodeArtifact endpoint (its write token is minted from the host identity), so a static write token must not also be set: remove mounts."
            <> name
            <> ".mirrorTargetToken (or "
            <> envKey
            <> ")"
