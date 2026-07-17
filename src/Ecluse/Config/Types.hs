-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.Types (
    Url (..),
    mkUrl,
    unUrl,
    MirrorCredential (..),
    MountConfig (..),
    AppConfig (..),
    ServerSettings (..),
    QueueSettings (..),
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

import Data.Char (isUpper)
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
    { mntEnabled :: Maybe Bool
    {- ^ The mount's explicit on\/off switch. Any operator-declared key under the
    mount activates it, so @enabled: true@ exists for the mount that needs no other
    key (a serve-only pure public gate on the template public upstream), and
    @enabled: false@ switches off a mount whose other keys remain in place.
    -}
    , mntPrivateUpstream :: Maybe RegistryUrl
    , mntPublicUpstream :: RegistryUrl
    , mntMirrorTarget :: Maybe RegistryUrl
    , mntMirrorTargetToken :: Maybe Secret
    , mntRespectUpstreamTarballHost :: Bool
    , mntMirrorCodeArtifactTokenDuration :: Maybe Natural
    , mntPublicationTarget :: Maybe RegistryUrl
    , mntPublicationTargetToken :: Maybe Secret
    , mntPublishAllow :: [Scope]
    , mntMinTrustedIntegrity :: Maybe MinTrustedIntegrity
    {- ^ A per-mount refinement of the global trusted-integrity floor, for the one
    legacy private registry whose loosening must not leak onto other mounts.
    -}
    , mntDivergencePolicy :: Maybe DivergencePolicy
    -- ^ A per-mount refinement of the global cross-upstream divergence policy.
    , mntAdditionalRules :: RulePatch
    }
    deriving stock (Eq, Show)

{- | The resolved application configuration, one sub-record per document group so a
field's home says what it governs (the document schema and this type mirror each
other one to one).
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
    {- ^ Required whenever a mount is active ('Ecluse.Config.loadConfig' refuses
    otherwise): served artifact URLs are rewritten against it.
    -}
    , srvAuthToken :: Maybe Secret
    , srvHelpMessage :: Maybe Text
    , srvShutdownDrainTimeout :: Int
    }
    deriving stock (Eq, Show)

{- | The @queue@ group: the mirror queue's destination and the in-memory rollover's
depth cap. The backend is derived from the URL's shape ("Ecluse.Config.QueueTarget"),
never named here.
-}
data QueueSettings = QueueSettings
    { qsUrl :: Maybe Url
    , qsMemoryMaxDepth :: Maybe Int
    -- ^ Computed from the runtime posture when unset; a configured value wins.
    }
    deriving stock (Eq, Show)

{- | The @limits@ group: the hostile-input bounds. The structural counts are pinned
policy defaults; the byte-valued caps are computed from the memory budget when
unset ("Ecluse.Composition.MemoryBudget"), a configured value always winning.
-}
data LimitsSettings = LimitsSettings
    { limMaxResponseBytes :: Maybe Int
    , limMaxVersionCount :: Int
    , limMaxNestingDepth :: Int
    , limMaxRequestBytes :: Maybe Int
    }
    deriving stock (Eq, Show)

-- | The @cache@ group: the metadata cache's TTL and its computed-by-default bounds.
data CacheSettings = CacheSettings
    { csTtl :: NominalDiffTime
    , csMaxEntries :: Maybe Int
    -- ^ Computed from the runtime posture when unset; a configured value wins.
    , csMaxBytes :: Maybe Int
    -- ^ Computed from the runtime posture when unset; a configured value wins.
    }
    deriving stock (Eq, Show)

{- | The @integrity@ group: the global integrity floors and divergence policy
(@minTrusted@ and @divergencePolicy@ refinable per mount).
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

-- | The @advisories@ group: the OSV/CVE pipeline's bucket, cadences, and bounds.
data AdvisoriesSettings = AdvisoriesSettings
    { advBucket :: Maybe Text
    , advPollInterval :: NominalDiffTime
    , advCompileInterval :: NominalDiffTime
    , advDataDir :: FilePath
    , advOsvExportBaseUrl :: Text
    , advMaxDatabaseBytes :: Int
    }
    deriving stock (Eq, Show)

{- | The @runtime@ group: the process-sizing overrides. Every field is optional;
unset, each is computed from the runtime posture (cgroups, RTS, file-descriptor
limit) with its provenance boot-logged.
-}
data RuntimeSettings = RuntimeSettings
    { rtCores :: Maybe Int
    , rtMaxHeapBytes :: Maybe Int
    , rtServeMaxInFlight :: Maybe Int
    , rtPublicConnectionsPerHost :: Maybe Int
    , rtPrivateConnectionsPerHost :: Maybe Int
    }
    deriving stock (Eq, Show)

-- | The @observability@ group: log shape and telemetry switch.
data ObservabilitySettings = ObservabilitySettings
    { obsLogFormat :: LogFormat
    , obsTelemetry :: TelemetrySwitch
    }
    deriving stock (Eq, Show)

data MountRegistries = MountRegistries
    { regPublicUpstream :: RegistryUrl
    , regMode :: MountMode
    }
    deriving stock (Eq, Show)

{- | Whether a mount mirrors, derived from its declared endpoints: a declared
@mirrorTarget@ makes the mount 'Mirrored' (and its private upstream is then required,
so the mirror can be read back), an absent one makes it 'ServeOnly' (never writes
anywhere; the private upstream is optional, and a mount with neither is the pure
public gate). The coupling is structural, so a mirrored mount without a readable
private leg is unrepresentable.
-}
data MountMode
    = -- | The mount mirrors admitted public artifacts; both legs are required.
      Mirrored MirroredLegs
    | -- | The mount never writes; the optional private upstream is still merged when present.
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

{- | The mount's private upstream, when it has one: total over both modes, so call
sites read as before while the compiler makes them face the serve-only absence.
-}
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
    | {- | A mount is active but @server.publicUrl@ is unset. Served artifact URLs
      must be rewritten against the proxy's own externally-reachable base URL; a
      relative @dist.tarball@ reads to the npm CLI as a @file:@ path and every
      install fails, so the omission is refused at boot rather than discovered
      client by client. Host-header derivation is deliberately not offered (a
      spoofed header would poison every shared-cache entry with an
      attacker-chosen artifact URL).
      -}
      PublicUrlRequired
    | {- | A __mirrored__ mount (one that declares a @mirrorTarget@) does not define
      its private upstream. The mirror write must be readable back through the
      private leg, so a mirrored mount without one is refused; a serve-only mount
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
        envKey = "ECLUSE_MOUNTS__" <> T.toUpper name <> "__PRIVATE_UPSTREAM"
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
        envKey = "ECLUSE_MOUNTS__" <> T.toUpper name <> "__" <> envKeyOf key
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
  where
    -- The env form of a camelCase mount key (the resolver's transliteration, inverted).
    envKeyOf :: Text -> Text
    envKeyOf = T.toUpper . T.concatMap (\c -> if isUpper c then "_" <> one c else one c)
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
