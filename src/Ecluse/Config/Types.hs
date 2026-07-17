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

data AppConfig = AppConfig
    { cfgPort :: Int
    , cfgMounts :: Map Ecosystem MountConfig
    , cfgQueueUrl :: Maybe Url
    , cfgQueueMemoryMaxDepth :: Int
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
