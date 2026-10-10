-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Resolve metadata origins with their credential posture and typed outcomes.
Private reads forward the caller's credential without caching. Public reads are anonymous.
Explicit access refusals remain distinct for the packument pipeline, and an origin that
answered 404 stays distinct from one that could not be read.
-}
module Ecluse.Core.Server.Pipeline.Origin (
    -- * A resolved contribution
    Contribution (..),
    fingerprintPiece,

    -- * The per-origin outcome
    OriginResult (..),
    OriginMiss (..),
    originManifest,
    originMiss,

    -- * Fetching the two origins
    fetchPrivateOrigin,
    fetchPublicOrigin,
    preparePublicMetadata,
    withPrivateMetadataClient,

    -- * One origin's coordinates
    mountOrigin,
) where

import Data.Map.Strict qualified as Map
import Katip (Severity (DebugS), logFM, ls)
import Network.HTTP.Client (Manager)
import UnliftIO (withRunInIO)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (Artifact (artEntryKey), PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Package.Merge (Provenance)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors, metadataRead))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (
    ContentDigest,
    Manifest,
    MetadataClient (fetchFullManifest),
    MetadataError (
        MetadataAbsent,
        MetadataAuthorisationFailure,
        MetadataBoundExceeded,
        MetadataFetch,
        MetadataHttpFailure,
        MetadataNameMismatch,
        MetadataUndecodable
    ),
    VersionRead,
 )
import Ecluse.Core.Registry.Origin (OriginClient, OriginFor, Private, Public, anonymousOrigin, chargingFullReads, originBaseUrl, originClient, originClientOf, perCallerOrigin)
import Ecluse.Core.Security (Limits (progressFloor))
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, awaitingFlight, charge, servingFlight)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille), FlightKey (FlightKey))
import Ecluse.Core.Server.Cache (Source (Source), metadataKey)
import Ecluse.Core.Server.Cache.Store (PreparedStore)
import Ecluse.Core.Server.Context (
    Handler,
    PackumentDeps (..),
    ServeRuntime (..),
    pdPrivateBaseUrl,
    pdPublicBaseUrl,
 )
import Ecluse.Core.Server.Metadata (MetadataReads, ecosystemMetadataReads, preparePublicVersion, privateMetadataClient, publicMetadataClient, withinRequestCap)
import Ecluse.Core.Server.Pipeline.Diagnostics (logInvalidEntries, logMetadataFailure)
import Ecluse.Core.Version (Version)

-- | A parsed contribution with opaque source bytes and a digest for the derived validator.
data Contribution = Contribution
    { srcProvenance :: Provenance
    , srcInfo :: PackageInfo
    , srcValue :: CachedDoc
    , srcDigest :: ContentDigest
    , srcBodyBytes :: Int
    -- ^ The decompressed source size, from which the listing's output charge takes its basis.
    }

-- | Scope surviving versions and exact artifact coordinates to their source digest and provenance.
fingerprintPiece :: Contribution -> (Provenance, ContentDigest, [(Text, [EntryKey])])
fingerprintPiece s =
    ( srcProvenance s
    , srcDigest s
    , [(version, map artEntryKey (toList (pkgArtifacts details))) | (version, details) <- Map.toList (infoVersions (srcInfo s))]
    )

-- | One origin's contribution, access refusal, identity mismatch, or absence.
data OriginResult
    = -- | A packument that decoded and whose self-reported name matched the request.
      OriginResolved Manifest
    | -- | An explicit upstream access refusal, retaining its 401 or 403.
      OriginAuthorisationFailure Int
    | -- | An invalid package identity, contributing a 502 when no valid origin remains.
      OriginNameMismatch
    | -- | The origin answered 404, so it holds no such package. It degrades to no contribution.
      OriginNotFound
    | -- | The origin was not read: unreachable, faulting, or undecodable. It degrades to no contribution.
      OriginUnresolved
    | -- | An unconfigured origin contributes neither metadata nor an availability failure.
      OriginAbsent

-- | Distinguish a settled absence from an unread origin that a retry may resolve.
data OriginMiss
    = -- | The origin holds no such package, or the mount configures no such origin.
      MissAbsent
    | -- | The origin was not read, so nothing yet says whether it holds the package.
      MissUnresolved
    deriving stock (Eq, Show)

-- | The resolved manifest an origin contributed, if any.
originManifest :: OriginResult -> Maybe Manifest
originManifest = \case
    OriginAuthorisationFailure _ -> Nothing
    OriginResolved manifest -> Just manifest
    OriginNameMismatch -> Nothing
    OriginNotFound -> Nothing
    OriginUnresolved -> Nothing
    OriginAbsent -> Nothing

-- | The miss an origin yielded, or 'Nothing' when it contributed a document or an explicit refusal.
originMiss :: OriginResult -> Maybe OriginMiss
originMiss = \case
    OriginAuthorisationFailure _ -> Nothing
    OriginResolved{} -> Nothing
    OriginNameMismatch -> Nothing
    OriginNotFound -> Just MissAbsent
    OriginUnresolved -> Just MissUnresolved
    OriginAbsent -> Just MissAbsent

originResultOf :: Either SomeException (Either MetadataError Manifest) -> OriginResult
originResultOf = \case
    Left _ -> OriginUnresolved
    Right (Left (MetadataAuthorisationFailure code)) -> OriginAuthorisationFailure code
    Right (Left (MetadataNameMismatch _)) -> OriginNameMismatch
    Right (Left MetadataAbsent) -> OriginNotFound
    Right (Left (MetadataHttpFailure _)) -> OriginUnresolved
    Right (Left MetadataUndecodable) -> OriginUnresolved
    Right (Left (MetadataBoundExceeded _)) -> OriginUnresolved
    Right (Left (MetadataFetch _)) -> OriginUnresolved
    Right (Right manifest) -> OriginResolved manifest

-- | Resolve the private origin uncached with the caller's credential, retaining explicit access refusals.
fetchPrivateOrigin :: PackumentDeps -> ServeRuntime -> MemoryTicket -> Maybe ClientCredential -> PackageName -> Handler OriginResult
fetchPrivateOrigin deps rt ticket token name = case pdPrivateBaseUrl deps of
    Nothing -> pure OriginAbsent
    Just privateBase -> do
        logFM DebugS (ls ("fetching private origin for " <> renderPackageName name))
        let origin = chargingFullReads (fullReadCharge deps ticket) (privateOrigin rt deps privateBase token)
        originResultOf <$> tryAny (withMetadataClient rt deps privateMetadataClient origin (`fetchFullManifest` name))

-- | Resolve the public (gated, anonymous) upstream origin through the metadata cache, keyed by the origin's base URL as its 'Source'.
fetchPublicOrigin :: PackumentDeps -> ServeRuntime -> MemoryTicket -> PackageName -> Handler OriginResult
fetchPublicOrigin deps rt ticket name = do
    logFM DebugS (ls ("fetching public origin for " <> renderPackageName name))
    -- Every request that shares this read waits on it, so whichever request leads it pays with their priority.
    let flight = FlightKey (metadataKey (publicSource deps) name)
        origin = chargingFullReads (fullReadCharge deps (servingFlight flight ticket)) (publicOrigin rt deps)
    originResultOf <$> tryAny (awaitingFlight ticket flight (withMetadataClient rt deps (publicMetadataClient (srMetadataCache rt) (publicSource deps)) origin (`fetchFullManifest` name)))

{- Run an action over a per-request read handle for one origin. 'withRunInIO' captures the request's
@katip@ context into the failure logs, and each read holds to the mount's 'Limits' and the serve cap. -}
withMetadataClient ::
    ServeRuntime ->
    PackumentDeps ->
    (MetadataReads posture -> client) ->
    OriginFor posture ->
    (client -> IO a) ->
    Handler a
withMetadataClient rt deps settle origin k =
    withRunInIO $ \runInIO ->
        k . settle . withinRequestCap (progressFloor (pdLimits deps)) $
            ecosystemMetadataReads
                (metadataRead (pdMetadata deps))
                (srTracing rt)
                (srMetrics rt)
                (\nm err -> runInIO (logMetadataFailure nm baseUrl err))
                (\nm entries -> runInIO (logInvalidEntries nm baseUrl entries))
                (\nm -> runInIO (logFM DebugS (ls ("fetching packument from origin for " <> renderPackageName nm))))
                origin
  where
    baseUrl = originBaseUrl (originClientOf origin)

-- The ticket pays for each full-read chunk at the ecosystem's factor.
fullReadCharge :: PackumentDeps -> MemoryTicket -> Int -> IO ()
fullReadCharge deps ticket = charge ticket . scaleCharge (cfFullReadPermille (metadataChargeFactors (pdMetadata deps)))

-- | Bypass shared caching so the private upstream authorises each caller's credential.
withPrivateMetadataClient :: ServeRuntime -> PackumentDeps -> RegistryUrl -> Maybe ClientCredential -> (MetadataClient -> IO a) -> Handler a
withPrivateMetadataClient rt deps baseUrl token =
    withMetadataClient rt deps privateMetadataClient (privateOrigin rt deps baseUrl token)

-- | Pin a public local value without starting remote work.
preparePublicMetadata :: ServeRuntime -> PackumentDeps -> PackageName -> Version -> Handler (PreparedStore MetadataError VersionRead)
preparePublicMetadata rt deps name version =
    withMetadataClient rt deps (preparePublicVersion (srMetadataCache rt) (publicSource deps)) (publicOrigin rt deps) (\prepare -> prepare name version)

privateOrigin :: ServeRuntime -> PackumentDeps -> RegistryUrl -> Maybe ClientCredential -> OriginFor Private
privateOrigin rt deps = perCallerOrigin (pdLimits deps) (srPrivateManager rt)

publicOrigin :: ServeRuntime -> PackumentDeps -> OriginFor Public
publicOrigin rt deps = anonymousOrigin (pdLimits deps) (srPublicManager rt) (pdPublicBaseUrl deps)

-- The public origin's key in the shared metadata cache.
publicSource :: PackumentDeps -> Source
publicSource deps = Source (registryUrlText (pdPublicBaseUrl deps))

-- | Build an origin with the mount's response bound and the caller-selected manager and credential.
mountOrigin :: PackumentDeps -> Manager -> RegistryUrl -> Maybe ClientCredential -> OriginClient
mountOrigin deps = originClient (pdLimits deps)
