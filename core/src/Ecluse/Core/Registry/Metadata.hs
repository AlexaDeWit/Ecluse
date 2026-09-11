-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The per-request metadata handle and typed outcomes shared by registry adapters.
HTTP refusals retain their cause before projection. The serve pipeline owns fallback policy.
-}
module Ecluse.Core.Registry.Metadata (
    -- * The read handle
    MetadataClient (..),

    -- * The full-manifest result
    Manifest (..),
    ContentDigest,
    digestOf,
    digestBytes,

    -- * The fetch-then-project step
    fetchThenProject,

    -- * One version's read
    VersionRead (..),

    -- * Errors
    MetadataError (..),

    -- * Single-version resolution
    VersionEvaluation (..),
    fetchVersionDetails,
    versionTransience,
) where

import Ecluse.Core.Package (PackageDetails, PackageInfo, PackageName)
import Ecluse.Core.Registry (FetchFault, RegistryResponse (responseBody, responseStatusCode), isAuthorisationFailure, isSuccessStatus)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Rules.Types (Transience (WillResolve, WontResolve))
import Ecluse.Core.Security (LimitError)
import Ecluse.Core.Snapshot (ContentDigest, digestBytes, digestOf)
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version)

-- | A package snapshot with source bytes for assembly and a digest for validators.
data Manifest = Manifest
    { manifestInfo :: PackageInfo
    -- ^ The typed packument view the rules and merge reason over.
    , manifestRaw :: CachedDoc
    -- ^ The raw upstream document ('CachedDoc') the served body is built from.
    , manifestDigest :: ContentDigest
    -- ^ Digest of the wire bytes behind 'manifestInfo' and 'manifestRaw'.
    }

-- | Per-origin metadata operations with typed failures and caller-selected caching.
data MetadataClient = MetadataClient
    { fetchFullManifest :: PackageName -> IO (Either MetadataError Manifest)
    -- ^ Return the full manifest or a typed failure, including explicit access refusal.
    , fetchVersionMetadata :: PackageName -> Version -> IO (Either MetadataError VersionRead)
    -- ^ One version's projection with the document's own @latest@. Errors retain the upstream failure.
    }

{- | The requested version and the @latest@ tag the same document declared. Both come from one
bounded read, so a caller needing the tag adds no second fetch.
-}
data VersionRead = VersionRead
    { vrDetails :: Maybe PackageDetails
    -- ^ 'Nothing' means the package resolved without this version.
    , vrUpstreamLatest :: Maybe Version
    {- ^ The document's @latest@ target, whether or not it is the requested version. 'Nothing'
    when the document declares none, or the ecosystem has no such tag.
    -}
    }
    deriving stock (Eq, Show)

-- | Project successful responses only, preserving HTTP refusals before decoding.
fetchThenProject ::
    TracingPort ->
    (PackageName -> IO (Either FetchFault RegistryResponse)) ->
    PackageName ->
    (ByteString -> Either MetadataError a) ->
    IO (Either MetadataError a)
fetchThenProject tracing fetch name project =
    spanMetadataFetch tracing name (fetch name) >>= \case
        Left fault -> pure (Left (MetadataFetch fault))
        Right response -> case responseStatusCode response of
            404 -> pure (Left MetadataAbsent)
            code
                | isAuthorisationFailure code -> pure (Left (MetadataAuthorisationFailure code))
                | isSuccessStatus code -> spanMetadataDecode tracing name (pure (project (responseBody response)))
                | otherwise -> pure (Left (MetadataHttpFailure code))

-- | Why a metadata fetch could not yield a usable result.
data MetadataError
    = -- | The upstream explicitly refused access. Carries the original 401 or 403.
      MetadataAuthorisationFailure Int
    | -- | The upstream answered 404. Its body cannot establish a package's identity.
      MetadataAbsent
    | -- | Another non-success HTTP status, retained for the consumer's retry policy.
      MetadataHttpFailure Int
    | -- | A failed exchange, with request-formation, bound, and transport causes kept distinct.
      MetadataFetch FetchFault
    | -- | The decoded structure crossed a limit, distinct from an exchange body-size failure.
      MetadataBoundExceeded LimitError
    | -- | Malformed bytes or a missing package identity prevented manifest decoding.
      MetadataUndecodable
    | -- | An invalid package identity, carried for diagnostics and excluded from the requested package.
      MetadataNameMismatch Text
    deriving stock (Eq, Show)

-- | A version lookup result shared by public admission and mirror workers.
data VersionEvaluation
    = {- | The version resolved and projected, ready for the rules engine. The second field is
      the same document's 'vrUpstreamLatest'.
      -}
      VersionPresent PackageDetails (Maybe Version)
    | -- | The package exists but does not supply the requested version.
      VersionMissing
    | -- | Metadata was unavailable. Public admission and workers retain their retry policy.
      VersionMetadataUnavailable
    deriving stock (Eq, Show)

-- | Classify a version lookup for public admission and workers, treating metadata failures as transient.
fetchVersionDetails :: MetadataClient -> PackageName -> Version -> IO VersionEvaluation
fetchVersionDetails client name version =
    fetchVersionMetadata client name version <&> \case
        Left _ -> VersionMetadataUnavailable
        Right versionRead -> case vrDetails versionRead of
            Nothing -> VersionMissing
            Just details -> VersionPresent details (vrUpstreamLatest versionRead)

-- | Classify unsuccessful lookups for retry. A resolved version has no transience.
versionTransience :: VersionEvaluation -> Maybe Transience
versionTransience = \case
    VersionMetadataUnavailable -> Just (WillResolve Nothing)
    -- A withdrawn version is gone for good, so no consumer waits for it to come back.
    VersionMissing -> Just WontResolve
    VersionPresent{} -> Nothing
