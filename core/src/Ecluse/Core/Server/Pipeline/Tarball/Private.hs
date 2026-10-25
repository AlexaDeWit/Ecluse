-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

{- | Read private archives under the caller's credential and the selected source policy.
Access refusals stop fallback, and managed views require retained completion proof.
-}
module Ecluse.Core.Server.Pipeline.Tarball.Private (
    PrivateLeg (..),
    streamPrivateArtifact,
    privateMetadataMiss,
) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (ResponseHeaders, status403, status404, status503)
import Network.Wai (ResponseReceived)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Mirror.State (MirrorVersion, recordVersion, versionFiles)
import Ecluse.Core.Package (
    Artifact (artFilename, artUrl),
    PackageDetails (pkgArtifacts),
    PackageInfo (PackageInfo),
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByFile, artifactByUrl, artifactPrivateAddressing), PrivateArtifactAddressing (..))
import Ecluse.Core.Registry.Metadata (
    MetadataClient (fetchVersionMetadata),
    MetadataError (
        MetadataAbsent,
        MetadataAuthorisationFailure,
        MetadataBoundExceeded,
        MetadataFetch,
        MetadataHttpFailure,
        MetadataNameMismatch,
        MetadataUndecodable
    ),
    VersionDoc (vdDetails),
    VersionRead (vrVersion),
 )
import Ecluse.Core.Registry.Request (BaseRequest)
import Ecluse.Core.Security (
    HostPort,
    Limits (progressFloor),
    Origin (TrustedOrigin),
    artifactAuthorityHonoured,
    hostPortAddress,
    thgEcosystemHosts,
    thgPrivateHostPort,
 )
import Ecluse.Core.Server.Admission.Meter (MemoryTicket)
import Ecluse.Core.Server.Context (
    Handler,
    PackumentDeps (..),
    ReadSourceSelection (..),
    ServeRuntime (..),
    pdPrivateBase,
    pdTarballHostGate,
    tarballHostHonoured,
 )
import Ecluse.Core.Server.MirrorView (mirrorVersionView, publicMirrorCoverage, validateMirrorProject)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Server.Pipeline.Origin (
    OriginMiss (MissAbsent, MissUnresolved),
    mountOrigin,
    withPrivateMetadataClient,
 )
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Pipeline.Tarball.Public (servePublicArtifactWithCoverage)
import Ecluse.Core.Server.Pipeline.Tarball.Relay (
    acceptPrivateArtifact,
    privateArtifactResponder,
    relayUnjudged,
    relayUpstreamResult,
    withMethod,
    withValidators,
 )
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..), relayResponder)
import Ecluse.Core.Server.Response (mkRefusal)
import Ecluse.Core.Server.Stream (UpstreamResult (..))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Version (renderVersion)
import UnliftIO (tryAny)

-- | The private leg's answer: the response it committed, or why it did not answer.
data PrivateLeg received
    = PrivateAnswered Metric.Decision received
    | PrivateMissed OriginMiss
    | PrivateHandled received

-- | Read the private origin, committing a response or reporting the miss that ends the leg.
streamPrivateArtifact ::
    ArtifactRequest response ->
    Maybe ClientCredential ->
    Handler (PrivateLeg ResponseReceived)
streamPrivateArtifact ctx token = do
    selected <- case pdMirrorState (arDeps ctx) of
        Just _
            | pdReadSourceSelection (arDeps ctx) /= PrivateOnly && not (pdFirstParty (arDeps ctx) (arPackage ctx)) ->
                withMetadataAdmission (arRuntime ctx) (pure . PrivateShed) (\ticket -> privateArtifactRequest (Just ticket) ctx token) pure
        _ -> privateArtifactRequest Nothing ctx token
    case selected of
        -- The admission shed already counted its decision.
        PrivateShed headers -> PrivateHandled <$> liftIO (respond (tarballError replies shedStatus headers (mkRefusal Nothing shedMessage)))
        PrivateUnavailable -> PrivateAnswered Metric.Unavailable <$> liftIO unavailable
        PrivateIncomplete expected -> PrivateHandled <$> servePublicArtifactWithCoverage (covered expected) ctx
        PrivateRefused -> PrivateAnswered Metric.Deny <$> liftIO refuse
        PrivateMissing miss -> pure (PrivateMissed miss)
        PrivateRequest managed req ->
            liftIO $
                relayUpstreamResult (arMode ctx) (srPrivateClient (arRuntime ctx)) (progressFloor (pdLimits (arDeps ctx))) (shaped req) acceptPrivateArtifact relayUnjudged privateResponder >>= \case
                    UpstreamRelayed (_, (decision, received)) -> pure (PrivateAnswered decision received)
                    UpstreamRejected status -> if managed then PrivateAnswered Metric.Unavailable <$> unavailable else pure (PrivateMissed (if status == status404 then MissAbsent else MissUnresolved))
                    UpstreamTransportFailure -> if managed then PrivateAnswered Metric.Unavailable <$> unavailable else pure (PrivateMissed MissUnresolved)
                    UpstreamCooling _ -> if managed then PrivateAnswered Metric.Unavailable <$> unavailable else pure (PrivateMissed MissUnresolved)
  where
    replies = arReplies ctx
    respond = arRespond ctx

    covered expected = maybe False $ \details -> publicMirrorCoverage (Map.singleton (renderVersion (arVersion ctx)) expected) (PackageInfo (arPackage ctx) (Map.singleton (renderVersion (arVersion ctx)) details) Map.empty [])
    shaped = withValidators (arValidators ctx) . withMethod (arMode ctx)
    unavailable = respond (tarballError replies status503 [] (mkRefusal (pdHelp (arDeps ctx)) "managed mirror view is unavailable"))
    refuse = respond (tarballError replies status403 [] (privateAuthorisationRefusal (pdHelp (arDeps ctx))))
    privateResponder =
        privateArtifactResponder ((Metric.Deny,) <$> refuse) (Metric.Admit,) $
            relayResponder replies respond

data PrivateArtifact
    = PrivateRequest Bool HTTP.Request
    | PrivateRefused
    | PrivateMissing OriginMiss
    | PrivateUnavailable
    | PrivateShed ResponseHeaders
    | PrivateIncomplete MirrorVersion

privateArtifactRequest :: Maybe MemoryTicket -> ArtifactRequest response -> Maybe ClientCredential -> Handler PrivateArtifact
privateArtifactRequest ticket ctx token = case pdPrivateBase deps of
    -- An unconfigured leg and a refused host settle the request here: neither changes on a retry.
    Nothing -> pure (PrivateMissing MissAbsent)
    Just privateBase
        | not (tarballHostHonoured TrustedOrigin deps privateHostPort privateHostPort) -> pure (PrivateMissing MissAbsent)
        | otherwise -> case artifactPrivateAddressing (pdArtifact deps) of
            ConventionalArtifactPath
                | pdReadSourceSelection deps == ManagedMirrorAndPublic -> pure (PrivateMissing MissAbsent)
                | otherwise -> pure (byConventionalPath privateBase)
            IndexedArtifactLocation -> byIndexedLocation ticket ctx token privateBase privateHostPort
  where
    deps = arDeps ctx

    privateHostPort = thgPrivateHostPort (pdTarballHostGate deps)

    byConventionalPath privateBase =
        either (const (PrivateMissing MissAbsent)) (PrivateRequest False) $
            artifactByFile (pdArtifact deps) (mountOrigin deps (srPrivateClient (arRuntime ctx)) privateBase token) (arPackage ctx) (unFilename (arFile ctx))

byIndexedLocation :: Maybe MemoryTicket -> ArtifactRequest response -> Maybe ClientCredential -> BaseRequest -> Maybe HostPort -> Handler PrivateArtifact
byIndexedLocation ticket ctx token privateBase privateHostPort = do
    resolved <- tryAny (withPrivateMetadataClient (arRuntime ctx) deps privateBase token (\client -> fetchVersionMetadata client (arPackage ctx) (arVersion ctx)))
    case resolved of
        Right (Left err) | isNothing (privateMetadataMiss err) -> pure PrivateRefused
        _ -> case (pdMirrorState deps, ticket) of
            (Just mirror, Just admitted) ->
                liftIO (validateMirrorProject mirror admitted (arPackage ctx)) >>= \case
                    Left _ -> pure PrivateUnavailable
                    Right Nothing -> pure (unmanaged resolved)
                    Right (Just (_, record)) -> case recordVersion (arVersion ctx) record of
                        Nothing -> pure (unmanaged resolved)
                        Just expected -> case mirrorVersionView (arVersion ctx) record (either (const Nothing) (either (const Nothing) vrVersion) resolved) of
                            Left _ -> pure (PrivateIncomplete expected)
                            Right _ | Map.notMember (unFilename (arFile ctx)) (versionFiles expected) -> pure (PrivateMissing MissAbsent)
                            Right restored -> pure (maybe PrivateUnavailable (PrivateRequest True) (restored >>= requestForDetails . vdDetails))
            _ -> pure (unmanaged resolved)
  where
    deps = arDeps ctx

    unmanaged resolved
        | pdReadSourceSelection deps == ManagedMirrorAndPublic = case resolved of
            Right (Left err) -> maybe PrivateRefused PrivateMissing (privateMetadataMiss err)
            Left _ -> PrivateMissing MissUnresolved
            _ -> PrivateMissing MissAbsent
        | otherwise = ordinary resolved

    ordinary = \case
        Left _ -> PrivateMissing MissUnresolved
        Right (Left err) -> maybe PrivateRefused PrivateMissing (privateMetadataMiss err)
        Right (Right versionRead) -> maybe (PrivateMissing MissAbsent) (PrivateRequest False) (vrVersion versionRead >>= requestForDetails . vdDetails)

    requestForDetails details = do
        artifact <- find ((== unFilename (arFile ctx)) . artFilename) (pkgArtifacts details)
        let target = hostPortAddress (artUrl artifact)
        guard (artifactAuthorityHonoured (thgEcosystemHosts (pdTarballHostGate deps)) privateHostPort target)
        let carried = if target == privateHostPort then token else Nothing
        rightToMaybe (artifactByUrl (pdArtifact deps) carried (artUrl artifact))

-- | Classify metadata misses, with 'Nothing' for access refusal and absence for identity faults.
privateMetadataMiss :: MetadataError -> Maybe OriginMiss
privateMetadataMiss = \case
    MetadataAuthorisationFailure{} -> Nothing
    MetadataAbsent -> Just MissAbsent
    MetadataNameMismatch{} -> Just MissAbsent
    MetadataHttpFailure{} -> Just MissUnresolved
    MetadataBoundExceeded{} -> Just MissUnresolved
    MetadataFetch{} -> Just MissUnresolved
    MetadataUndecodable -> Just MissUnresolved
