-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

{- | Read private archives under the caller's credential.
Access refusals stop fallback, and managed mirror copies require retained completion proof.
-}
module Ecluse.Core.Server.Pipeline.Tarball.Private (
    PrivateLeg (..),
    streamPrivateArtifact,
    privateMetadataMiss,
) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (ResponseHeaders, status404, status503, statusCode)
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
    ServeRuntime (..),
    pdPrivateBase,
    pdTarballHostGate,
    tarballHostHonoured,
 )
import Ecluse.Core.Server.MirrorView (mirrorVersionView, mirrorViewDocuments, publicMirrorCoverage, validateMirrorProject)
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
    selected <-
        if privateMetadataAdmissionRequired ctx
            then withMetadataAdmission (arRuntime ctx) (pure . PrivateShed) (\ticket -> privateArtifactRequest (Just ticket) ctx token) pure
            else privateArtifactRequest Nothing ctx token
    answer selected
  where
    answer prepared = case prepared of
        -- The admission shed already counted its decision.
        PrivateShed headers -> PrivateHandled <$> liftIO (respond (tarballError replies shedStatus headers (mkRefusal Nothing shedMessage)))
        PrivateUnavailable -> PrivateAnswered Metric.Unavailable <$> liftIO unavailable
        PrivateIncomplete expected -> PrivateHandled <$> servePublicArtifactWithCoverage (covered expected) ctx
        PrivateRefused code -> PrivateAnswered Metric.Deny <$> liftIO (refuse code)
        PrivateMissing miss -> pure (PrivateMissed miss)
        PrivateRequest managed req ->
            liftIO $
                relayUpstreamResult (arMode ctx) (srPrivateClient (arRuntime ctx)) (progressFloor (pdLimits (arDeps ctx))) (shaped req) acceptPrivateArtifact relayUnjudged privateResponder >>= \case
                    UpstreamRelayed (_, (decision, received)) -> pure (PrivateAnswered decision received)
                    UpstreamRejected status -> if managed then PrivateAnswered Metric.Unavailable <$> unavailable else pure (PrivateMissed (if status == status404 then MissAbsent else MissUnresolved))
                    UpstreamTransportFailure -> if managed then PrivateAnswered Metric.Unavailable <$> unavailable else pure (PrivateMissed MissUnresolved)
                    UpstreamCooling _ -> if managed then PrivateAnswered Metric.Unavailable <$> unavailable else pure (PrivateMissed MissUnresolved)
    replies = arReplies ctx
    respond = arRespond ctx

    covered expected = maybe False $ \details -> publicMirrorCoverage (Map.singleton (renderVersion (arVersion ctx)) expected) (PackageInfo (arPackage ctx) (Map.singleton (renderVersion (arVersion ctx)) details) Map.empty [])
    shaped request = withValidators (arValidators ctx) (withMethod (arMode ctx) request)
    unavailable = respond (tarballError replies status503 [] (mkRefusal (pdHelp (arDeps ctx)) "managed mirror view is unavailable"))
    refuse code = respond (tarballPrivateRefused replies code [] (privateAuthorisationRefusal (pdHelp (arDeps ctx))))
    privateResponder =
        privateArtifactResponder (\status -> (Metric.Deny,) <$> refuse (statusCode status)) (Metric.Admit,) $
            relayResponder replies respond

privateMetadataAdmissionRequired :: ArtifactRequest response -> Bool
privateMetadataAdmissionRequired ctx =
    artifactPrivateAddressing (pdArtifact deps) == AdmittedIndexedArtifactLocation
        || (isJust (pdMirrorState deps) && not (pdFirstParty deps (arPackage ctx)))
  where
    deps = arDeps ctx

data PrivateArtifact
    = PrivateRequest Bool HTTP.Request
    | PrivateRefused Int
    | PrivateMissing OriginMiss
    | PrivateUnavailable
    | PrivateShed ResponseHeaders
    | PrivateIncomplete MirrorVersion

privateArtifactRequest :: Maybe MemoryTicket -> ArtifactRequest response -> Maybe ClientCredential -> Handler PrivateArtifact
privateArtifactRequest ticket ctx token
    | privateMetadataAdmissionRequired ctx && isNothing ticket = pure PrivateUnavailable
    | otherwise = case pdPrivateBase deps of
        Nothing -> pure (PrivateMissing MissAbsent)
        Just privateBase
            | not (tarballHostHonoured TrustedOrigin deps privateHostPort privateHostPort) -> pure (PrivateMissing MissAbsent)
            | otherwise -> case artifactPrivateAddressing (pdArtifact deps) of
                ConventionalArtifactPath -> pure (byConventionalPath privateBase)
                IndexedArtifactLocation -> indexed privateBase
                AdmittedIndexedArtifactLocation -> indexed privateBase
  where
    deps = arDeps ctx

    privateHostPort = thgPrivateHostPort (pdTarballHostGate deps)
    indexed privateBase = byIndexedLocation ticket ctx token privateBase privateHostPort

    byConventionalPath privateBase =
        either (const (PrivateMissing MissAbsent)) (PrivateRequest False) $
            artifactByFile (pdArtifact deps) (mountOrigin deps (srPrivateClient (arRuntime ctx)) privateBase token) (arPackage ctx) (unFilename (arFile ctx))

byIndexedLocation :: Maybe MemoryTicket -> ArtifactRequest response -> Maybe ClientCredential -> BaseRequest -> Maybe HostPort -> Handler PrivateArtifact
byIndexedLocation ticket ctx token privateBase privateHostPort = do
    resolved <- tryAny (withPrivateMetadataClient (arRuntime ctx) deps ticket privateBase token (\client -> fetchVersionMetadata client (arPackage ctx) (arVersion ctx)))
    case resolved of
        Right (Left (MetadataAuthorisationFailure code)) -> pure (PrivateRefused code)
        _ -> case (pdMirrorState deps, ticket) of
            (Just mirror, Just admitted)
                | not (pdFirstParty deps (arPackage ctx)) ->
                    liftIO (validateMirrorProject mirror admitted (arPackage ctx)) >>= \case
                        Left _ -> pure PrivateUnavailable
                        Right Nothing -> pure (ordinary resolved)
                        Right (Just (_, record)) -> case recordVersion (arVersion ctx) record of
                            Nothing -> pure (ordinary resolved)
                            Just expected -> case mirrorVersionView (mirrorViewDocuments mirror) (arVersion ctx) record (either (const Nothing) (either (const Nothing) vrVersion) resolved) of
                                Left _ -> pure (PrivateIncomplete expected)
                                Right _ | Map.notMember (unFilename (arFile ctx)) (versionFiles expected) -> pure (PrivateMissing MissAbsent)
                                Right restored -> pure (maybe PrivateUnavailable (PrivateRequest True) (restored >>= requestForDetails . vdDetails))
            _ -> pure (ordinary resolved)
  where
    deps = arDeps ctx

    ordinary = \case
        Left _ -> PrivateMissing MissUnresolved
        Right (Left err) -> privateMetadataFailure err
        Right (Right versionRead) -> maybe (PrivateMissing MissAbsent) (PrivateRequest False) (vrVersion versionRead >>= requestForDetails . vdDetails)

    requestForDetails details = do
        artifact <- find ((== unFilename (arFile ctx)) . artFilename) (pkgArtifacts details)
        let target = hostPortAddress (artUrl artifact)
        guard (artifactAuthorityHonoured (thgEcosystemHosts (pdTarballHostGate deps)) privateHostPort target)
        let carried = if target == privateHostPort then token else Nothing
        rightToMaybe (artifactByUrl (pdArtifact deps) carried (artUrl artifact))

privateMetadataFailure :: MetadataError -> PrivateArtifact
privateMetadataFailure = \case
    MetadataAuthorisationFailure code -> PrivateRefused code
    err -> PrivateMissing (fromMaybe MissUnresolved (privateMetadataMiss err))

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
