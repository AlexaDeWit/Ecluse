-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

{- | The trusted leg of the artifact path: read the private origin under the caller's credential.

An access refusal commits the local error and never relays upstream's body, so a private @401@ or
@403@ cannot be answered from the public registry. Any other miss falls through to the caller.
-}
module Ecluse.Core.Server.Pipeline.Tarball.Private (
    PrivateLeg (..),
    streamPrivateArtifact,
    privateMetadataMiss,
) where

import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status403, status404, statusCode)
import Network.Wai (ResponseReceived)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (
    Artifact (artFilename, artUrl),
    PackageDetails (pkgArtifacts),
 )
import Ecluse.Core.Registry (isAuthorisationFailure)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByFile, artifactByUrl, artifactHosts))
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
import Ecluse.Core.Security (
    HostPort,
    Limits (progressFloor),
    Origin (TrustedOrigin),
    artifactAuthorityHonoured,
    hostPortAddress,
    thgEcosystemHosts,
    thgPrivateHostPort,
 )
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Server.Context (
    Handler,
    PackumentDeps (..),
    ServeRuntime (..),
    pdPrivateBaseUrl,
    pdTarballHostGate,
    tarballHostHonoured,
 )
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Server.Pipeline.Origin (
    OriginMiss (MissAbsent, MissUnresolved),
    mountOrigin,
    withPrivateMetadataClient,
 )
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Pipeline.Tarball.Relay (
    acceptArtifact,
    relayUnjudged,
    relayUpstreamResult,
    withMethod,
    withValidators,
 )
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..))
import Ecluse.Core.Server.Stream (RelayResponder (RelayResponder), UpstreamResult (..))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import UnliftIO (tryAny)

-- | The private leg's answer: the response it committed, or why it did not answer.
data PrivateLeg received
    = PrivateAnswered Metric.Decision received
    | PrivateMissed OriginMiss

-- | Read the private origin, committing a response or reporting the miss that ends the leg.
streamPrivateArtifact ::
    ArtifactRequest response ->
    Maybe ClientCredential ->
    Handler (PrivateLeg ResponseReceived)
streamPrivateArtifact ctx token =
    privateArtifactRequest ctx token >>= \case
        PrivateRefused -> PrivateAnswered Metric.Deny <$> liftIO refuse
        PrivateMissing miss -> pure (PrivateMissed miss)
        PrivateRequest req ->
            liftIO $
                relayUpstreamResult (arMode ctx) (srPrivateManager (arRuntime ctx)) (progressFloor (pdLimits (arDeps ctx))) (shaped req) acceptPrivate relayUnjudged privateResponder >>= \case
                    UpstreamRelayed (_, (decision, received)) -> pure (PrivateAnswered decision received)
                    UpstreamRejected status -> pure (PrivateMissed (if status == status404 then MissAbsent else MissUnresolved))
                    UpstreamTransportFailure -> pure (PrivateMissed MissUnresolved)
  where
    replies = arReplies ctx
    respond = arRespond ctx

    shaped = withValidators (arValidators ctx) . withMethod (arMode ctx)
    refuse = respond (tarballError replies status403 [] (privateAuthorisationRefusal (pdHelp (arDeps ctx))))
    acceptPrivate status = acceptArtifact status || isAuthorisationFailure (statusCode status)

    privateResponder =
        RelayResponder
            (\status headers body -> answerPrivate status (respond (tarballStream replies status headers body)))
            (\status headers -> answerPrivate status (respond (tarballEmpty replies status headers)))

    answerPrivate status admitted
        | isAuthorisationFailure (statusCode status) = (Metric.Deny,) <$> refuse
        | otherwise = (Metric.Admit,) <$> admitted

data PrivateArtifact
    = PrivateRequest HTTP.Request
    | PrivateRefused
    | PrivateMissing OriginMiss

privateArtifactRequest :: ArtifactRequest response -> Maybe ClientCredential -> Handler PrivateArtifact
privateArtifactRequest ctx token = case pdPrivateBaseUrl deps of
    -- An unconfigured leg and a refused host settle the request here: neither changes on a retry.
    Nothing -> pure (PrivateMissing MissAbsent)
    Just privateBase
        | not (tarballHostHonoured TrustedOrigin deps privateHostPort privateHostPort) -> pure (PrivateMissing MissAbsent)
        | null (artifactHosts (pdArtifact deps)) -> pure (byConventionalPath privateBase)
        | otherwise -> byIndexedLocation ctx token privateBase privateHostPort
  where
    deps = arDeps ctx

    privateHostPort = thgPrivateHostPort (pdTarballHostGate deps)

    byConventionalPath privateBase =
        either (const (PrivateMissing MissAbsent)) PrivateRequest $
            artifactByFile (pdArtifact deps) (mountOrigin deps (srPrivateManager (arRuntime ctx)) privateBase token) (arPackage ctx) (unFilename (arFile ctx))

byIndexedLocation :: ArtifactRequest response -> Maybe ClientCredential -> RegistryUrl -> Maybe HostPort -> Handler PrivateArtifact
byIndexedLocation ctx token privateBase privateHostPort = do
    resolved <- tryAny (withPrivateMetadataClient (arRuntime ctx) deps privateBase token (\client -> fetchVersionMetadata client (arPackage ctx) (arVersion ctx)))
    pure $ case resolved of
        Left _ -> PrivateMissing MissUnresolved
        Right (Left err) -> maybe PrivateRefused PrivateMissing (privateMetadataMiss err)
        Right (Right versionRead) -> maybe (PrivateMissing MissAbsent) PrivateRequest (vrVersion versionRead >>= requestForDetails . vdDetails)
  where
    deps = arDeps ctx

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
