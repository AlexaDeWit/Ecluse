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
import Network.HTTP.Types (status403, statusCode)
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
    relayUpstreamWhen,
    withMethod,
    withValidators,
 )
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..))
import Ecluse.Core.Server.Stream (RelayResponder (RelayResponder))
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
        -- The relay reports no cause of its own, so a rejected private status and an unreachable
        -- private host are one miss, each keeping today's fall-through.
        PrivateRequest req ->
            liftIO $
                maybe (PrivateMissed MissAbsent) (uncurry PrivateAnswered . snd)
                    <$> relayUpstreamWhen (arMode ctx) (srPrivateManager (arRuntime ctx)) (shaped req) acceptPrivate relayUnjudged privateResponder
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

-- The private leg's outcome before any relay: a request to make, an explicit refusal, or a miss.
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
        | otherwise -> byIndexedLocation ctx token privateBase
  where
    deps = arDeps ctx

    -- The precomputed private authority. A conventionally-built URL is on the private base, so
    -- the gate stays applied and trivially satisfied without re-parsing the URL.
    privateHostPort = thgPrivateHostPort (pdTarballHostGate deps)

    byConventionalPath privateBase =
        either (const (PrivateMissing MissAbsent)) PrivateRequest $
            artifactByFile (pdArtifact deps) (mountOrigin deps (srPrivateManager (arRuntime ctx)) privateBase token) (arPackage ctx) (unFilename (arFile ctx))

{- The location is gated from the same definition the download gate reads, and the credential
rides only when the target is the private upstream itself. -}
byIndexedLocation :: ArtifactRequest response -> Maybe ClientCredential -> RegistryUrl -> Handler PrivateArtifact
byIndexedLocation ctx token privateBase = do
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
            privateHostPort = thgPrivateHostPort (pdTarballHostGate deps)
        guard (artifactAuthorityHonoured (thgEcosystemHosts (pdTarballHostGate deps)) privateHostPort target)
        let carried = if target == privateHostPort then token else Nothing
        rightToMaybe (artifactByUrl (pdArtifact deps) carried (artUrl artifact))

{- | The miss a private metadata failure leaves the artifact path, or 'Nothing' for an explicit
access refusal. An identity fault settles the request, because this path renders no @502@ for one.
-}
privateMetadataMiss :: MetadataError -> Maybe OriginMiss
privateMetadataMiss = \case
    MetadataAuthorisationFailure{} -> Nothing
    MetadataAbsent -> Just MissAbsent
    MetadataNameMismatch{} -> Just MissAbsent
    MetadataHttpFailure{} -> Just MissUnresolved
    MetadataBoundExceeded{} -> Just MissUnresolved
    MetadataFetch{} -> Just MissUnresolved
    MetadataUndecodable -> Just MissUnresolved
