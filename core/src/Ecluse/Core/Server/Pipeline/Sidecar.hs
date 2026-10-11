-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Relay declared distribution companions under their parent's origin and integrity authority.
module Ecluse.Core.Server.Pipeline.Sidecar (
    sidecarAction,
    sidecarUrl,
    selectSidecar,
) where

import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (Method, Status, status401, status403, status404, status500, statusCode)
import Network.Wai (Request, ResponseReceived, requestHeaders)
import UnliftIO (tryAny)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (Artifact (..), CoreMetadata (..), PackageDetails (pkgArtifacts), PackageName)
import Ecluse.Core.Package.Integrity (IntegrityFloor, partitionByFloor)
import Ecluse.Core.Registry (isAuthorisationFailure)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Metadata (MetadataClient (fetchVersionMetadata), VersionDoc (vdDetails), VersionRead (vrVersion))
import Ecluse.Core.Security (HostPort, Limits (progressFloor), Origin (..), hostPortAddress, thgPrivateHostPort, thgPublicHostPort)
import Ecluse.Core.Server.Cache.Store (executePrepared)
import Ecluse.Core.Server.Conditional (forwardValidators)
import Ecluse.Core.Server.Context (Handler, MountBinding (bindingPackumentDeps), PackumentDeps (..), ResponseAction (RunPipeline), ServeRuntime (..), ctxMount, ctxRuntime, pdPrivateBaseUrl, pdTarballHostGate, tarballHostHonoured)
import Ecluse.Core.Server.Path (Filename, unFilename)
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss (..), preparePublicMetadata, withPrivateMetadataClient)
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Pipeline.Tarball.Private (privateMetadataMiss)
import Ecluse.Core.Server.Pipeline.Tarball.Refusal (artifactError, firstPartyMissRefusal, upstreamUnavailable, versionAbsent)
import Ecluse.Core.Server.Pipeline.Tarball.Relay (ArtifactServe (..), acceptArtifact, relayUnjudged, relayUpstreamResult, withMethod, withValidators)
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..))
import Ecluse.Core.Server.Response (ServeDecision, mkRefusal)
import Ecluse.Core.Server.Route (isHead)
import Ecluse.Core.Server.Stream (RelayResponder (..), UpstreamResult (..))
import Ecluse.Core.Version (Version)

-- | Bind GET and HEAD to the same metadata selection and bounded relay.
sidecarAction :: TarballReplies response -> Method -> PackageName -> Version -> Filename -> ResponseAction response
sidecarAction replies method name version file =
    RunPipeline (tarballError replies status500 [] (mkRefusal Nothing "internal server error")) (serveSidecar mode replies name version file)
  where
    mode = if isHead method then ServeHead else ServeFull

serveSidecar :: ArtifactServe -> TarballReplies response -> PackageName -> Version -> Filename -> Request -> (response -> IO ResponseReceived) -> Handler ResponseReceived
serveSidecar mode replies name version file request respond = do
    mount <- asks ctxMount
    runtime <- asks ctxRuntime
    let deps = bindingPackumentDeps mount
        token = forwardedCredential mount request
        ctx = ArtifactRequest mode replies runtime deps (forwardValidators (requestHeaders request)) name version file respond
    if edgeTokenMatches (pdInboundToken deps) token
        then admittedRead ctx (privateSelection ctx token) (servePrivate ctx)
        else liftIO (respond (tarballError replies status401 [] (mkRefusal Nothing unauthorisedMessage)))

-- A surviving private version owns a companion absence as well as a fetchable companion.
data Selection = Selected HTTP.Request | Refused | Missing OriginMiss | CompanionAbsent

admittedRead :: ArtifactRequest response -> Handler Selection -> (Selection -> Handler ResponseReceived) -> Handler ResponseReceived
admittedRead ctx readSelection =
    withMetadataAdmission (arRuntime ctx) shed (const readSelection)
  where
    shed = liftIO (arRespond ctx (tarballError (arReplies ctx) shedStatus [shedRetryAfter] (mkRefusal Nothing shedMessage)))

privateSelection :: ArtifactRequest response -> Maybe ClientCredential -> Handler Selection
privateSelection ctx token = case pdPrivateBaseUrl deps of
    Nothing -> pure (Missing MissAbsent)
    Just base -> do
        result <- tryAny (withPrivateMetadataClient (arRuntime ctx) deps base token (\client -> fetchVersionMetadata client (arPackage ctx) (arVersion ctx)))
        pure $ case result of
            Left _ -> Missing MissUnresolved
            Right (Left err) -> maybe Refused Missing (privateMetadataMiss err)
            Right (Right readVersion) -> case vrVersion readVersion of
                Nothing -> Missing MissAbsent
                Just doc -> case partitionByFloor (pdMinTrustedIntegrity deps) artHashes (pkgArtifacts (vdDetails doc)) of
                    Left _ -> Missing MissAbsent
                    Right _ -> case selectRequest ctx TrustedOrigin (pdMinTrustedIntegrity deps) privateHost token readVersion of
                        Missing _ -> CompanionAbsent
                        selected -> selected
  where
    deps = arDeps ctx
    privateHost = thgPrivateHostPort (pdTarballHostGate deps)

publicSelection :: ArtifactRequest response -> Handler Selection
publicSelection ctx = do
    result <- tryAny (preparePublicMetadata (arRuntime ctx) deps (arPackage ctx) (arVersion ctx) >>= liftIO . executePrepared)
    pure $ case result of
        Left _ -> Missing MissUnresolved
        Right (Left err) -> Missing (fromMaybe MissUnresolved (privateMetadataMiss err))
        Right (Right readVersion) -> selectRequest ctx UntrustedOrigin (pdMinIntegrity deps) publicHost Nothing readVersion
  where
    deps = arDeps ctx
    publicHost = thgPublicHostPort (pdTarballHostGate deps)

selectRequest :: (IntegrityFloor floor) => ArtifactRequest response -> Origin -> floor -> Maybe HostPort -> Maybe ClientCredential -> VersionRead -> Selection
selectRequest ctx origin floorSpec authority token readVersion =
    fromMaybe (Missing MissAbsent) $ do
        details <- vdDetails <$> vrVersion readVersion
        artifact <- selectSidecar floorSpec (tarballHostHonoured origin deps authority . hostPortAddress . artUrl) (unFilename (arFile ctx)) details
        let url = sidecarUrl (artUrl artifact)
            target = hostPortAddress url
            credential = if origin == TrustedOrigin && target == authority then token else Nothing
        guard (tarballHostHonoured origin deps authority target)
        Selected <$> rightToMaybe (artifactByUrl (pdArtifact deps) credential url)
  where
    deps = arDeps ctx

-- | Select the first eligible source entry before checking its companion declaration.
selectSidecar :: (IntegrityFloor floor) => floor -> (Artifact -> Bool) -> Text -> PackageDetails -> Maybe Artifact
selectSidecar floorSpec allowed filename details = do
    survivors <- rightToMaybe (partitionByFloor floorSpec artHashes (pkgArtifacts details))
    artifact <- find (\candidate -> artFilename candidate == filename && allowed candidate) survivors
    guard $ case artCoreMetadata artifact of
        Just (MetadataAvailable available) -> available
        Just (MetadataDigests digests) -> not (null digests)
        Nothing -> False
    pure artifact

-- | Append the companion suffix to the path while preserving query and fragment text.
sidecarUrl :: Text -> Text
sidecarUrl url = path <> ".metadata" <> suffix
  where
    (path, suffix) = T.break (\character -> character == '?' || character == '#') url

servePrivate :: ArtifactRequest response -> Selection -> Handler ResponseReceived
servePrivate ctx = \case
    CompanionAbsent -> answerError ctx versionAbsent
    Refused -> refusePrivate ctx
    Missing miss -> fallback ctx miss
    Selected request -> do
        result <- relay ctx True (srPrivateManager (arRuntime ctx)) acceptPrivate request
        case result of
            UpstreamRelayed (_, received) -> pure received
            UpstreamRejected status -> answerError ctx (if status == status404 then versionAbsent else upstreamUnavailable)
            UpstreamTransportFailure -> answerError ctx upstreamUnavailable
  where
    acceptPrivate status = acceptArtifact status || isAuthorisationFailure (statusCode status)

fallback :: ArtifactRequest response -> OriginMiss -> Handler ResponseReceived
fallback ctx miss
    | pdFirstParty (arDeps ctx) (arPackage ctx) = answerError ctx (firstPartyMissRefusal miss)
    | otherwise = admittedRead ctx (publicSelection ctx) (servePublic ctx)

servePublic :: ArtifactRequest response -> Selection -> Handler ResponseReceived
servePublic ctx = \case
    CompanionAbsent -> answerError ctx versionAbsent
    Refused -> answerError ctx upstreamUnavailable
    Missing MissAbsent -> answerError ctx versionAbsent
    Missing MissUnresolved -> answerError ctx upstreamUnavailable
    Selected request ->
        relay ctx False (srPublicManager (arRuntime ctx)) (const True) request >>= \case
            UpstreamRelayed (_, received) -> pure received
            _ -> answerError ctx upstreamUnavailable

relay :: ArtifactRequest response -> Bool -> HTTP.Manager -> (Status -> Bool) -> HTTP.Request -> Handler (UpstreamResult ((), ResponseReceived))
relay ctx private manager accepts request =
    liftIO $
        relayUpstreamResult (arMode ctx) manager (progressFloor (pdLimits (arDeps ctx))) shaped accepts relayUnjudged responder
  where
    shaped = withValidators (arValidators ctx) (withMethod (arMode ctx) request)
    replies = arReplies ctx
    respond = arRespond ctx
    responder =
        RelayResponder
            (\status headers body -> respond (if private && isAuthorisationFailure (statusCode status) then privateRefusal ctx else tarballStream replies status headers body))
            (\status headers -> respond (if private && isAuthorisationFailure (statusCode status) then privateRefusal ctx else tarballEmpty replies status headers))

privateRefusal :: ArtifactRequest response -> response
privateRefusal ctx = tarballError (arReplies ctx) status403 [] (privateAuthorisationRefusal (pdHelp (arDeps ctx)))

refusePrivate :: ArtifactRequest response -> Handler ResponseReceived
refusePrivate ctx = liftIO (arRespond ctx (privateRefusal ctx))

answerError :: ArtifactRequest response -> ServeDecision -> Handler ResponseReceived
answerError ctx decision = liftIO (arRespond ctx (artifactError (arReplies ctx) (arDeps ctx) decision))
