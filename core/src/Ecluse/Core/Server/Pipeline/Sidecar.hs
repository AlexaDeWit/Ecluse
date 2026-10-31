-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Relay declared distribution companions under their parent's origin and integrity authority.
module Ecluse.Core.Server.Pipeline.Sidecar (
    sidecarAction,
    sidecarUrl,
    selectSidecar,
) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.Map.Strict qualified as Map
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (Method, Status, hContentLength, status200, status304, status401, status403, status404, status500, statusCode)
import Network.Wai (Request, ResponseReceived, requestHeaders)
import UnliftIO (tryAny)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Fault (RetryAfter)
import Ecluse.Core.Mirror.State (MirrorVersion, recordVersion, versionFiles)
import Ecluse.Core.Package (Artifact (..), CoreMetadata (..), PackageDetails (pkgArtifacts), PackageInfo (PackageInfo), PackageName)
import Ecluse.Core.Package.Integrity (IntegrityFloor, partitionByFloor)
import Ecluse.Core.Registry (FetchFault (FetchCooldown))
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Metadata (MetadataClient (fetchVersionMetadata), MetadataError (MetadataFetch, MetadataNameMismatch), VersionDoc (vdDetails), VersionRead (vrVersion))
import Ecluse.Core.Registry.Request (sidecarUrl)
import Ecluse.Core.Security (HostPort, Limits (progressFloor), Origin (..), hostPortAddress, thgPrivateHostPort, thgPublicHostPort)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket)
import Ecluse.Core.Server.Conditional (Conditional (..), etagHeader, evaluateETag, forwardValidators, mkStrongETag)
import Ecluse.Core.Server.Context (Handler, MountBinding (bindingPackumentDeps), PackumentDeps (..), ResponseAction (RunPipeline), ServeRuntime (..), ctxMount, ctxRuntime, pdPrivateBase, pdTarballHostGate, tarballHostHonoured)
import Ecluse.Core.Server.MirrorView (mirrorVersionView, mirrorViewDocuments, publicMirrorCoverage, readMirrorCompanion, validateMirrorProject)
import Ecluse.Core.Server.Path (Filename, unFilename)
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss (..), ReadFailure (ReadUnavailable), statusReadFailure, withPrivateMetadataClient, withPublicMetadata)
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Pipeline.Tarball.Private (RecordedCopy (RecordedCopyGone), privateMetadataMiss, privateReadFailure, recordedCopy)
import Ecluse.Core.Server.Pipeline.Tarball.Refusal (artifactError, privateMissRefusal, privateReadRefusal, upstreamUnavailable, versionAbsent)
import Ecluse.Core.Server.Pipeline.Tarball.Relay (ArtifactServe (..), acceptPrivateArtifact, privateArtifactResponder, relayPublicResult, relayUnjudged, relayUpstreamResult, withMethod, withValidators)
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..), relayResponder)
import Ecluse.Core.Server.Response (ServeDecision, mkRefusal)
import Ecluse.Core.Server.Route (isHead)
import Ecluse.Core.Server.Stream (UpstreamResult (..))
import Ecluse.Core.Telemetry.Upstream qualified as PublicRead
import Ecluse.Core.Version (Version, renderVersion)

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

-- A recorded private file owns its companion absence after the complete mirror proof.
data Selection = Selected HTTP.Request | Retained ByteString | Incomplete RecordedCopy MirrorVersion | NewPublicFile | Unavailable | Cooling RetryAfter | Refused | Missing OriginMiss | CompanionAbsent

admittedRead :: ArtifactRequest response -> (MemoryTicket -> Handler Selection) -> (Selection -> Handler ResponseReceived) -> Handler ResponseReceived
admittedRead ctx readSelection answer =
    withMetadataAdmission (arRuntime ctx) shed retainedSelection (either pure answer)
  where
    retainedSelection ticket =
        readSelection ticket >>= \case
            Retained bytes -> Left <$> serveRetained ctx bytes
            selected -> pure (Right selected)
    shed headers = liftIO (arRespond ctx (tarballError (arReplies ctx) shedStatus headers (mkRefusal Nothing shedMessage)))

privateSelection :: ArtifactRequest response -> Maybe ClientCredential -> MemoryTicket -> Handler Selection
privateSelection ctx token ticket = case pdPrivateBase deps of
    Nothing -> pure (Missing MissAbsent)
    Just base -> do
        result <- tryAny (withPrivateMetadataClient (arRuntime ctx) deps (Just ticket) base token (\client -> fetchVersionMetadata client (arPackage ctx) (arVersion ctx)))
        case result of
            Right (Left err) | isNothing (privateMetadataMiss err) -> pure Refused
            _ | Just failure <- privateReadFailure result -> pure (Missing (MissUnresolved failure))
            _ -> case pdMirrorState deps of
                Just mirror
                    | not (pdFirstParty deps (arPackage ctx)) ->
                        liftIO (validateMirrorProject mirror ticket (arPackage ctx)) >>= \case
                            Left _ -> pure Unavailable
                            Right Nothing -> pure (ordinary result)
                            Right (Just (_, record)) -> case recordVersion (arVersion ctx) record of
                                Nothing -> pure (ordinary result)
                                Just expected -> case mirrorVersionView (mirrorViewDocuments mirror) (arVersion ctx) record (either (const Nothing) (either (const Nothing) vrVersion) result) of
                                    Left _ -> pure (Incomplete (recordedCopy result) expected)
                                    Right _ | Map.notMember (unFilename (arFile ctx)) (versionFiles expected) -> pure NewPublicFile
                                    Right restored -> case restored >>= selectSidecar (pdMinTrustedIntegrity deps) (tarballHostHonoured TrustedOrigin deps privateHost . hostPortAddress . artUrl) (unFilename (arFile ctx)) . vdDetails of
                                        Nothing -> pure CompanionAbsent
                                        Just _ -> either (const Unavailable) Retained <$> liftIO (readMirrorCompanion mirror ticket record (arVersion ctx) (unFilename (arFile ctx)))
                _ -> pure (ordinary result)
  where
    deps = arDeps ctx
    privateHost = thgPrivateHostPort (pdTarballHostGate deps)

    ordinary = \case
        Left _ -> Missing (MissUnresolved (ReadUnavailable Nothing))
        Right (Left err) -> maybe Refused Missing (privateMetadataMiss err)
        Right (Right readVersion) -> case vrVersion readVersion of
            Nothing -> Missing MissAbsent
            Just doc -> case partitionByFloor (pdMinTrustedIntegrity deps) artHashes (pkgArtifacts (vdDetails doc)) of
                Left _ -> Missing MissAbsent
                Right _ -> case selectRequest ctx TrustedOrigin (pdMinTrustedIntegrity deps) privateHost token readVersion of
                    Missing _ -> CompanionAbsent
                    selected -> selected

publicSelection :: ArtifactRequest response -> MemoryTicket -> Handler Selection
publicSelection ctx ticket = do
    result <- tryAny (withPublicMetadata (arRuntime ctx) deps ticket (arPackage ctx) (arVersion ctx) liftIO)
    pure $ case result of
        Left _ -> Missing (MissUnresolved (ReadUnavailable Nothing))
        Right (Left (MetadataFetch (FetchCooldown retry))) -> Cooling retry
        Right (Left err) -> Missing (publicMetadataMiss err)
        Right (Right readVersion) -> selectRequest ctx UntrustedOrigin (pdMinIntegrity deps) publicHost Nothing readVersion
  where
    deps = arDeps ctx
    publicHost = thgPublicHostPort (pdTarballHostGate deps)

-- A public origin that names another package drops out, so its mismatch reads as an absence.
publicMetadataMiss :: MetadataError -> OriginMiss
publicMetadataMiss = \case
    MetadataNameMismatch _ -> MissAbsent
    err -> fromMaybe (MissUnresolved (ReadUnavailable Nothing)) (privateMetadataMiss err)

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

servePrivate :: ArtifactRequest response -> Selection -> Handler ResponseReceived
servePrivate ctx = \case
    Retained bytes -> serveRetained ctx bytes
    Unavailable -> answerError ctx upstreamUnavailable
    Cooling retry -> answerError ctx (upstreamCooling retry)
    Incomplete copy expected -> admittedRead ctx (publicSelectionCovered ctx (Just (copy, expected))) (servePublic ctx)
    NewPublicFile -> admittedRead ctx (publicSelectionCovered ctx Nothing) (servePublic ctx)
    CompanionAbsent -> answerError ctx versionAbsent
    Refused -> refusePrivate ctx
    Missing miss -> fallback ctx miss
    Selected request -> do
        result <- relay ctx True (srPrivateClient (arRuntime ctx)) acceptPrivateArtifact request
        case result of
            UpstreamRelayed (_, received) -> pure received
            UpstreamRejected status retry
                | status == status404 -> answerError ctx versionAbsent
                | otherwise -> readFailed (statusReadFailure (statusCode status) retry)
            UpstreamTransportFailure -> readFailed (ReadUnavailable Nothing)
            UpstreamCooling retry -> answerError ctx (upstreamCooling retry)
  where
    readFailed = answerError ctx . privateReadRefusal (arDeps ctx) (arPackage ctx)

fallback :: ArtifactRequest response -> OriginMiss -> Handler ResponseReceived
fallback ctx miss =
    maybe (admittedRead ctx (publicSelection ctx) (servePublic ctx)) (answerError ctx) (privateMissRefusal (arDeps ctx) (arPackage ctx) miss)

servePublic :: ArtifactRequest response -> Selection -> Handler ResponseReceived
servePublic ctx = \case
    Retained _ -> answerError ctx upstreamUnavailable
    Unavailable -> answerError ctx upstreamUnavailable
    Cooling retry -> answerError ctx (upstreamCooling retry)
    Incomplete _ _ -> answerError ctx upstreamUnavailable
    NewPublicFile -> answerError ctx upstreamUnavailable
    CompanionAbsent -> answerError ctx versionAbsent
    Refused -> answerError ctx upstreamUnavailable
    Missing MissAbsent -> answerError ctx versionAbsent
    Missing (MissUnresolved _) -> answerError ctx upstreamUnavailable
    Selected request ->
        relay ctx False (srPublicClient (arRuntime ctx)) (const True) request >>= \case
            UpstreamRelayed (_, received) -> pure received
            UpstreamCooling retry -> answerError ctx (upstreamCooling retry)
            _ -> answerError ctx upstreamUnavailable

relay :: ArtifactRequest response -> Bool -> ExchangeClient -> (Status -> Bool) -> HTTP.Request -> Handler (UpstreamResult ((), ResponseReceived))
relay ctx private client accepts request =
    liftIO $
        relayRead (arMode ctx) client (progressFloor (pdLimits (arDeps ctx))) shaped accepts relayUnjudged responder
  where
    relayRead = if private then relayUpstreamResult else relayPublicResult (srPublicControl (arRuntime ctx)) PublicRead.Companion
    shaped = withValidators (arValidators ctx) (withMethod (arMode ctx) request)
    replies = arReplies ctx
    respond = arRespond ctx
    responder =
        if private
            then privateArtifactResponder (const (respond (privateRefusal ctx))) id upstreamResponder
            else upstreamResponder
    upstreamResponder = relayResponder replies respond

privateRefusal :: ArtifactRequest response -> response
privateRefusal ctx = tarballError (arReplies ctx) status403 [] (privateAuthorisationRefusal (pdHelp (arDeps ctx)))

refusePrivate :: ArtifactRequest response -> Handler ResponseReceived
refusePrivate ctx = liftIO (arRespond ctx (privateRefusal ctx))

answerError :: ArtifactRequest response -> ServeDecision -> Handler ResponseReceived
answerError ctx decision = liftIO (arRespond ctx (artifactError (arReplies ctx) (arDeps ctx) decision))

serveRetained :: ArtifactRequest response -> ByteString -> Handler ResponseReceived
serveRetained ctx bytes = liftIO $ arRespond ctx $ case evaluateETag (arValidators ctx) etag of
    NotModified matched -> tarballEmpty replies status304 [etagHeader matched]
    Modified fresh -> case arMode ctx of
        ServeHead -> tarballEmpty replies status200 (headers fresh)
        ServeFull -> tarballStream replies status200 (headers fresh) (\send flush -> send (byteString bytes) >> flush)
  where
    replies = arReplies ctx
    etag = mkStrongETag (hash bytes :: Digest SHA256)
    headers fresh = [etagHeader fresh, (hContentLength, show (BS.length bytes))]

publicSelectionCovered :: ArtifactRequest response -> Maybe (RecordedCopy, MirrorVersion) -> MemoryTicket -> Handler Selection
publicSelectionCovered ctx expected ticket = do
    result <- tryAny (withPublicMetadata (arRuntime ctx) deps ticket (arPackage ctx) (arVersion ctx) liftIO)
    case result of
        Right (Right readVersion) | Just doc <- vrVersion readVersion -> do
            let info = PackageInfo (arPackage ctx) (Map.singleton (renderVersion (arVersion ctx)) (vdDetails doc)) Map.empty []
            if maybe True (\(_, mirror) -> publicMirrorCoverage (Map.singleton (renderVersion (arVersion ctx)) mirror) info) expected
                then pure (selectRequest ctx UntrustedOrigin (pdMinIntegrity deps) publicHost Nothing readVersion)
                else pure Unavailable
        Right (Left (MetadataFetch (FetchCooldown retry))) -> pure (Cooling retry)
        -- A public index for another project is an unusable reply, never the absence of a recorded version.
        Right (Left (MetadataNameMismatch _)) | isJust expected -> pure Unavailable
        Right (Right _) | missSettles -> pure (Missing MissAbsent)
        Right (Left err) | missSettles -> pure (Missing (publicMetadataMiss err))
        _ -> pure Unavailable
  where
    deps = arDeps ctx
    -- A public miss settles a new file, or a recorded version whose private copy is gone.
    missSettles = maybe True ((== RecordedCopyGone) . fst) expected
    publicHost = thgPublicHostPort (pdTarballHostGate deps)
