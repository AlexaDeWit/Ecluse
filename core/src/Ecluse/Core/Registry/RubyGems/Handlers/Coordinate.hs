-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native filename captures resolve only against admitted inventory coordinates.
module Ecluse.Core.Registry.RubyGems.Handlers.Coordinate (
    serveGemArtifact,
    serveCompactSpec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Network.HTTP.Types (Method)
import Network.Wai (Request, ResponseReceived)
import UnliftIO (try)

import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Mirror.State (recordVersion, versionFiles)
import Ecluse.Core.Package (Artifact (artFilename), PackageDetails (pkgArtifacts, pkgName, pkgVersion), PackageName)
import Ecluse.Core.Package.Merge (Provenance (..))
import Ecluse.Core.Registry (BodyOutcome (..), FetchFault (FetchUrlUnformable))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.CachedDocument (rubyGemsCached)
import Ecluse.Core.Registry.Exchange (boundedChargedRead, chargedRead, formThen, withSuccessBodyUsing)
import Ecluse.Core.Registry.Public (ReadControl (..))
import Ecluse.Core.Registry.Request (artifactRequestByUrl)
import Ecluse.Core.Registry.RubyGems.CompactSpec (compactSpecBytes, validateCompactSpec)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Handlers.Budget (newNativeDocumentBudget, newNativeReadBudget, withNativeRequest)
import Ecluse.Core.Registry.RubyGems.Project (candidateCoordinates, coordinateFilename, coordinateName, coordinateVersion)
import Ecluse.Core.Registry.RubyGems.Request (gemCompactSpecUrl)
import Ecluse.Core.Registry.RubyGems.Route.Response (RubyResponse, rubyBadRequest, rubyReplies)
import Ecluse.Core.Rules.Types (EvalContext (ctxAdvisoryEtag))
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Security.Limits (BodyLimit (MetadataBodyLimit), Limits (..))
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, charge, newChargeRegion)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfOutputPermille))
import Ecluse.Core.Server.Context (Handler, MountBinding (bindingPackumentDeps), PackumentDeps (..), ServeRuntime (..), ctxMount, ctxRuntime, pdPrivateBaseUrl, pdPublicBaseUrl)
import Ecluse.Core.Server.MirrorView (readMirrorCompanion, validateMirrorProject)
import Ecluse.Core.Server.Path (Filename, unFilename)
import Ecluse.Core.Server.Pipeline.Internal (VersionVerdict (VersionVerdict), logDenials, recordDenials, statusServeDecision)
import Ecluse.Core.Server.Pipeline.Origin (ObservedReadBoundExceeded (ObservedReadBoundExceeded), ReadFailure (ReadInvalid, ReadUnavailable), fetchReadFailure, statusReadFailure)
import Ecluse.Core.Server.Pipeline.Packument (DocumentRejectionObserver (..), PackumentReplies (..), answerMetadataBytes, mapPackumentReplies, noSurvivors, withObservedAdmittedDocument)
import Ecluse.Core.Server.Pipeline.Shared (forwardedCredential, retryAfterHeaders, withListingAdmission, withMetadataAdmission)
import Ecluse.Core.Server.Pipeline.Tarball (headTarball, serveTarball)
import Ecluse.Core.Server.Pipeline.Tarball.Types (TarballReplies)
import Ecluse.Core.Server.Response (RejectReason (..), Rejection (rejectionReason), ServeDecision (Reject), mkRefusal, packumentStatus)
import Ecluse.Core.Telemetry.Record (MetricsPort (mpServeDecision))
import Ecluse.Core.Telemetry.Upstream (PublicReadKind (Companion))
import Ecluse.Core.Version (renderVersion)

-- | Resolve the exact native filename before entering the shared artifact relay.
serveGemArtifact :: TarballReplies response -> Method -> Filename -> Request -> (response -> IO ResponseReceived) -> (RubyResponse -> IO ResponseReceived) -> Handler ResponseReceived
serveGemArtifact replies method file request respond refuse =
    withNativeRequest
        withMetadataAdmission
        request
        (liftIO . refuse)
        (const (Left (packumentUnavailable rubyReplies [] (mkRefusal Nothing "the coordinate lookup exceeded its deadline"))))
        ( \ticket deps -> do
            observe <- liftIO (newNativeReadBudget (maxMetadataBytes (pdLimits deps)))
            resolveCoordinate ticket observe request file
        )
        ( \case
            Left failure -> liftIO (refuse failure)
            Right (row, _) ->
                let coordinate = grCoordinate row
                 in (if method == "HEAD" then headTarball else serveTarball) replies (coordinateName coordinate) (coordinateVersion coordinate) file request respond
        )

-- | Validate original compact bytes after current coordinate admission, including retained mirror source data.
serveCompactSpec :: Filename -> Request -> (RubyResponse -> IO ResponseReceived) -> Handler ResponseReceived
serveCompactSpec file request respond =
    withNativeRequest
        withListingAdmission
        request
        (liftIO . respond)
        (\deps -> packumentUnavailable rubyReplies [] (mkRefusal (pdHelp deps) "the compact specification request exceeded its deadline"))
        ( \ticket deps -> do
            observe <- liftIO (newNativeReadBudget (maxMetadataBytes (pdLimits deps)))
            resolveCoordinate ticket observe request file >>= either pure (fetchCompact ticket observe request)
        )
        (liftIO . respond)

data CandidateRead a
    = CandidateAbsent
    | CandidateDenied RubyResponse
    | CandidateRefused RubyResponse
    | CandidateRejected CoordinateRejection
    | CandidateAdmitted a

data CoordinateRejection = CoordinateRejection PackageName Text (Maybe DbEtag) Rejection

resolveCoordinate :: MemoryTicket -> (Int -> IO ()) -> Request -> Filename -> Handler (Either RubyResponse (GemRow, Provenance))
resolveCoordinate ticket observe request file = do
    deps <- asks (bindingPackumentDeps . ctxMount)
    case candidateCoordinates (maxMetadataBytes (pdLimits deps)) file of
        Left _ -> pure (Left (rubyBadRequest "the native coordinate is invalid or ambiguous"))
        Right candidates -> do
            checkDocument <- liftIO (newNativeDocumentBudget (maxMetadataBytes (pdLimits deps)) (CandidateRefused (rubyBadRequest "the coordinate lookup exceeds its cumulative metadata bound")))
            collect deps observe checkDocument candidates 0 [] Nothing (Set.toAscList (Set.fromList (map coordinateName candidates)))
  where
    collect deps observer checkDocument candidates spent matches denied = \case
        [] -> case matches of
            [selected] -> pure (Right selected)
            [] -> case denied of
                Nothing -> pure (Left (packumentNotFound rubyReplies [] (mkRefusal Nothing "the native coordinate is absent")))
                Just (Left refusal) -> pure (Left refusal)
                Just (Right rejection) -> Left <$> coordinateRefusal deps rejection
            _ -> pure (Left (rubyBadRequest "the native coordinate names more than one admitted artifact"))
        name : names -> do
            rejected <- liftIO (newIORef Nothing)
            let replies =
                    (mapPackumentReplies CandidateRefused rubyReplies)
                        { packumentNotFound = \_ _ -> CandidateAbsent
                        , packumentForbidden = \headers refusal -> CandidateDenied (packumentForbidden rubyReplies headers refusal)
                        }
                recordRejected ctx details rejection =
                    when (any (\candidate -> coordinateName candidate == pkgName details && coordinateVersion candidate == pkgVersion details) candidates && any ((== unFilename file) . artFilename) (pkgArtifacts details)) $
                        atomicModifyIORef' rejected $ \previous ->
                            let current = CoordinateRejection (pkgName details) (renderVersion (pkgVersion details)) (ctxAdvisoryEtag ctx) rejection
                             in (if terminalRejection rejection then Just current else previous <|> Just current, ())
                rejectionObserver =
                    DocumentRejectionObserver
                        { observeVersionRejection = recordRejected
                        , onCheckedNoSurvivorAdmission = maybe CandidateAbsent CandidateRejected <$> readIORef rejected
                        }
                continueRejected current@(CoordinateRejection _ _ _ rejection) next
                    | terminalRejection rejection = Left <$> coordinateRefusal deps current
                    | otherwise = next (denied <|> Just (Right current)) names
            result <- withObservedAdmittedDocument replies ticket observer checkDocument (Just rejectionObserver) name request CandidateAbsent $ \cached sourceBytes origins -> do
                liftIO (charge ticket (scaleCharge (cfOutputPermille (metadataChargeFactors (pdMetadata deps))) sourceBytes))
                pure (CandidateAdmitted (cached, sourceBytes, origins))
            case result of
                CandidateRefused refusal -> pure (Left refusal)
                CandidateAbsent -> collect deps observer checkDocument candidates spent matches denied names
                CandidateDenied refusal -> collect deps observer checkDocument candidates spent matches (denied <|> Just (Left refusal)) names
                CandidateRejected current -> continueRejected current (collect deps observer checkDocument candidates spent matches)
                CandidateAdmitted (cached, sourceBytes, origins)
                    | spent + toInteger sourceBytes > toInteger (maxMetadataBytes (pdLimits deps)) -> pure (Left (rubyBadRequest "the coordinate lookup exceeds its cumulative metadata bound"))
                    | otherwise -> case snd rubyGemsCached cached of
                        Nothing -> pure (Left (packumentInternal rubyReplies [] (mkRefusal Nothing "the admitted coordinate source is not RubyGems metadata")))
                        Just document -> do
                            let found = [(row, origin) | row <- gemRows document, grCoordinate row `elem` candidates, coordinateFilename (grCoordinate row) == file, Just origin <- [Map.lookup (renderVersion (coordinateVersion (grCoordinate row))) origins]]
                                next = collect deps observer checkDocument candidates (spent + toInteger sourceBytes) (matches <> found)
                            refusal <- if null found then liftIO (readIORef rejected) else pure Nothing
                            case refusal of
                                Just current -> continueRejected current next
                                Nothing -> next denied names

terminalRejection :: Rejection -> Bool
terminalRejection rejection = case rejectionReason rejection of
    Unavailable{} -> True
    UpstreamInvalid -> True
    ByPolicy{} -> False
    MissingIntegrity -> False
    BelowIntegrityFloor -> False

coordinateRefusal :: PackumentDeps -> CoordinateRejection -> Handler RubyResponse
coordinateRefusal deps (CoordinateRejection name version etag rejection) = do
    rt <- asks ctxRuntime
    let decision = Reject rejection
        status = packumentStatus [decision]
    liftIO $ do
        recordDenials (srMetrics rt) [decision]
        mpServeDecision (srMetrics rt) (statusServeDecision status)
    logDenials name etag [VersionVerdict version decision]
    pure (noSurvivors rubyReplies (pdHelp deps) status [decision])

fetchCompact :: MemoryTicket -> (Int -> IO ()) -> Request -> (GemRow, Provenance) -> Handler RubyResponse
fetchCompact ticket observe request (row, provenance) = do
    mount <- asks ctxMount
    rt <- asks ctxRuntime
    let deps = bindingPackumentDeps mount
        coordinate = grCoordinate row
        limits = pdLimits deps
        token = forwardedCredential mount request
        unavailableAfter retry = packumentUnavailable rubyReplies (retryAfterHeaders retry) (mkRefusal (pdHelp deps) "the compact specification source is unavailable")
        unavailable = unavailableAfter Nothing
        invalid = packumentBadGateway rubyReplies [] (mkRefusal (pdHelp deps) "the compact specification disagrees with its admitted coordinate")
        -- A failed private read refuses by cause. A public one stays unavailable.
        privateFailure = \case
            ReadUnavailable retry -> unavailableAfter retry
            ReadInvalid -> packumentBadGateway rubyReplies [] (mkRefusal (pdHelp deps) "the private compact specification source returned an invalid or unusable reply")
    retained <- case (provenance, pdMirrorState deps) of
        (TrustedSource, Just mirror) ->
            liftIO (validateMirrorProject mirror ticket (coordinateName coordinate)) >>= \case
                Left _ -> pure (Just (Left unavailable))
                Right Nothing -> pure Nothing
                Right (Just (_, record)) -> case recordVersion (coordinateVersion coordinate) record of
                    Just version
                        | Map.member (unFilename (coordinateFilename coordinate)) (versionFiles version) ->
                            Just . first (const unavailable) <$> liftIO (readMirrorCompanion mirror ticket record (coordinateVersion coordinate) (unFilename (coordinateFilename coordinate)))
                    _ -> pure Nothing
        _ -> pure Nothing
    bytes <- case retained of
        Just result -> pure result
        Nothing -> case sourceLocation deps provenance of
            Nothing -> pure (Left unavailable)
            Just base -> do
                let private = provenance == TrustedSource
                    client = if private then srPrivateClient rt else srPublicClient rt
                    credential = if private then token else Nothing
                    control = if private then Uncontrolled else Controlled (srPublicControl rt) Companion
                pay <- liftIO (newChargeRegion (charge ticket))
                liftIO
                    ( try
                        ( formThen
                            FetchUrlUnformable
                            (withSuccessBodyUsing control client (progressFloor limits) (boundedChargedRead pay (MetadataBodyLimit (maxMetadataBytes limits)) . chargedRead observe))
                            (gemCompactSpecUrl (registryUrlText base) coordinate >>= artifactRequestByUrl rubyReadCredential credential)
                        )
                    )
                    <&> \case
                        Left (ObservedReadBoundExceeded fault) -> Left (packumentBadGateway rubyReplies [] (mkRefusal Nothing ("the cumulative compact input exceeded its bound: " <> show fault)))
                        Right (Right (SuccessBody _ (_, body))) -> Right body
                        Right (Right (UnreadStatus code _)) | private && (code == 401 || code == 403) -> Left (packumentPrivateRefused rubyReplies code [] (mkRefusal Nothing "the private compact specification source refused access"))
                        Right (Right (UnreadStatus 404 _)) -> Left (packumentNotFound rubyReplies [] (mkRefusal Nothing "the compact specification is absent"))
                        Right (Right (UnreadStatus code retry)) | private -> Left (privateFailure (statusReadFailure code retry))
                        Right (Left fault) | private -> Left (privateFailure (fetchReadFailure fault))
                        _ -> Left unavailable
    case bytes of
        Left refusal -> pure refusal
        Right body -> do
            pay <- liftIO (newChargeRegion (charge ticket))
            checked <- liftIO (validateCompactSpec limits pay coordinate row body)
            case checked of
                Left _ -> pure invalid
                Right compact -> answerMetadataBytes rubyReplies deps ticket request (compactSpecBytes compact)

sourceLocation :: PackumentDeps -> Provenance -> Maybe RegistryUrl
sourceLocation deps TrustedSource = pdPrivateBaseUrl deps
sourceLocation deps GatedSource = Just (pdPublicBaseUrl deps)
