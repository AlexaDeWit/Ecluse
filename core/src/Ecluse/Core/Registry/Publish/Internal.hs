-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bound mirror reads and writes to the selected role credential. Every request disables redirects.
module Ecluse.Core.Registry.Publish.Internal (
    PublishPlan (..),
    PreparedMirrorVersion (..),
    PreparedMirrorUpload (..),
    PublishCodec (..),
    MirrorCompanion (..),
    fetchVersionList,
    relayPublishRequest,
    MirrorTransport (..),
    MirrorPublish (..),
    newMirrorPublish,
) where

import Data.JsonStream.Lexer (Cursor)
import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (BodyOutcome, FetchFault (FetchBoundExceeded, FetchUrlUnformable), ParseError, PublishError (PublishError), PublishFault (PublishFetch, PublishRejected), PublishRelayResponse, UrlFormationError)
import Ecluse.Core.Registry.Exchange (accumulatingCharge, boundedExchange, boundedRelay, formThen, withSuccessBody)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecDeadlines))
import Ecluse.Core.Registry.Json.Walk (Step, readJsonWalk)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (..), VersionDoc)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead, prepareManifest)
import Ecluse.Core.Registry.MirrorUpload (ExpectedMirrorFile, MirrorInventory, MirrorUploadData, VerifiedArtifactBytes)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocChargeFullReadWorkspace, ocClient, ocLimits), originClient)
import Ecluse.Core.Registry.Request (sealRequest)
import Ecluse.Core.Registry.VersionList (VersionListState, finishVersionList)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (progressFloor), maxMetadataBytes)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Meter (newChargeRegion)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Core.Version (Version)

-- | The release tag the target must carry after publication.
newtype PublishPlan = PublishPlan {ppLatest :: Version}
    deriving stock (Eq, Show)

-- | A complete source snapshot with adapter-owned per-file upload preparation.
data PreparedMirrorVersion = PreparedMirrorVersion
    { pmvInventoryValue :: MirrorInventory
    , pmvPrepareUploadAction :: (Int -> IO ()) -> ExpectedMirrorFile -> VerifiedArtifactBytes -> IO (Either PublishFault PreparedMirrorUpload)
    }

-- | Publication captures checked metadata and verified bytes, so a caller cannot replace the body.
newtype PreparedMirrorUpload = PreparedMirrorUpload
    { runPreparedUpload :: PublishPlan -> IO (Either PublishFault ())
    }

-- | Native companions retain source metadata under the artifact's existing authority.
data MirrorCompanion = MirrorCompanion
    { mcLocation :: ExpectedMirrorFile -> Text -> Either PublishFault Text
    -- ^ Derive a companion location from an authorised artifact location.
    , mcValidate :: Deadlines -> Limits -> (Int -> IO ()) -> MirrorInventory -> ExpectedMirrorFile -> ByteString -> IO (Either PublishFault ByteString)
    -- ^ Validate native facts and return the original bounded bytes.
    }

-- | The codec owns source and upload metadata while the worker owns whole-version orchestration.
data PublishCodec = forall source metadata. PublishCodec
    { pcPrepareSource :: PackageName -> Version -> VersionDoc -> Either PublishFault (MirrorInventory, source)
    , pcPrepareUpload :: Deadlines -> Limits -> (Int -> IO ()) -> MirrorInventory -> source -> ExpectedMirrorFile -> VerifiedArtifactBytes -> IO (Either PublishFault (MirrorUploadData metadata))
    , pcMirrorCompanion :: Maybe MirrorCompanion
    , pcNativeComplete :: PackageName -> Version -> PackageInfo -> Bool
    -- ^ Atomic native inventories may prove completion without retained external source state.
    , pcTargetRead :: PrepareRead
    , pcTargetReadPermille :: Int
    -- ^ The adapter's existing full-read heap charge per thousand source bytes.
    , pcProbeRequest :: Text -> Maybe ClientCredential -> PackageName -> Either UrlFormationError Request
    , pcVersionListWalk :: Maybe (Limits -> Cursor -> Step VersionListState)
    -- ^ The protocol maintenance caller uses this cheaper inventory where the adapter supplies one.
    , pcPrepareTarget :: OriginClient -> RegistryUrl -> IO (Either MetadataError RegistryUrl)
    -- ^ Prepare only after the role credential exists. Discovery cannot widen the approved upload root.
    , pcPublishRequest :: Text -> Maybe ClientCredential -> PublishPlan -> MirrorUploadData metadata -> Either PublishFault Request
    , pcPublishOutcome :: Int -> ByteString -> Either PublishFault ()
    }

-- | Process resources for the target's bounded exchanges.
data MirrorTransport = MirrorTransport
    { ptClient :: ExchangeClient
    , ptMintCredential :: IO (Maybe ClientCredential)
    , ptLimits :: Limits
    , ptTracing :: TracingPort
    }

-- | A target-bound version reader and checked upload preparation capability.
data MirrorPublish = MirrorPublish
    { mpMirrorCompanion :: Maybe MirrorCompanion
    , mpProbeInventory :: (Int -> IO ()) -> PackageName -> IO (Either MetadataError PackageInfo)
    , mpNativeComplete :: PackageName -> Version -> PackageInfo -> Bool
    , mpPrepareVersion :: PackageName -> Version -> VersionDoc -> Either PublishFault PreparedMirrorVersion
    }

-- | The read and upload endpoints are explicit because native upload routes need not share the read root.
newMirrorPublish :: MirrorTransport -> RegistryUrl -> RegistryUrl -> PublishCodec -> MirrorPublish
newMirrorPublish transport readTarget uploadTarget PublishCodec{pcPrepareSource, pcPrepareUpload, pcMirrorCompanion, pcNativeComplete, pcTargetRead, pcTargetReadPermille, pcPrepareTarget, pcPublishRequest, pcPublishOutcome} =
    MirrorPublish
        { mpMirrorCompanion = pcMirrorCompanion
        , mpProbeInventory = probeInventory transport readTarget pcTargetRead pcTargetReadPermille
        , mpNativeComplete = pcNativeComplete
        , mpPrepareVersion = \name version doc -> do
            (inventory, source) <- pcPrepareSource name version doc
            pure
                PreparedMirrorVersion
                    { pmvInventoryValue = inventory
                    , pmvPrepareUploadAction = \charge file bytes -> do
                        peak <- newIORef 0
                        let pay amount = modifyIORef' peak (max amount) >> charge amount
                        prepared <- pcPrepareUpload (ecDeadlines (ptClient transport)) (ptLimits transport) pay inventory source file bytes
                        retained <- readIORef peak
                        pure (PreparedMirrorUpload . publishUpload transport readTarget uploadTarget (charge . (+ retained) . scaleCharge pcTargetReadPermille) pcPrepareTarget pcPublishRequest pcPublishOutcome <$> prepared)
                    }
        }

probeInventory :: MirrorTransport -> RegistryUrl -> PrepareRead -> Int -> (Int -> IO ()) -> PackageName -> IO (Either MetadataError PackageInfo)
probeInventory transport target targetRead factor charge name = do
    pay <- accumulatingCharge charge
    workspace <- newChargeRegion pay
    credential <- ptMintCredential transport
    -- The initial and final target snapshots can overlap during completion.
    let origin = (originClient (ptLimits transport) (ptClient transport) target credential){ocChargeFullRead = pay . scaleCharge (2 * factor), ocChargeFullReadWorkspace = workspace}
    fmap manifestInfo <$> prepareManifest targetRead (ptTracing transport) origin name

-- | Preserve the native publication response under the origin's metadata and progress limits.
relayPublishRequest :: OriginClient -> Either UrlFormationError Request -> IO (Either FetchFault PublishRelayResponse)
relayPublishRequest origin =
    formThen
        FetchUrlUnformable
        (boundedRelay (ocClient origin) (progressFloor (ocLimits origin)) (MetadataBodyLimit (maxMetadataBytes (ocLimits origin))))

-- | Read bounded version identifiers for the protocol maintenance caller.
fetchVersionList :: ExchangeClient -> Limits -> (Cursor -> Step VersionListState) -> Request -> IO (Either FetchFault (BodyOutcome (Either ParseError [Version])))
fetchVersionList client limits walk request =
    fmap (fmap versions) <$> withSuccessBody client (progressFloor limits) (readJsonWalk (MetadataBodyLimit (maxMetadataBytes limits)) walk) request
  where
    versions streamed = streamValue streamed >>= finishVersionList

publishUpload :: MirrorTransport -> RegistryUrl -> RegistryUrl -> (Int -> IO ()) -> (OriginClient -> RegistryUrl -> IO (Either MetadataError RegistryUrl)) -> (Text -> Maybe ClientCredential -> PublishPlan -> MirrorUploadData metadata -> Either PublishFault Request) -> (Int -> ByteString -> Either PublishFault ()) -> MirrorUploadData metadata -> PublishPlan -> IO (Either PublishFault ())
publishUpload transport readTarget target charge prepare request outcome upload plan = do
    credential <- ptMintCredential transport
    pay <- accumulatingCharge charge
    let origin = (originClient (ptLimits transport) (ptClient transport) readTarget credential){ocChargeFullRead = pay}
    prepared <- prepare origin target
    case prepared of
        Left fault -> pure (Left (publishTargetFault fault))
        Right approved ->
            either
                (pure . Left)
                (writeUpload transport outcome . sealRequest)
                (request (registryUrlText approved) credential plan upload)

publishTargetFault :: MetadataError -> PublishFault
publishTargetFault = \case
    MetadataFetch fault -> PublishFetch fault
    MetadataBoundExceeded fault -> PublishFetch (FetchBoundExceeded fault)
    MetadataAuthorisationFailure status -> rejected status
    MetadataHttpFailure status -> rejected status
    MetadataAbsent -> rejected 404
    MetadataUndecodable -> PublishRejected (PublishError "the target discovery is malformed or names an unapproved API root")
    MetadataNameMismatch _ -> PublishRejected (PublishError "the target discovery has an inconsistent identity")
  where
    rejected :: Int -> PublishFault
    rejected status = PublishRejected (PublishError ("target discovery failed with HTTP status " <> show status))

writeUpload :: MirrorTransport -> (Int -> ByteString -> Either PublishFault ()) -> Request -> IO (Either PublishFault ())
writeUpload transport outcome request =
    boundedExchange (\status _ body -> (status, body)) (ptClient transport) (progressFloor (ptLimits transport)) (MetadataBodyLimit (maxMetadataBytes (ptLimits transport))) request
        <&> \case
            Left fault -> Left (PublishFetch fault)
            Right (status, body) -> outcome status body
