-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bind complete mirror upload data to bounded registry reads and writes.
Every request disables redirects, and credentials are minted for each exchange.
-}
module Ecluse.Core.Registry.Publish.Internal (
    PublishPlan (..),
    PreparedMirrorVersion (..),
    PreparedMirrorUpload (..),
    PublishCodec (..),
    fetchVersionList,
    MirrorTransport (..),
    MirrorPublish (..),
    newMirrorPublish,
) where

import Data.JsonStream.Lexer (Cursor)
import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (BodyOutcome, FetchFault, ParseError, PublishFault (PublishFetch), UrlFormationError)
import Ecluse.Core.Registry.Exchange (accumulatingCharge, boundedExchange, withSuccessBody)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Json.Walk (Step, readJsonWalk)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError, VersionDoc)
import Ecluse.Core.Registry.Metadata.Fetch (fetchManifest)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead)
import Ecluse.Core.Registry.MirrorUpload (ExpectedMirrorFile, MirrorInventory, MirrorUploadData, VerifiedArtifactBytes)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead), originClient)
import Ecluse.Core.Registry.Request (sealRequest)
import Ecluse.Core.Registry.VersionList (VersionListState, finishVersionList)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (progressFloor), maxMetadataBytes)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
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

-- | The codec owns source and upload metadata while the worker owns whole-version orchestration.
data PublishCodec = forall source metadata. PublishCodec
    { pcPrepareSource :: PackageName -> Version -> VersionDoc -> Either PublishFault (MirrorInventory, source)
    , pcPrepareUpload :: Limits -> (Int -> IO ()) -> MirrorInventory -> source -> ExpectedMirrorFile -> VerifiedArtifactBytes -> IO (Either PublishFault (MirrorUploadData metadata))
    , pcNativeComplete :: PackageName -> Version -> PackageInfo -> Bool
    -- ^ Atomic native inventories may prove completion without retained external source state.
    , pcTargetRead :: EcosystemRead
    , pcTargetReadPermille :: Int
    -- ^ The adapter's existing full-read heap charge per thousand source bytes.
    , pcProbeRequest :: Text -> Maybe ClientCredential -> PackageName -> Either UrlFormationError Request
    , pcVersionListWalk :: Maybe (Limits -> Cursor -> Step VersionListState)
    -- ^ The protocol maintenance caller uses this cheaper inventory where the adapter supplies one.
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
    { mpProbeInventory :: (Int -> IO ()) -> PackageName -> IO (Either MetadataError PackageInfo)
    , mpNativeComplete :: PackageName -> Version -> PackageInfo -> Bool
    , mpPrepareVersion :: PackageName -> Version -> VersionDoc -> Either PublishFault PreparedMirrorVersion
    }

-- | The read and upload endpoints are explicit because native upload routes need not share the read root.
newMirrorPublish :: MirrorTransport -> RegistryUrl -> RegistryUrl -> PublishCodec -> MirrorPublish
newMirrorPublish transport readTarget uploadTarget PublishCodec{pcPrepareSource, pcPrepareUpload, pcNativeComplete, pcTargetRead, pcTargetReadPermille, pcPublishRequest, pcPublishOutcome} =
    MirrorPublish
        { mpProbeInventory = probeInventory transport readTarget pcTargetRead pcTargetReadPermille
        , mpNativeComplete = pcNativeComplete
        , mpPrepareVersion = \name version doc -> do
            (inventory, source) <- pcPrepareSource name version doc
            pure
                PreparedMirrorVersion
                    { pmvInventoryValue = inventory
                    , pmvPrepareUploadAction = \charge file bytes ->
                        fmap (PreparedMirrorUpload . publishUpload transport uploadTarget pcPublishRequest pcPublishOutcome)
                            <$> pcPrepareUpload (ptLimits transport) charge inventory source file bytes
                    }
        }

probeInventory :: MirrorTransport -> RegistryUrl -> EcosystemRead -> Int -> (Int -> IO ()) -> PackageName -> IO (Either MetadataError PackageInfo)
probeInventory transport target targetRead factor charge name = do
    pay <- accumulatingCharge charge
    credential <- ptMintCredential transport
    -- The initial and final target snapshots can overlap during completion.
    let origin = (originClient (ptLimits transport) (ptClient transport) target credential){ocChargeFullRead = pay . scaleCharge (2 * factor)}
    fmap manifestInfo <$> fetchManifest targetRead (ptTracing transport) origin name

-- | Read bounded version identifiers for the protocol maintenance caller.
fetchVersionList :: ExchangeClient -> Limits -> (Cursor -> Step VersionListState) -> Request -> IO (Either FetchFault (BodyOutcome (Either ParseError [Version])))
fetchVersionList client limits walk request =
    fmap (fmap versions) <$> withSuccessBody client (progressFloor limits) (readJsonWalk (MetadataBodyLimit (maxMetadataBytes limits)) walk) request
  where
    versions streamed = streamValue streamed >>= finishVersionList

publishUpload :: MirrorTransport -> RegistryUrl -> (Text -> Maybe ClientCredential -> PublishPlan -> MirrorUploadData metadata -> Either PublishFault Request) -> (Int -> ByteString -> Either PublishFault ()) -> MirrorUploadData metadata -> PublishPlan -> IO (Either PublishFault ())
publishUpload transport target request outcome upload plan = do
    credential <- ptMintCredential transport
    either
        (pure . Left)
        (writeUpload transport outcome . sealRequest)
        (request (registryUrlText target) credential plan upload)

writeUpload :: MirrorTransport -> (Int -> ByteString -> Either PublishFault ()) -> Request -> IO (Either PublishFault ())
writeUpload transport outcome request =
    boundedExchange (\status _ body -> (status, body)) (ptClient transport) (progressFloor (ptLimits transport)) (MetadataBodyLimit (maxMetadataBytes (ptLimits transport))) request
        <&> \case
            Left fault -> Left (PublishFetch fault)
            Right (status, body) -> outcome status body
