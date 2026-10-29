-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo publication binds complete native metadata to unchanged verified archive bytes.
module Ecluse.Core.Registry.Cargo.Publish (
    cargoPublish,
    cargoPublishCodec,
    uploadClaims,
    preparePublishTarget,
    classifyPublish,
) where

import Data.Aeson (Value (..), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS, RequestBodyLBS))
import Network.HTTP.Types.Header (hAccept, hContentType)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Deadline (Deadlines, inspectionDeadline)
import Ecluse.Core.Package (PackageName, hashValue)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), ParseError (..), PublishBodyFault (PublishMalformed), PublishError (PublishError), PublishFault (..), UrlFormationError (UnparseableUrl), isSuccessStatus)
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (..), MirrorRequirements (..), MirrorStateRequirement (DurableVersionState), PublishExtras (NoPublishExtras), PublishRequestBody (..), UploadEndpointRequirement (ExplicitUploadEndpoint), UploadIdentityRequirement (TokenIdentity), WorkerMetadataRead (NativeChargedWorkerRead))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Archive (cargoPublicationAgreement, cargoPublicationMetadata, cargoUploadAgreement, inspectCargoArchive)
import Ecluse.Core.Registry.Cargo.Credential (cargoCredential)
import Ecluse.Core.Registry.Cargo.Discovery (cargoDiscovery, discoveryApi)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Manifest (CargoPublishMetadata, publishMetadataValue)
import Ecluse.Core.Registry.Cargo.Metadata (prepareCargoRead)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Publish.Body qualified as Body
import Ecluse.Core.Registry.Cargo.Request (cargoIndexPath)
import Ecluse.Core.Registry.Cargo.Wire (decodeCargoRecord)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable), VersionDoc (vdRaw))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.MirrorUpload.Internal (checkedMirrorUploadData)
import Ecluse.Core.Registry.Origin (OriginClient (ocLimits, ocToken), originBaseUrl)
import Ecluse.Core.Registry.Publish (PublishCodec (..), relayPublishRequest)
import Ecluse.Core.Registry.Request (apiEndpoint, attachCredential, joinPath, metadataGet, parseRequestEither)
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Security.Limits (Limits (maxMetadataBytes))
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille))
import Ecluse.Core.Server.MemoryModel (saturatingCharge)
import Ecluse.Core.Version (renderVersion)

-- | First-party relay preserves validated native bytes and the registry's response.
cargoPublish :: ChargeFactors -> AdapterPublish
cargoPublish factors =
    AdapterPublish
        { publishCredential = cargoCredential
        , publishRequestExtras = const (Right NoPublishExtras)
        , publishRelay = \origin name body -> relayPublishRequest origin (relayRequest origin name body)
        , publishUploadClaims = uploadClaims
        , publishCodec = cargoPublishCodec factors
        , publishMirrorRequirements = MirrorRequirements (NativeChargedWorkerRead prepareCargoRead) DurableVersionState ExplicitUploadEndpoint
        , publishIdentity = TokenIdentity
        }

-- | A native archive proves physical presence only. Completion also requires the retained crates.io row.
cargoPublishCodec :: ChargeFactors -> PublishCodec
cargoPublishCodec factors =
    PublishCodec
        { pcPrepareSource = \name version doc -> do
            source <- maybeToRight (PublishSourceUnavailable "Cargo snapshot has no paired source index") (vdRaw doc >>= snd cargoCached)
            inventory <- checkedMirrorInventory name version doc (CargoMirrorRead source)
            row <- case cargoRecords source of
                [(_, value)] -> Right value
                _ -> Left (PublishSourceUnavailable "Cargo publication requires one selected source row")
            pure (inventory, row)
        , pcPrepareUpload = prepareUpload
        , pcMirrorCompanion = Nothing
        , pcNativeComplete = \_ _ _ -> False
        , pcTargetRead = prepareCargoRead
        , pcTargetReadPermille = cfFullReadPermille factors
        , pcProbeRequest = \target token name -> metadataGet cargoCredential token "text/plain" <$> (joinPath target (cargoIndexPath name) >>= parseRequestEither)
        , pcVersionListWalk = Nothing
        , pcPrepareTarget = preparePublishTarget
        , pcPublishRequest = \target credential _ upload -> do
            body <- first (PublishSourceUnavailable . parseErrorMessage) (Body.encodeUpload (publishMetadataValue (uploadMetadata upload)) (uploadBytes upload))
            first (PublishFetch . FetchUrlUnformable) (bodyRequest target credential (RequestBodyLBS body))
        , pcPublishOutcome = classifyPublish
        }

prepareUpload :: Deadlines -> Limits -> (Int -> IO ()) -> MirrorInventory -> Value -> ExpectedMirrorFile -> VerifiedArtifactBytes -> IO (Either PublishFault (MirrorUploadData CargoPublishMetadata))
prepareUpload deadlines limits charge inventory row file verified =
    inspectionDeadline deadlines (PublishSourceUnavailable "Cargo publication preparation exceeded its deadline") prepare
  where
    prepare = case sourceName row of
        Left fault -> pure (Left fault)
        Right name -> do
            inspected <- inspectCargoArchive limits charge name (renderVersion (inventoryVersion inventory)) (Just (hashValue (verifiedTargetHash verified))) (verifiedBytes verified)
            case inspected of
                Left fault -> pure (Left (PublishSourceUnavailable (parseErrorMessage fault)))
                Right archive -> do
                    metadata <- cargoPublicationMetadata limits charge archive
                    case metadata of
                        Left fault -> pure (Left (PublishSourceUnavailable (parseErrorMessage fault)))
                        Right extracted -> do
                            agreement <- cargoPublicationAgreement limits charge archive row extracted
                            -- Final decisions must run while the preparation timer remains active.
                            evaluate $ do
                                checked <- first (PublishSourceUnavailable . parseErrorMessage) agreement
                                let metadataBytes = LBS.take (fromIntegral (maxMetadataBytes limits) + 1) (encode (publishMetadataValue checked))
                                when (LBS.length metadataBytes > fromIntegral (maxMetadataBytes limits)) (Left (PublishSourceUnavailable "Cargo publication metadata exceeds its byte bound"))
                                checkedMirrorUploadData inventory file verified checked

sourceName :: Value -> Either PublishFault Text
sourceName = \case
    Object fields -> maybeToRight (PublishSourceUnavailable "Cargo source row has no published name") (stringField "name" fields)
    _ -> Left (PublishSourceUnavailable "Cargo source row is not an object")

-- | Bind resolver claims to the verified archive before any publication discovery or upstream write.
uploadClaims :: Limits -> (Int -> IO ()) -> Text -> PublishRequestBody -> IO (Either PublishBodyFault [Text])
uploadClaims limits charge registry body = do
    decoded <- Body.readUpload limits charge (publishBodyBytes body)
    case decoded of
        Left fault -> pure (Left (PublishMalformed fault))
        Right (upload, metadataCost) -> do
            let archive = Body.uploadArchive upload
                withMetadata cost = charge (saturatingCharge (toInteger metadataCost + toInteger cost))
            inspected <- inspectCargoArchive limits withMetadata (Body.uploadName upload) (Body.uploadVersion upload) Nothing archive
            case inspected of
                Left fault -> pure (Left (PublishMalformed fault))
                Right metadata -> do
                    let logicalRegistry = "sparse+" <> T.dropWhileEnd (== '/') registry <> "/"
                    agreement <- cargoUploadAgreement limits withMetadata metadataCost logicalRegistry (Body.uploadMetadata upload) metadata
                    pure $ first PublishMalformed agreement $> [Body.uploadName upload]

-- | Discovery cannot grant an API root. The complete configured base path must agree before a token is attached.
preparePublishTarget :: OriginClient -> RegistryUrl -> IO (Either MetadataError RegistryUrl)
preparePublishTarget origin approved = do
    discovered <- cargoDiscovery origin
    pure $ do
        discovery <- discovered
        advertised <- maybeToRight MetadataUndecodable (discoveryApi discovery)
        unless (T.dropWhileEnd (== '/') advertised == T.dropWhileEnd (== '/') (registryUrlText approved)) (Left MetadataUndecodable)
        pure approved

relayRequest :: OriginClient -> PackageName -> PublishRequestBody -> Either UrlFormationError Request
relayRequest origin name body = do
    upload <- first (const (UnparseableUrl "Cargo relay body is not a valid native upload")) (Body.decodeUpload (ocLimits origin) (publishBodyBytes body))
    unless (projectName (Body.uploadName upload) == Right name) (Left (UnparseableUrl "Cargo relay name differs from the authorised package"))
    bodyRequest (originBaseUrl origin) (ocToken origin) (RequestBodyBS (publishBodyBytes body))

bodyRequest :: Text -> Maybe ClientCredential -> RequestBody -> Either UrlFormationError Request
bodyRequest target credential body = do
    base <- apiEndpoint (UnparseableUrl "Cargo API root must not contain a query or fragment") target "api/v1/crates/new"
    pure . attachCredential cargoCredential credential $
        base
            { method = "PUT"
            , requestHeaders = [(hContentType, "application/octet-stream"), (hAccept, "application/json")]
            , requestBody = body
            }

-- | A duplicate reply and a native errors response both require independent target proof.
classifyPublish :: Int -> ByteString -> Either PublishFault ()
classifyPublish status body
    | isSuccessStatus status
    , Right (Object fields) <- decodeCargoRecord 32 body
    , maybe True emptyErrors (KeyMap.lookup "errors" fields) =
        Right ()
    | otherwise = Left (PublishRejected (PublishError ("Cargo publication failed with HTTP status " <> show status)))
  where
    emptyErrors = \case
        Array errors -> null errors
        _ -> False
