-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo persistence over production data-plane handles and an independent wire boundary.
module Ecluse.Test.Contract.PackageStore.Cargo (cargoStoreDriver, readCargoStoreManifestOver) where

import Data.Aeson (Object, Value (Array, Object, String), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (Header, Status, status200, status400, status401, status404, status503)

import Ecluse.Core.Credential (ClientCredential (..), mkSecret)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Metadata (prepareCargoRead)
import Ecluse.Core.Registry.Cargo.Publish (cargoPublishCodec)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactRequest)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance, StoreManifestRead)
import Ecluse.Core.Registry.Origin (OriginClient (..), ocBaseUrl, originClient)
import Ecluse.Core.Registry.Publish (MirrorTransport (..), newMirrorPublish)
import Ecluse.Core.Registry.Request (nativePresentation)
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Test.Contract.PackageStore
import Ecluse.Test.Contract.PackageStore.Cargo.Fixtures
import Ecluse.Test.Contract.PackageStore.Provider
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Cargo.Fixtures
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestVersion)
import Ecluse.Test.Registry.Cargo.Reference (UploadFrame (..), referenceUpload)
import Ecluse.Test.Stub (Captured (..), headerValue)

-- | Every persistence law uses native discovery, sparse rows, framed uploads and archive downloads.
cargoStoreDriver :: Text -> (OriginClient -> StoreBoundary -> IO StoreMaintenance) -> FullPackageStoreDriver
cargoStoreDriver subject maintenance =
    FullPackageStoreDriver
        { psdSubject = subject
        , psdPackage = packageText
        , psdPrimary = storeRelease primaryNative
        , psdSibling = storeRelease siblingNative
        , psdWithSession = withCargoSession maintenance
        }

withCargoSession :: (OriginClient -> StoreBoundary -> IO StoreMaintenance) -> (PackageStoreSession -> IO ()) -> IO ()
withCargoSession maintenance use = do
    held <- newIORef Map.empty
    writes <- newIORef []
    failWrite <- newIORef False
    failDelete <- newIORef False
    let boundary = heldBoundary storeRelease held failDelete
    withStoreSession ((== "/registry/index/" <> encodeUtf8 (cargoShard packageText)) . capPath) (reply held writes failWrite) (build boundary) (reverse <$> readIORef writes) failWrite failDelete use
  where
    build boundary base exchanges = do
        let origin = originClient defaultLimits exchanges (loopbackRegistryUrl (base <> "/registry/index")) (Just credential)
            source = originClient defaultLimits exchanges (loopbackRegistryUrl (base <> "/source")) Nothing
            publisher = newMirrorPublish (MirrorTransport exchanges (pure (Just credential)) defaultLimits passthroughTracingPort) (ocBaseUrl origin) (loopbackRegistryUrl (base <> "/registry")) (cargoPublishCodec (ChargeFactors 1000 1000 1000) nativePresentation)
            readArchive version _ = fetchStoredArtifact (MirrorArtifactBodyLimit 32768) origin (cargoArtifactRequest (ocToken origin) (base <> "/registry/" <> cargoArchivePathFor packageText version))
        store <- maintenance origin boundary
        pure (storeOperations packageName cargoTestVersion (readNativeRelease cargoRead origin) readArchive (publishNativeRelease cargoRead source publisher) store)

-- | Prepare discovery under the authenticated store origin supplied by backend composition.
readCargoStoreManifestOver :: OriginClient -> StoreManifestRead
readCargoStoreManifestOver = prepareStoreManifestOver prepareCargoRead

cargoRead :: NativeStoreRead
cargoRead = NativeStoreRead prepareCargoRead cargoArtifactRequest packageName cargoTestVersion selectedFields

selectedFields :: CachedDoc -> Either Text (Map Text Text)
selectedFields cached = do
    raw <- maybeToRight "production read lost Cargo source metadata" (snd cargoCached cached)
    fields <- case cargoRecords raw of
        [(_, Object record)] -> Right record
        _ -> Left "production selected read did not retain exactly one native row"
    features <- maybeToRight "native metadata lacks features" (KeyMap.lookup "features" fields)
    rustVersion <- fieldText "rust_version" fields
    pure (Map.fromList [("features", decodeUtf8 (toStrict (encode features))), ("rust_version", rustVersion)])

reply :: IORef (Map Text CargoRelease) -> IORef [PackageStoreRelease] -> IORef Bool -> Captured -> IO (Status, [Header], LByteString)
reply held writes failWrite captured
    | "/source/" `BS.isPrefixOf` capPath captured =
        pure $
            if isNothing (headerValue "Authorization" captured)
                then readReply captured "/source" Nothing False [primaryNative, siblingNative]
                else (status401, [], "public source requires anonymous reads")
    | headerValue "Authorization" captured /= Just "fixture-token" = pure (status401, [], "credential required")
    | otherwise = case (capMethod captured, capPath captured) of
        ("PUT", "/registry/api/v1/crates/new") -> do
            blocked <- takeStoreFailure failWrite
            if blocked then pure (status503, [], "injected write failure") else acceptUpload held writes (capBody captured)
        _ -> readReply captured "/registry" (Just "/registry") True . Map.elems <$> readIORef held

readReply :: Captured -> Text -> Maybe Text -> Bool -> [CargoRelease] -> (Status, [Header], LByteString)
readReply captured root api authenticated releases
    | capMethod captured /= "GET" = (status404, [], "missing")
    | capPath captured == indexRoot <> "/config.json" =
        (status200, [("Content-Type", "application/json")], LBS.fromStrict (cargoViewDiscovery (base <> root) ((base <>) <$> api) authenticated))
    | capPath captured == indexRoot <> "/" <> encodeUtf8 (cargoShard packageText) =
        if null releases then (status404, [], "missing") else (status200, [("Content-Type", "text/plain")], LBS.fromStrict (cargoIndex releases))
    | otherwise = case find (\release -> capPath captured == encodeUtf8 (root <> "/" <> cargoArchivePath (releaseCrate release))) releases of
        Nothing -> (status404, [], "missing")
        Just release -> (status200, [], LBS.fromStrict (releaseArchive release))
  where
    base = "http://" <> decodeUtf8 (fromMaybe "missing-host" (headerValue "Host" captured))
    indexRoot = encodeUtf8 (if root == "/source" then root else root <> "/index")

acceptUpload :: IORef (Map Text CargoRelease) -> IORef [PackageStoreRelease] -> ByteString -> IO (Status, [Header], LByteString)
acceptUpload held writes bytes = case decodeNativeUpload bytes of
    Left reason -> pure (status400, [], encode (object ["errors" .= [object ["detail" .= reason]]]))
    Right native -> do
        let version = crateVersion (releaseCrate native)
        duplicate <- Map.member version <$> readIORef held
        if duplicate
            then pure (status400, [], "{\"errors\":[{\"detail\":\"crate version already exists\"}]}")
            else do
                modifyIORef' held (Map.insert version native)
                modifyIORef' writes (storeRelease native :)
                pure (status200, [("Content-Type", "application/json")], "{\"warnings\":{\"invalid_categories\":[],\"invalid_badges\":[],\"other\":[]}}")

decodeNativeUpload :: ByteString -> Either Text CargoRelease
decodeNativeUpload bytes = do
    UploadFrame fields archive <- referenceUpload bytes
    name <- fieldText "name" fields
    version <- fieldText "vers" fields
    readme <- fieldText "readme" fields
    unless (name == packageText) (Left "native upload belongs to another package")
    unless (KeyMap.lookup "rust_version" fields == Just (String "1.74")) (Left "native upload lost the required Rust version")
    case KeyMap.lookup "deps" fields of
        Just (Array dependencies) | null dependencies -> pure ()
        _ -> Left "native upload changed the dependency inventory"
    features <- case KeyMap.lookup "features" fields of
        Just features@(Object _) -> Right features
        _ -> Left "native upload has no feature declarations"
    let original = minimalCargoRelease name version
    pure
        original
            { releaseCrate = (releaseCrate original){crateFeatures = features}
            , releaseArchive = archive
            , releaseChecksum = hexSha256Of archive
            , releaseReadme = readme
            }

fieldText :: Text -> Object -> Either Text Text
fieldText key fields = case KeyMap.lookup (Key.fromText key) fields of
    Just (String value) -> Right value
    _ -> Left ("native metadata lacks a string field: " <> key)

credential :: ClientCredential
credential = ClientCredential Nothing (mkSecret "fixture-token")
