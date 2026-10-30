-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | npm production adapters exercised against an independently decoded stateful HTTP boundary.
module Ecluse.Test.Contract.PackageStore.Protocol (
    verdaccioDriver,
    npmStoreDriver,
    readStoreManifestOver,
) where

import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (Header, Status, status200, status201, status400, status401, status404, status409, status503)

import Ecluse.Core.Credential (bareCredential)
import Ecluse.Core.Package (Artifact (..), HashAlg (SRI), PackageDetails (pkgArtifacts), PackageName, hashAlg, hashValue)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMaintenance (maintenanceListing, maintenanceVersionDelete))
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance (..), StoreManifestRead, storeFaultOfMetadata)
import Ecluse.Core.Registry.Maintenance.Protocol (ProtocolRead (..), ProtocolStore (..), newProtocolMaintenance)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataAbsent), VersionDoc (..), VersionRead (vrVersion))
import Ecluse.Core.Registry.Metadata.Fetch (fetchManifest, fetchVersion)
import Ecluse.Core.Registry.Npm.Maintenance (npmMaintenance)
import Ecluse.Core.Registry.Npm.Metadata (npmRead)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Npm.Request (artifactRequestByUrl)
import Ecluse.Core.Registry.Origin (OriginClient (..), ocBaseUrl)
import Ecluse.Core.Registry.Publish (MirrorPublish (..), MirrorTransport (..), PublishPlan (..), newMirrorPublish)
import Ecluse.Core.Registry.Request (nativePresentation)
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Test.Contract.PackageStore
import Ecluse.Test.Contract.PackageStore.Provider
import Ecluse.Test.Package (npmVersion, sriSha512Of, unscopedNpm)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Npm (mirrorWriteToken, writeTokenNpmConfig)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmVersion)
import Ecluse.Test.Registry.Publish (publishOnlyFile)
import Ecluse.Test.Stub (Captured (..), headerValue)

-- | Verdaccio's supported npm profile includes protocol inventory and deletion.
verdaccioDriver :: FullPackageStoreDriver
verdaccioDriver = npmStoreDriver "verdaccio" protocolMaintenance

-- | Reuse npm's shipped data plane while supplying the backend's production maintenance handle.
npmStoreDriver :: Text -> (OriginClient -> StoreBoundary -> IO StoreMaintenance) -> FullPackageStoreDriver
npmStoreDriver subject maintenance =
    FullPackageStoreDriver
        { psdSubject = subject
        , psdPackage = packageText
        , psdPrimary = release "1.0.0" "first artifact bytes\NUL\255" "first.js"
        , psdSibling = release "2.0.0" "independent sibling bytes" "second.js"
        , psdWithSession = withNpmSession maintenance
        }

withNpmSession :: (OriginClient -> StoreBoundary -> IO StoreMaintenance) -> (PackageStoreSession -> IO ()) -> IO ()
withNpmSession maintenance use = do
    held <- newIORef Map.empty
    writes <- newIORef []
    failWrite <- newIORef False
    failDelete <- newIORef False
    let boundary =
            StoreBoundary
                { sbReleases = Map.elems <$> readIORef held
                , sbRemove = \versions -> modifyIORef' held (\releases -> foldr Map.delete releases versions)
                , sbTakeDeleteFailure = takeStoreFailure failDelete
                }
    withStoreSession (reply held writes failWrite failDelete) (build boundary) (reverse <$> readIORef writes) failWrite failDelete use
  where
    build boundary base exchanges = do
        let origin = writeTokenNpmConfig (loopbackRegistryUrl base) exchanges defaultLimits
            publisher = newMirrorPublish (MirrorTransport exchanges (pure (Just (bareCredential mirrorWriteToken))) defaultLimits passthroughTracingPort) (ocBaseUrl origin) (ocBaseUrl origin) (npmPublishCodec nativePresentation)
        store <- maintenance origin boundary
        pure (operations origin publisher store)

operations :: OriginClient -> MirrorPublish -> StoreMaintenance -> PackageStoreOperations
operations origin publisher = storeOperations packageName npmVersion (readRelease origin) (writeRelease publisher)

readRelease :: OriginClient -> Text -> IO (Either Text (Maybe PackageStoreRelease))
readRelease origin version =
    fetchVersion npmRead passthroughTracingPort origin packageName (npmVersion version) >>= \case
        Left MetadataAbsent -> pure (Right Nothing)
        Left fault -> pure (Left (show fault))
        Right found -> case vrVersion found of
            Nothing -> pure (Right Nothing)
            Just document -> do
                artifacts <- traverse (readArtifact origin) (pkgArtifacts (vdDetails document))
                pure $ do
                    files <- sequence artifacts
                    raw <- maybeToRight "the production read lost source metadata" (vdRaw document >>= snd npmCached)
                    metadata <- metadataOf raw
                    pure (Just (PackageStoreRelease version metadata files))

readArtifact :: OriginClient -> Artifact -> IO (Either Text PackageStoreArtifact)
readArtifact origin artifact = do
    response <- fetchStoreArtifact (MirrorArtifactBodyLimit 4096) origin (artifactRequestByUrl (ocToken origin) (artUrl artifact))
    pure $ do
        bytes <- response
        digest <- maybeToRight "stored artifact has no SRI digest" (hashValue <$> find ((== SRI) . hashAlg) (artHashes artifact))
        pure (PackageStoreArtifact (artFilename artifact) bytes digest)

writeRelease :: MirrorPublish -> PackageStoreRelease -> IO (Either Text ())
writeRelease publisher release' = case toList (psrArtifacts release') of
    [artifact] -> case first show (projectNpmVersion defaultLimits packageName version body) >>= maybeToRight "missing fixture release" . vrVersion of
        Left fault -> pure (Left fault)
        Right document -> first show <$> publishOnlyFile publisher document (PublishPlan version) (psaBytes artifact)
      where
        version = npmVersion (psrVersion release')
        body = toStrict (encode (object ["name" .= packageText, "versions" .= object [Key.fromText (psrVersion release') .= versionDocument "" release' artifact]]))
    _ -> pure (Left "the shipped npm writer accepts one tarball per version")

protocolMaintenance :: OriginClient -> StoreBoundary -> IO StoreMaintenance
protocolMaintenance origin _ = do
    listing <- maybe (fail "npm has no maintenance listing") pure (maintenanceListing npmMaintenance)
    deletion <- maybe (fail "npm has no maintenance deletion") pure (maintenanceVersionDelete npmMaintenance)
    pure $
        newProtocolMaintenance
            ProtocolStore
                { psRead =
                    ProtocolRead
                        { prOrigin = origin
                        , prListing = listing
                        , prCodec = npmPublishCodec nativePresentation
                        , prReadManifest = readStoreManifestOver origin
                        , prBackendName = "verdaccio"
                        , prPermitDeletion = True
                        , prConsentDescriptor = "test fixture permits deletion"
                        }
                , psDeleteOrigin = origin
                , psDelete = deletion
                }

-- | Read manifests through the same production metadata path used by backend composition.
readStoreManifestOver :: OriginClient -> StoreManifestRead
readStoreManifestOver origin name = first storeFaultOfMetadata <$> fetchManifest npmRead passthroughTracingPort origin name

reply :: IORef (Map Text PackageStoreRelease) -> IORef [PackageStoreRelease] -> IORef Bool -> IORef Bool -> Captured -> IO (Status, [Header], LByteString)
reply held writes failWrite failDelete captured
    | headerValue "Authorization" captured /= Just "Bearer write-token" = pure (status401, [], "{}")
    | otherwise = case (capMethod captured, capPath captured) of
        ("GET", "/-/all") -> do
            releases <- readIORef held
            pure (status200, [], encode (if Map.null releases then object [] else object [Key.fromText packageText .= object []]))
        ("GET", path) | path == packagePath -> do
            releases <- readIORef held
            pure $ if Map.null releases then (status404, [], "{}") else (status200, [], encode (packument captured releases))
        ("GET", _) -> do
            releases <- Map.elems <$> readIORef held
            pure $ case find ((== capPath captured) . artifactPath) (concatMap (toList . psrArtifacts) releases) of
                Nothing -> (status404, [], "{}")
                Just artifact -> (status200, [], LBS.fromStrict (psaBytes artifact))
        ("PUT", path) | path == packagePath -> do
            blocked <- takeStoreFailure failWrite
            if blocked then pure (status503, [], "{}") else acceptPublish held writes (capBody captured)
        ("PUT", path) | path == packagePath <> "/-rev/fixture" -> do
            blocked <- takeStoreFailure failDelete
            if blocked then pure (status503, [], "{}") else applyEdit held (capBody captured)
        ("DELETE", path) | path == packagePath <> "/-rev/fixture" -> do
            blocked <- takeStoreFailure failDelete
            if blocked then pure (status503, [], "{}") else writeIORef held Map.empty >> pure (status200, [], "{}")
        ("DELETE", _) -> pure (status200, [], "{}")
        _ -> pure (status404, [], "{}")

acceptPublish :: IORef (Map Text PackageStoreRelease) -> IORef [PackageStoreRelease] -> ByteString -> IO (Status, [Header], LByteString)
acceptPublish held writes bytes = case decodePublish bytes of
    Left reason -> pure (status400, [], encode reason)
    Right release' -> do
        existing <- Map.member (psrVersion release') <$> readIORef held
        if existing
            then pure (status409, [], "{}")
            else do
                modifyIORef' held (Map.insert (psrVersion release') release')
                modifyIORef' writes (release' :)
                pure (status201, [], "{}")

applyEdit :: IORef (Map Text PackageStoreRelease) -> ByteString -> IO (Status, [Header], LByteString)
applyEdit held bytes = case decodeStrict bytes >>= fieldObject "versions" of
    Nothing -> pure (status400, [], "{}")
    Just versions -> do
        modifyIORef' held (Map.filterWithKey (\version _ -> KeyMap.member (Key.fromText version) versions))
        pure (status201, [], "{}")

decodePublish :: ByteString -> Either Text PackageStoreRelease
decodePublish bytes = do
    document <- maybeToRight "publish body is not JSON" (decodeStrict bytes)
    name <- fieldText "name" document
    unless (name == packageText) (Left "publish has the wrong package name")
    versions <- maybeToRight "publish has no versions" (fieldObject "versions" document)
    (version, value) <- case KeyMap.toList versions of
        [item] -> Right item
        _ -> Left "npm publish must contain exactly one version"
    declaredVersion <- fieldText "version" value
    unless (declaredVersion == Key.toText version) (Left "publish version identity differs")
    declaredName <- fieldText "name" value
    unless (declaredName == packageText) (Left "version belongs to another package")
    metadata <- metadataOf value
    dist <- maybeToRight "publish has no dist" (fieldValue "dist" value)
    filename <- fieldText "tarball" dist
    digest <- fieldText "integrity" dist
    attachments <- maybeToRight "publish has no attachments" (fieldObject "_attachments" document)
    unless (KeyMap.keys attachments == [Key.fromText filename]) (Left "npm publish carries an unexpected attachment inventory")
    attachment <- maybeToRight "publish attachment is missing" (KeyMap.lookup (Key.fromText filename) attachments)
    encoded <- fieldText "data" attachment
    payload <- first toText (convertFromBase Base64 (TE.encodeUtf8 encoded) :: Either String ByteString)
    unless (fieldValue "length" attachment == Just (Number (fromIntegral (BS.length payload)))) (Left "attachment length differs from its decoded bytes")
    pure (PackageStoreRelease declaredVersion metadata (PackageStoreArtifact filename payload digest :| []))

packument :: Captured -> Map Text PackageStoreRelease -> Value
packument captured releases =
    object
        [ "name" .= packageText
        , "_rev" .= ("fixture" :: Text)
        , "versions" .= object [Key.fromText version .= versionDocument base release' (NE.head (psrArtifacts release')) | (version, release') <- Map.toList releases]
        , "dist-tags" .= object ["latest" .= maybe "" fst (Map.lookupMax releases)]
        , "time" .= object [Key.fromText version .= ("2020-01-01T00:00:00Z" :: Text) | version <- Map.keys releases]
        ]
  where
    base = "http://" <> decodeUtf8 (fromMaybe "missing-host" (headerValue "Host" captured))

versionDocument :: Text -> PackageStoreRelease -> PackageStoreArtifact -> Value
versionDocument base release' artifact =
    object $
        [ "name" .= packageText
        , "version" .= psrVersion release'
        , "dist" .= object ["tarball" .= location, "integrity" .= psaDigest artifact]
        ]
            <> [Key.fromText key .= value | (key, value) <- Map.toList (psrMetadata release')]
  where
    location = if T.null base then psaFilename artifact else base <> decodeUtf8 (artifactPath artifact)

metadataOf :: Value -> Either Text (Map Text Text)
metadataOf document = Map.fromList <$> traverse (\key -> (key,) <$> fieldText key document) ["main", "license"]

fieldValue :: Text -> Value -> Maybe Value
fieldValue name (Object fields) = KeyMap.lookup (Key.fromText name) fields
fieldValue _ _ = Nothing

fieldObject :: Text -> Value -> Maybe (KeyMap.KeyMap Value)
fieldObject name value = case fieldValue name value of
    Just (Object fields) -> Just fields
    _ -> Nothing

fieldText :: Text -> Value -> Either Text Text
fieldText name value = case fieldValue name value of
    Just (String found) -> Right found
    _ -> Left ("missing string field: " <> name)

release :: Text -> ByteString -> Text -> PackageStoreRelease
release version bytes mainFile =
    PackageStoreRelease
        version
        (Map.fromList [("main", mainFile), ("license", "MIT")])
        (PackageStoreArtifact (packageText <> "-" <> version <> ".tgz") bytes (sriSha512Of bytes) :| [])

artifactPath :: PackageStoreArtifact -> ByteString
artifactPath artifact = packagePath <> "/-/" <> encodeUtf8 (psaFilename artifact)

packagePath :: ByteString
packagePath = "/" <> encodeUtf8 packageText

packageText :: Text
packageText = "contract-package"

packageName :: PackageName
packageName = unscopedNpm packageText
