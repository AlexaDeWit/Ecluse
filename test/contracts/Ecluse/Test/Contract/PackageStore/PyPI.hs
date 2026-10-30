-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI persistence through production reads and checked multipart publication.
module Ecluse.Test.Contract.PackageStore.PyPI (pypiStoreDriver, readPyPIStoreManifestOver) where

import Data.Aeson (Value (Object, String), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (Header, Status, status200, status400, status401, status404, status503)
import Network.Wai.Parse (FileInfo (fileContent, fileName))

import Ecluse.Core.Credential (ClientCredential (..), mkSecret)
import Ecluse.Core.Package (Artifact (..), HashAlg (SHA256), PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName, hashAlg, hashValue)
import Ecluse.Core.Registry.CachedDocument (pypiSimpleCached)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance, StoreManifestRead, storeFaultOfMetadata)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataAbsent), VersionDoc (..), VersionRead (vrVersion))
import Ecluse.Core.Registry.Metadata.Fetch (fetchManifest, fetchVersion)
import Ecluse.Core.Registry.MirrorUpload (expectedArtifact, inventoryFiles, verifyMirrorBytes)
import Ecluse.Core.Registry.Origin (OriginClient (..), ocBaseUrl, originClient)
import Ecluse.Core.Registry.Publish (MirrorPublish (..), MirrorTransport (..), PublishPlan (..), newMirrorPublish, pmvInventory, pmvPrepareUpload, publishPrepared)
import Ecluse.Core.Registry.PyPI.Document (simpleFiles)
import Ecluse.Core.Registry.PyPI.Metadata (pypiMirrorRead, pypiRead)
import Ecluse.Core.Registry.PyPI.Publish (pypiPublishCodec)
import Ecluse.Core.Registry.PyPI.Request (artifactRequestByUrl)
import Ecluse.Core.Registry.Request (nativePresentation)
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Version (Version, canonicalPep440, renderVersion)
import Ecluse.Test.Contract.PackageStore
import Ecluse.Test.Contract.PackageStore.Provider
import Ecluse.Test.Contract.PackageStore.PyPI.Fixtures
import Ecluse.Test.Package (hexSha256Of, pypiVersion, unscopedPyPI)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Registry.PyPI.Multipart (decodeUpload)
import Ecluse.Test.Stub (Captured (..), headerValue)

-- | Each version contains two wheels and one sdist with independently pinned metadata and digests.
pypiStoreDriver :: Text -> (OriginClient -> StoreBoundary -> IO StoreMaintenance) -> FullPackageStoreDriver
pypiStoreDriver subject maintenance =
    FullPackageStoreDriver
        { psdSubject = subject
        , psdPackage = packageText
        , psdPrimary = primaryRelease
        , psdSibling = siblingRelease
        , psdWithSession = withPyPISession maintenance
        }

withPyPISession :: (OriginClient -> StoreBoundary -> IO StoreMaintenance) -> (PackageStoreSession -> IO ()) -> IO ()
withPyPISession maintenance use = do
    held <- newIORef Map.empty
    order <- newIORef []
    failWrite <- newIORef False
    failDelete <- newIORef False
    let observed = do
            releases <- storedReleases <$> readIORef held
            versions <- readIORef order
            pure [release | version <- versions, release <- releases, psrVersion release == version]
        boundary =
            StoreBoundary
                { sbReleases = storedReleases <$> readIORef held
                , sbRemove = \versions -> modifyIORef' held (Map.filter (\file -> nativeVersion file `notElem` versions))
                , sbTakeDeleteFailure = takeStoreFailure failDelete
                }
    withStoreSession ((== "/simple/contract-package/") . capPath) (reply held order failWrite) (build boundary) observed failWrite failDelete use
  where
    build boundary base exchanges = do
        let origin = originClient defaultLimits exchanges (loopbackRegistryUrl base) (Just credential)
            publisher = newMirrorPublish (MirrorTransport exchanges (pure (Just credential)) defaultLimits passthroughTracingPort) (ocBaseUrl origin) (loopbackRegistryUrl (base <> "/legacy/")) (pypiPublishCodec nativePresentation)
            readStoredFile _ filename = fetchStoredArtifact (MirrorArtifactBodyLimit 32768) origin (artifactRequestByUrl (ocToken origin) (base <> decodeUtf8 (filePath filename)))
        store <- maintenance origin boundary
        pure (storeOperations packageName pypiVersion (readRelease origin) readStoredFile (writeRelease publisher) store)

-- | Maintenance uses the production PyPI manifest reader independently of upload preparation.
readPyPIStoreManifestOver :: OriginClient -> StoreManifestRead
readPyPIStoreManifestOver origin name = first storeFaultOfMetadata <$> fetchManifest pypiRead passthroughTracingPort origin name

readRelease :: OriginClient -> Text -> IO (Either Text (Maybe PackageStoreRelease))
readRelease origin version = case canonicalReleaseVersion version of
    Left fault -> pure (Left fault)
    Right selected ->
        fetchVersion pypiMirrorRead passthroughTracingPort origin packageName selected >>= \case
            Left MetadataAbsent -> pure (Right Nothing)
            Left fault -> pure (Left (show fault))
            Right found -> case vrVersion found of
                Nothing -> pure (Right Nothing)
                Just document -> do
                    artifacts <- traverse (readArtifact origin) (pkgArtifacts (vdDetails document))
                    pure $ do
                        files <- sequence artifacts
                        raw <- maybeToRight "production read lost PyPI metadata" (vdRaw document >>= snd pypiSimpleCached)
                        metadata <- Map.fromList <$> traverse (metadataOf . snd) (simpleFiles raw)
                        pure (Just (PackageStoreRelease version metadata (NE.sortWith psaFilename files)))
  where
    metadataOf (Object fields) = case (KeyMap.lookup "filename" fields, KeyMap.lookup "requires-python" fields) of
        (Just (String filename), Just (String requirement)) -> Right (filename, requirement)
        _ -> Left "production read lost a consumer metadata field"
    metadataOf _ = Left "production read returned a non-object file"

readArtifact :: OriginClient -> Artifact -> IO (Either Text PackageStoreArtifact)
readArtifact origin artifact = do
    response <- fetchStoreArtifact (MirrorArtifactBodyLimit 32768) origin (artifactRequestByUrl (ocToken origin) (artUrl artifact))
    pure $ do
        bytes <- response
        digest <- maybeToRight "stored artifact has no SHA-256 digest" (hashValue <$> find ((== SHA256) . hashAlg) (artHashes artifact))
        unless (hexSha256Of bytes == digest) (Left "stored artifact bytes disagree with its digest")
        pure (PackageStoreArtifact (artFilename artifact) bytes digest)

writeRelease :: MirrorPublish -> PackageStoreRelease -> IO (Either Text ())
writeRelease publisher release = case prepare of
    Left fault -> pure (Left fault)
    Right (version, prepared) -> fmap (void . sequence) $ forM (inventoryFiles (pmvInventory prepared)) $ \file ->
        case find ((== artFilename (expectedArtifact file)) . psaFilename) (psrArtifacts release) of
            Nothing -> pure (Left "prepared upload names an absent source file")
            Just artifact -> case first show (verifyMirrorBytes file (psaBytes artifact)) of
                Left fault -> pure (Left fault)
                Right bytes ->
                    pmvPrepareUpload prepared (const (pure ())) file bytes >>= \case
                        Left fault -> pure (Left (show fault))
                        Right upload -> first show <$> publishPrepared upload (PublishPlan version)
  where
    prepare = do
        version <- canonicalReleaseVersion (psrVersion release)
        (info, raw) <- first show (projectPyPIIndex defaultLimits packageName (LBS.toStrict (encode (sourceIndex release))))
        details <- maybeToRight "source fixture has no version" (Map.lookup (renderVersion version) (infoVersions info))
        prepared <- first show (mpPrepareVersion publisher packageName version (VersionDoc details (Just (fst pypiSimpleCached raw))))
        pure (version, prepared)

canonicalReleaseVersion :: Text -> Either Text Version
canonicalReleaseVersion = maybeToRight "fixture version is not PEP 440" . canonicalPep440

sourceIndex :: PackageStoreRelease -> Value
sourceIndex release =
    simpleIndex
        [ fileEntry "https://source.example.test" artifact requirement
        | artifact <- toList (psrArtifacts release)
        , let requirement = Map.findWithDefault "" (psaFilename artifact) (psrMetadata release)
        ]

data NativeFile = NativeFile
    { nativeVersion :: Text
    , nativeArtifact :: PackageStoreArtifact
    , nativeRequirement :: Text
    }

storedReleases :: Map Text NativeFile -> [PackageStoreRelease]
storedReleases files =
    [ PackageStoreRelease
        version
        (Map.fromList [(psaFilename (nativeArtifact file), nativeRequirement file) | file <- toList entries])
        (NE.sortWith psaFilename (nativeArtifact <$> entries))
    | (version, entries) <- Map.toList (Map.fromListWith (<>) [(nativeVersion file, file :| []) | file <- Map.elems files])
    ]

reply :: IORef (Map Text NativeFile) -> IORef [Text] -> IORef Bool -> Captured -> IO (Status, [Header], LByteString)
reply held order failWrite captured
    | headerValue "Authorization" captured /= Just "Basic YXdzOmZpeHR1cmUtdG9rZW4=" = pure (status401, [], "credential required")
    | otherwise = case (capMethod captured, capPath captured) of
        ("POST", "/legacy/") -> do
            blocked <- takeStoreFailure failWrite
            if blocked
                then pure (status503, [], "injected write failure")
                else do
                    decoded <- decodeUpload (headerValue "Content-Type" captured) (capBody captured)
                    case decoded >>= uncurry decodeNativeFile of
                        Left reason -> pure (status400, [], encode reason)
                        Right file -> do
                            files <- readIORef held
                            let filename = psaFilename (nativeArtifact file)
                            if Map.member filename files
                                then pure (status400, [], "File already exists.")
                                else do
                                    modifyIORef' held (Map.insert filename file)
                                    modifyIORef' order (ordNub . (<> [nativeVersion file]))
                                    pure (status200, [], "stored")
        ("GET", "/simple/contract-package/") -> do
            files <- Map.elems <$> readIORef held
            let base = "http://" <> decodeUtf8 (fromMaybe "missing-host" (headerValue "Host" captured))
            pure $
                if null files
                    then (status404, [], "missing")
                    else
                        (status200, [("Content-Type", "application/vnd.pypi.simple.v1+json")], encode (simpleIndex [fileEntry base (nativeArtifact file) (nativeRequirement file) | file <- sortOn (psaFilename . nativeArtifact) files]))
        ("GET", path) -> do
            files <- Map.elems <$> readIORef held
            pure $ case find ((== path) . artifactPath . nativeArtifact) files of
                Nothing -> (status404, [], "missing")
                Just file -> (status200, [], LBS.fromStrict (psaBytes (nativeArtifact file)))
        _ -> pure (status404, [], "missing")

decodeNativeFile :: [(ByteString, ByteString)] -> FileInfo LByteString -> Either Text NativeFile
decodeNativeFile fields file = do
    action <- field ":action"
    protocol <- field "protocol_version"
    name <- field "name"
    version <- field "version"
    digest <- field "sha256_digest"
    requirement <- field "requires_python"
    filetype <- field "filetype"
    python <- field "pyversion"
    unless (action == "file_upload" && protocol == "1" && name == packageText) (Left "invalid native upload identity")
    unless (digest == hexSha256Of bytes) (Left "submitted digest differs from submitted bytes")
    unless (filetype == if wheel then "bdist_wheel" else "sdist") (Left "wrong native file type")
    unless (python == if wheel then fromMaybe "" (listToMaybe (drop 2 (T.splitOn "-" filename))) else "source") (Left "wrong native Python tag")
    pure (NativeFile version (PackageStoreArtifact filename bytes digest) requirement)
  where
    filename = decodeUtf8 (fileName file)
    bytes = LBS.toStrict (fileContent file)
    wheel = ".whl" `T.isSuffixOf` filename
    field :: ByteString -> Either Text Text
    field key = case [decodeUtf8 value | (name, value) <- fields, name == key] of
        [value] -> Right value
        _ -> Left ("missing or repeated native field: " <> decodeUtf8 key)

simpleIndex :: [Value] -> Value
simpleIndex files = object ["name" .= packageText, "meta" .= object ["api-version" .= ("1.1" :: Text)], "files" .= files]

fileEntry :: Text -> PackageStoreArtifact -> Text -> Value
fileEntry base artifact requirement =
    object
        [ "filename" .= psaFilename artifact
        , "url" .= (base <> decodeUtf8 (artifactPath artifact))
        , "hashes" .= object ["sha256" .= psaDigest artifact]
        , "requires-python" .= requirement
        ]

artifactPath :: PackageStoreArtifact -> ByteString
artifactPath = filePath . psaFilename

filePath :: Text -> ByteString
filePath filename = "/simple/contract-package/" <> encodeUtf8 filename

credential :: ClientCredential
credential = ClientCredential (Just "aws") (mkSecret "fixture-token")

packageText :: Text
packageText = "contract-package"

packageName :: PackageName
packageName = unscopedPyPI packageText
