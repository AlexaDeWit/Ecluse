-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared store sessions, native reads and publication for protocol providers.
module Ecluse.Test.Contract.PackageStore.Provider (
    StoreBoundary (..),
    heldBoundary,
    withStoreSession,
    storeOperations,
    NativeStoreRead (..),
    readNativeRelease,
    publishNativeRelease,
    prepareStoreManifestOver,
    readStoreArtifact,
    fetchStoreArtifact,
    fetchStoredArtifact,
    takeStoreFailure,
) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Request, defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, Status, status503)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (Artifact (..), HashAlg (SHA256), PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName, hashAlg, hashValue, renderPackageName)
import Ecluse.Core.Registry (RegistryResponse (..), UrlFormationError)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Exchange (singleAttemptSettings)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance (..), StoreManifestRead, VersionOutcome (VersionRemoved), collectPages, storeFaultOfMetadata, storedVersion)
import Ecluse.Core.Registry.Maintenance.NameSpace (wholeNameSpace)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataAbsent), VersionDoc (..), VersionRead (vrVersion))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead, prepareManifest, prepareVersion)
import Ecluse.Core.Registry.MirrorUpload (ExpectedMirrorFile, expectedArtifact, expectedTargetMatchesWith, inventoryFiles, verifyMirrorBytes)
import Ecluse.Core.Registry.Origin (OriginClient (..))
import Ecluse.Core.Registry.Publish (MirrorPublish (..), PreparedMirrorVersion, PublishPlan (..), pmvInventory, pmvPrepareUpload, publishPrepared)
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), Limits (progressFloor))
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Contract.PackageStore
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Maintenance (testDeleteGuard)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Metadata.Fetch (fetchTestBody)
import Ecluse.Test.Stub (Captured, stubBaseUrl, withRoutedStubIO)

-- | Control planes observe only state accepted from independently decoded wire requests.
data StoreBoundary = StoreBoundary
    { sbReleases :: IO [PackageStoreRelease]
    , sbRemove :: [Text] -> IO ()
    , sbTakeDeleteFailure :: IO Bool
    }

-- | A wire boundary holding one native release per version, which only accepted deletions remove.
heldBoundary :: (native -> PackageStoreRelease) -> IORef (Map Text native) -> IORef Bool -> StoreBoundary
heldBoundary release held failDelete =
    StoreBoundary
        { sbReleases = map release . Map.elems <$> readIORef held
        , sbRemove = \versions -> modifyIORef' held (\releases -> foldr Map.delete releases versions)
        , sbTakeDeleteFailure = takeStoreFailure failDelete
        }

{- | Keep the native wire boundary alive across initial and reopened transport handles. The
predicate names the package-document read that an injected read failure answers with a 503.
-}
withStoreSession :: (Captured -> Bool) -> (Captured -> IO (Status, [Header], LByteString)) -> (Text -> ExchangeClient -> IO PackageStoreOperations) -> IO [PackageStoreRelease] -> IORef Bool -> IORef Bool -> (PackageStoreSession -> IO ()) -> IO ()
withStoreSession documentRead respond build observed failWrite failDelete use = do
    failRead <- newIORef False
    let answer captured = do
            blocked <- if documentRead captured then takeStoreFailure failRead else pure False
            if blocked then pure (status503, [], "injected read failure") else respond captured
    withRoutedStubIO answer $ \stub -> do
        let reopen = withExchangeClient (newManager (singleAttemptSettings defaultManagerSettings)) (build (stubBaseUrl stub))
        initial <- reopen
        use
            PackageStoreSession
                { pssOperations = initial
                , pssObservedWrites = observed
                , pssFailNextRead = writeIORef failRead True
                , pssFailNextWrite = writeIORef failWrite True
                , pssFailNextDelete = writeIORef failDelete True
                , pssReopen = reopen
                }

-- | Native read and write providers share the production maintenance interface.
storeOperations :: PackageName -> (Text -> Version) -> (Text -> IO (Either Text (Maybe PackageStoreRelease))) -> (Text -> Text -> IO (Either Text (Maybe ByteString))) -> (PackageStoreRelease -> IO (Either Text ())) -> StoreMaintenance -> PackageStoreOperations
storeOperations package version readRelease readArtifact writeRelease store =
    PackageStoreOperations
        { psoRead = readRelease
        , psoArtifact = readArtifact
        , psoWrite = writeRelease
        , psoInventory = do
            names <- collectPages (listPackagesIn store wholeNameSpace)
            case names of
                Left fault -> pure (Left (show fault))
                Right packages -> fmap sequence $ forM packages $ \name -> do
                    versions <- enumerateVersions store name
                    pure (first show (fmap (\items -> (renderPackageName name, sort (map (renderVersion . storedVersion) items))) versions))
        , psoDelete = \versions ->
            map (bimap renderVersion deletionResult) <$> deleteVersions store testDeleteGuard package (map version versions)
        }
  where
    deletionResult VersionRemoved = Right ()
    deletionResult other = Left (show other)

-- | One ecosystem's production read, artifact request and consumer fields for a fixed package.
data NativeStoreRead = NativeStoreRead
    { nsrPrepare :: PrepareRead
    , nsrArtifact :: Maybe ClientCredential -> Text -> Either UrlFormationError Request
    , nsrPackage :: PackageName
    , nsrVersion :: Text -> Version
    , nsrMetadata :: CachedDoc -> Either Text (Map Text Text)
    }

-- | Read one version through the production read, then fetch each listed artifact at its URL.
readNativeRelease :: NativeStoreRead -> OriginClient -> Text -> IO (Either Text (Maybe PackageStoreRelease))
readNativeRelease native origin rawVersion =
    prepareVersion (nsrPrepare native) passthroughTracingPort origin (nsrPackage native) (nsrVersion native rawVersion) >>= \case
        Left MetadataAbsent -> pure (Right Nothing)
        Left fault -> pure (Left (show fault))
        Right found -> case vrVersion found of
            Nothing -> pure (Right Nothing)
            Just document -> do
                files <- traverse (readStoreArtifact (nsrArtifact native) origin) (pkgArtifacts (vdDetails document))
                pure $ do
                    artifacts <- sequence files
                    raw <- maybeToRight "production read lost the native source" (vdRaw document)
                    metadata <- nsrMetadata native raw
                    pure (Just (PackageStoreRelease rawVersion metadata artifacts))

-- | Prepare the version from the source through production code. Skip it when the target holds every file.
publishNativeRelease :: NativeStoreRead -> OriginClient -> MirrorPublish -> PackageStoreRelease -> IO (Either Text ())
publishNativeRelease native source publisher release = do
    let package = nsrPackage native
        version = nsrVersion native (psrVersion release)
    selected <- prepareVersion (nsrPrepare native) passthroughTracingPort source package version
    case first show selected >>= maybeToRight "source fixture has no native release" . vrVersion >>= first show . mpPrepareVersion publisher package version of
        Left fault -> pure (Left fault)
        Right prepared -> do
            target <- mpProbeInventory publisher (const (pure ())) package
            case target of
                Left MetadataAbsent -> uploadFiles version prepared release
                Left fault -> pure (Left (show fault))
                Right inventory ->
                    if maybe False (matchesInventory (inventoryFiles (pmvInventory prepared))) (Map.lookup (psrVersion release) (infoVersions inventory))
                        then pure (Right ())
                        else uploadFiles version prepared release

matchesInventory :: NonEmpty ExpectedMirrorFile -> PackageDetails -> Bool
matchesInventory expected stored =
    length expected == length (pkgArtifacts stored)
        && all (\file -> any (expectedTargetMatchesWith file Nothing) (pkgArtifacts stored)) expected

uploadFiles :: Version -> PreparedMirrorVersion -> PackageStoreRelease -> IO (Either Text ())
uploadFiles version prepared release = fmap (void . sequence) $ forM (inventoryFiles (pmvInventory prepared)) $ \file ->
    case find ((== artFilename (expectedArtifact file)) . psaFilename) (psrArtifacts release) of
        Nothing -> pure (Left "upload inventory names an absent source artifact")
        Just artifact -> case first show (verifyMirrorBytes file (psaBytes artifact)) of
            Left fault -> pure (Left fault)
            Right bytes ->
                pmvPrepareUpload prepared (const (pure ())) file bytes >>= \case
                    Left fault -> pure (Left (show fault))
                    Right upload -> first show <$> publishPrepared upload (PublishPlan version)

-- | Discovery through a production read under the authenticated store origin supplied by backend composition.
prepareStoreManifestOver :: PrepareRead -> OriginClient -> StoreManifestRead
prepareStoreManifestOver prepared origin name =
    first storeFaultOfMetadata <$> prepareManifest prepared passthroughTracingPort origin name

-- | Require HTTP 200 bytes whose SHA-256 matches the digest the store's metadata declares.
readStoreArtifact :: (Maybe ClientCredential -> Text -> Either UrlFormationError Request) -> OriginClient -> Artifact -> IO (Either Text PackageStoreArtifact)
readStoreArtifact request origin artifact = do
    response <- fetchStoreArtifact (MirrorArtifactBodyLimit 32768) origin (request (ocToken origin) (artUrl artifact))
    pure $ do
        bytes <- response
        digest <- maybeToRight "stored artifact has no SHA-256 digest" (hashValue <$> find ((== SHA256) . hashAlg) (artHashes artifact))
        unless (hexSha256Of bytes == digest) (Left "stored artifact bytes disagree with its digest")
        pure (PackageStoreArtifact (artFilename artifact) bytes digest)

-- | Preserve transport failures and require a complete HTTP 200 artifact body.
fetchStoreArtifact :: BodyLimit -> OriginClient -> Either UrlFormationError Request -> IO (Either Text ByteString)
fetchStoreArtifact limit origin request = (>>= maybeToRight "stored artifact did not answer HTTP 200") <$> fetchStoredArtifact limit origin request

-- | 'fetchStoreArtifact', reading a 404 as an artifact the store no longer holds.
fetchStoredArtifact :: BodyLimit -> OriginClient -> Either UrlFormationError Request -> IO (Either Text (Maybe ByteString))
fetchStoredArtifact limit origin = \case
    Left fault -> pure (Left (show fault))
    Right request -> do
        response <- fetchTestBody (ocClient origin) (progressFloor (ocLimits origin)) limit request
        pure $ do
            received <- first show response
            case responseStatusCode received of
                200 -> Right (Just (responseBody received))
                404 -> Right Nothing
                _ -> Left "stored artifact did not answer HTTP 200 or 404"

-- | A fault affects one transport attempt, including attempts from reopened handles.
takeStoreFailure :: IORef Bool -> IO Bool
takeStoreFailure flag = atomicModifyIORef' flag (False,)
