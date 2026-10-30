-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared store observations and maintenance operations for protocol providers.
module Ecluse.Test.Contract.PackageStore.Provider (StoreBoundary (..), withStoreSession, storeOperations, fetchStoreArtifact, fetchStoredArtifact, takeStoreFailure) where

import Network.HTTP.Client (Request, defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, Status, status503)

import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (RegistryResponse (..), UrlFormationError)
import Ecluse.Core.Registry.Exchange (singleAttemptSettings)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance (..), VersionOutcome (VersionRemoved), collectPages, storedVersion)
import Ecluse.Core.Registry.Maintenance.NameSpace (wholeNameSpace)
import Ecluse.Core.Registry.Origin (OriginClient (..))
import Ecluse.Core.Security (BodyLimit, Limits (progressFloor))
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Contract.PackageStore
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Maintenance (testDeleteGuard)
import Ecluse.Test.Registry.Metadata.Fetch (fetchTestBody)
import Ecluse.Test.Stub (Captured, stubBaseUrl, withRoutedStubIO)

-- | Control planes observe only state accepted from independently decoded wire requests.
data StoreBoundary = StoreBoundary
    { sbReleases :: IO [PackageStoreRelease]
    , sbRemove :: [Text] -> IO ()
    , sbTakeDeleteFailure :: IO Bool
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
