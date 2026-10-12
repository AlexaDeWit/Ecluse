-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared store observations and maintenance operations for protocol providers.
module Ecluse.Test.Contract.PackageStore.Provider (StoreBoundary (..), storeOperations, fetchStoreArtifact, takeStoreFailure) where

import Network.HTTP.Client (Request)

import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (RegistryResponse (..), UrlFormationError)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance (..), VersionOutcome (VersionRemoved), collectPages, storedVersion)
import Ecluse.Core.Registry.Maintenance.NameSpace (wholeNameSpace)
import Ecluse.Core.Registry.Origin (OriginClient (..))
import Ecluse.Core.Security (BodyLimit, Limits (progressFloor))
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Contract.PackageStore
import Ecluse.Test.Maintenance (testDeleteGuard)
import Ecluse.Test.Registry.Metadata.Fetch (fetchTestBody)

-- | Control planes observe only state accepted from independently decoded wire requests.
data StoreBoundary = StoreBoundary
    { sbReleases :: IO [PackageStoreRelease]
    , sbRemove :: [Text] -> IO ()
    , sbTakeDeleteFailure :: IO Bool
    }

-- | Native read and write providers share the production maintenance interface.
storeOperations :: PackageName -> (Text -> Version) -> (Text -> IO (Either Text (Maybe PackageStoreRelease))) -> (PackageStoreRelease -> IO (Either Text ())) -> StoreMaintenance -> PackageStoreOperations
storeOperations package version readRelease writeRelease store =
    PackageStoreOperations
        { psoRead = readRelease
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
fetchStoreArtifact limit origin = \case
    Left fault -> pure (Left (show fault))
    Right request -> do
        response <- fetchTestBody (ocManager origin) (progressFloor (ocLimits origin)) limit request
        pure $ do
            received <- first show response
            unless (responseStatusCode received == 200) (Left "stored artifact did not answer HTTP 200")
            pure (responseBody received)

-- | A fault affects one transport attempt, including attempts from reopened handles.
takeStoreFailure :: IORef Bool -> IO Bool
takeStoreFailure flag = atomicModifyIORef' flag (False,)
