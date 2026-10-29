-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Live registry protocol checks keep transport absence separate from invalid responses.
module Ecluse.Test.Contract.RegistrySmoke (liveMetadata, liveRegistryResponse) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Fault (TransportCause (TransportTimeout, TransportUnreachable), TransportFault (tfCause))
import Ecluse.Core.Package (PackageInfo (infoName, infoVersions), renderPackageName)
import Ecluse.Core.Registry (FetchFault (FetchTransport), RegistryResponse (responseStatusCode))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (MetadataFetch))
import Ecluse.Core.Security (Limits (maxVersionCount), defaultLimits)
import Ecluse.Test.Support (expectRight)

-- | Check a live full read, then return its facts for native protocol assertions.
liveMetadata :: Text -> IO (Either MetadataError Manifest) -> IO PackageInfo
liveMetadata expected fetch = do
    fetched <- fetch
    case fetched of
        Left (MetadataFetch (FetchTransport fault)) -> pendingUnavailable "registry" fault
        _ -> pure ()
    manifest <- expectRight fetched
    let info = manifestInfo manifest
    renderPackageName (infoName info) `shouldBe` expected
    Map.size (infoVersions info) `shouldSatisfy` (> 0)
    Map.size (infoVersions info) `shouldSatisfy` (<= maxVersionCount defaultLimits)
    pure info

-- | Return one successful bounded response. Only a timed-out or unreachable peer makes it pending.
liveRegistryResponse :: Text -> IO (Either FetchFault RegistryResponse) -> IO RegistryResponse
liveRegistryResponse label fetch = do
    fetched <- fetch
    case fetched of
        Left (FetchTransport fault) -> pendingUnavailable label fault
        _ -> pure ()
    response <- expectRight fetched
    responseStatusCode response `shouldBe` 200
    pure response

pendingUnavailable :: Text -> TransportFault -> IO ()
pendingUnavailable label fault =
    when (tfCause fault `elem` [TransportTimeout, TransportUnreachable]) $
        pendingWith (toString label <> " unreachable: " <> show fault)
