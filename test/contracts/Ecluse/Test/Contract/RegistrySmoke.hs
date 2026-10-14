-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Live metadata identity and admission, with transport absence reported separately.
module Ecluse.Test.Contract.RegistrySmoke (liveMetadata) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Fault (TransportCause (TransportTimeout, TransportUnreachable), TransportFault (tfCause))
import Ecluse.Core.Package (PackageInfo (infoName, infoVersions), renderPackageName)
import Ecluse.Core.Registry (FetchFault (FetchTransport))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (MetadataFetch))
import Ecluse.Core.Security (Limits (maxVersionCount), defaultLimits)
import Ecluse.Test.Support (expectRight)

-- | Check a live full read, then return its facts for native protocol assertions.
liveMetadata :: Text -> IO (Either MetadataError Manifest) -> IO PackageInfo
liveMetadata expected fetch = do
    fetched <- fetch
    case fetched of
        Left (MetadataFetch (FetchTransport fault))
            | tfCause fault `elem` [TransportTimeout, TransportUnreachable] -> pendingWith ("registry unreachable: " <> show fault)
        _ -> pure ()
    manifest <- expectRight fetched
    let info = manifestInfo manifest
    renderPackageName (infoName info) `shouldBe` expected
    Map.size (infoVersions info) `shouldSatisfy` (> 0)
    Map.size (infoVersions info) `shouldSatisfy` (<= maxVersionCount defaultLimits)
    pure info
