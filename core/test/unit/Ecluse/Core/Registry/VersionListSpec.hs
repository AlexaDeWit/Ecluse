-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Inventory limits count all source entries before projection and deduplication.
module Ecluse.Core.Registry.VersionListSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.Npm.Project (versionListParser)
import Ecluse.Core.Registry.VersionList
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (TooManyVersions), Limits (maxVersionCount), defaultLimits)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)

spec :: Spec
spec = describe "collectVersionList" $
    it "refuses unusable source entries before filtering them from the inventory" $ do
        let body = "{\"versions\":{\"1\":null,\"1\":null,\"2\":null}}"
            limits = defaultLimits{maxVersionCount = 1}
        void
            ( parseJsonChunks
                (MetadataBodyLimit (BS.length body))
                (versionListParser limits)
                (collectVersionList limits)
                emptyVersionList
                [body]
            )
            `shouldBe` Left (TooManyVersions 2 1)
