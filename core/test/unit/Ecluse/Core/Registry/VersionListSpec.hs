-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Inventory limits count all source entries before projection and deduplication.
module Ecluse.Core.Registry.VersionListSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry (ParseError, RegistryResponse (RegistryResponse))
import Ecluse.Core.Registry.Npm.Project (versionListParser)
import Ecluse.Core.Registry.VersionList
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (TooManyVersions), Limits (maxVersionCount), defaultLimits)
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Registry.Npm.Project (parseVersionList)

-- | Reject unusable inventory shapes without changing established empty-list semantics.
spec :: Spec
spec = describe "collectVersionList" $ do
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

    forM_ ["[]", "[{}]", "\"invalid\"", "0", "true", "false"] $ \invalid -> do
        it ("refuses an invalid inventory container: " <> show invalid) $
            inventory ("{\"versions\":" <> invalid <> "}") `shouldSatisfy` isLeft
        it ("keeps an invalid first inventory even before a valid duplicate: " <> show invalid) $
            inventory (duplicate invalid populated) `shouldSatisfy` isLeft
        it ("keeps a valid first inventory before an invalid duplicate: " <> show invalid) $
            inventory (duplicate populated invalid) `shouldBe` Right ["1.0.0"]
        it ("keeps a null first inventory before an invalid duplicate: " <> show invalid) $
            inventory (duplicate "null" invalid) `shouldBe` Right []

    forM_ ["{}", "{\"versions\":null}", "{\"versions\":{}}"] $ \body ->
        it ("preserves established empty inventory semantics: " <> show body) $
            inventory body `shouldBe` Right []

    it "ignores unrelated invalid policy maps during an inventory read" $
        inventory ("{\"time\":false,\"dist-tags\":[],\"versions\":" <> populated <> "}") `shouldBe` Right ["1.0.0"]
  where
    inventory :: ByteString -> Either ParseError [Text]
    inventory body = fmap (map renderVersion) (parseVersionList (RegistryResponse 200 (BS.length body) body))
    duplicate :: ByteString -> ByteString -> ByteString
    duplicate firstValue secondValue = "{\"versions\":" <> firstValue <> ",\"versions\":" <> secondValue <> "}"
    populated :: ByteString
    populated = "{\"1.0.0\":{\"name\":\"thing\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://registry.npmjs.org/thing/-/thing-1.0.0.tgz\"}}}"
