-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native dependency coordinates and actual gated archive responses satisfy the shared serving laws.
module Ecluse.Core.Registry.RubyGems.AdapterIntegrationSpec (spec) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (Method, RequestHeaders, methodGet, methodHead, status200)
import Network.Wai.Test (SResponse (simpleStatus))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Server.Pipeline.ServingDriver (NativeObservation (..), nativeByteDriver, responseBytes, responseEvidence)
import Ecluse.Server.Pipeline.TestSupport (publishedDaysAgo)
import Ecluse.Test.Contract.Serving
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)
import Ecluse.Test.Server.Mount (requiredAdapter)

spec :: Spec
spec = servingContract "rubygems" rubyServingDriver

rubyServingDriver :: ServingDriver
rubyServingDriver = nativeByteDriver RubyGems (requiredAdapter RubyGems) "/rubygems" "/info/thing" "/info/thing" artifact document observation hexSha256Of
  where
    artifact version = encodeUtf8 ("/gems/thing-" <> version <> ".gem")
    document _ releases = "---\n" <> BS.intercalate "\n" (map releaseRow releases) <> "\n"
    releaseRow release = encodeUtf8 (releaseVersion release <> " |created_at:" <> publishedDaysAgo (releaseAgeDays release) <> if releaseHasIntegrity release then ",checksum:" <> hexSha256Of (releaseBytes release) else "")
    observation = NativeObservation metadata (\_ response -> pure (responseEvidence response [] Map.empty))

metadata :: (Method -> RequestHeaders -> Text -> IO SResponse) -> Method -> SResponse -> IO ServingResponse
metadata artifact method response
    | simpleStatus response /= status200 || method == methodHead = pure (responseEvidence response [] Map.empty)
    | otherwise = do
        rows <- either (fail . toString) pure (referenceInfo (responseBytes method response))
        for_ rows $ \row -> rowPlatform row `shouldBe` "ruby"
        digests <- forM rows $ \row -> do
            actual <- artifact methodGet [] (rowVersion row)
            let checksums = [checksum | ("checksum", checksum) <- rowFields row]
            when (simpleStatus actual == status200) $
                checksums `shouldBe` [hexSha256Of (responseBytes methodGet actual)]
            pure (rowVersion row, checksums)
        pure (responseEvidence response (sort (map rowVersion rows)) (Map.fromList digests))
