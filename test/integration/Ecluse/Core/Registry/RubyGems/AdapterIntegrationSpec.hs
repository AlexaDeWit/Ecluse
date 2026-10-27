-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native dependency coordinates and actual gated archive responses satisfy the shared serving laws.
module Ecluse.Core.Registry.RubyGems.AdapterIntegrationSpec (spec) where

import Data.Aeson (FromJSON (parseJSON), Value, withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (Method, RequestHeaders, methodGet, methodHead, status200)
import Network.Wai.Test (SResponse (simpleStatus))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Registry.Adapter (RegistryAdapter, adapterFor)
import Ecluse.Server.Pipeline.ServingDriver (NativeObservation (..), nativeByteDriver, responseBytes, responseEvidence)
import Ecluse.Server.Pipeline.TestSupport (publishedDaysAgo)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Serving
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.RubyGems.Reference (runRubyReference)

spec :: Spec
spec = do
    let driver = rubyServingDriver
    it (caseLabel "integration/rubygems/source-layout" "retains the authorised public/private aggregate view") $
        servingLayout driver `shouldBe` CombinedRegistry
    servingContract "rubygems" driver

requiredAdapter :: IO RegistryAdapter
requiredAdapter = maybe (fail "RubyGems has no production adapter registration") pure (adapterFor RubyGems)

rubyServingDriver :: ServingDriver
rubyServingDriver = nativeByteDriver RubyGems requiredAdapter "/rubygems" "/api/v1/dependencies?gems=thing" "/info/thing" artifact document observation hexSha256Of
  where
    artifact version = encodeUtf8 ("/gems/thing-" <> version <> ".gem")
    document _ releases = "---\n" <> BS.intercalate "\n" (map releaseRow releases) <> "\n"
    releaseRow release = encodeUtf8 (releaseVersion release <> " |created_at:" <> publishedDaysAgo (releaseAgeDays release) <> if releaseHasIntegrity release then ",checksum:" <> hexSha256Of (releaseBytes release) else "")
    observation = NativeObservation metadata (\_ response -> pure (responseEvidence response [] Map.empty))

metadata :: (Method -> RequestHeaders -> Text -> IO SResponse) -> Method -> SResponse -> IO ServingResponse
metadata artifact method response
    | simpleStatus response /= status200 || method == methodHead = pure (responseEvidence response [] Map.empty)
    | otherwise = do
        decoded <- runRubyReference "dependencies" (responseBytes method response)
        values <- either fail pure (parseEither parseJSON decoded :: Either String [Value])
        coordinates <- traverse (either fail pure . parseEither (withObject "native dependency" (\row -> (,,) <$> row .: "name" <*> row .: "number" <*> row .: "platform"))) values
        for_ coordinates $ \(name, _, platform) -> do
            name `shouldBe` ("thing" :: Text)
            platform `shouldBe` ("ruby" :: Text)
        digests <- forM coordinates $ \(_, version, _) -> do
            actual <- artifact methodGet [] version
            let checksums = [hexSha256Of (responseBytes methodGet actual) | simpleStatus actual == status200]
            pure (version, checksums)
        pure (responseEvidence response (sort [version | (_, version, _) <- coordinates]) (Map.fromList digests))
