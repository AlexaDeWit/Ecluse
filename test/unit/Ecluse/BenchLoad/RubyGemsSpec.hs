-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native load fixtures preserve source fields and expose the required work before timing.
module Ecluse.BenchLoad.RubyGemsSpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf, lookup)
import Data.Map.Strict qualified as Map
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentLength, status200, status404)
import Network.HTTP.Types.Header (hTransferEncoding)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (..))
import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Fixture (fetchChecked, loadCorpusBodies)
import Ecluse.BenchLoad.Harness (Scenario (..), UpstreamFixture (fixtureScenarios))
import Ecluse.BenchLoad.RubyGems (RelayShape (KnownLengthRelay, SlowChunkedRelay), infoStubWithArtifacts, relayArtifactFixture, rubyGemsFixture, selectWarmArtifacts)
import Ecluse.Test.Corpus (rubyGemsCorpusPackages)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)
import Ecluse.Test.Support (expectRight)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = do
    describe "native measurement work" $ do
        it "keeps batch, original quick-spec and complete-copy work before replay patterns" $
            map scenarioName (fixtureScenarios rubyGemsFixture) `shouldSatisfy` isInfixOf ["batch-dependencies", "quick-gemspec", "gem-verified-known-length", "gem-verified-slow-readers", "worker-mirroring", "pattern-hot-set-control"]
        it "retains the shared stress settings and unlimited-only worker" $ do
            let selected = [(scenarioName item, scenarioConcurrencyScale item, scenarioServiceTime item, scenarioUpstreamLatencyMicros item) | item <- fixtureScenarios rubyGemsFixture, scenarioName item `elem` ["gem-ceiling", "herd", "herd-under-heavy-private", "warm-under-cold", "ramp"]]
            selected `shouldBe` [("gem-ceiling", 4, True, Just 2000), ("herd", 1, False, Nothing), ("herd-under-heavy-private", 1, False, Nothing), ("warm-under-cold", 1, False, Nothing), ("ramp", 1, False, Nothing)]
            [scenarioInProcess item | item <- fixtureScenarios rubyGemsFixture, scenarioName item == "worker-mirroring"] `shouldBe` [True]
        it "declares verified-read observations and retains slow-delivery work in the loaded pass" $ do
            let observed = [(scenarioName item, scenarioVerifiedArtifactMetrics item, scenarioServiceTime item) | item <- fixtureScenarios rubyGemsFixture, scenarioName item `elem` ["gem-verified-known-length", "gem-verified-slow-readers"]]
            observed `shouldBe` [("gem-verified-known-length", True, True), ("gem-verified-slow-readers", True, False)]
        it "resolves every real native workload pin before selecting artifacts" $ do
            captures <- loadCorpusBodies rubyGemsCorpusPackages
            selected <- selectWarmArtifacts rubyGemsCorpusPackages captures
            Map.keys selected `shouldMatchList` ["rails", "nokogiri", "aws-sdk-ec2"]
            for_ (Map.elems selected) $ \artifact -> saUpstreamUrl artifact `shouldBe` "https://rubygems.org/" <> saProxyPath artifact
        it "fails missing capture data instead of supplying selected placeholders" $
            selectWarmArtifacts rubyGemsCorpusPackages Map.empty `shouldThrow` (\(BenchLoadError _) -> True)
    describe "verified relay source framing" $
        for_ [KnownLengthRelay, SlowChunkedRelay] $ \shape ->
            it (if shape == KnownLengthRelay then "transmits a genuine known Content-Length" else "transmits chunked bytes without a declared length") $ do
                let payload = LBS.replicate (128 * 1024) 97
                testWithApplication (pure (relayArtifactFixture shape 0 payload)) $ \port -> do
                    response <- fetchChecked status200 [] (localhost port <> "/gems/rails-9999.0.2.gem")
                    HTTP.responseBody response `shouldBe` payload
                    if shape == KnownLengthRelay
                        then lookup hContentLength (HTTP.responseHeaders response) `shouldBe` Just "131072"
                        else do
                            lookup hContentLength (HTTP.responseHeaders response) `shouldBe` Nothing
                            lookup hTransferEncoding (HTTP.responseHeaders response) `shouldBe` Just "chunked"
    describe "native relay fixture" $
        it "changes only checksums and serves exact selected bytes under native artifact paths" $ do
            raw <- readFileLBS "bench/corpus/rubygems/nokogiri.info"
            let payload = "independently owned relay bytes"
                artifacts = Map.singleton "/gems/selected.gem" payload
            testWithApplication (pure (infoStubWithArtifacts 0 (Map.singleton "nokogiri" raw) artifacts)) $ \port -> do
                response <- fetchChecked status200 [] (localhost port <> "/info/nokogiri")
                original <- expectRight (referenceInfo (toStrict raw))
                rewritten <- expectRight (referenceInfo (toStrict (HTTP.responseBody response)))
                map withoutChecksum rewritten `shouldBe` map withoutChecksum original
                map (lookup "checksum" . rowFields) rewritten `shouldBe` replicate (length rewritten) (Just (hexSha256Of (toStrict payload)))
                LBS.length (HTTP.responseBody response) `shouldBe` LBS.length raw
                selected <- fetchChecked status200 [] (localhost port <> "/gems/selected.gem")
                HTTP.responseBody selected `shouldBe` payload
                void (fetchChecked status404 [] (localhost port <> "/gems/absent.gem"))
  where
    withoutChecksum row = row{rowFields = filter ((/= "checksum") . fst) (rowFields row)}
