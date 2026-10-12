-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The order of the PyPI fixture's scenarios.
module Ecluse.BenchLoad.PyPISpec (spec) where

import Data.Aeson ((.=))
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.Fixture (fetchChecked, loadCorpusBodies)
import Ecluse.BenchLoad.Harness (Scenario (scenarioConcurrencyScale, scenarioName, scenarioServiceTime, scenarioUpstreamLatencyMicros), UpstreamFixture (fixtureScenarios))
import Ecluse.BenchLoad.NpmArtifact (SelectedArtifact (..))
import Ecluse.BenchLoad.PyPI (indexStubWithArtifacts, pypiFixture, selectWarmArtifacts, wheelStubWith)
import Ecluse.Test.Corpus (pypiCorpusPackages)
import Ecluse.Test.Wai (localhost, rebaseAuthority)

spec :: Spec
spec = do
    describe "PyPI advisory variants" $
        it "run right after their no-database counterparts" $ do
            names `shouldSatisfy` isInfixOf ["index-cold", "index-cold-advisories", "index-cold-all-advisory-rules"]
            names `shouldSatisfy` isInfixOf ["revalidate-not-modified", "revalidate-not-modified-advisories"]
    describe "PyPI private copies" $
        it "run after the cold listing and its advisory variants, from the smallest share to the complete capture" $
            names `shouldSatisfy` isInfixOf ["index-cold", "index-cold-advisories", "index-cold-all-advisory-rules", "heavy-private-5pct", "heavy-private-25pct", "heavy-private"]
    describe "PyPI stress measurements" $ do
        it "adds four stress cases before replay patterns with the existing stress settings" $ do
            names `shouldSatisfy` isInfixOf ["wheel-onboarding", "wheel-ceiling", "herd", "warm-under-cold", "ramp", "pattern-hot-set-control"]
            let observed = [(scenarioName item, scenarioConcurrencyScale item, scenarioServiceTime item, scenarioUpstreamLatencyMicros item) | item <- fixtureScenarios pypiFixture, scenarioName item `elem` ["wheel-ceiling", "herd", "warm-under-cold", "ramp"]]
            observed `shouldBe` [("wheel-ceiling", 4, True, Just 2000), ("herd", 1, False, Nothing), ("warm-under-cold", 1, False, Nothing), ("ramp", 1, False, Nothing)]
        it "resolves real captured PyPI pins to native selected artifact routes" $ do
            bodies <- loadCorpusBodies pypiCorpusPackages
            selected <- selectWarmArtifacts pypiCorpusPackages bodies
            Map.keys selected `shouldMatchList` ["boto3", "numpy", "requests"]
            for_ (Map.toList selected) $ \(name, artifact) -> do
                saProxyPath artifact `shouldSatisfy` (("simple/" <> name <> "/") `T.isPrefixOf`)
                saUpstreamUrl artifact `shouldSatisfy` ("https://files.pythonhosted.org/" `T.isPrefixOf`)
    describe "PyPI paired-load stubs" $ do
        it "serves only selected captured artifacts and preserves the index bytes" $ do
            body <- readFileLBS "bench/corpus/pypi/requests.simple.json"
            rewritten <- newIORef Map.empty
            let artifacts = Map.singleton "/packages/selected.whl" "synthetic relay bytes"
            testWithApplication (pure (indexStubWithArtifacts rewritten 0 (Map.singleton "requests" body) artifacts)) $ \port -> do
                artifact <- fetchChecked status200 [] (localhost port <> "/packages/selected.whl")
                HTTP.responseBody artifact `shouldBe` "synthetic relay bytes"
                _ <- fetchChecked status404 [] (localhost port <> "/packages/absent.whl")
                index <- fetchChecked status200 [] (localhost port <> "/simple/requests")
                HTTP.responseBody index `shouldBe` rebaseAuthority "https://files.pythonhosted.org" (localhost port) body
        it "changes only the requested cold private listing on every request" $ do
            served <- newIORef (0 :: Int)
            let nonce name
                    | name == "boto3" = (\n -> ["description" .= ("nonce " <> show n :: Text)]) <$> atomicModifyIORef' served (\c -> (c + 1, c))
                    | otherwise = pure []
                fetchBody port path = HTTP.responseBody <$> fetchChecked status200 [] (localhost port <> path)
            testWithApplication (pure (wheelStubWith nonce 0 "trusted")) $ \port -> do
                cold <- replicateM 2 (fetchBody port "/simple/boto3")
                warm <- replicateM 2 (fetchBody port "/simple/requests")
                ordNub cold `shouldSatisfy` ((== 2) . length)
                ordNub warm `shouldSatisfy` ((== 1) . length)
                fetchBody port "/simple/requests/requests-9999.0.2-py3-none-any.whl" `shouldReturn` "trusted"
  where
    names = map scenarioName (fixtureScenarios pypiFixture)
