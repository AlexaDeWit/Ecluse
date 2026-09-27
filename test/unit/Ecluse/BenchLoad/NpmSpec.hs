-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The npm stubs' artifact paths, and the pattern knobs refused before any proxy boots.
module Ecluse.BenchLoad.NpmSpec (spec) where

import Data.Aeson ((.=))
import Data.Map.Strict qualified as Map
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Fixture (fetchChecked)
import Ecluse.BenchLoad.Harness (LoadKnobs (..), Scenario (..), Target, UpstreamFixture (fixtureScenarios), defaultLoadKnobs)
import Ecluse.BenchLoad.Npm (corpusPublicStub, npmFixture, privateOverlayStub, privateOverlayStubWith)
import Ecluse.Test.Env (withEnvVars)
import Ecluse.Test.Wai (localhost, rebaseAuthority)

spec :: Spec
spec = describe "npm artifact fixture paths" $ do
    it "misses captured public tarballs privately while retaining the trusted hot path" $
        testWithApplication (pure (privateOverlayStub 0 "trusted")) $ \port -> do
            publicMiss <- fetchChecked status404 [] (localhost port <> "/request/-/request-2.88.2.tgz")
            HTTP.responseBody publicMiss `shouldBe` ""
            trusted <- fetchChecked status200 [] (localhost port <> "/request/-/request-9999.0.2.tgz")
            HTTP.responseBody trusted `shouldBe` "trusted"
    it "serves only known selected artifact paths and preserves the metadata capture" $ do
        body <- readFileLBS "bench/corpus/npm/request.full.json"
        rewritten <- newIORef Map.empty
        let artifacts = Map.singleton "/request/-/request-2.88.2.tgz" "synthetic relay bytes"
        testWithApplication (pure (corpusPublicStub rewritten 0 (Map.singleton "request" body) artifacts)) $ \port -> do
            artifact <- fetchChecked status200 [] (localhost port <> "/request/-/request-2.88.2.tgz")
            HTTP.responseBody artifact `shouldBe` "synthetic relay bytes"
            _ <- fetchChecked status404 [] (localhost port <> "/request/-/request-0.0.0.tgz")
            metadata <- fetchChecked status200 [] (localhost port <> "/request")
            HTTP.responseBody metadata `shouldBe` rebaseAuthority "https://registry.npmjs.org" (localhost port) body

    it "draws extra fields on every private request, so a nonce defeats assembled reuse" $ do
        served <- newIORef (0 :: Int)
        let nonce name
                | name == "webpack" = (\n -> ["description" .= ("nonce " <> show n :: Text)]) <$> atomicModifyIORef' served (\c -> (c + 1, c))
                | otherwise = pure []
            fetchBody port name = HTTP.responseBody <$> fetchChecked status200 [] (localhost port <> "/" <> name)
        testWithApplication (pure (privateOverlayStubWith nonce 0 "trusted")) $ \port -> do
            changing <- traverse (const (fetchBody port "webpack")) [1 :: Int, 2]
            stable <- traverse (const (fetchBody port "lodash")) [1 :: Int, 2]
            ordNub changing `shouldSatisfy` ((== 2) . length)
            ordNub stable `shouldSatisfy` ((== 1) . length)

    it "rejects a full-retention budget before starting a replay" $
        bootSelectedPattern (154 * 1024 * 1024) (const (expectationFailure "a nonzero full budget reached the replay"))
            `shouldThrow` (\(BenchLoadError message) -> message == "BENCH_PATTERN_FULL_BYTES must be zero: the local backend never retains full metadata")

bootSelectedPattern :: Int -> (Target -> IO ()) -> IO ()
bootSelectedPattern fullBytes action =
    withEnvVars (map fst entries) entries $
        case find ((== "pattern-cold-install") . scenarioName) (fixtureScenarios npmFixture) of
            Nothing -> expectationFailure "missing cold-install scenario"
            Just scenario -> scenarioBoot scenario defaultLoadKnobs{lkUpstreamLatencyMicros = 0, lkPayloadBytes = 32} action
  where
    entries =
        [ ("BENCH_PATTERN_NAMES", "1")
        , ("BENCH_PATTERN_SELECTED_VERSION", "pinned")
        , ("BENCH_PATTERN_FULL_BYTES", show fullBytes)
        , ("BENCH_PATTERN_NOW", "2026-09-21T00:00:00Z")
        ]
