-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo stress settings, pinned selection and relay stubs retain their measurement contracts.
module Ecluse.BenchLoad.CargoSpec (spec) where

import Data.Aeson (Value (String), (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (..))
import Ecluse.BenchLoad.Cargo (cargoFixture, crateStubWith, indexStubWithArtifacts, selectWarmArtifacts)
import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Fixture (fetchChecked, loadCorpusBodies)
import Ecluse.BenchLoad.Harness (Scenario (scenarioConcurrencyScale, scenarioInProcess, scenarioName, scenarioServiceTime, scenarioUpstreamLatencyMicros), UpstreamFixture (fixtureScenarios))
import Ecluse.Test.Corpus (cargoCorpusPackages, readCorpusPins)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Cargo.Reference (IndexLine (indexValue), referenceDiscovery, referenceIndex)
import Ecluse.Test.Support (expectRight, expectRightText)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = do
    describe "Cargo stress measurements" $ do
        it "keeps five stress cases before the worker and replay patterns with the existing stress settings" $ do
            names `shouldSatisfy` isInfixOf ["crate-onboarding", "crate-ceiling", "herd", "herd-under-heavy-private", "warm-under-cold", "ramp", "worker-mirroring", "pattern-hot-set-control"]
            let observed = [(scenarioName item, scenarioConcurrencyScale item, scenarioServiceTime item, scenarioUpstreamLatencyMicros item) | item <- fixtureScenarios cargoFixture, scenarioName item `elem` ["crate-ceiling", "herd", "herd-under-heavy-private", "warm-under-cold", "ramp"]]
            observed `shouldBe` [("crate-ceiling", 4, True, Just 2000), ("herd", 1, False, Nothing), ("herd-under-heavy-private", 1, False, Nothing), ("warm-under-cold", 1, False, Nothing), ("ramp", 1, False, Nothing)]
            [scenarioInProcess item | item <- fixtureScenarios cargoFixture, scenarioName item == "worker-mirroring"] `shouldBe` [True]
        it "resolves real captured Cargo pins to native archive routes and crates.io downloads" $ do
            bodies <- loadCorpusBodies cargoCorpusPackages
            pins <- readCorpusPins (.: "cargoPins") >>= expectRight
            selected <- selectWarmArtifacts cargoCorpusPackages bodies
            Map.keys selected `shouldMatchList` ["tokio", "windows", "aws-sdk-ec2"]
            for_ (Map.toList selected) $ \(name, artifact) -> do
                version <- expectRightText (maybeToRight "missing Cargo workload pin" (Map.lookup name pins))
                let file = name <> "-" <> version <> ".crate"
                saProxyPath artifact `shouldBe` "crates/" <> name <> "/" <> version <> "/" <> file
                saUpstreamUrl artifact `shouldBe` "https://static.crates.io/crates/" <> name <> "/" <> file
        it "fails missing capture data instead of supplying selected placeholders" $
            selectWarmArtifacts cargoCorpusPackages Map.empty `shouldThrow` (\(BenchLoadError _) -> True)
    describe "Cargo paired-load stubs" $ do
        it "changes only checksums, keeps the wire width and serves exact selected bytes" $ do
            raw <- readFileLBS "bench/corpus/cargo/tokio.index.ndjson"
            let payload = "synthetic relay bytes"
                digest = hexSha256Of (toStrict payload)
            rewritten <- newIORef Map.empty
            testWithApplication (pure (indexStubWithArtifacts rewritten 0 (Map.singleton "tokio" raw) (Map.singleton "/packages/selected.crate" payload))) $ \port -> do
                selected <- fetchChecked status200 [] (localhost port <> "/packages/selected.crate")
                HTTP.responseBody selected `shouldBe` payload
                void (fetchChecked status404 [] (localhost port <> "/packages/absent.crate"))
                response <- fetchChecked status200 [] (localhost port <> "/to/ki/tokio")
                let served = HTTP.responseBody response
                original <- rows raw
                relayed <- rows served
                map (KeyMap.delete "cksum") relayed `shouldBe` map (KeyMap.delete "cksum") original
                map (KeyMap.lookup "cksum") relayed `shouldBe` replicate (length original) (Just (String digest))
                LBS.length served `shouldBe` LBS.length raw
                archive <- fetchChecked status200 [] (localhost port <> "/crates/tokio/tokio-1.53.1.crate")
                HTTP.responseBody archive `shouldBe` payload
                void (fetchChecked status404 [] (localhost port <> "/crates/tokio/tokio-9999.0.2.crate"))
                discovery <- fetchChecked status200 [] (localhost port <> "/config.json")
                (template, _, _, _) <- expectRightText (referenceDiscovery (toStrict (HTTP.responseBody discovery)))
                template `shouldSatisfy` T.isPrefixOf (localhost port <> "/crates/")
        it "changes only the requested cold private listing on every request" $ do
            served <- newIORef (0 :: Int)
            let nonce name
                    | name == "aws-sdk-ec2" = (\n -> ["ecluse-load-revision" .= n]) <$> atomicModifyIORef' served (\c -> (c + 1, c))
                    | otherwise = pure []
                fetchBody port path = HTTP.responseBody <$> fetchChecked status200 [] (localhost port <> path)
            testWithApplication (pure (crateStubWith nonce 0 "trusted")) $ \port -> do
                cold <- replicateM 2 (fetchBody port "/aw/s-/aws-sdk-ec2")
                warm <- replicateM 2 (fetchBody port "/to/ki/tokio")
                ordNub cold `shouldSatisfy` ((== 2) . length)
                ordNub warm `shouldSatisfy` ((== 1) . length)
                records <- traverse rows (take 1 warm)
                map (map (KeyMap.lookup "cksum")) records `shouldBe` [[Just (String (hexSha256Of "trusted"))]]
                fetchBody port "/crates/tokio/tokio-9999.0.2.crate" `shouldReturn` "trusted"
  where
    names = map scenarioName (fixtureScenarios cargoFixture)
    rows body = traverse (expectRightText . indexValue) (referenceIndex (toStrict body))
