-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the configuration a measured proxy boots from.
module Ecluse.BenchLoad.ProxyProcessSpec (spec) where

import Data.List (lookup)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec

import Ecluse.BenchLoad.Pod (PodShape (Limited, Unlimited))
import Ecluse.BenchLoad.ProxyProcess (ProxySettings (..), proxyEnvironment, proxySettings)
import Ecluse.Composition.Support (expectPlanFor, noCeiling)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline), MirrorRole (ServeAndMirror))
import Ecluse.Config (loadConfig, renderConfigError)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))

spec :: Spec
spec = describe "proxyEnvironment" $ do
    let settings = (proxySettings Npm 0){psCacheMaxEntries = Just 3, psServeMaxInFlight = Just 12}
        base =
            [ ("PATH", "/bin")
            , ("GHCRTS", "-N3")
            , ("ECLUSE_CACHE__TTL", "99")
            , ("OTEL_METRICS_EXPORTER", "otlp")
            , ("__ECLUSE_RUNTIME_RTS_APPLIED", "1")
            ]
        environmentFor shape = proxyEnvironment settings shape 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) base
        podEnvironment = environmentFor (Limited 2 (512 * 1024 * 1024))
    it "keeps the harness environment without its RTS flags or its own configuration" $ do
        lookup "PATH" podEnvironment `shouldBe` Just "/bin"
        lookup "GHCRTS" podEnvironment `shouldBe` Nothing
        lookup "__ECLUSE_RUNTIME_RTS_APPLIED" podEnvironment `shouldBe` Nothing
        lookup "ECLUSE_CACHE__TTL" podEnvironment `shouldBe` Just "0"
        lookup "OTEL_METRICS_EXPORTER" podEnvironment `shouldBe` Just "prometheus"
        length (filter ((== "ECLUSE_CACHE__TTL") . fst) podEnvironment) `shouldBe` 1
    it "names both stub upstreams over https under the scenario's mount" $ do
        lookup "ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM__REGISTRY__URL" podEnvironment `shouldBe` Just "https://localhost:9001"
        lookup "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL" podEnvironment `shouldBe` Just "https://localhost:9002"
        lookup "ECLUSE_MOUNTS__PYPI__PUBLIC_UPSTREAM__REGISTRY__URL" (proxyEnvironment (proxySettings PyPI 0) Unlimited 3 "/tmp/proxy" (1, 2, 3) 9001 Nothing [])
            `shouldBe` Just "https://localhost:9001"
    it "leaves the cores to the cgroup under a pod shape, and pins them when unlimited" $ do
        lookup "ECLUSE_RUNTIME__CORES" podEnvironment `shouldBe` Nothing
        lookup "ECLUSE_RUNTIME__CORES" (environmentFor Unlimited) `shouldBe` Just "3"
    it "names only keys the proxy's configuration loads, and a boot plan the serve role accepts" $
        for_ [Npm, PyPI] $ \ecosystem -> do
            let everyPin =
                    (proxySettings ecosystem 60)
                        { psCacheMaxEntries = Just 64
                        , psCacheMaxBytes = Just (64 * 1024 * 1024)
                        , psMaxResponseBytes = Just (256 * 1024 * 1024)
                        , psServeMaxInFlight = Just 12
                        , psPublicConnections = Just 32
                        , psPrivateConnections = Just 64
                        , psClock = Just (UTCTime (fromGregorian 2026 9 22) 0)
                        }
                environment = proxyEnvironment everyPin (Limited 2 (512 * 1024 * 1024)) 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
            case loadConfig environment Nothing of
                Left errs -> expectationFailure (toString (unlines (map renderConfigError errs)))
                Right config -> void (expectPlanFor (BootMirrorPipeline ServeAndMirror) environment Nothing config noCeiling)
    it "pins only the bounds the scenario sets" $ do
        lookup "ECLUSE_CACHE__MAX_ENTRIES" podEnvironment `shouldBe` Just "3"
        lookup "ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT" podEnvironment `shouldBe` Just "12"
        lookup "ECLUSE_CACHE__MAX_BYTES" podEnvironment `shouldBe` Nothing
        lookup "ECLUSE_LIMITS__MAX_RESPONSE_BYTES" podEnvironment `shouldBe` Nothing
        lookup "BENCH_PROXY_CONTROL_PORT" podEnvironment `shouldBe` Just "8081"
        lookup "OTEL_EXPORTER_PROMETHEUS_PORT" podEnvironment `shouldBe` Just "8082"
