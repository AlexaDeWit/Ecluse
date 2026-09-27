-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the configuration a measured proxy boots from.
module Ecluse.BenchLoad.ProxyProcessSpec (spec) where

import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Error (mkIOError, permissionErrorType)
import System.Process.Typed (proc)
import Test.Hspec
import UnliftIO (throwIO)
import UnliftIO.Async (cancel, withAsync)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.Pod (PodShape (Limited, Unlimited))
import Ecluse.BenchLoad.ProxyProcess (BootFailure (..), ProxySettings (..), bootDiagnostic, bootDrained, guardDiagnostic, proxyEnvironment, proxySettings, retryingBoot)
import Ecluse.Composition.Support (expectPlanFor, noCeiling)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline), MirrorRole (ServeAndMirror))
import Ecluse.Config (loadConfig, renderConfigError)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Rts (readIfExists)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Wai (freePort)

spec :: Spec
spec = do
    environmentSpec
    bootSpec

bootSpec :: Spec
bootSpec = do
    describe "bootDrained" $ do
        let failedWith expected = \case
                Left failure -> for_ expected $ \(field, text) -> field failure `shouldSatisfy` T.isInfixOf text
                Right _ -> expectationFailure "the boot succeeded"
        it "stops a process that never answers and returns the tails of both streams" $ do
            manager <- newManager defaultManagerSettings
            port <- freePort
            outcome <- bootDrained manager 5 port (proc "/bin/sh" ["-c", "echo boot line; echo boot fault >&2; exec sleep 60"])
            failedWith [(bfReason, "did not become ready"), (bfLog, "boot line"), (bfStderr, "boot fault")] outcome
        it "stops the process when the readiness wait is interrupted" $
            withSystemTempDirectory "ecluse-boot-interrupt" $ \dir -> do
                manager <- newManager defaultManagerSettings
                port <- freePort
                let pidFile = dir </> "pid"
                    command = proc "/bin/sh" ["-c", "echo $$ > \"$0\"; exec sleep 60", pidFile]
                pid <- withAsync (bootDrained manager 600 port command) $ \booting -> do
                    started <- pollUntil 50 100_000 isJust ((readMaybe . toString . T.strip =<<) <$> readIfExists pidFile)
                    cancel booting
                    pure (started :: Maybe Int)
                pid `shouldSatisfy` isJust
                traverse (\p -> doesDirectoryExist ("/proc/" <> show p)) pid `shouldReturn` Just False
        it "returns the tails of a process that exits during boot" $ do
            manager <- newManager defaultManagerSettings
            port <- freePort
            outcome <- bootDrained manager 50 port (proc "/bin/sh" ["-c", "echo last words; echo refused >&2; exit 2"])
            failedWith [(bfReason, "exited during boot"), (bfLog, "last words"), (bfStderr, "refused")] outcome
    describe "retryingBoot" $ do
        let threadStart = (BootFailure "exited during boot" "bench-load: failed to create OS thread: Resource temporarily unavailable" "", "diagnostic: counts")
            scripted outcomes = do
                remaining <- newIORef outcomes
                attempts <- newIORef []
                let boot number = do
                        modifyIORef' attempts (number :)
                        atomicModifyIORef' remaining $ \case
                            next : rest -> (rest, next)
                            [] -> ([], Left (BootFailure "ran out of scripted boots" "" "", ""))
                pure (boot, reverse <$> readIORef attempts)
            retrying = retryingBoot 0 (const pass)
        it "boots again once when the RTS could not start an OS thread, and keeps the diagnostic" $ do
            (boot, attempts) <- scripted [Left threadStart, Right ()]
            (notes, outcome) <- retrying boot
            isRight outcome `shouldBe` True
            attempts `shouldReturn` [1, 2]
            notes `shouldSatisfy` \case
                [note] -> all (`T.isInfixOf` note) ["failed to create OS thread", "diagnostic: counts"]
                _ -> False
        it "retries at most once" $ do
            (boot, attempts) <- scripted [Left threadStart, Left threadStart, Right ()]
            (notes, outcome) <- retrying boot
            isLeft outcome `shouldBe` True
            attempts `shouldReturn` [1, 2]
            length notes `shouldBe` 2
        it "does not retry any other boot failure" $ do
            (boot, attempts) <- scripted [Left (BootFailure "exited during boot" "configuration refused" "", ""), Right ()]
            (_, outcome) <- retrying boot
            isLeft outcome `shouldBe` True
            attempts `shouldReturn` [1]
    describe "bootDiagnostic" $ do
        it "names the process limits, the user's task count, and the proxy cgroup's leftover siblings" $
            withSystemTempDirectory "ecluse-diagnostic" $ \root -> do
                traverse_ (createDirectory . (root </>)) ["proxy-1-1", "proxy-2-1", "harness"]
                diagnostic <- bootDiagnostic (Just (root </> "proxy-1-1"))
                for_ ["Max processes", "proxy cgroup pids.max: absent", "other proxy cgroups still present: 1"] $ \expected ->
                    diagnostic `shouldSatisfy` T.isInfixOf expected
                let taskCount = readMaybe . toString =<< listToMaybe (mapMaybe (T.stripPrefix "tasks of this user: ") (lines diagnostic))
                (taskCount :: Maybe Int) `shouldSatisfy` maybe False (> 0)
        it "turns an error reading the diagnostic into a line of it" $ do
            diagnostic <- guardDiagnostic (throwIO (mkIOError permissionErrorType "listDirectory" Nothing (Just "/proc")))
            diagnostic `shouldSatisfy` T.isInfixOf "diagnostic: could not be read"
            diagnostic `shouldSatisfy` T.isInfixOf "/proc"

environmentSpec :: Spec
environmentSpec = describe "proxyEnvironment" $ do
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
        for_ [(ecosystem, shape) | ecosystem <- [Npm, PyPI], shape <- [Limited 2 (512 * 1024 * 1024), Unlimited]] $ \(ecosystem, shape) -> do
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
                environment = proxyEnvironment everyPin shape 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
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
