-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the configuration a measured proxy boots from.
module Ecluse.BenchLoad.ProxyProcessSpec (spec) where

import Data.Aeson (Value (Object), decode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
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

import Ecluse.BenchLoad.Advisories (advisoryDenyRules)
import Ecluse.BenchLoad.Pod (PodShape (Limited, Unlimited))
import Ecluse.BenchLoad.ProxyProcess (
    AdvisoryFeed (AdvisoryFeed),
    BootFailure (..),
    ProxySettings (..),
    bootDiagnostic,
    bootDrained,
    guardDiagnostic,
    proxyEnvironment,
    proxyListening,
    proxySettings,
    retryingBoot,
 )
import Ecluse.Composition.Support (expectPlanFor, noCeiling)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline), MirrorRole (ServeAndMirror))
import Ecluse.Config (loadConfig, renderConfigError)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Rts (readIfExists)
import Ecluse.Runtime.Server (listeningPrefix, proxyListener)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Wai (freePort)

spec :: Spec
spec = do
    environmentSpec
    bootSpec
    listeningSpec

listeningSpec :: Spec
listeningSpec = describe "proxyListening" $ do
    let logLine message = LBS.toStrict (encode (object ["message" .= (message :: Text), "status" .= ("info" :: Text)]))
    it "accepts the line the proxy logs from the shared prefix once its listener has bound" $
        proxyListening [logLine "rule 1: AllowIfOlderThan (precedence 100)", logLine (listeningPrefix proxyListener <> "4873")] `shouldBe` True
    it "does not take the prefix in the middle of another message" $
        proxyListening [logLine ("bench: " <> listeningPrefix proxyListener <> "4873")] `shouldBe` False
    it "waits while the log holds only boot lines" $
        proxyListening [logLine "rule boot order for mount npm:", logLine "rule 1: AllowIfOlderThan (precedence 100)"] `shouldBe` False

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
    it "keeps the harness environment with only the RTS statistics flag and without its own configuration" $ do
        lookup "PATH" podEnvironment `shouldBe` Just "/bin"
        filter ((== "GHCRTS") . fst) podEnvironment `shouldBe` [("GHCRTS", "-T")]
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
                        , psAdvisories = Just (AdvisoryFeed 9003 advisoryDenyRules)
                        }
                environment = proxyEnvironment everyPin shape 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
            case loadConfig environment Nothing of
                Left errs -> expectationFailure (toString (unlines (map renderConfigError errs)))
                Right config -> void (expectPlanFor (BootMirrorPipeline ServeAndMirror) environment Nothing config noCeiling)
    it "points an advisory feed's proxy at the loopback store under a stand-in identity, never the harness's own" $ do
        let advised rules = proxyEnvironment settings{psAdvisories = Just (AdvisoryFeed 9003 rules)} Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) awsBase
            awsBase = [("AWS_PROFILE", "harness"), ("AWS_ACCESS_KEY_ID", "harness-key")]
            shipped = advised []
        lookup "ECLUSE_ADVISORIES__URL" shipped `shouldBe` Just "s3://ecluse-bench-advisories"
        lookup "AWS_ENDPOINT_URL" shipped `shouldBe` Just "http://127.0.0.1:9003"
        filter ((== "AWS_ACCESS_KEY_ID") . fst) shipped `shouldBe` [("AWS_ACCESS_KEY_ID", "test")]
        lookup "AWS_PROFILE" shipped `shouldBe` Nothing
        lookup "AWS_ACCESS_KEY_ID" (environmentFor Unlimited) `shouldBe` Nothing
    it "adds an advisory feed's rules to the shipped policy, and leaves the policy alone without any" $ do
        let rulesFor rules = lookup "ECLUSE_RULES" (proxyEnvironment settings{psAdvisories = Just (AdvisoryFeed 9003 rules)} Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 Nothing [])
            ruleNames = \case
                Just (Object entries) -> sort (map Key.toText (KeyMap.keys entries))
                _ -> []
        rulesFor [] `shouldBe` Nothing
        ruleNames (decode . encodeUtf8 =<< rulesFor advisoryDenyRules) `shouldBe` ["deny-exploitable-cves", "deny-known-cves"]
    it "pins only the bounds the scenario sets" $ do
        lookup "ECLUSE_CACHE__MAX_ENTRIES" podEnvironment `shouldBe` Just "3"
        lookup "ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT" podEnvironment `shouldBe` Just "12"
        lookup "ECLUSE_CACHE__MAX_BYTES" podEnvironment `shouldBe` Nothing
        lookup "ECLUSE_LIMITS__MAX_RESPONSE_BYTES" podEnvironment `shouldBe` Nothing
        lookup "BENCH_PROXY_CONTROL_PORT" podEnvironment `shouldBe` Just "8081"
        lookup "OTEL_EXPORTER_PROMETHEUS_PORT" podEnvironment `shouldBe` Just "8082"
