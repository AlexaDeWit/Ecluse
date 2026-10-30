-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin measured-proxy configuration and process cleanup.
module Ecluse.BenchLoad.ProxyProcessSpec (spec) where

import Data.Aeson (Value (Object), decode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf, lookup)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError, isPermissionError, mkIOError, permissionErrorType)
import System.Process.Typed (proc)
import Test.Hspec
import UnliftIO (throwIO)
import UnliftIO.Async (async, cancel, poll, withAsync)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.Advisories (advisoryDenyRules)
import Ecluse.BenchLoad.AdvisoryStore (AdvisoryObject (AdvisoryObject))
import Ecluse.BenchLoad.Pod (PodShape (Limited, MemoryOnly, Unlimited), ProxyTuning (..), noTuning)
import Ecluse.BenchLoad.ProxyProcess (
    AdvisoryFeed (AdvisoryFeed),
    BootFailure (..),
    ProxySettings (..),
    bootDiagnostic,
    bootDrained,
    bootDrainedWith,
    createConfiguredCgroup,
    guardDiagnostic,
    proxyEnvironment,
    proxyListening,
    proxySettings,
    requireIdentityListings,
    retryingBoot,
 )
import Ecluse.Composition.Support (expectPlanFor, noCeiling)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline), MirrorRole (ServeAndMirror))
import Ecluse.Config (AppConfig (cfgServer), Config (configApp), ServerSettings (srvMetadataGzip), loadConfig, renderConfigError)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Server.Encoding (MetadataCompression (IdentityOnly))
import Ecluse.Rts.Cgroup (readIfExists)
import Ecluse.Runtime.Server (listeningPrefix, proxyListener)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Wai (freePort)

spec :: Spec
spec = do
    environmentSpec
    bootSpec
    acquisitionSpec
    listeningSpec

listeningSpec :: Spec
listeningSpec = describe "proxyListening" $ do
    let logLine message = LBS.toStrict (encode (object ["message" .= (message :: Text), "status" .= ("info" :: Text)]))
    it "accepts the line the proxy logs from the shared prefix once its listener has bound" $
        proxyListening [logLine "rule 1: AllowIfOlderThan (precedence 100, applies at admission and revocation)", logLine (listeningPrefix proxyListener <> "4873")] `shouldBe` True
    it "does not take the prefix in the middle of another message" $
        proxyListening [logLine ("bench: " <> listeningPrefix proxyListener <> "4873")] `shouldBe` False
    it "waits while the log holds only boot lines" $
        proxyListening [logLine "rule boot order for mount npm:", logLine "rule 1: AllowIfOlderThan (precedence 100, applies at admission and revocation)"] `shouldBe` False

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

acquisitionSpec :: Spec
acquisitionSpec = describe "partial acquisition" $ do
    it "removes a new cgroup when a control write fails and preserves the write error" $
        withSystemTempDirectory "ecluse-cgroup-acquire" $ \root -> do
            let dir = root </> "proxy"
            createConfiguredCgroup dir (\created -> writeFileText (created </> "absent" </> "memory.max") "1")
                `shouldThrow` isDoesNotExistError
            doesDirectoryExist dir `shouldReturn` False
    for_ ["stdout", "stderr"] $ \failedStream ->
        it ("stops the process and acquired workers when acquiring " <> toString failedStream <> " fails") $
            withSystemTempDirectory "ecluse-drain-acquire" $ \dir -> do
                manager <- newManager defaultManagerSettings
                port <- freePort
                workers <- newIORef []
                child <- newIORef Nothing
                let pidFile = dir </> "pid"
                    command = proc "/bin/sh" ["-c", "echo $$ > \"$0\"; exec sleep 60", pidFile]
                    startWorker stream action
                        | stream == failedStream = do
                            pid <- pollUntil 50 100_000 isJust ((readMaybe . toString . T.strip =<<) <$> readIfExists pidFile)
                            writeIORef child (pid :: Maybe Int)
                            throwIO (mkIOError permissionErrorType "start drain" Nothing Nothing)
                        | otherwise = do
                            worker <- async action
                            modifyIORef' workers (worker :)
                            pure worker
                bootDrainedWith startWorker manager 600 port command `shouldThrow` isPermissionError
                pid <- readIORef child
                pid `shouldSatisfy` isJust
                traverse (\p -> doesDirectoryExist ("/proc/" <> show p)) pid `shouldReturn` Just False
                acquired <- readIORef workers
                length acquired `shouldBe` if failedStream == "stdout" then 0 else 1
                traverse_ (poll >=> (`shouldSatisfy` isJust)) acquired

environmentSpec :: Spec
environmentSpec = describe "proxyEnvironment" $ do
    let settings = (proxySettings Npm 0){psCacheMaxEntries = Just 3, psServeMaxInFlight = Just 12}
        advisoryObject = AdvisoryObject Npm (UTCTime (fromGregorian 2026 9 27) 0) "compiled advisory bytes"
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
    it "adds durable state only for managed mirror scenarios" $ do
        lookup "ECLUSE_MIRROR_STATE__URL" podEnvironment `shouldBe` Nothing
        let managed = (proxySettings PyPI 3600){psMirrorStatePort = Just 9003}
            environment = proxyEnvironment managed (Limited 2 (512 * 1024 * 1024)) 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
        lookup "ECLUSE_MOUNTS__PYPI__PUBLIC_UPSTREAM__REGISTRY__URL" environment `shouldBe` Just "https://127.0.0.1:9001"
        lookup "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL" environment `shouldBe` Just "https://localhost:9002"
        lookup "ECLUSE_MIRROR_STATE__URL" environment `shouldBe` Just "s3://ecluse-bench-mirror-state"
        lookup "AWS_ENDPOINT_URL" environment `shouldBe` Just "http://127.0.0.1:9003"
        lookup "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__URL" environment `shouldBe` Just "https://localhost:9002"
        lookup "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__UPLOAD_URL" environment `shouldBe` Just "https://localhost:9002/upload"
        case loadConfig environment Nothing of
            Left errors -> expectationFailure (toString (unlines (map renderConfigError errors)))
            Right config -> void (expectPlanFor (BootMirrorPipeline ServeAndMirror) environment Nothing config noCeiling)
    it "boots every ecosystem's ordinary proxy on the same upstream hosts, without durable state or a mirror target" $
        for_ [Npm, PyPI, Cargo, RubyGems] $ \ecosystem -> do
            let environment = proxyEnvironment (proxySettings ecosystem 60) Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
                upstreamKey leg = "ECLUSE_MOUNTS__" <> toString (T.toUpper (ecosystemName ecosystem)) <> "__" <> leg <> "__REGISTRY__URL"
            lookup (upstreamKey "PUBLIC_UPSTREAM") environment `shouldBe` Just "https://localhost:9001"
            lookup (upstreamKey "PRIVATE_UPSTREAM") environment `shouldBe` Just "https://localhost:9002"
            lookup "ECLUSE_MIRROR_STATE__URL" environment `shouldBe` Nothing
            filter (("__MIRROR_TARGET__" `isInfixOf`) . fst) environment `shouldBe` []
    it "overrides only the public hostname with or without durable state" $
        for_ [Nothing, Just 9003] $ \statePort -> do
            let pinned = (proxySettings PyPI 3600){psPublicHost = Just "127.0.0.2", psMirrorStatePort = statePort}
                environment = proxyEnvironment pinned Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
            lookup "ECLUSE_MOUNTS__PYPI__PUBLIC_UPSTREAM__REGISTRY__URL" environment `shouldBe` Just "https://127.0.0.2:9001"
            lookup "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL" environment `shouldBe` Just "https://localhost:9002"
            lookup "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__URL" environment `shouldBe` ("https://localhost:9002" <$ statePort)
    it "keeps Cargo public discovery on localhost and trusted mirror identities on the same literal address" $ do
        let managed = (proxySettings Cargo 3600){psMirrorStatePort = Just 9003}
            environment = proxyEnvironment managed Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
        lookup "ECLUSE_MOUNTS__CARGO__PUBLIC_UPSTREAM__REGISTRY__URL" environment `shouldBe` Just "https://localhost:9001"
        lookup "ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__REGISTRY__URL" environment `shouldBe` Just "https://127.0.0.1:9002"
        lookup "ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__REGISTRY__URL" environment `shouldBe` Just "https://127.0.0.1:9002"
        lookup "ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__REGISTRY__UPLOAD_URL" environment `shouldBe` Just "https://127.0.0.1:9002/upload"
        case loadConfig environment Nothing of
            Left errors -> expectationFailure (toString (unlines (map renderConfigError errors)))
            Right config -> void (expectPlanFor (BootMirrorPipeline ServeAndMirror) environment Nothing config noCeiling)
    it "keeps a Cargo public hostname override separate from its trusted mirror address" $
        for_ [Nothing, Just 9003] $ \statePort -> do
            let pinned = (proxySettings Cargo 3600){psPublicHost = Just "127.0.0.2", psMirrorStatePort = statePort}
                environment = proxyEnvironment pinned Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
            lookup "ECLUSE_MOUNTS__CARGO__PUBLIC_UPSTREAM__REGISTRY__URL" environment `shouldBe` Just "https://127.0.0.2:9001"
            lookup "ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__REGISTRY__URL" environment `shouldBe` Just "https://127.0.0.1:9002"
            lookup "ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__REGISTRY__URL" environment `shouldBe` ("https://127.0.0.1:9002" <$ statePort)
    it "sets no mirror uploadUrl where the protocol uploads below its url" $
        for_ [Npm, RubyGems] $ \ecosystem -> do
            let managed = (proxySettings ecosystem 3600){psMirrorStatePort = Just 9003}
                environment = proxyEnvironment managed Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
                mirrorKey key = "ECLUSE_MOUNTS__" <> toString (T.toUpper (ecosystemName ecosystem)) <> "__MIRROR_TARGET__REGISTRY__" <> key
            lookup (mirrorKey "URL") environment `shouldSatisfy` isJust
            lookup (mirrorKey "UPLOAD_URL") environment `shouldBe` Nothing
            whenLeft_ (loadConfig environment Nothing) (expectationFailure . toString . unlines . map renderConfigError)
    it "derives GHC capabilities from the cgroup under a limited shape, and overrides the count when unlimited" $ do
        lookup "ECLUSE_RUNTIME__CORES" podEnvironment `shouldBe` Nothing
        lookup "ECLUSE_RUNTIME__CORES" (environmentFor Unlimited) `shouldBe` Just "3"
    it "pins the runtime settings a run tunes, over the unlimited shape's count, as the configuration parser reads them" $
        for_ [Unlimited, MemoryOnly (1024 * 1024 * 1024)] $ \shape -> do
            let tuned = settings{psTuning = noTuning{ptCores = Just 8, ptMaxHeapBytes = Just 872415232, ptAllocAreaMib = Just 32}}
                environment = proxyEnvironment tuned shape 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) base
            lookup "ECLUSE_RUNTIME__CORES" environment `shouldBe` Just "8"
            lookup "ECLUSE_RUNTIME__MAX_HEAP_BYTES" environment `shouldBe` Just "872415232"
            filter ((== "GHCRTS") . fst) environment `shouldBe` [("GHCRTS", "-T -A32m")]
            case loadConfig environment Nothing of
                Left errs -> expectationFailure (toString (unlines (map renderConfigError errs)))
                Right config -> void (expectPlanFor (BootMirrorPipeline ServeAndMirror) environment Nothing config noCeiling)
    it "leaves the heap ceiling and the allocation area to the boot when a run tunes neither" $ do
        lookup "ECLUSE_RUNTIME__MAX_HEAP_BYTES" (environmentFor Unlimited) `shouldBe` Nothing
        lookup "ECLUSE_RUNTIME__MAX_HEAP_BYTES" podEnvironment `shouldBe` Nothing
    it "turns native metadata gzip off for every ecosystem and pod shape, as the configuration parser reads it" $
        for_ [(ecosystem, shape) | ecosystem <- [Npm, PyPI], shape <- [Limited 2 (512 * 1024 * 1024), Unlimited]] $ \(ecosystem, shape) -> do
            let environment = proxyEnvironment (proxySettings ecosystem 60) shape 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) [("ECLUSE_SERVER__METADATA_GZIP", "true")]
            filter ((== "ECLUSE_SERVER__METADATA_GZIP") . fst) environment `shouldBe` [("ECLUSE_SERVER__METADATA_GZIP", "false")]
            case loadConfig environment Nothing of
                Left errs -> expectationFailure (toString (unlines (map renderConfigError errs)))
                Right config -> srvMetadataGzip (cfgServer (configApp config)) `shouldBe` IdentityOnly
    it "refuses a proxy whose boot log states another gzip posture" $ do
        requireIdentityListings ["config: server.metadataGzip = false (environment)"]
        requireIdentityListings ["config: server.metadataGzip = true (default)"] `shouldThrow` anyException
        requireIdentityListings [] `shouldThrow` anyException
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
                        , psAdvisories = Just (AdvisoryFeed 9003 advisoryDenyRules advisoryObject)
                        }
                environment = proxyEnvironment everyPin shape 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
            case loadConfig environment Nothing of
                Left errs -> expectationFailure (toString (unlines (map renderConfigError errs)))
                Right config -> void (expectPlanFor (BootMirrorPipeline ServeAndMirror) environment Nothing config noCeiling)
    it "points an advisory feed's proxy at the loopback store under a stand-in identity, never the harness's own" $ do
        let advised rules = proxyEnvironment settings{psAdvisories = Just (AdvisoryFeed 9003 rules advisoryObject)} Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) awsBase
            awsBase = [("AWS_PROFILE", "harness"), ("AWS_ACCESS_KEY_ID", "harness-key")]
            shipped = advised []
        lookup "ECLUSE_ADVISORIES__URL" shipped `shouldBe` Just "s3://ecluse-bench-advisories"
        lookup "AWS_ENDPOINT_URL" shipped `shouldBe` Just "http://127.0.0.1:9003"
        filter ((== "AWS_ACCESS_KEY_ID") . fst) shipped `shouldBe` [("AWS_ACCESS_KEY_ID", "test")]
        lookup "AWS_PROFILE" shipped `shouldBe` Nothing
        let noDatabase = proxyEnvironment settings Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) awsBase
        filter (("AWS_" `isPrefixOf`) . fst) noDatabase `shouldBe` []
    it "selects one endpoint and one identity when Cargo uses advisory and native state buckets" $ do
        let feed = AdvisoryFeed 9003 advisoryDenyRules (AdvisoryObject Cargo (UTCTime (fromGregorian 2026 9 27) 0) "compiled advisory bytes")
            combined = (proxySettings Cargo 0){psAdvisories = Just feed, psMirrorStatePort = Just 9004}
            environment = proxyEnvironment combined Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 (Just 9002) []
        filter ((== "AWS_ENDPOINT_URL") . fst) environment `shouldBe` [("AWS_ENDPOINT_URL", "http://127.0.0.1:9004")]
        filter ((== "AWS_ACCESS_KEY_ID") . fst) environment `shouldBe` [("AWS_ACCESS_KEY_ID", "test")]
        lookup "ECLUSE_ADVISORIES__URL" environment `shouldBe` Just "s3://ecluse-bench-advisories"
        lookup "ECLUSE_MIRROR_STATE__URL" environment `shouldBe` Just "s3://ecluse-bench-mirror-state"
    it "adds an advisory feed's rules to the shipped policy, and leaves the policy alone without any" $ do
        let rulesFor rules = lookup "ECLUSE_RULES" (proxyEnvironment settings{psAdvisories = Just (AdvisoryFeed 9003 rules advisoryObject)} Unlimited 3 "/tmp/proxy" (8080, 8081, 8082) 9001 Nothing [])
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
