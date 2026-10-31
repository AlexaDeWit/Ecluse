-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Run each measured proxy in its own cgroup with bounded log drains and explicit fixture configuration.
module Ecluse.BenchLoad.ProxyProcess (
    -- * Configuration
    ProxySettings (..),
    proxySettings,
    AdvisoryFeed (..),
    advisoryBucket,
    mirrorStateBucket,
    publicStubHost,
    privateStubHost,
    podShapeFromEnv,
    serveProxyFlag,
    proxyEnvironment,
    requireIdentityListings,
    sweepProxyCgroups,
    createConfiguredCgroup,

    -- * A running proxy
    ProxyProcess,
    withProxyProcess,
    proxyPort,
    proxyListening,
    proxyBootLines,
    proxyRuleLines,
    proxyIdleRts,
    proxyIdleCgroupBytes,
    proxyBootRetries,
    proxySnapshot,
    proxyCgroupNow,
    proxyTasksNow,
    proxyScrape,

    -- * Stopping
    ProxyEnd (..),
    stopProxy,

    -- * A drained process
    Drained,
    bootDrained,
    bootDrainedWith,
    BootFailure (..),
    retryingBoot,
    bootDiagnostic,
    guardDiagnostic,
) where

import Control.Concurrent (modifyMVar, threadDelay)
import Data.Aeson (eitherDecode, encode, object)
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Conc (getNumCapabilities)
import Network.HTTP.Client (
    HttpException,
    Manager,
    ManagerSettings (managerResponseTimeout),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
    responseStatus,
    responseTimeoutMicro,
 )
import Network.HTTP.Types (statusCode)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, listDirectory, removeDirectory)
import System.Environment (getEnvironment, getExecutablePath)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.User (getRealUserID)
import System.Process (getPid, terminateProcess)
import System.Process.Typed (
    ExitCode (ExitFailure, ExitSuccess),
    Process,
    ProcessConfig,
    createPipe,
    getExitCode,
    getStderr,
    getStdout,
    nullStream,
    proc,
    setEnv,
    setStderr,
    setStdin,
    setStdout,
    startProcess,
    stopProcess,
    unsafeProcessHandle,
    waitExitCode,
 )
import UnliftIO (bracket, bracketOnError, finally, mask, onException, try, tryAny, tryIO)
import UnliftIO.Async (Async, async, cancel, link, poll)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.AdvisoryStore (advisoryBucket)
import Ecluse.BenchLoad.BootLines (bootMessages, logMessages, metadataCompression, renderMetadataCompression, ruleMessages)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Exposition (Sample, advisoryDatabaseInstalled, parseExposition)
import Ecluse.BenchLoad.Placement (readCpuList)
import Ecluse.BenchLoad.Pod (
    CgroupReading (..),
    PodShape (Unlimited),
    ProxyTuning (..),
    counter,
    keyedCounters,
    needsCgroup,
    noTuning,
    parsePodShape,
    podMemoryLimit,
    proxyCgroupFiles,
    renderPodShape,
 )
import Ecluse.BenchLoad.RtsProbe (rtsStatsFlag)
import Ecluse.BenchLoad.RtsWindow (Collection (MajorCollection), RtsSnapshot, collectionName)
import Ecluse.BenchLoad.Verdict (ProxyEnding, classifyEnding)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Registry.Adapter (uploadEndpointFor)
import Ecluse.Core.Registry.Adapter.Capability (UploadEndpointRequirement (ExplicitUploadEndpoint))
import Ecluse.Core.Server.Encoding (MetadataCompression (IdentityOnly))
import Ecluse.Rts.Cgroup (parseMemoryMax, readIfExists)
import Ecluse.Runtime.Server (listeningPrefix, proxyListener)
import Ecluse.Test.Env (ambientAwsEntries)
import Ecluse.Test.Poll (awaitUntil, pollUntil)
import Ecluse.Test.Wai (freePort)

-- | The argument that makes this executable serve as the measured proxy.
serveProxyFlag :: String
serveProxyFlag = "--serve-proxy"

-- | Scenario settings. 'Nothing' preserves each setting's existing default.
data ProxySettings = ProxySettings
    { psEcosystem :: Ecosystem
    , psCacheTtlSeconds :: Int
    , psCacheMaxEntries :: Maybe Int
    , psCacheMaxBytes :: Maybe Int
    , psMaxResponseBytes :: Maybe Int
    , psServeMaxInFlight :: Maybe Int
    , psPublicConnections :: Maybe Int
    , psPrivateConnections :: Maybe Int
    , psClock :: Maybe UTCTime
    -- ^ A fixed evaluation clock for the rules, 'Nothing' for the wall clock.
    , psMirrorStatePort :: Maybe Int
    -- ^ An optional S3-compatible fixture for durable managed mirror reads.
    , psAdvisories :: Maybe AdvisoryFeed
    , psTuning :: ProxyTuning
    }

-- | Settings for one mount with the given cache TTL, every bound left to the boot.
proxySettings :: Ecosystem -> Int -> ProxySettings
proxySettings ecosystem ttl =
    ProxySettings
        { psEcosystem = ecosystem
        , psCacheTtlSeconds = ttl
        , psCacheMaxEntries = Nothing
        , psCacheMaxBytes = Nothing
        , psMaxResponseBytes = Nothing
        , psServeMaxInFlight = Nothing
        , psPublicConnections = Nothing
        , psPrivateConnections = Nothing
        , psClock = Nothing
        , psMirrorStatePort = Nothing
        , psAdvisories = Nothing
        , psTuning = noTuning
        }

-- | The loopback object store serving the advisory database, and the named rules the policy adds to read it.
data AdvisoryFeed = AdvisoryFeed
    { afStorePort :: Int
    , afRules :: [Pair]
    }
    deriving stock (Eq, Show)

-- | The bucket holding the managed-read fixture's project record.
mirrorStateBucket :: Text
mirrorStateBucket = "ecluse-bench-mirror-state"

{- | The name the proxy dials the public stub by. The internal-range block refuses a public artifact URL at a
loopback literal, and boot refuses a mirror target on the public upstream's host, so it differs from 'privateStubHost'.
-}
publicStubHost :: Text
publicStubHost = "localhost"

-- | The address the proxy dials the private and mirror target stubs by, and the host their prepared mirror records name.
privateStubHost :: Text
privateStubHost = "127.0.0.1"

-- | The pod shape in @BENCH_LOAD_POD@, unlimited when unset.
podShapeFromEnv :: IO PodShape
podShapeFromEnv =
    lookupEnv "BENCH_LOAD_POD" >>= \case
        Nothing -> pure Unlimited
        Just raw -> either benchFail pure (parsePodShape (toText raw))

-- | A booted proxy, ready and idle, with the readings taken before any load.
data ProxyProcess = ProxyProcess
    { ppPort :: Int
    , ppControlPort :: Int
    , ppScrapePort :: Int
    , ppDrained :: Drained
    , ppCgroup :: Maybe FilePath
    , ppManager :: Manager
    , ppBootLines :: [Text]
    , ppRuleLines :: [Text]
    , ppIdleRts :: Maybe RtsSnapshot
    , ppIdleCgroupBytes :: Maybe Int
    , ppBootRetries :: [Text]
    , ppEnd :: MVar (Maybe ProxyEnd)
    }

-- | The proxy's listening port on loopback.
proxyPort :: ProxyProcess -> Int
proxyPort = ppPort

-- | Whether a JSON log holds the line the proxy logs once its listener has bound.
proxyListening :: [ByteString] -> Bool
proxyListening = any (listeningPrefix proxyListener `T.isPrefixOf`) . logMessages

-- | The runtime and admission lines the proxy logged at boot.
proxyBootLines :: ProxyProcess -> [Text]
proxyBootLines = ppBootLines

-- | The rule configuration and rule boot order the proxy logged at boot.
proxyRuleLines :: ProxyProcess -> [Text]
proxyRuleLines = ppRuleLines

-- | RTS counters after a major collection on the booted, idle proxy: its idle floor.
proxyIdleRts :: ProxyProcess -> Maybe RtsSnapshot
proxyIdleRts = ppIdleRts

-- | The proxy cgroup's @memory.current@ at the idle floor.
proxyIdleCgroupBytes :: ProxyProcess -> Maybe Int
proxyIdleCgroupBytes = ppIdleCgroupBytes

-- | The failed boots retried before this one, each with its diagnostic.
proxyBootRetries :: ProxyProcess -> [Text]
proxyBootRetries = ppBootRetries

-- | How the proxy ended and what its cgroup recorded.
data ProxyEnd = ProxyEnd
    { peEnding :: ProxyEnding
    , peExitedEarly :: Bool
    -- ^ The process had already exited when the harness came to stop it.
    , peStderrTail :: Text
    , peCgroup :: Maybe CgroupReading
    }

-- | Boot and stop a measured proxy. Retry once only when the RTS cannot start a thread.
withProxyProcess :: ProxySettings -> Int -> Maybe Int -> (ProxyProcess -> IO a) -> IO a
withProxyProcess settings publicPort privatePort body = do
    shape <- podShapeFromEnv
    root <- lookupEnv "BENCH_LOAD_CGROUP"
    withSystemTempDirectory "ecluse-bench-proxy" $ \dir ->
        bracket (launch settings shape root dir publicPort privatePort) release body
  where
    release proxy = stopProxy proxy `finally` traverse_ retireCgroup (ppCgroup proxy)

-- Each boot attempt gets a cgroup of its own, so a retried boot's counters start from zero.
acquireCgroup :: PodShape -> ProxyTuning -> Maybe FilePath -> Int -> IO (Maybe FilePath)
acquireCgroup shape tuning root attempt = case root of
    Nothing
        | needsCgroup shape tuning ->
            benchFail ("the pod shape " <> renderPodShape shape <> " or the CPU placement needs BENCH_LOAD_CGROUP: a cgroup v2 directory delegated to this user, with the cpuset, cpu, memory, and pids controllers enabled")
        | otherwise -> pure Nothing
    Just base -> do
        pid <- getProcessID
        let dir = base </> ("proxy-" <> show pid <> "-" <> show attempt)
        Just
            <$> createConfiguredCgroup
                dir
                ( \created -> do
                    for_ (proxyCgroupFiles shape tuning) $ \(file, value) -> writeFileText (created </> file) value
                    swapFile <- doesFileExist (created </> "memory.swap.max")
                    when (swapFile && isJust (podMemoryLimit shape)) (writeFileText (created </> "memory.swap.max") "0")
                )

-- | Retire a newly created cgroup if writing its controls fails.
createConfiguredCgroup :: FilePath -> (FilePath -> IO ()) -> IO FilePath
createConfiguredCgroup dir configure =
    bracketOnError (createDirectory dir) (const (retireCgroup dir)) $ \() ->
        configure dir $> dir

-- Kill anything left in the cgroup, wait for it to empty, and remove it.
retireCgroup :: FilePath -> IO ()
retireCgroup dir = do
    killable <- doesFileExist (dir </> "cgroup.kill")
    when killable (void (tryIO (writeFileText (dir </> "cgroup.kill") "1")))
    void (pollUntil 50 100_000 (maybe True (T.null . T.strip)) (readIfExists (dir </> "cgroup.procs")))
    tryIO (removeDirectory dir) >>= \case
        Right () -> pass
        Left err -> TIO.hPutStrLn stderr ("bench-load: could not remove the proxy cgroup " <> toText dir <> ": " <> show err)

-- | Retire every proxy cgroup a killed harness left under @BENCH_LOAD_CGROUP@.
sweepProxyCgroups :: IO ()
sweepProxyCgroups =
    lookupEnv "BENCH_LOAD_CGROUP" >>= traverse_ sweep
  where
    sweep base = do
        present <- doesDirectoryExist base
        entries <- if present then listDirectory base else pure []
        traverse_ (retireCgroup . (base </>)) (filter ("proxy-" `isPrefixOf`) entries)

launch :: ProxySettings -> PodShape -> Maybe FilePath -> FilePath -> Int -> Maybe Int -> IO ProxyProcess
launch settings shape root dir publicPort privatePort = do
    (port, controlPort, scrapePort) <- distinctPorts
    self <- getExecutablePath
    capabilities <- getNumCapabilities
    base <- getEnvironment
    manager <- newManager defaultManagerSettings{managerResponseTimeout = responseTimeoutMicro 60_000_000}
    let environment = proxyEnvironment settings shape capabilities dir (port, controlPort, scrapePort) publicPort privatePort base
        commandIn = \case
            Nothing -> proc self [serveProxyFlag]
            -- The shell joins the cgroup and then becomes the proxy, so the boot already sees its limits.
            Just cg -> proc "/bin/sh" ["-c", "echo $$ > \"$0\" && exec \"$@\"", cg </> "cgroup.procs", self, serveProxyFlag]
        bootAttempt attempt = do
            cgroup <- acquireCgroup shape (psTuning settings) root attempt
            outcome <- bootDrained manager 1200 port (setEnv environment (commandIn cgroup)) `onException` traverse_ retireCgroup cgroup
            case outcome of
                Right drained -> pure (Right (drained, cgroup))
                Left failure -> do
                    -- Read the failed attempt's cgroup before it goes.
                    diagnostic <- guardDiagnostic (bootDiagnostic cgroup)
                    traverse_ retireCgroup cgroup
                    pure (Left (failure, diagnostic))
    (retries, booting) <- retryingBoot 2_000_000 (TIO.hPutStrLn stderr) bootAttempt
    (drained, cgroup) <- either (const (benchFail ("bench-load: the proxy did not boot\n" <> T.intercalate "\n\n" retries))) pure booting
    endVar <- newMVar Nothing
    let booted =
            ProxyProcess
                { ppPort = port
                , ppControlPort = controlPort
                , ppScrapePort = scrapePort
                , ppDrained = drained
                , ppCgroup = cgroup
                , ppManager = manager
                , ppBootLines = []
                , ppRuleLines = []
                , ppIdleRts = Nothing
                , ppIdleCgroupBytes = Nothing
                , ppBootRetries = retries
                , ppEnd = endVar
                }
    (`onException` (stopProxy booted `finally` traverse_ retireCgroup cgroup)) $ do
        -- Every boot line precedes the listening line. Give the drain a moment to catch up.
        logged <- pollUntil 50 100_000 proxyListening (BS8.lines . capturedHead <$> readIORef (drStdout drained))
        requireIdentityListings (bootMessages logged)
        when (isJust (psAdvisories settings)) (awaitAdvisoryDatabase booted (psEcosystem settings))
        idle <- proxySnapshot booted MajorCollection
        idleCgroup <- proxyCgroupNow booted
        pure booted{ppBootLines = bootMessages logged, ppRuleLines = ruleMessages logged, ppIdleRts = idle, ppIdleCgroupBytes = crMemoryCurrent =<< idleCgroup}

-- | Refuse a proxy whose boot log does not state native metadata gzip off, the posture the floors hold.
requireIdentityListings :: [Text] -> IO ()
requireIdentityListings lines' = case metadataCompression lines' of
    Just IdentityOnly -> pass
    posture -> benchFail ("bench-load: the proxy resolved native metadata gzip as " <> renderMetadataCompression posture <> ", but load runs measure identity listings")

-- | Wait at most a minute for advisory installation, stopping failed proxies and reporting both output tails.
awaitAdvisoryDatabase :: ProxyProcess -> Ecosystem -> IO ()
awaitAdvisoryDatabase proxy ecosystem = do
    void (awaitUntil 60_000_000 100_000 (settled <$> installation))
    installation >>= \case
        Ready -> pass
        waiting -> do
            end <- stopProxy proxy
            logTail <- capturedTailText (drStdout (ppDrained proxy))
            benchFail ("bench-load: " <> renderBootFailure (BootFailure (reason waiting) (peStderrTail end) logTail))
  where
    installation =
        getExitCode (drProcess (ppDrained proxy)) >>= \case
            Just code -> pure (ExitedDuringBoot code)
            Nothing -> bool Booting Ready . maybe False (advisoryDatabaseInstalled ecosystem) <$> proxyScrape proxy
    reason = \case
        ExitedDuringBoot code -> "exited with " <> show code <> " before installing its advisory database"
        _ -> "did not install its advisory database within 60 s"

-- Three distinct free ports: the proxy, its RTS control listener, and its scrape listener.
distinctPorts :: IO (Int, Int, Int)
distinctPorts = do
    a <- freePort
    b <- freePort
    c <- freePort
    if a /= b && b /= c && a /= c then pure (a, b, c) else distinctPorts

-- | Replace inherited proxy settings and RTS flags with this scenario's explicit environment.
proxyEnvironment :: ProxySettings -> PodShape -> Int -> FilePath -> (Int, Int, Int) -> Int -> Maybe Int -> [(String, String)] -> [(String, String)]
proxyEnvironment settings shape capabilities dir (port, controlPort, scrapePort) publicPort privatePort base =
    filter (inherited . fst) base <> map (bimap toString toString) (fixed <> pinned)
  where
    inherited key = key /= "GHCRTS" && not (any (`isPrefixOf` key) ["ECLUSE_", "OTEL_", "AWS_", "__ECLUSE"])
    mount = T.toUpper (ecosystemName (psEcosystem settings))
    upstream :: Text -> Int -> (Text, Text)
    upstream leg p = ("ECLUSE_MOUNTS__" <> mount <> "__" <> leg <> "__REGISTRY__URL", upstreamUrl leg p)
    upstreamUrl :: Text -> Int -> Text
    upstreamUrl leg p = "https://" <> upstreamHost leg <> ":" <> show p
    upstreamHost :: Text -> Text
    upstreamHost leg
        | leg == "PUBLIC_UPSTREAM" = publicStubHost
        | otherwise = privateStubHost
    fixed =
        [ ("ECLUSE_SERVER__PORT", show port)
        , ("ECLUSE_SERVER__PUBLIC_URL", "https://bench.proxy")
        , ("ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT", "10")
        , -- The floors hold the identity baseline, so every load proxy turns native gzip off.
          ("ECLUSE_SERVER__METADATA_GZIP", "false")
        , ("ECLUSE_ADVISORIES__DATA_DIR", toText (dir </> "advisories"))
        , ("ECLUSE_CACHE__TTL", show (psCacheTtlSeconds settings))
        , ("ECLUSE_OBSERVABILITY__TELEMETRY", "on")
        , ("OTEL_METRICS_EXPORTER", "prometheus")
        , ("OTEL_EXPORTER_PROMETHEUS_HOST", "127.0.0.1")
        , ("OTEL_EXPORTER_PROMETHEUS_PORT", show scrapePort)
        , ("OTEL_TRACES_EXPORTER", "none")
        , ("OTEL_LOGS_EXPORTER", "none")
        , ("BENCH_PROXY_CONTROL_PORT", show controlPort)
        , ("GHCRTS", T.unwords (toText rtsStatsFlag : ["-A" <> show area <> "m" | Just area <- [ptAllocAreaMib tuning]]))
        , upstream "PUBLIC_UPSTREAM" publicPort
        ]
    tuning = psTuning settings
    advisories feed =
        [ ("ECLUSE_ADVISORIES__URL", "s3://" <> advisoryBucket)
        ]
            <> [("ECLUSE_RULES", decodeUtf8 (encode (object (afRules feed)))) | not (null (afRules feed))]
    mirrorState =
        [ ("ECLUSE_MIRROR_STATE__URL", "s3://" <> mirrorStateBucket)
        , ("ECLUSE_MOUNTS__" <> mount <> "__MIRROR_TARGET__REGISTRY__TOKEN", "fixture-token")
        , ("ECLUSE_MOUNTS__" <> mount <> "__MIRROR_TARGET__REGISTRY__USERNAME", "fixture-user")
        ]
            <> foldMap (\p -> upstream "MIRROR_TARGET" p : [("ECLUSE_MOUNTS__" <> mount <> "__MIRROR_TARGET__REGISTRY__UPLOAD_URL", upstreamUrl "MIRROR_TARGET" p <> "/upload") | separateUpload]) privatePort
    -- A protocol that derives its upload address from url refuses an uploadUrl at load.
    separateUpload = uploadEndpointFor (psEcosystem settings) == Just ExplicitUploadEndpoint
    pinned =
        catMaybes
            [ upstream "PRIVATE_UPSTREAM" <$> privatePort
            , ("ECLUSE_CACHE__MAX_ENTRIES",) . show <$> psCacheMaxEntries settings
            , ("ECLUSE_CACHE__MAX_BYTES",) . show <$> psCacheMaxBytes settings
            , ("ECLUSE_LIMITS__MAX_RESPONSE_BYTES",) . show <$> psMaxResponseBytes settings
            , ("ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT",) . show <$> psServeMaxInFlight settings
            , ("ECLUSE_RUNTIME__PUBLIC_CONNECTIONS_PER_HOST",) . show <$> psPublicConnections settings
            , ("ECLUSE_RUNTIME__PRIVATE_CONNECTIONS_PER_HOST",) . show <$> psPrivateConnections settings
            , ("BENCH_PROXY_NOW",) . toText . iso8601Show <$> psClock settings
            , -- Without a quota or a pinned count, runtime.cores sets GHC capabilities to the harness's count.
              ("ECLUSE_RUNTIME__CORES",) . show <$> (ptCores tuning <|> capabilities <$ guard (shape == Unlimited))
            , ("ECLUSE_RUNTIME__MAX_HEAP_BYTES",) . show <$> ptMaxHeapBytes tuning
            ]
            <> foldMap advisories (psAdvisories settings)
            <> foldMap (const mirrorState) (psMirrorStatePort settings)
            <> foldMap storeEnvironment (psMirrorStatePort settings <|> (afStorePort <$> psAdvisories settings))
    storeEnvironment endpoint =
        ("AWS_ENDPOINT_URL", "http://127.0.0.1:" <> show endpoint) : map (bimap toText toText) ambientAwsEntries

-- | RTS counters from the proxy after the given collection. 'Nothing' once it has gone.
proxySnapshot :: ProxyProcess -> Collection -> IO (Maybe RtsSnapshot)
proxySnapshot proxy collection = do
    body <- getOk proxy (ppControlPort proxy) ("/rts?gc=" <> toString (collectionName collection))
    pure (body >>= rightToMaybe . eitherDecode)

-- | The proxy's cgroup files now, 'Nothing' when it runs outside a cgroup of its own.
proxyCgroupNow :: ProxyProcess -> IO (Maybe CgroupReading)
proxyCgroupNow = traverse readCgroup . ppCgroup

-- | The proxy's thread count from @pids.current@ alone, cheap enough to read every second.
proxyTasksNow :: ProxyProcess -> IO (Maybe Int)
proxyTasksNow proxy = case ppCgroup proxy of
    Nothing -> pure Nothing
    Just dir -> (readMaybe . toString . T.strip =<<) <$> readIfExists (dir </> "pids.current")

-- | One scrape of the proxy's metrics. 'Nothing' when it fails, which a sampler counts as a miss.
proxyScrape :: ProxyProcess -> IO (Maybe [Sample])
proxyScrape proxy = fmap (parseExposition . decodeUtf8) <$> getOk proxy (ppScrapePort proxy) "/metrics"

getOk :: ProxyProcess -> Int -> String -> IO (Maybe LByteString)
getOk = getFrom . ppManager

getFrom :: Manager -> Int -> String -> IO (Maybe LByteString)
getFrom manager port path = do
    request <- parseRequest ("http://127.0.0.1:" <> show port <> path)
    outcome <- try (httpLbs request manager)
    pure $ case outcome of
        Left (_ :: HttpException) -> Nothing
        Right response
            | statusCode (responseStatus response) == 200 -> Just (responseBody response)
            | otherwise -> Nothing

readCgroup :: FilePath -> IO CgroupReading
readCgroup dir = do
    maxBytes <- bytesAt "memory.max"
    peak <- bytesAt "memory.peak"
    current <- bytesAt "memory.current"
    events <- maybe mempty keyedCounters <$> readIfExists (dir </> "memory.events")
    stat <- maybe mempty keyedCounters <$> readIfExists (dir </> "memory.stat")
    cpu <- maybe mempty keyedCounters <$> readIfExists (dir </> "cpu.stat")
    cpus <- readCpuList (dir </> "cpuset.cpus.effective")
    pure (CgroupReading maxBytes peak current events stat cpu cpus)
  where
    bytesAt file = (>>= parseMemoryMax) <$> readIfExists (dir </> file)

-- | Request graceful shutdown, kill after thirty seconds, and retain the result for later callers.
stopProxy :: ProxyProcess -> IO ProxyEnd
stopProxy proxy = modifyMVar (ppEnd proxy) $ \case
    Just end -> pure (Just end, end)
    Nothing -> do
        end <- terminate proxy
        pure (Just end, end)

terminate :: ProxyProcess -> IO ProxyEnd
terminate proxy = do
    let drained = ppDrained proxy
    alreadyExited <- isJust <$> getExitCode (drProcess drained)
    (code, harnessKilled) <- stopDrained drained
    errText <- capturedTailText (drStderr drained)
    reading <- proxyCgroupNow proxy
    let oomKills = maybe 0 (counter "oom_kill" . crMemoryEvents) reading
        status = case code of
            ExitSuccess -> 0
            ExitFailure n -> n
    pure
        ProxyEnd
            { peEnding = classifyEnding status errText oomKills harnessKilled
            , peExitedEarly = alreadyExited
            , peStderrTail = errText
            , peCgroup = reading
            }

-- | A process whose stdout and stderr the harness drains, keeping each stream's head and tail.
data Drained = Drained
    { drProcess :: Process () Handle Handle
    , drStdout :: IORef Captured
    , drStderr :: IORef Captured
    , drDrains :: [Async ()]
    }

-- | Why a boot failed, with the tails of both streams, read after the process was stopped.
data BootFailure = BootFailure
    { bfReason :: Text
    , bfStderr :: Text
    , bfLog :: Text
    }
    deriving stock (Show)

renderBootFailure :: BootFailure -> Text
renderBootFailure failure =
    "the proxy " <> bfReason failure <> "\nstderr:\n" <> bfStderr failure <> "\nlog tail:\n" <> bfLog failure

-- | Drain process output while polling readiness every 100 ms, returning bounded diagnostics on failure.
bootDrained :: Manager -> Int -> Int -> ProcessConfig () () () -> IO (Either BootFailure Drained)
bootDrained = bootDrainedWith (const async)

-- | Supply drain-worker creation for deterministic acquisition-failure tests.
bootDrainedWith :: (Text -> IO () -> IO (Async ())) -> Manager -> Int -> Int -> ProcessConfig () () () -> IO (Either BootFailure Drained)
bootDrainedWith startWorker manager attempts port command = mask $ \restore -> do
    out <- newIORef emptyCaptured
    err <- newIORef emptyCaptured
    bracketOnError
        (startProcess (setStdin nullStream (setStdout createPipe (setStderr createPipe command))))
        (\process -> void (stopDrained (Drained process out err [])))
        $ \process -> do
            let partial = Drained process out err
                startDrain stream source captured = do
                    worker <- startWorker stream (restore (drain stream source captured))
                    link worker `onException` cancel worker
                    pure worker
            stdoutWorker <- startDrain "stdout" (getStdout process) out
            stderrWorker <-
                startDrain "stderr" (getStderr process) err
                    `onException` stopDrained (partial [stdoutWorker])
            let drained = partial [stdoutWorker, stderrWorker]
            restore
                ( do
                    readiness <- pollUntil attempts 100_000 settled (probe drained)
                    case readiness of
                        Ready -> pure (Right drained)
                        ExitedDuringBoot code -> Left <$> failBoot drained ("exited during boot with " <> show code)
                        Booting -> Left <$> failBoot drained ("did not become ready within " <> show (attempts `div` 10) <> " s")
                )
                `onException` stopDrained drained
  where
    probe drained =
        getExitCode (drProcess drained) >>= \case
            Just code -> pure (ExitedDuringBoot code)
            Nothing -> bool Booting Ready . isJust <$> getFrom manager port "/readyz"
    failBoot drained reason = do
        void (stopDrained drained)
        BootFailure reason <$> capturedTailText (drStderr drained) <*> capturedTailText (drStdout drained)

-- | Retry once after an RTS thread-start failure, preserving diagnostics from both attempts.
retryingBoot :: Int -> (Text -> IO ()) -> (Int -> IO (Either (BootFailure, Text) a)) -> IO ([Text], Either BootFailure a)
retryingBoot delayMicros announce boot = attempt 1 []
  where
    attempt number notes =
        boot number >>= \case
            Right booted -> pure (reverse notes, Right booted)
            Left (failure, diagnostic) -> do
                let note = renderBootFailure failure <> "\n" <> diagnostic
                    retry = number < 2 && "failed to create OS thread" `T.isInfixOf` bfStderr failure
                if retry
                    then do
                        announce ("bench-load: retrying a proxy boot once\n" <> note)
                        threadDelay delayMicros
                        attempt (number + 1) (note : notes)
                    else pure (reverse (note : notes), Left failure)

-- | A diagnostic that cannot fail: an error reading it becomes a line of its text.
guardDiagnostic :: IO Text -> IO Text
guardDiagnostic diagnose =
    either (\err -> "diagnostic: could not be read: " <> toText (displayException err)) id <$> tryAny diagnose

-- | Report the failed process and parent cgroup limits alongside host memory and task counts.
bootDiagnostic :: Maybe FilePath -> IO Text
bootDiagnostic cgroup = do
    limits <- readIfExists "/proc/self/limits"
    uid <- getRealUserID
    tasks <- userTasks (fromIntegral uid)
    loadavg <- readIfExists "/proc/loadavg"
    threadsMax <- readIfExists "/proc/sys/kernel/threads-max"
    pidMax <- readIfExists "/proc/sys/kernel/pid_max"
    meminfo <- readIfExists "/proc/meminfo"
    overcommit <- readIfExists "/proc/sys/vm/overcommit_memory"
    cgroupLines <- maybe (pure ["proxy cgroup: none"]) cgroupFacts cgroup
    pure . T.unlines $
        [ "diagnostic:"
        , "limits of this process: " <> maybe "unreadable" (T.intercalate "; " . filter (\l -> any (`T.isPrefixOf` l) ["Max processes", "Max stack size", "Max address space"]) . lines) limits
        , "tasks of this user: " <> show tasks
        , "loadavg (running/total tasks in the fourth field): " <> maybe "unreadable" T.strip loadavg
        , "kernel threads-max / pid_max: " <> maybe "?" T.strip threadsMax <> " / " <> maybe "?" T.strip pidMax
        , "memory: " <> maybe "unreadable" (T.intercalate "; " . filter (\l -> any (`T.isPrefixOf` l) ["MemAvailable", "CommitLimit", "Committed_AS"]) . lines) meminfo
        , "vm.overcommit_memory: " <> maybe "?" T.strip overcommit
        ]
            <> cgroupLines
  where
    cgroupFacts dir = do
        facts <- traverse (\file -> (file,) <$> readIfExists (dir </> file)) ["pids.current", "pids.max", "memory.current", "memory.max", "memory.events"]
        siblings <- filter (/= takeFileName dir) . filter ("proxy-" `isPrefixOf`) <$> listDirectory (takeDirectory dir)
        pure $
            ["proxy cgroup " <> toText file <> ": " <> maybe "absent" (T.unwords . words) value | (file, value) <- facts]
                <> ["other proxy cgroups still present: " <> show (length siblings)]

-- The threads of every process this user owns, from each process's status file.
userTasks :: Int -> IO Int
userTasks uid = do
    entries <- filter (all (`elem` ['0' .. '9'])) <$> listDirectory "/proc"
    counts <- traverse (fmap (>>= threadsOf) . readIfExists . (\pid -> "/proc" </> pid </> "status")) entries
    pure (sum (catMaybes counts))
  where
    threadsOf status = do
        let fields = mapMaybe (\l -> (,) <$> listToMaybe (words l) <*> listToMaybe (drop 1 (words l))) (lines status)
        owner <- readMaybe . toString =<< lookup "Uid:" fields
        guard (owner == uid)
        readMaybe . toString =<< lookup "Threads:" fields

data Readiness = Booting | Ready | ExitedDuringBoot ExitCode

settled :: Readiness -> Bool
settled = \case
    Booting -> False
    _ -> True

-- Polling keeps cleanup outside timeout cancellation while subprocess handlers hold an uninterruptible mask.
stopDrained :: Drained -> IO (ExitCode, Bool)
stopDrained drained = do
    let process = drProcess drained
    stopped <-
        getExitCode process >>= \case
            Just code -> pure (code, False)
            Nothing -> do
                terminateProcess (unsafeProcessHandle process)
                pollUntil 300 100_000 isJust (getExitCode process) >>= \case
                    Just code -> pure (code, False)
                    Nothing -> do
                        getPid (unsafeProcessHandle process) >>= traverse_ (signalProcess sigKILL)
                        code <- waitExitCode process
                        pure (code, True)
    for_ (drDrains drained) $ \worker -> do
        finished <- pollUntil 100 100_000 isJust (poll worker)
        when (isNothing finished) (cancel worker)
    -- The process has exited, so this only releases what typed-process holds for it.
    stopProcess process
    pure stopped

-- Read a pipe until the process closes it, keeping its head and tail and dropping the rest.
drain :: Text -> Handle -> IORef Captured -> IO ()
drain stream source captured = tryIO copy >>= either failed pure
  where
    copy = do
        chunk <- BS.hGetSome source 65_536
        unless (BS.null chunk) $ do
            atomicModifyIORef' captured (\held -> (capture chunk held, ()))
            copy
    failed err = benchFail ("bench-load: reading the proxy's " <> stream <> " failed: " <> show err)

-- The first mebibyte of a stream, in reverse chunks, and its last four kibibytes.
data Captured = Captured
    { cHead :: [ByteString]
    , cHeadBytes :: Int
    , cTail :: ByteString
    }

emptyCaptured :: Captured
emptyCaptured = Captured [] 0 mempty

-- Per chunk, copies the head's remaining room at most, and for the tail appends up to 8 KiB and
-- then copies 4 KiB, about 12 KiB in all.
capture :: ByteString -> Captured -> Captured
capture chunk held =
    Captured
        { cHead = if room > 0 then BS.copy (BS.take room chunk) : cHead held else cHead held
        , cHeadBytes = cHeadBytes held + min room (BS.length chunk)
        , cTail = BS.copy (BS.takeEnd tailBytes (cTail held <> BS.takeEnd tailBytes chunk))
        }
  where
    room = 1_048_576 - cHeadBytes held
    tailBytes = 4_096

capturedHead :: Captured -> ByteString
capturedHead = BS.concat . reverse . cHead

capturedTailText :: IORef Captured -> IO Text
capturedTailText = fmap (decodeUtf8 . cTail) . readIORef
