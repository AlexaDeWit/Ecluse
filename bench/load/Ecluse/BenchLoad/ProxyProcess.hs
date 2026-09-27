-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Start, observe, and stop the proxy process a scenario measures. The proxy is this executable
under 'serveProxyFlag', configured through @ECLUSE_*@ variables as a deployment would be. Under a
pod shape it runs in its own child of the cgroup named by @BENCH_LOAD_CGROUP@, so the limit bounds
it alone. The cgroup outlives the process, so an OOM kill stays readable after it. The proxy logs
into pipes the harness drains into bounded memory, so no log page is charged to the limit.
-}
module Ecluse.BenchLoad.ProxyProcess (
    -- * Configuration
    ProxySettings (..),
    proxySettings,
    podShapeFromEnv,
    serveProxyFlag,
    proxyEnvironment,
    sweepProxyCgroups,

    -- * A running proxy
    ProxyProcess,
    withProxyProcess,
    proxyPort,
    proxyBootLines,
    proxyIdleRts,
    proxyIdleCgroupBytes,
    proxySnapshot,
    proxyCgroupNow,
    proxyScrape,

    -- * Stopping
    ProxyEnd (..),
    stopProxy,

    -- * A drained process
    Drained,
    bootDrained,
) where

import Control.Concurrent (modifyMVar)
import Data.Aeson (eitherDecode)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
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
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, signalProcess)
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
import UnliftIO (bracket, onException, try, tryIO)
import UnliftIO.Async (Async, async, cancel, link, poll)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.BootLines (bootMessages)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Exposition (Sample, parseExposition)
import Ecluse.BenchLoad.Pod (CgroupReading (..), PodShape (Limited, Unlimited), counter, cpuMaxValue, keyedCounters, parsePodShape, renderPodShape)
import Ecluse.BenchLoad.RtsWindow (Collection (MajorCollection), RtsSnapshot, collectionName)
import Ecluse.BenchLoad.Verdict (ProxyEnding, classifyEnding)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Rts (parseMemoryMax, readIfExists)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Wai (freePort)

-- | The argument that makes this executable serve as the measured proxy.
serveProxyFlag :: String
serveProxyFlag = "--serve-proxy"

{- | The configuration a scenario gives its proxy. 'Nothing' leaves a bound to the boot's own
computation, which is what an unconfigured pod gets.
-}
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
    }

-- | Settings for one mount with the given cache TTL, every bound left to the boot.
proxySettings :: Ecosystem -> Int -> ProxySettings
proxySettings ecosystem ttl = ProxySettings ecosystem ttl Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
    , ppIdleRts :: Maybe RtsSnapshot
    , ppIdleCgroupBytes :: Maybe Int
    , ppEnd :: MVar (Maybe ProxyEnd)
    }

-- | The proxy's listening port on loopback.
proxyPort :: ProxyProcess -> Int
proxyPort = ppPort

-- | The runtime and admission lines the proxy logged at boot.
proxyBootLines :: ProxyProcess -> [Text]
proxyBootLines = ppBootLines

-- | RTS counters after a major collection on the booted, idle proxy: its idle floor.
proxyIdleRts :: ProxyProcess -> Maybe RtsSnapshot
proxyIdleRts = ppIdleRts

-- | The proxy cgroup's @memory.current@ at the idle floor.
proxyIdleCgroupBytes :: ProxyProcess -> Maybe Int
proxyIdleCgroupBytes = ppIdleCgroupBytes

-- | How the proxy ended and what its cgroup recorded.
data ProxyEnd = ProxyEnd
    { peEnding :: ProxyEnding
    , peExitedEarly :: Bool
    -- ^ The process had already exited when the harness came to stop it.
    , peStderrTail :: Text
    , peCgroup :: Maybe CgroupReading
    }

{- | Boot a proxy in front of the stub upstreams, run the action, then stop it. A boot that
exits or does not become ready within two minutes fails the harness with the proxy's stderr.
-}
withProxyProcess :: ProxySettings -> Int -> Maybe Int -> (ProxyProcess -> IO a) -> IO a
withProxyProcess settings publicPort privatePort body = do
    shape <- podShapeFromEnv
    root <- lookupEnv "BENCH_LOAD_CGROUP"
    withSystemTempDirectory "ecluse-bench-proxy" $ \dir ->
        bracket (acquireCgroup shape root) (traverse_ retireCgroup) $ \cgroup ->
            bracket (launch settings shape dir cgroup publicPort privatePort) (void . stopProxy) body

acquireCgroup :: PodShape -> Maybe FilePath -> IO (Maybe FilePath)
acquireCgroup shape root = case (shape, root) of
    (Unlimited, Nothing) -> pure Nothing
    (Limited _ _, Nothing) ->
        benchFail ("pod shape " <> renderPodShape shape <> " needs BENCH_LOAD_CGROUP: a cgroup v2 directory delegated to this user, with the cpu and memory controllers enabled")
    (_, Just base) -> do
        pid <- getProcessID
        let dir = base </> ("proxy-" <> show pid)
        createDirectory dir
        case shape of
            Unlimited -> pass
            Limited cpus bytes -> do
                writeFileText (dir </> "memory.max") (show bytes)
                swapFile <- doesFileExist (dir </> "memory.swap.max")
                when swapFile (writeFileText (dir </> "memory.swap.max") "0")
                writeFileText (dir </> "cpu.max") (cpuMaxValue cpus)
        pure (Just dir)

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

launch :: ProxySettings -> PodShape -> FilePath -> Maybe FilePath -> Int -> Maybe Int -> IO ProxyProcess
launch settings shape dir cgroup publicPort privatePort = do
    (port, controlPort, scrapePort) <- distinctPorts
    self <- getExecutablePath
    cores <- getNumCapabilities
    base <- getEnvironment
    let environment = proxyEnvironment settings shape cores dir (port, controlPort, scrapePort) publicPort privatePort base
        command = case cgroup of
            Nothing -> proc self [serveProxyFlag]
            -- The shell joins the cgroup and then becomes the proxy, so the boot already sees its limits.
            Just cg -> proc "/bin/sh" ["-c", "echo $$ > \"$0\" && exec \"$@\"", cg </> "cgroup.procs", self, serveProxyFlag]
    manager <- newManager defaultManagerSettings{managerResponseTimeout = responseTimeoutMicro 60_000_000}
    drained <- bootDrained manager 1200 port (setEnv environment command)
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
                , ppIdleRts = Nothing
                , ppIdleCgroupBytes = Nothing
                , ppEnd = endVar
                }
    (`onException` stopProxy booted) $ do
        -- The boot logged its plan before it listened. Give the drain a moment to catch up.
        bootLines <- pollUntil 50 100_000 (any ("memory plan:" `T.isPrefixOf`)) (bootMessages . BS8.lines . capturedHead <$> readIORef (drStdout drained))
        idle <- proxySnapshot booted MajorCollection
        idleCgroup <- proxyCgroupNow booted
        pure booted{ppBootLines = bootLines, ppIdleRts = idle, ppIdleCgroupBytes = crMemoryCurrent =<< idleCgroup}

-- Three distinct free ports: the proxy, its RTS control listener, and its scrape listener.
distinctPorts :: IO (Int, Int, Int)
distinctPorts = do
    a <- freePort
    b <- freePort
    c <- freePort
    if a /= b && b /= c && a /= c then pure (a, b, c) else distinctPorts

{- | The proxy's environment: the harness's own, less its RTS flags and any proxy configuration,
plus the scenario's. The upstreams are named over https for the configuration to accept them.
-}
proxyEnvironment :: ProxySettings -> PodShape -> Int -> FilePath -> (Int, Int, Int) -> Int -> Maybe Int -> [(String, String)] -> [(String, String)]
proxyEnvironment settings shape cores dir (port, controlPort, scrapePort) publicPort privatePort base =
    filter (inherited . fst) base <> map (bimap toString toString) (fixed <> pinned)
  where
    -- The proxy reads its whole configuration from here, so nothing of the harness's own leaks in.
    inherited key = key /= "GHCRTS" && not (any (`isPrefixOf` key) ["ECLUSE_", "OTEL_", "__ECLUSE"])
    mount = T.toUpper (ecosystemName (psEcosystem settings))
    upstream leg p = ("ECLUSE_MOUNTS__" <> mount <> "__" <> leg <> "__REGISTRY__URL", "https://localhost:" <> show p)
    fixed =
        [ ("ECLUSE_SERVER__PORT", show port)
        , ("ECLUSE_SERVER__PUBLIC_URL", "https://bench.proxy")
        , ("ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT", "10")
        , ("ECLUSE_ADVISORIES__DATA_DIR", toText (dir </> "advisories"))
        , ("ECLUSE_CACHE__TTL", show (psCacheTtlSeconds settings))
        , ("ECLUSE_OBSERVABILITY__TELEMETRY", "on")
        , ("OTEL_METRICS_EXPORTER", "prometheus")
        , ("OTEL_EXPORTER_PROMETHEUS_HOST", "127.0.0.1")
        , ("OTEL_EXPORTER_PROMETHEUS_PORT", show scrapePort)
        , ("OTEL_TRACES_EXPORTER", "none")
        , ("OTEL_LOGS_EXPORTER", "none")
        , ("BENCH_PROXY_CONTROL_PORT", show controlPort)
        , upstream "PUBLIC_UPSTREAM" publicPort
        ]
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
            , -- Unbounded, the harness's own capability count stands in for the missing CPU quota.
              ("ECLUSE_RUNTIME__CORES", show cores) <$ guard (shape == Unlimited)
            ]

-- | RTS counters from the proxy after the given collection. 'Nothing' once it has gone.
proxySnapshot :: ProxyProcess -> Collection -> IO (Maybe RtsSnapshot)
proxySnapshot proxy collection = do
    body <- getOk proxy (ppControlPort proxy) ("/rts?gc=" <> toString (collectionName collection))
    pure (body >>= rightToMaybe . eitherDecode)

-- | The proxy's cgroup files now, 'Nothing' when it runs outside a cgroup of its own.
proxyCgroupNow :: ProxyProcess -> IO (Maybe CgroupReading)
proxyCgroupNow = traverse readCgroup . ppCgroup

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
    pure (CgroupReading maxBytes peak current events stat cpu)
  where
    bytesAt file = (>>= parseMemoryMax) <$> readIfExists (dir </> file)

{- | Stop the proxy and read how it ended. SIGTERM starts its graceful drain. A process still
alive after thirty seconds is killed. The result is kept, so a second call returns it unchanged.
-}
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

{- | Start a process on drained pipes and wait, 100 ms per attempt, for @/readyz@ on the port. A
process that exits or never answers is stopped, and the harness fails with both streams' tails.
-}
bootDrained :: Manager -> Int -> Int -> ProcessConfig () () () -> IO Drained
bootDrained manager attempts port command = do
    process <- startProcess (setStdin nullStream (setStdout createPipe (setStderr createPipe command)))
    out <- newIORef emptyCaptured
    err <- newIORef emptyCaptured
    drains <- traverse startDrain [("stdout", getStdout process, out), ("stderr", getStderr process, err)]
    let drained = Drained process out err drains
    -- A drain failure or an interrupt during the wait must not leave the process behind.
    readiness <- pollUntil attempts 100_000 settled (probe drained) `onException` stopDrained drained
    case readiness of
        Ready -> pure drained
        ExitedDuringBoot code -> failBoot drained ("exited during boot with " <> show code)
        Booting -> failBoot drained ("did not become ready within " <> show (attempts `div` 10) <> " s")
  where
    -- A failed read would leave the pipe full and stall the proxy, so it fails the scenario at once.
    startDrain (stream, source, captured) = do
        worker <- async (drain stream source captured)
        link worker
        pure worker
    settled = \case
        Booting -> False
        _ -> True
    probe drained =
        getExitCode (drProcess drained) >>= \case
            Just code -> pure (ExitedDuringBoot code)
            Nothing -> bool Booting Ready . isJust <$> getFrom manager port "/readyz"
    failBoot drained reason = do
        void (stopDrained drained)
        errText <- capturedTailText (drStderr drained)
        logText <- capturedTailText (drStdout drained)
        benchFail ("bench-load: the proxy " <> reason <> "\nstderr:\n" <> errText <> "\nlog tail:\n" <> logText)

data Readiness = Booting | Ready | ExitedDuringBoot ExitCode

{- The waits poll rather than use 'timeout': a cleanup handler runs under 'uninterruptibleMask',
where a timeout cannot fire. -}
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
