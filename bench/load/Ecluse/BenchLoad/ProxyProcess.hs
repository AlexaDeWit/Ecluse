-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Start, observe, and stop the proxy process a scenario measures. The proxy is this executable
under 'serveProxyFlag', configured through @ECLUSE_*@ variables as a deployment would be. Under a
pod shape it runs in its own child of the cgroup named by @BENCH_LOAD_CGROUP@, so the limit bounds
it alone, and that cgroup outlives the process, so an OOM kill stays readable after it.
-}
module Ecluse.BenchLoad.ProxyProcess (
    -- * Configuration
    ProxySettings (..),
    proxySettings,
    podShapeFromEnv,
    serveProxyFlag,
    proxyEnvironment,

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
import System.Directory (createDirectory, doesFileExist, removeDirectory)
import System.Environment (getEnvironment, getExecutablePath)
import System.FilePath ((</>))
import System.IO (SeekMode (SeekFromEnd), hClose, hFileSize, hSeek, openBinaryFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (getPid, terminateProcess)
import System.Process.Typed (
    ExitCode (ExitFailure, ExitSuccess),
    Process,
    getExitCode,
    nullStream,
    proc,
    setEnv,
    setStderr,
    setStdin,
    setStdout,
    startProcess,
    stopProcess,
    unsafeProcessHandle,
    useHandleOpen,
    waitExitCode,
 )
import UnliftIO (bracket, onException, timeout, try, tryIO, tryJust)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.BootLines (bootMessages)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Exposition (Sample, parseExposition)
import Ecluse.BenchLoad.Pod (CgroupReading (..), PodShape (Limited, Unlimited), counter, cpuMaxValue, keyedCounters, parsePodShape, renderPodShape)
import Ecluse.BenchLoad.RtsWindow (Collection (MajorCollection), RtsSnapshot, collectionName)
import Ecluse.BenchLoad.Verdict (ProxyEnding, classifyEnding)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Rts (parseMemoryMax)
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
    , ppProcess :: Process () () ()
    , ppDirectory :: FilePath
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
        bracket (acquireCgroup shape root) (traverse_ releaseCgroup) $ \cgroup ->
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

releaseCgroup :: FilePath -> IO ()
releaseCgroup dir =
    tryIO (removeDirectory dir) >>= \case
        Right () -> pass
        Left err -> TIO.hPutStrLn stderr ("bench-load: could not remove the proxy cgroup " <> toText dir <> ": " <> show err)

launch :: ProxySettings -> PodShape -> FilePath -> Maybe FilePath -> Int -> Maybe Int -> IO ProxyProcess
launch settings shape dir cgroup publicPort privatePort = do
    (port, controlPort, scrapePort) <- distinctPorts
    self <- getExecutablePath
    cores <- getNumCapabilities
    base <- getEnvironment
    out <- openBinaryFile (dir </> "proxy.log") WriteMode
    err <- openBinaryFile (dir </> "proxy.err") WriteMode
    let environment = proxyEnvironment settings shape cores dir (port, controlPort, scrapePort) publicPort privatePort base
        command = case cgroup of
            Nothing -> proc self [serveProxyFlag]
            -- The shell joins the cgroup and then becomes the proxy, so the boot already sees its limits.
            Just cg -> proc "/bin/sh" ["-c", "echo $$ > \"$0\" && exec \"$@\"", cg </> "cgroup.procs", self, serveProxyFlag]
    process <- startProcess (setEnv environment (setStdin nullStream (setStdout (useHandleOpen out) (setStderr (useHandleOpen err) command))))
    -- The child holds its own descriptors. GHC locks a file this process has open for writing
    -- against its own readers, so the harness closes its copies before it reads the logs.
    hClose out
    hClose err
    manager <- newManager defaultManagerSettings{managerResponseTimeout = responseTimeoutMicro 60_000_000}
    endVar <- newMVar Nothing
    let booting =
            ProxyProcess
                { ppPort = port
                , ppControlPort = controlPort
                , ppScrapePort = scrapePort
                , ppProcess = process
                , ppDirectory = dir
                , ppCgroup = cgroup
                , ppManager = manager
                , ppBootLines = []
                , ppIdleRts = Nothing
                , ppIdleCgroupBytes = Nothing
                , ppEnd = endVar
                }
    (`onException` stopProxy booting) $ do
        awaitReady booting
        bootLines <- bootMessages . BS8.lines <$> readHead (dir </> "proxy.log")
        idle <- proxySnapshot booting MajorCollection
        idleCgroup <- proxyCgroupNow booting
        pure booting{ppBootLines = bootLines, ppIdleRts = idle, ppIdleCgroupBytes = crMemoryCurrent =<< idleCgroup}

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

data Readiness = Booting | Ready | ExitedDuringBoot ExitCode

awaitReady :: ProxyProcess -> IO ()
awaitReady proxy =
    pollUntil 1200 100_000 settled probe >>= \case
        Ready -> pass
        ExitedDuringBoot code -> failBoot ("exited during boot with " <> show code)
        Booting -> failBoot "did not become ready within two minutes"
  where
    settled = \case
        Booting -> False
        _ -> True
    probe =
        getExitCode (ppProcess proxy) >>= \case
            Just code -> pure (ExitedDuringBoot code)
            Nothing -> do
                answered <- getOk proxy (ppPort proxy) "/readyz"
                pure (if isJust answered then Ready else Booting)
    failBoot reason = do
        errText <- tailOf (ppDirectory proxy </> "proxy.err") 4_096
        logText <- tailOf (ppDirectory proxy </> "proxy.log") 4_096
        benchFail ("bench-load: the proxy " <> reason <> "\nstderr:\n" <> errText <> "\nlog tail:\n" <> logText)

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
getOk proxy port path = do
    request <- parseRequest ("http://127.0.0.1:" <> show port <> path)
    outcome <- try (httpLbs request (ppManager proxy))
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
    cpu <- maybe mempty keyedCounters <$> readIfExists (dir </> "cpu.stat")
    pure (CgroupReading maxBytes peak current events cpu)
  where
    bytesAt file = (>>= parseMemoryMax) <$> readIfExists (dir </> file)

readIfExists :: FilePath -> IO (Maybe Text)
readIfExists path = rightToMaybe <$> tryJust (guard . isDoesNotExistError) (decodeUtf8 <$> readFileBS path)

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
    let process = ppProcess proxy
    alreadyExited <- getExitCode process
    (code, harnessKilled) <- case alreadyExited of
        Just code -> pure (code, False)
        Nothing -> do
            terminateProcess (unsafeProcessHandle process)
            timeout 30_000_000 (waitExitCode process) >>= \case
                Just code -> pure (code, False)
                Nothing -> do
                    getPid (unsafeProcessHandle process) >>= traverse_ (signalProcess sigKILL)
                    code <- waitExitCode process
                    pure (code, True)
    -- The process has exited, so this only releases what typed-process holds for it.
    stopProcess process
    errText <- tailOf (ppDirectory proxy </> "proxy.err") 4_096
    reading <- proxyCgroupNow proxy
    let oomKills = maybe 0 (counter "oom_kill" . crMemoryEvents) reading
        status = case code of
            ExitSuccess -> 0
            ExitFailure n -> n
    pure
        ProxyEnd
            { peEnding = classifyEnding status errText oomKills harnessKilled
            , peExitedEarly = isJust alreadyExited
            , peStderrTail = errText
            , peCgroup = reading
            }

-- The first mebibyte of a file: the boot lines precede any served request.
readHead :: FilePath -> IO ByteString
readHead path = withFile path ReadMode (`BS.hGetSome` 1_048_576)

-- The last bytes of a file as text, for a failure report.
tailOf :: FilePath -> Integer -> IO Text
tailOf path bytes =
    fmap (fromRight "") . tryIO . withFile path ReadMode $ \handle -> do
        size <- hFileSize handle
        when (size > bytes) (hSeek handle SeekFromEnd (negate bytes))
        decodeUtf8 <$> BS.hGetContents handle
