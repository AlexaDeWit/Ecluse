-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Loopback upstreams and a separate measured proxy, with HTTP preflights before load.
module Ecluse.BenchLoad.Fixture (
    withProxyOverStubs,
    withProxyConfigured,
    withProxyPortsConfigured,
    ProxyConfigure,
    withLoopbackStub,
    httpTarget,
    longCacheTtl,
    artifactBytes,
    loadCorpusBodies,
    loadCorpusCuts,
    weightedMix,
    selfHosted,
    primeETag,
    fetchChecked,
    benchNow,
    delayUpstream,
    absentUpstream,
    selectPinnedArtifacts,
    rewriteLines,
    swapChecksumPreservingWidth,
    preflightDistinct,
    probeLiveFetch,
) where

import Control.Concurrent (threadDelay)

import Data.Aeson (Key, Value, encode, (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Streaming.Network (bindPortTCP, bindRandomPortTCP)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import GHC.IO.Exception (IOErrorType (UnsupportedOperation))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (Header, Status, status200, status304, status404)
import Network.HTTP.Types.Header (hETag, hIfNoneMatch)
import Network.Socket (Socket, close, setCloseOnExecIfNeeded, withFdSocket)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (Port, defaultSettings, defaultShouldDisplayException, runSettingsSocket)
import System.IO.Error (ioeGetErrorType, isAlreadyInUseError)
import UnliftIO (bracket, catch, evaluate, onException, throwIO, tryIO)
import UnliftIO.Async (link, mapConcurrently_, withAsync)
import UnliftIO.Concurrent (myThreadId, throwTo)

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (Driver (DriveHttp), LoadKnobs (..), Target, proxied, urlLoad)
import Ecluse.BenchLoad.Normalise (LiveFetch (LiveFetch, NoLiveFetch))
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess, ProxySettings (..), proxyPort, proxySettings, withProxyProcess)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Package (PackageName, pkgEcosystem)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), cpName)
import Ecluse.Test.Corpus qualified as Corpus
import Ecluse.Test.EcosystemBench (CorpusFraming (JsonFraming, NativeFraming), EcosystemBench (ebFraming), ecosystemBenchFor)
import Ecluse.Test.Wai (rebaseAuthority)

-- | Boot a proxy over two stubs with the given cache TTL and optional entry bound.
withProxyOverStubs :: Ecosystem -> LoadKnobs -> Int -> Maybe Int -> Application -> Application -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withProxyOverStubs ecosystem knobs ttl entries =
    withProxyConfigured ecosystem knobs (\_ -> pure (\settings -> settings{psCacheTtlSeconds = ttl, psCacheMaxEntries = entries}))

-- | 'withProxyOverStubs' with settings derived once the public stub's port is known.
withProxyConfigured :: Ecosystem -> LoadKnobs -> (Int -> IO (ProxySettings -> ProxySettings)) -> Application -> Application -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withProxyConfigured ecosystem knobs configure = withProxyPortsConfigured ecosystem knobs (const configure)

-- | Settings derived from the private and the public registry port, in that order, before the proxy boots.
type ProxyConfigure = Int -> Int -> IO (ProxySettings -> ProxySettings)

-- | Configure a measured proxy after both registry ports are available.
withProxyPortsConfigured :: Ecosystem -> LoadKnobs -> ProxyConfigure -> Application -> Application -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withProxyPortsConfigured ecosystem knobs configure privateApp publicApp mkMix body =
    withLoopbackStub privateApp $ \privatePort ->
        withLoopbackStub publicApp $ \publicPort -> do
            adjust <- configure privatePort publicPort
            withProxyProcess (adjust knobSettings) publicPort (Just privatePort) $ \proxy ->
                body proxy (mkMix (proxyPort proxy))
  where
    knobSettings =
        (proxySettings ecosystem 60)
            { psServeMaxInFlight = lkServeMaxInFlight knobs
            , psPublicConnections = lkPublicConnectionsPerHost knobs
            , psPrivateConnections = lkPrivateConnectionsPerHost knobs
            , psAdvisories = lkAdvisories knobs
            , psTuning = lkProxyTuning knobs
            }

{- | Serve a stub on one free port at 127.0.0.1 and, when the host has an IPv6 loopback, at ::1, since http-client
dials the second address of a name such as @localhost@ 250 ms after the first. Application exceptions reach the caller.
-}
withLoopbackStub :: Application -> (Port -> IO a) -> IO a
withLoopbackStub app use = do
    caller <- myThreadId
    let reported request respond =
            app request respond `catch` \(failure :: SomeException) -> do
                when (defaultShouldDisplayException failure) (throwTo caller failure)
                throwIO failure
    bracket loopbackListeners (traverse_ close . snd) $ \(port, listeners) -> do
        -- The measured proxy is a child process, so it must not inherit a listener.
        traverse_ (`withFdSocket` setCloseOnExecIfNeeded) listeners
        withAsync (mapConcurrently_ (\listener -> runSettingsSocket defaultSettings listener reported) listeners) $ \server ->
            link server >> use port

-- One free port at 127.0.0.1, bound at ::1 as well unless the host has no IPv6 loopback.
loopbackListeners :: IO (Port, [Socket])
loopbackListeners = do
    (port, ipv4) <- bindRandomPortTCP "127.0.0.1"
    (tryIO (bindPortTCP port "::1") `onException` close ipv4) >>= \case
        Right ipv6 -> pure (port, [ipv4, ipv6])
        Left err
            | ioeGetErrorType err == UnsupportedOperation -> pure (port, [ipv4])
            | isAlreadyInUseError err -> close ipv4 >> loopbackListeners
            | otherwise -> close ipv4 >> throwIO err

-- | The target for a duration-driven load over the URL mix.
httpTarget :: (Target -> IO a) -> ProxyProcess -> [Text] -> IO a
httpTarget k proxy = k . proxied proxy . DriveHttp . urlLoad

-- | Keep entries alive throughout warm-up and measurement, leaving eviction as the tested axis.
longCacheTtl :: Int
longCacheTtl = 3600

-- | A payload-sized body shared by artifact relays and integrity verification.
artifactBytes :: Int -> LByteString
artifactBytes size = LBS.replicate (fromIntegral (max 1 size)) 0x61

-- | Authenticate the selected corpus, refusing empty captures before starting load.
loadCorpusBodies :: [CorpusPackage] -> IO (Map Text LByteString)
loadCorpusBodies packages = Map.fromList <$> traverse (\cp -> (cpName cp,) <$> readCapture cp) packages

-- | 'loadCorpusBodies' with each capture cut and encoded before load, refusing a capture the cut rejects.
loadCorpusCuts :: (PackageName -> ByteString -> Either String Value) -> [CorpusPackage] -> IO (Map Text LByteString)
loadCorpusCuts cut packages = Map.fromList <$> traverse load packages
  where
    load cp = do
        bytes <- readCapture cp
        document <- either (\reason -> benchFail ("bench-load: cannot cut " <> toText (cpPath cp) <> ": " <> toText reason)) pure (cut (cpPackage cp) (toStrict bytes))
        provider <- maybe (benchFail ("bench-load: no corpus provider for " <> toText (cpPath cp))) pure (ecosystemBenchFor (pkgEcosystem (cpPackage cp)))
        body <- case ebFraming provider of
            JsonFraming -> evaluate (toStrict (encode document))
            NativeFraming _ encodeValues -> either benchFail evaluate (encodeValues document)
        pure (cpName cp, toLazy body)

-- | Each package's URL on the proxy's port, repeated by its weight.
weightedMix :: (CorpusPackage -> Int) -> (Int -> Text -> Text) -> [CorpusPackage] -> Int -> [Text]
weightedMix weight url packages port = concatMap (\cp -> replicate (weight cp) (url port (cpName cp))) packages

readCapture :: CorpusPackage -> IO LByteString
readCapture cp = do
    bytes <- toLazy <$> Corpus.readCapture cp
    when (LBS.null bytes) (benchFail ("bench-load: corpus capture is empty: " <> toText (cpPath cp)))
    pure bytes

-- | Rebase captured artifact URLs once per stub, outside repeated metadata responses.
selfHosted :: Text -> IORef (Map Text LByteString) -> Text -> Map Text LByteString -> IO (Map Text LByteString)
selfHosted capturedAuthority rewritten authority bodies =
    readIORef rewritten >>= \case
        cached | not (Map.null cached) -> pure cached
        _ -> do
            let served = Map.map (rebaseAuthority capturedAuthority authority) bodies
            writeIORef rewritten served
            pure served

-- | Require a successful priming GET followed by an empty 304 for its served validator.
primeETag :: Text -> IO Text
primeETag url = do
    response <- fetchChecked status200 [] url
    tag <- maybe (benchFail "revalidate-not-modified: the priming GET returned no ETag") pure (List.lookup hETag (HTTP.responseHeaders response))
    conditional <- fetchChecked status304 [(hIfNoneMatch, tag)] url
    unless (LBS.null (HTTP.responseBody conditional)) (benchFail "revalidate-not-modified: the 304 response carried a body")
    pure (decodeUtf8 tag)

-- | Fetch without redirects and fail if the fixture serves an unexpected status.
fetchChecked :: Status -> [Header] -> Text -> IO (HTTP.Response LByteString)
fetchChecked expected headers url = do
    manager <- newManager defaultManagerSettings
    request <- HTTP.parseRequest (toString url)
    response <- HTTP.httpLbs request{HTTP.requestHeaders = headers, HTTP.redirectCount = 0, HTTP.checkResponse = \_ _ -> pass} manager
    unless (HTTP.responseStatus response == expected) $
        benchFail ("bench-load preflight " <> url <> ": expected " <> show expected <> ", got " <> show (HTTP.responseStatus response))
    pure response

-- | A fixed date the stub documents are dated against, so their ages never depend on the run date.
benchNow :: UTCTime
benchNow = UTCTime (fromGregorian 2026 6 1) 0

-- | Skip zero and negative delays while preserving each positive injected wait.
delayUpstream :: Int -> IO ()
delayUpstream latency = when (latency > 0) (threadDelay latency)

-- | An upstream that answers every request with a 404 after the injected latency.
absentUpstream :: Int -> Application
absentUpstream latency _ respond = delayUpstream latency >> respond (responseLBS status404 [] "missing")

-- | Resolve each package's workload pin in its capture, failing the run on a missing pin, capture or release.
selectPinnedArtifacts :: Key -> (CorpusPackage -> Text -> ByteString -> IO (Either Text SelectedArtifact)) -> [CorpusPackage] -> Map Text LByteString -> IO (Map Text SelectedArtifact)
selectPinnedArtifacts pinsKey select packages captures = do
    pins <- Corpus.readCorpusPins (.: pinsKey) >>= either (benchFail . toText) pure
    fmap Map.fromList . forM packages $ \package -> do
        let name = cpName package
            label = ecosystemName (pkgEcosystem (cpPackage package))
        version <- maybe (benchFail ("missing " <> label <> " workload pin: " <> name)) pure (Map.lookup name pins)
        bytes <- maybe (benchFail ("missing " <> label <> " capture: " <> name)) pure (Map.lookup name captures)
        artifact <- select package version (toStrict bytes) >>= either benchFail pure
        pure (name, artifact)

-- | Rewrite each LF-delimited line by its zero-based position, keeping the original framing byte for byte.
rewriteLines :: (Int -> ByteString -> Either Text ByteString) -> LByteString -> Either Text LByteString
rewriteLines rewrite original = fromStrict . BS.intercalate "\n" <$> traverse (uncurry rewrite) (zip [0 ..] (BS.split 10 (toStrict original)))

-- | Replace one exact checksum field with a value of the same byte width, refusing an absent field.
swapChecksumPreservingWidth :: Text -> Text -> Text -> ByteString -> Either Text ByteString
swapChecksumPreservingWidth label before after line = do
    let raw = decodeUtf8 line
    when (T.null before || not (before `T.isInfixOf` raw)) (Left (label <> " relay fixture has no exact checksum field"))
    let replaced = encodeUtf8 (T.replace before after raw)
    unless (BS.length replaced == BS.length line) (Left (label <> " relay fixture changed its native wire width"))
    pure replaced

-- | Check each selected URL once before invoking the measurement callback.
preflightDistinct :: (Text -> IO ()) -> [Text] -> IO a -> IO a
preflightDistinct check urls body = traverse_ check (ordNub urls) >> body

-- | Warm one native connection, then time each successful fetch. 'NoLiveFetch' when the probe is off or none succeeded.
probeLiveFetch :: Bool -> [Text] -> (Text -> IO (Maybe Double)) -> IO LiveFetch
probeLiveFetch enabled names fetch =
    case (enabled, names) of
        (True, warm : _) ->
            fetch warm >>= \case
                Nothing -> pure NoLiveFetch
                Just _ -> do
                    samples <- catMaybes <$> traverse fetch names
                    pure $ case nonEmpty samples of
                        Nothing -> NoLiveFetch
                        Just values -> LiveFetch (fromIntegral (round (sum values / fromIntegral (length values)) :: Int)) (length values)
        _ -> pure NoLiveFetch
