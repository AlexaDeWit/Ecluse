-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Loopback upstreams and a separate measured proxy, with HTTP preflights before load.
module Ecluse.BenchLoad.Fixture (
    withProxyOverStubs,
    withProxyConfigured,
    withProxyPortsConfigured,
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
    preflightDistinct,
    probeBaseline,
) where

import Control.Concurrent (threadDelay)

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (Header, Status, status200, status304)
import Network.HTTP.Types.Header (hETag, hIfNoneMatch)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication)
import UnliftIO (evaluate)

import Ecluse.BenchLoad.Cargo.CopyState (withCopyState)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (Driver (DriveHttp), LoadKnobs (..), Target, proxied, urlLoad)
import Ecluse.BenchLoad.Normalise (BaselineSource (InjectedFallback, MeasuredRtt))
import Ecluse.BenchLoad.ProxyProcess (AdvisoryFeed (afObject), ProxyProcess, ProxySettings (..), mirrorStateBucket, proxyPort, proxySettings, withProxyProcess)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, RubyGems))
import Ecluse.Core.Package (PackageName, pkgEcosystem)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), cpName)
import Ecluse.Test.Corpus qualified as Corpus
import Ecluse.Test.Corpus.Cargo (encodeCargoValues)
import Ecluse.Test.Corpus.RubyGems (encodeRubyGemsValues)
import Ecluse.Test.Wai (rebaseAuthority)

-- | Boot a proxy over two stubs with the given cache TTL and optional entry bound.
withProxyOverStubs :: Ecosystem -> LoadKnobs -> Int -> Maybe Int -> Application -> Application -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withProxyOverStubs ecosystem knobs ttl entries =
    withProxyConfigured ecosystem knobs (\_ -> pure (\settings -> settings{psCacheTtlSeconds = ttl, psCacheMaxEntries = entries}))

-- | 'withProxyOverStubs' with settings derived once the public stub's port is known.
withProxyConfigured :: Ecosystem -> LoadKnobs -> (Int -> IO (ProxySettings -> ProxySettings)) -> Application -> Application -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withProxyConfigured ecosystem knobs configure = withProxyPortsConfigured ecosystem knobs (const configure)

-- | Configure a measured proxy after both registry ports are available.
withProxyPortsConfigured :: Ecosystem -> LoadKnobs -> (Int -> Int -> IO (ProxySettings -> ProxySettings)) -> Application -> Application -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withProxyPortsConfigured ecosystem knobs configure privateApp publicApp mkMix body =
    testWithApplication (pure privateApp) $ \privatePort ->
        testWithApplication (pure publicApp) $ \publicPort -> do
            adjust <- configure privatePort publicPort
            let settings = adjust knobSettings
            withNativeState (afObject <$> psAdvisories settings) privatePort publicPort $ \nativeState ->
                withProxyProcess settings{psMirrorStatePort = psMirrorStatePort settings <|> nativeState} publicPort (Just privatePort) $ \proxy ->
                    body proxy (mkMix (proxyPort proxy))
  where
    withNativeState advisory privatePort publicPort use
        | ecosystem == Cargo = withCopyState (lkUpstreamLatencyMicros knobs) privatePort publicPort mirrorStateBucket advisory (use . Just)
        | otherwise = use Nothing
    knobSettings =
        (proxySettings ecosystem 60)
            { psServeMaxInFlight = lkServeMaxInFlight knobs
            , psPublicConnections = lkPublicConnectionsPerHost knobs
            , psPrivateConnections = lkPrivateConnectionsPerHost knobs
            , psAdvisories = lkAdvisories knobs
            , psTuning = lkProxyTuning knobs
            }

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
        body <- case pkgEcosystem (cpPackage cp) of
            Cargo -> either benchFail evaluate (encodeCargoValues document)
            RubyGems -> either benchFail evaluate (encodeRubyGemsValues document)
            _ -> evaluate (toStrict (encode document))
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

-- | Check each selected URL once before invoking the measurement callback.
preflightDistinct :: (Text -> IO ()) -> [Text] -> IO a -> IO a
preflightDistinct check urls body = traverse_ check (ordNub urls) >> body

-- | Warm one native connection, then measure successful fetches or declare the configured fallback.
probeBaseline :: Double -> Bool -> [Text] -> (Text -> IO (Maybe Double)) -> IO BaselineSource
probeBaseline configured enabled names fetch =
    case (enabled, names) of
        (True, warm : _) ->
            fetch warm >>= \case
                Nothing -> pure fallback
                Just _ -> do
                    samples <- catMaybes <$> traverse fetch names
                    pure $ case nonEmpty samples of
                        Nothing -> fallback
                        Just values -> MeasuredRtt (fromIntegral (round (sum values / fromIntegral (length values)) :: Int)) (length values)
        _ -> pure fallback
  where
    fallback = InjectedFallback configured
