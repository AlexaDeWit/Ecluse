-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Matched native and managed PyPI listings through the measured HTTP proxy and state transport.
module Ecluse.BenchLoad.ManagedMirror (managedMirrorScenarios) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentType, status200, status304, status404)
import Network.HTTP.Types.Header (hETag, hIfNoneMatch)
import Network.Wai (Application, pathInfo, rawPathInfo, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (delayUpstream, fetchChecked, httpTarget, longCacheTtl, withProxyPortsConfigured)
import Ecluse.BenchLoad.Harness (LoadKnobs (lkUpstreamLatencyMicros), Scenario (..), Target, scenario)
import Ecluse.BenchLoad.ProxyProcess (ProxySettings (..), mirrorStateBucket, privateStubHost)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State (stateTarget)
import Ecluse.Core.Mirror.State.Internal (projectKey)
import Ecluse.Core.Package (infoVersions, pkgArtifacts)
import Ecluse.Core.Registry.PyPI.Wire (simpleIndexMediaType)
import Ecluse.Test.Mirror.Read (ReadFixture (..), makeReadFixtureFor, readLimits)
import Ecluse.Test.Mirror.State (mirrorPackage)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Wai (localhost, rebaseAuthority, selfBaseUrl)

data Mode = Native | ManagedWarm | ManagedReload
    deriving stock (Eq)

-- | All rows use the same private payload, latency, concurrency, and pod limits.
managedMirrorScenarios :: [Scenario]
managedMirrorScenarios = map managedScenario [Native, ManagedWarm, ManagedReload]

managedScenario :: Mode -> Scenario
managedScenario mode =
    (scenario name description (boot mode))
        { scenarioPublicLegUnavailableReason = Just "This comparison includes private metadata and, for managed rows, a conditional state exchange. One public RTT does not isolate its service cost."
        }
  where
    (name, description) = case mode of
        Native -> ("mirror-native-baseline", "GET the private 1000-version, three-file project plus an added file and an unmanaged version, without durable mirror state.")
        ManagedWarm -> ("mirror-managed-warm", "GET the same native project with a primed durable record. Every request validates state over HTTP with 304 and restores source fields before packed rendering.")
        ManagedReload -> ("mirror-managed-revision", "GET the same project after its record adds a file. Every state GET returns that complete record under a fresh opaque revision, exercising replacement decode and packed rendering.")

boot :: Mode -> LoadKnobs -> (Target -> IO a) -> IO a
boot mode knobs use = do
    fixture <- newIORef Nothing
    advanced <- newIORef False
    churn <- newIORef False
    counts <- newIORef Map.empty
    let latency = lkUpstreamLatencyMicros knobs
    testWithApplication (pure (stateApp latency fixture advanced churn counts)) $ \statePort ->
        withProxyPortsConfigured
            PyPI
            knobs
            ( \privatePort _ -> do
                input <- makeReadFixtureFor (stateTarget ("https://" <> privateStubHost <> ":" <> show privatePort)) 1000
                writeIORef fixture (Just input)
                pure (\settings -> settings{psCacheTtlSeconds = longCacheTtl, psMirrorStatePort = statePort <$ guard (mode /= Native)})
            )
            (privateApp latency fixture counts)
            (publicApp latency counts)
            (\port -> [localhost port <> "/pypi/simple/demo"])
            ( \proxy urls -> do
                url <- maybe (benchFail "managed mirror fixture has no proxy URL") pure (listToMaybe urls)
                preflight mode counts advanced churn url
                measured mode counts (httpTarget use proxy urls)
            )

preflight :: Mode -> IORef (Map Text Int) -> IORef Bool -> IORef Bool -> Text -> IO ()
preflight mode counts advanced churn url = do
    checked mode counts url (if mode == Native then 3004 else 3003)
    when (mode /= Native) $ do
        writeIORef advanced True
        checked mode counts url 3004
        checked mode counts url 3004
        totals <- readIORef counts
        unless (Map.findWithDefault 0 "state304" totals == 1 && Map.findWithDefault 0 "state200" totals == 2) (benchFail "managed mirror preflight did not exercise cold, changed, and 304 responses")
    writeIORef churn (mode == ManagedReload)

measured :: Mode -> IORef (Map Text Int) -> IO a -> IO a
measured mode counts action = do
    before <- readIORef counts
    result <- action
    after <- readIORef counts
    let delta = Map.unionWith (-) after before
    TIO.hPutStrLn stderr ("managed-mirror-http-counts " <> decodeUtf8 (LBS.toStrict (encode (object ["managed" .= (mode /= Native), "replacement" .= (mode == ManagedReload), "requests" .= delta]))))
    when (mode /= Native && Map.findWithDefault 0 "state" delta == 0) (benchFail "managed mirror load made no HTTP state requests")
    when (Map.findWithDefault 0 "state" delta > Map.findWithDefault 0 "private" delta) (benchFail "managed mirror load made more state exchanges than private requests")
    pure result

privateApp :: Int -> IORef (Maybe ReadFixture) -> IORef (Map Text Int) -> Application
privateApp latency fixture counts request respond = do
    delayUpstream latency
    _ <- increment counts "private"
    input <- requireFixture fixture
    respond $ case filter (not . T.null) (pathInfo request) of
        ["simple", "demo"] -> responseLBS status200 [(hContentType, simpleIndexMediaType)] (rebaseAuthority "https://private.test" (selfBaseUrl request) (LBS.fromStrict (fixturePrivate input)))
        _ -> responseLBS status404 [] ""

publicApp :: Int -> IORef (Map Text Int) -> Application
publicApp latency counts _ respond = do
    delayUpstream latency
    _ <- increment counts "public"
    respond (responseLBS status404 [] "")

stateApp :: Int -> IORef (Maybe ReadFixture) -> IORef Bool -> IORef Bool -> IORef (Map Text Int) -> Application
stateApp latency fixture advanced churn counts request respond = do
    delayUpstream latency
    sequenceNumber <- increment counts "state"
    input <- requireFixture fixture
    newer <- readIORef advanced
    changing <- readIORef churn
    let path = TE.encodeUtf8 ("/" <> mirrorStateBucket <> "/" <> projectKey (fixtureTarget input) mirrorPackage <> "/project.json")
        revision = TE.encodeUtf8 ("\"" <> (if changing then show sequenceNumber else if newer then "new" else "old") <> "\"")
        unchanged = lookup hIfNoneMatch (requestHeaders request) == Just revision
        body = if newer then fixtureNew input else fixtureOld input
    if rawPathInfo request /= path
        then respond (responseLBS status404 [] "")
        else
            if unchanged
                then increment counts "state304" >> respond (responseLBS status304 [(hETag, revision)] "")
                else increment counts "state200" >> respond (responseLBS status200 [(hETag, revision), (hContentType, "application/json")] (LBS.fromStrict body))

checked :: Mode -> IORef (Map Text Int) -> Text -> Int -> IO ()
checked mode counts url expected = do
    before <- readIORef counts
    response <- fetchChecked status200 [("Accept", simpleIndexMediaType)] url
    after <- readIORef counts
    let delta name = Map.findWithDefault 0 name after - Map.findWithDefault 0 name before
    unless (delta "private" == 1 && delta "state" == if mode == Native then 0 else 1) (benchFail "managed mirror preflight violated the one-state-exchange request budget")
    (info, _) <- either (benchFail . show) pure (projectPyPIIndex readLimits mirrorPackage (LBS.toStrict (HTTP.responseBody response)))
    unless (Map.size (infoVersions info) == 1001 && sum (map (length . pkgArtifacts) (Map.elems (infoVersions info))) == expected) (benchFail "managed mirror preflight lost a version or file")

requireFixture :: IORef (Maybe ReadFixture) -> IO ReadFixture
requireFixture fixture = readIORef fixture >>= maybe (benchFail "managed mirror fixture was requested before configuration") pure

increment :: IORef (Map Text Int) -> Text -> IO Int
increment counts name = atomicModifyIORef' counts $ \current ->
    let next = Map.findWithDefault 0 name current + 1
     in (Map.insert name next current, next)
