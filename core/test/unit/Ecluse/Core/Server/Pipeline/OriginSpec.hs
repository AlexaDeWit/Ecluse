-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Origin outcomes preserve authority, while selected flights share their admission priority.
module Ecluse.Core.Server.Pipeline.OriginSpec (spec) where

import Data.Aeson (Value (Array, Object, String), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Network.HTTP.Types (status200)
import System.Mem (performGC)
import System.Mem.Weak (Weak, deRefWeak, mkWeak)
import Test.Hspec
import UnliftIO (timeout, wait, withAsync)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Registry.Adapter (RegistryAdapter, adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Credential (cargoCredential)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Route (cargoRouter)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataAbsent), VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch (readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (PrepareRead))
import Ecluse.Core.Registry.Metadata.Fetch.Types (ReadTerms (ReadTerms))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, MeterSettings (..), charge, meterSnapshot, newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Cache (CacheConfig (cacheMaxEntries), MetadataCache, Source (Source), newMetadataCacheWithProvider, resolveVersion)
import Ecluse.Core.Server.Cache.Backend (BackendStorage (LocalStorage), RetentionOperations (roInsert))
import Ecluse.Core.Server.Cache.Provider (cacheProvider)
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Server.Context (MountBinding (..), PackumentDeps (..), RequestCtx (RequestCtx), ServeRuntime (srMemoryMeter, srMetadataCache), pdPublicBaseUrl, runHandler)
import Ecluse.Core.Server.Pipeline.Origin (
    OriginMiss (MissAbsent, MissUnresolved),
    OriginResult (
        OriginAbsent,
        OriginAuthorisationFailure,
        OriginNameMismatch,
        OriginNotFound,
        OriginResolved,
        OriginUnresolved
    ),
    originManifest,
    originMiss,
    withPublicMetadata,
 )
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpVersionCacheRequest, mpVersionCacheResidentBytes))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Package (sampleManifest, thingName)
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)
import Ecluse.Test.Registry.Cargo.Metadata (cargoRead, cargoTestDiscovery, cargoTestName, cargoTestRecord, cargoTestVersion)
import Ecluse.Test.Registry.Metadata.Fetch (heldBody)
import Ecluse.Test.Server.Cache (cachedVersion, defaultCacheConfig, newLocalRetention, newMetadataCache)
import Ecluse.Test.Server.Handler (testServeRuntime)
import Ecluse.Test.Server.Mount (inertDepsFor, inertPackumentDeps, serveDepsFor)
import Ecluse.Test.Stub (Captured (capPath), stubBaseUrl, withRoutedStubIO)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "originMiss -- the outcomes that leave nothing to serve" $ do
    retainedConsumerSpec
    it "reports no miss for a document, an access refusal, or an identity refusal" $
        map
            originMiss
            [ OriginResolved (sampleManifest thingName [])
            , OriginAuthorisationFailure 401
            , OriginAuthorisationFailure 403
            , OriginNameMismatch
            ]
            `shouldBe` [Nothing, Nothing, Nothing, Nothing]

    it "separates an origin that answered 404 from one that was never read" $
        map originMiss [OriginNotFound, OriginUnresolved]
            `shouldBe` [Just MissAbsent, Just MissUnresolved]

    it "groups an unconfigured origin with an absence, because no retry reaches it" $
        originMiss OriginAbsent `shouldBe` Just MissAbsent

    it "contributes no manifest from any miss" $
        map (isNothing . originManifest) [OriginNotFound, OriginUnresolved, OriginAbsent]
            `shouldBe` [True, True, True]

retainedConsumerSpec :: Spec
retainedConsumerSpec = describe "successful selected consumers" $ do
    it "keeps a local Cargo hit charged after cache eviction and upstream absence" $ withSweeper $ \deadlines -> do
        adapter <- requiredCargoAdapter
        let name = cargoTestName
            version = cargoTestVersion "1.0.0"
            replacement = cargoTestVersion "2.0.0"
            absent = VersionRead Nothing 0 Nothing
            config = defaultCacheConfig{cacheMaxEntries = 1}
        cache <- newMetadataCache config
        resident <- newIORef (0 :: Int)
        misses <- newIORef (0 :: Int)
        let metrics = noopMetricsPort{mpVersionCacheResidentBytes = writeIORef resident}
        original <- testServeRuntime deadlines metrics
        meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = maxBound `div` 2, msStepBytes = 1, msEntryRoom = 0, msEntryWaitMicros = 0}
        let base = inertDepsFor adapter
            factory = PrepareRead (\_ _ -> modifyIORef' misses (+ 1) $> Left MetadataAbsent)
            deps = base{pdMetadata = (pdMetadata base){metadataRead = factory}}
            source = Source (registryUrlText (pdPublicBaseUrl deps))
            runtime = original{srMemoryMeter = meter, srMetadataCache = cache}
        (expected, roots) <- seedRetainedCargo metrics cache source
        completed <- withMemoryEntry metrics meter $ \ticket ->
            runCargoConsumer runtime deps ticket $ \received -> do
                received `shouldBe` expected
                let required = weighVersion received
                required `shouldSatisfy` (> 0)
                snChargedBytes <$> meterSnapshot meter `shouldReturn` required
                resolveVersion metrics cache source name replacement (pure (Right absent)) `shouldReturn` Right absent
                cachedVersion metrics cache source name version `shouldReturn` Nothing
                unavailable <- withMemoryEntry metrics meter (\next -> runCargoConsumer runtime deps next pure)
                unavailable `shouldBe` Right (Left MetadataAbsent)
                readIORef resident >>= (`shouldSatisfy` (< required))
                meterSnapshot meter >>= (`shouldSatisfy` (>= required)) . snChargedBytes
                assertCargoReleased roots
                weighVersion received `shouldBe` required
        completed `shouldBe` Right (Right ())
        readIORef misses `shouldReturn` 1
        snChargedBytes <$> meterSnapshot meter `shouldReturn` 0
    it "does not pin a raw cache row in the deferred serving continuation" $ withSweeper $ \deadlines -> do
        adapter <- requiredCargoAdapter
        cache <- newMetadataCache defaultCacheConfig{cacheMaxEntries = 1}
        original <- testServeRuntime deadlines noopMetricsPort
        let base = inertDepsFor adapter
            factory = PrepareRead (\_ _ -> pure (Left MetadataAbsent))
            deps = base{pdMetadata = (pdMetadata base){metadataRead = factory}}
            source = Source (registryUrlText (pdPublicBaseUrl deps))
            runtime = original{srMetadataCache = cache}
        (_, roots) <- seedRetainedCargo noopMetricsPort cache source
        completed <- withMemoryEntry noopMetricsPort (srMemoryMeter runtime) $ \ticket ->
            runCargoRead runtime deps ticket $ \readSelected -> do
                let replacement = VersionRead Nothing 0 Nothing
                resolveVersion noopMetricsPort cache source cargoTestName (cargoTestVersion "2.0.0") (pure (Right replacement)) `shouldReturn` Right replacement
                assertCargoReleased roots
                readSelected `shouldReturn` Left MetadataAbsent
        completed `shouldBe` Right ()
    for_ [False, True] $ \holdsToken ->
        it (if holdsToken then "completes a successful oversized Cargo flight when the follower holds the overdraw token" else "keeps an oversized successful Cargo follower charged after its leader exits") $ withSweeper $ \deadlines -> do
            adapter <- requiredCargoAdapter
            (body, _, expected) <- retainedCargoFixture
            started <- newEmptyMVar
            releaseSource <- newEmptyMVar
            followerJoined <- newEmptyMVar
            followerEntered <- newEmptyMVar
            allowFollower <- newEmptyMVar
            followerHolding <- newEmptyMVar
            releaseFollower <- newEmptyMVar
            sourceReads <- newIORef (0 :: Int)
            let reply captured
                    | capPath captured == "/config.json" = pure (status200, [], "{\"dl\":\"https://static.crates.io/crates/{crate}/{crate}-{version}.crate\"}")
                    | otherwise = do
                        modifyIORef' sourceReads (+ 1)
                        putMVar started ()
                        takeMVar releaseSource
                        pure (status200, [], toLazy body)
                metrics = noopMetricsPort{mpVersionCacheRequest = \result -> when (result == Metric.Collapsed) (putMVar followerJoined ())}
            withRoutedStubIO reply $ \stub -> do
                original <- testServeRuntime deadlines metrics
                observedRoots <- newIORef Nothing
                operations <- newLocalRetention 60 1 1 weighVersion
                let observe record refused key value = do
                        roots <- cargoRoots value
                        writeIORef observedRoots (Just roots)
                        assertCargoRetained roots
                        roInsert operations record refused key value
                    provider = cacheProvider LocalStorage Nothing (Just operations{roInsert = observe}) Nothing
                cache <- newMetadataCacheWithProvider provider
                let step = 1024 * 1024
                    budget = if holdsToken then 3 * step else maxBound `div` 2
                meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = budget, msStepBytes = step, msEntryRoom = 0, msEntryWaitMicros = 0}
                let runtime = original{srMemoryMeter = meter, srMetadataCache = cache}
                    deps = serveDepsFor adapter Nothing (loopbackRegistryUrl (stubBaseUrl stub)) NoMirrorWrite [] (pdNow inertPackumentDeps)
                    source = Source (registryUrlText (pdPublicBaseUrl deps))
                    required = weighVersion expected
                    leader = withMemoryEntry metrics meter $ \ticket -> runCargoConsumer runtime deps ticket (pure . weighVersion)
                    follower = withMemoryEntry metrics meter $ \ticket -> do
                        putMVar followerEntered ()
                        takeMVar allowFollower
                        when holdsToken (charge ticket (4 * step))
                        runCargoConsumer runtime deps ticket $ \received -> do
                            meterSnapshot meter >>= (`shouldSatisfy` (>= required)) . snChargedBytes
                            unless holdsToken $ do
                                putMVar followerHolding ()
                                takeMVar releaseFollower
                            received `shouldBe` expected
                            pure (weighVersion received)
                completed <- timeout 5_000_000 $
                    withAsync follower $ \following -> do
                        takeMVar followerEntered
                        withAsync leader $ \leading -> do
                            takeMVar started
                            putMVar allowFollower ()
                            takeMVar followerJoined
                            putMVar releaseSource ()
                            if holdsToken
                                then (,) <$> wait leading <*> wait following
                                else do
                                    leadResult <- wait leading
                                    takeMVar followerHolding
                                    meterSnapshot meter >>= (`shouldSatisfy` (>= required)) . snChargedBytes
                                    cachedVersion metrics cache source cargoTestName (cargoTestVersion "1.0.0") `shouldReturn` Nothing
                                    assertObservedReleased observedRoots
                                    putMVar releaseFollower ()
                                    (leadResult,) <$> wait following
                completed `shouldBe` Just (Right (Right required), Right (Right required))
                assertObservedReleased observedRoots
                readIORef sourceReads `shouldReturn` 1
                snChargedBytes <$> meterSnapshot meter `shouldReturn` 0

retainedCargoFixture :: IO (ByteString, VersionRead, VersionRead)
retainedCargoFixture = do
    let row = case cargoTestRecord "1.0.0" of
            Object fields -> Object (KeyMap.insert "features" (object ["large" .= [T.replicate (2 * 1024 * 1024) "x"]]) fields)
            value -> value
        body = toStrict (encode row)
    discovery <- expectRight cargoTestDiscovery
    selected <- expectRight =<< readVersion (cargoRead discovery) passthroughTracingPort (ReadTerms defaultLimits "https://index.crates.io" (const pass)) cargoTestName (cargoTestVersion "1.0.0") (heldBody [body])
    let served = selected{vrVersion = fmap (\document -> document{vdRaw = Nothing}) (vrVersion selected)}
    pure (body, selected, served)

requiredCargoAdapter :: IO RegistryAdapter
requiredCargoAdapter = maybe (fail "Cargo has no registered adapter") pure (adapterFor Cargo)

runCargoConsumer :: ServeRuntime -> PackumentDeps -> MemoryTicket -> (VersionRead -> IO a) -> IO (Either MetadataError a)
runCargoConsumer runtime deps ticket consume =
    runCargoRead runtime deps ticket (>>= traverse consume)

runCargoRead :: ServeRuntime -> PackumentDeps -> MemoryTicket -> (IO (Either MetadataError VersionRead) -> IO a) -> IO a
runCargoRead runtime deps ticket consume = do
    logEnv <- newTestLogEnv
    let binding = MountBinding ("cargo" :| []) (cargoRouter Nothing) cargoCredential deps Nothing
    runHandler logEnv mempty (RequestCtx runtime binding) $
        withPublicMetadata runtime deps ticket cargoTestName (cargoTestVersion "1.0.0") (liftIO . consume)

data CargoRoots = CargoRoots !(Weak ()) !(Weak ())

cargoRoots :: VersionRead -> IO CargoRoots
cargoRoots selected = do
    source <- maybe (fail "selected Cargo source is absent") pure (vrVersion selected >>= vdRaw >>= snd cargoCached)
    value <- maybe (fail "selected Cargo row is absent") (pure . snd) (listToMaybe (cargoRecords source))
    payload <- case value of
        Object fields -> case KeyMap.lookup "features" fields of
            Just (Object features) -> case KeyMap.lookup "large" features of
                Just (Array values) -> case toList values of
                    [feature@(String _)] -> pure feature
                    _ -> fail "large feature payload is absent"
                _ -> fail "large feature array is absent"
            _ -> fail "native features are absent"
        _ -> fail "native Cargo row is not an object"
    CargoRoots <$> track value <*> track payload
  where
    track key = evaluate key >>= \evaluated -> mkWeak evaluated () Nothing

assertCargoRetained :: CargoRoots -> Expectation
assertCargoRetained (CargoRoots row payload) = do
    performGC
    isJust <$> deRefWeak row `shouldReturn` True
    isJust <$> deRefWeak payload `shouldReturn` True

assertCargoReleased :: CargoRoots -> Expectation
assertCargoReleased (CargoRoots row payload) = do
    performGC
    isNothing <$> deRefWeak row `shouldReturn` True
    isNothing <$> deRefWeak payload `shouldReturn` True

assertObservedReleased :: IORef (Maybe CargoRoots) -> Expectation
assertObservedReleased observed =
    readIORef observed >>= maybe (expectationFailure "cache insertion did not observe a successful raw row") assertCargoReleased

seedRetainedCargo :: MetricsPort -> MetadataCache -> Source -> IO (VersionRead, CargoRoots)
seedRetainedCargo metrics cache source = do
    (_, selected, served) <- retainedCargoFixture
    weighVersion selected `shouldSatisfy` (> 1024 * 1024)
    roots <- cargoRoots selected
    resolveVersion metrics cache source cargoTestName (cargoTestVersion "1.0.0") (pure (Right selected)) `shouldReturn` Right selected
    assertCargoRetained roots
    pure (served, roots)
