-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Origin outcomes preserve authority, while selected flights share their admission priority.
module Ecluse.Core.Server.Pipeline.OriginSpec (spec) where

import Data.Aeson (Value (Array, Object, String), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Array.Byte (ByteArray (ByteArray))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Primitive.Array qualified as Primitive
import Data.Text qualified as T
import Data.Text.Internal qualified as Text
import Data.Text.Unsafe (lengthWord8)
import Data.Vector qualified as Vector
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Exts (UnliftedType, mkWeakNoFinalizer#)
import GHC.IO (IO (IO))
import GHC.Weak (Weak (Weak))
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Mem (performGC)
import System.Mem.Weak (deRefWeak)
import Test.Hspec
import UnliftIO (timeout, wait, withAsync)
import UnliftIO.Exception (bracket, evaluate, throwIO)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (PackageInfo (infoVersions))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.Adapter (RegistryAdapter, adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors, metadataRead))
import Ecluse.Core.Registry.CachedDocument (cargoCached, cargoPacked, cargoRendered)
import Ecluse.Core.Registry.Cargo.Credential (cargoCredential)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Route (cargoRouter)
import Ecluse.Core.Registry.Exchange (chargedRead)
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (MetadataAbsent), VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch (readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (PrepareRead), prepareRead)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (..), ReadTerms (ReadTerms))
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Npm.Route (npmRouter)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocChargeFullReadWorkspace))
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (maxVersionCount), defaultLimits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, MemoryTicket, MeterSettings (..), charge, meterSnapshot, newChargePart, newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille, cfKeptPermille), MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Cache (CacheConfig (cacheMaxEntries), MetadataCache, Source (Source), newMetadataCacheWithProvider, resolveVersion)
import Ecluse.Core.Server.Cache.Backend (BackendStorage (LocalStorage), RetentionOperations (roInsert))
import Ecluse.Core.Server.Cache.Provider (cacheProvider)
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Server.Context (MountBinding (..), PackumentDeps (..), RequestCtx (RequestCtx), ServeRuntime (srMemoryMeter, srMetadataCache), pdPublicBaseUrl, runHandler)
import Ecluse.Core.Server.Pipeline.Origin (
    ObservedReadBoundExceeded (ObservedReadBoundExceeded),
    OriginMiss (MissAbsent, MissUnresolved),
    OriginResult (
        OriginAbsent,
        OriginAuthorisationFailure,
        OriginBudgetExceeded,
        OriginNameMismatch,
        OriginNotFound,
        OriginResolved,
        OriginUnresolved
    ),
    fetchPrivateOriginObserved,
    fetchPublicOriginObserved,
    originManifest,
    originMiss,
    withPublicMetadata,
 )
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpCacheRequest, mpVersionCacheRequest, mpVersionCacheResidentBytes))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Package (leftpadName, sampleManifest, thingName)
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)
import Ecluse.Test.Registry.Cargo.Metadata (cargoFeatureRecord, cargoRead, cargoTestDiscovery, cargoTestName, cargoTestRecord, cargoTestVersion)
import Ecluse.Test.Registry.Metadata.Fetch (heldBody)
import Ecluse.Test.Server.Cache (cachedVersion, defaultCacheConfig, newLocalRetention, newMetadataCache)
import Ecluse.Test.Server.Handler (testServeRuntime)
import Ecluse.Test.Server.Mount (inertDepsFor, inertPackumentDeps, serveDepsFor, withPrivateBaseUrl)
import Ecluse.Test.Stub (Captured (capPath), stubBaseUrl, withRoutedStubIO)
import Ecluse.Test.Support (expectRight)

-- | Check origin outcomes, read charges and selected Cargo ownership.
spec :: Spec
spec = describe "originMiss -- the outcomes that leave nothing to serve" $ do
    observedReadSpec
    retainedConsumerSpec
    fullReadSpec
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

fullReadSpec :: Spec
fullReadSpec = describe "full Cargo read payments" $ do
    for_ [False, True] $ \public ->
        for_ [False, True] $ \refused ->
            it ((if public then "public" else "private") <> (if refused then " releases source and workspace after a refused read" else " retains only packed source after a full read")) $ withSweeper $ \deadlines -> do
                adapter <- requiredCargoAdapter
                let body = toStrict (encode (cargoFeatureRecord "1.0.0" 16384)) <> "\n" <> toStrict (encode (cargoTestRecord "2.0.0"))
                    reply captured
                        | capPath captured == "/config.json" = pure (status200, [], "{\"dl\":\"https://static.crates.io/crates/{crate}/{crate}-{version}.crate\"}")
                        | otherwise = pure (status200, [], toLazy body)
                withRoutedStubIO reply $ \stub -> do
                    original <- testServeRuntime deadlines noopMetricsPort
                    meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = maxBound `div` 2, msStepBytes = 1, msEntryRoom = 0, msEntryWaitMicros = 0}
                    sourcePaid <- newIORef (0 :: Int)
                    peak <- newIORef (0 :: Int)
                    balances <- newIORef ([] :: [Int])
                    let url = loopbackRegistryUrl (stubBaseUrl stub)
                        base = serveDepsFor adapter (Just url) url NoMirrorWrite [] (pdNow inertPackumentDeps)
                        factors = metadataChargeFactors (pdMetadata base)
                        observe = observingFullRead meter (cfFullReadPermille factors) sourcePaid peak balances
                        deps = base{pdMetadata = (pdMetadata base){metadataRead = observe (metadataRead (pdMetadata base))}, pdLimits = if refused then defaultLimits{maxVersionCount = 1} else defaultLimits}
                        runtime = original{srMemoryMeter = meter}
                    completed <- withMemoryEntry noopMetricsPort meter $ \ticket -> do
                        result <- runCargoFull runtime deps ticket public
                        if refused
                            then do
                                originMiss result `shouldBe` Just MissUnresolved
                                snChargedBytes <$> meterSnapshot meter `shouldReturn` 0
                            else do
                                manifest <- maybe (fail "full Cargo read returned no manifest") pure (originManifest result)
                                manifestBodyBytes manifest `shouldBe` BS.length body
                                Map.keys (infoVersions (manifestInfo manifest)) `shouldBe` ["1.0.0", "2.0.0"]
                                isJust (snd cargoPacked (manifestRaw manifest)) `shouldBe` True
                                snChargedBytes <$> meterSnapshot meter `shouldReturn` scaleCharge (cfKeptPermille factors) (BS.length body)
                        readIORef sourcePaid >>= (`shouldSatisfy` (> 0))
                        readIORef peak >>= (`shouldSatisfy` (> 16384 * 24))
                        readIORef balances >>= (`shouldSatisfy` (\observed -> not (null observed) && all (== 0) observed))
                    completed `shouldBe` Right ()
                    snChargedBytes <$> meterSnapshot meter `shouldReturn` 0
    it "charges a collapsed full flight with the overdraw owner's priority" $ withSweeper $ \deadlines -> do
        adapter <- requiredCargoAdapter
        started <- newEmptyMVar
        releaseSource <- newEmptyMVar
        followerEntered <- newEmptyMVar
        allowFollower <- newEmptyMVar
        followerJoined <- newEmptyMVar
        sourceReads <- newIORef (0 :: Int)
        let body = toStrict (encode (cargoFeatureRecord "1.0.0" 16384))
            reply captured
                | capPath captured == "/config.json" = pure (status200, [], "{\"dl\":\"https://static.crates.io/crates/{crate}/{crate}-{version}.crate\"}")
                | otherwise = do
                    modifyIORef' sourceReads (+ 1)
                    putMVar started ()
                    takeMVar releaseSource
                    pure (status200, [], toLazy body)
            metrics = noopMetricsPort{mpCacheRequest = \result -> when (result == Metric.Collapsed) (putMVar followerJoined ())}
        withRoutedStubIO reply $ \stub -> do
            original <- testServeRuntime deadlines metrics
            let step = 1024 * 1024
            meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = 3 * step, msStepBytes = step, msEntryRoom = 0, msEntryWaitMicros = 0}
            sourcePaid <- newIORef (0 :: Int)
            peak <- newIORef (0 :: Int)
            balances <- newIORef ([] :: [Int])
            let base = serveDepsFor adapter Nothing (loopbackRegistryUrl (stubBaseUrl stub)) NoMirrorWrite [] (pdNow inertPackumentDeps)
                observe = observingFullRead meter (cfFullReadPermille (metadataChargeFactors (pdMetadata base))) sourcePaid peak balances
                deps = base{pdMetadata = (pdMetadata base){metadataRead = observe (metadataRead (pdMetadata base))}}
                runtime = original{srMemoryMeter = meter}
                consume ticket = isJust . originManifest <$> runCargoFull runtime deps ticket True
                leader = withMemoryEntry metrics meter consume
                follower = withMemoryEntry metrics meter $ \ticket -> do
                    putMVar followerEntered ()
                    takeMVar allowFollower
                    charge ticket (4 * step)
                    consume ticket
            completed <- timeout 5_000_000 $ withAsync follower $ \following -> do
                takeMVar followerEntered
                withAsync leader $ \leading -> do
                    takeMVar started
                    putMVar allowFollower ()
                    takeMVar followerJoined
                    putMVar releaseSource ()
                    (,) <$> wait leading <*> wait following
            completed `shouldBe` Just (Right True, Right True)
            readIORef peak >>= (`shouldSatisfy` (> 16384 * 24))
            readIORef sourceReads `shouldReturn` 1
            readIORef balances >>= (`shouldSatisfy` (\observed -> not (null observed) && all (>= 0) observed))
            snChargedBytes <$> meterSnapshot meter `shouldReturn` 0

observingFullRead :: MemoryMeter -> Int -> IORef Int -> IORef Int -> IORef [Int] -> PrepareRead -> PrepareRead
observingFullRead meter factor sourcePaid peak balances factory = PrepareRead $ \tracing origin ->
    let recordSource amount = modifyIORef' sourcePaid (+ scaleCharge factor amount)
        source amount = ocChargeFullRead origin amount >> recordSource amount
        -- The fetch retains the original origin, so body payments bypass the preparation wrapper.
        observeBody EcosystemRead{erRequest, erUniqueFields, erWalkFull, erFinishFull, erWalkSelected, erFinishSelected} =
            EcosystemRead
                { erRequest
                , erUniqueFields
                , erWalkFull = \limits name base bound table next -> erWalkFull limits name base bound table (chargedRead recordSource next)
                , erFinishFull
                , erWalkSelected
                , erFinishSelected
                }
        workspace amount = do
            ocChargeFullReadWorkspace origin amount
            modifyIORef' peak (max amount)
            sourceHeld <- readIORef sourcePaid
            rowHeld <- readIORef peak
            held <- snChargedBytes <$> meterSnapshot meter
            modifyIORef' balances ((held - sourceHeld - rowHeld) :)
     in fmap observeBody <$> prepareRead factory tracing origin{ocChargeFullRead = source, ocChargeFullReadWorkspace = workspace}

runCargoFull :: ServeRuntime -> PackumentDeps -> MemoryTicket -> Bool -> IO OriginResult
runCargoFull runtime deps ticket public = do
    part <- newChargePart ticket
    logEnv <- newTestLogEnv
    let binding = MountBinding ("cargo" :| []) (cargoRouter Nothing) cargoCredential deps Nothing
    runHandler logEnv mempty (RequestCtx runtime binding) $
        if public
            then fetchPublicOriginObserved (const pass) deps runtime ticket part cargoTestName
            else fetchPrivateOriginObserved (const pass) deps runtime part Nothing cargoTestName

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
                let observe record refused key value = withCargoRoots value $ \rooted roots readOwner -> do
                        writeIORef observedRoots (Just roots)
                        assertCargoRetained roots readOwner
                        roInsert operations record refused key rooted
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
    void (evaluate (weighVersion served))
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

-- These keys observe native feature-array storage and payload bytes, not every map fragment.
data CargoRoots = CargoRoots
    { crContainerWeak :: Weak ()
    , crPayloadWeak :: Weak ()
    }

data CargoSnapshot = CargoSnapshot
    { csRow :: KeyMap.KeyMap Value
    , csValues :: Vector.Vector Value
    , csPayload :: Text
    , csRecordCount :: Int
    , csEntry :: EntryKey
    }

-- Keep registered graph values out of callers that later collect after release.
{-# NOINLINE withCargoRoots #-}
withCargoRoots :: VersionRead -> (VersionRead -> CargoRoots -> IO (Maybe VersionRead) -> IO a) -> IO a
withCargoRoots selected consume =
    -- Key extraction and insertion must use the same rooted graph.
    bracket (evaluate selected >>= newStablePtr) freeStablePtr $ \root -> do
        rooted <- deRefStablePtr root
        snapshot <- either (fail . toString) pure (cargoSnapshot rooted)
        containerWeak <- case Vector.toArraySlice (csValues snapshot) of
            (Primitive.Array values, _, _) -> track values
        payloadWeak <- case csPayload snapshot of
            Text.Text (ByteArray bytes) _ _ -> track bytes
        let roots = CargoRoots containerWeak payloadWeak
        consume rooted roots (Just <$> deRefStablePtr root)

-- Lifted wrappers can be reconstructed while their primitive storage stays reachable.
track :: forall (key :: UnliftedType). key -> IO (Weak ())
track key = IO $ \world0 ->
    case mkWeakNoFinalizer# key () world0 of
        (# next, weak #) -> (# next, Weak weak #)

cargoSnapshot :: VersionRead -> Either Text CargoSnapshot
cargoSnapshot selected = do
    raw <- maybeToRight "selected Cargo source is absent" (vrVersion selected >>= vdRaw)
    when (isJust (snd cargoPacked raw) || isJust (snd cargoRendered raw)) (Left "selected Cargo source is not tree-backed")
    source <- maybeToRight "selected Cargo source is absent" (snd cargoCached raw)
    (entry, value) <- maybeToRight "selected Cargo row is absent" (listToMaybe (cargoRecords source))
    row <- case value of
        Object fields -> Right fields
        _ -> Left "native Cargo row is not an object"
    values <- case KeyMap.lookup "features" row of
        Just (Object features) -> case KeyMap.lookup "large" features of
            Just (Array values) -> Right values
            _ -> Left "large feature array is absent"
        _ -> Left "native features are absent"
    payload <- case toList values of
        [String feature] -> Right feature
        _ -> Left "large feature payload is absent"
    pure (CargoSnapshot row values payload (length (cargoRecords source)) entry)

assertCargoRetained :: CargoRoots -> IO (Maybe VersionRead) -> Expectation
assertCargoRetained roots readOwner = do
    performGC
    containerAlive <- isJust <$> deRefWeak (crContainerWeak roots)
    payloadAlive <- isJust <$> deRefWeak (crPayloadWeak roots)
    assertCargoOwner readOwner
    containerAlive `shouldBe` True
    payloadAlive `shouldBe` True

-- Keep the reread owner out of callers that later collect after release.
{-# NOINLINE assertCargoOwner #-}
assertCargoOwner :: IO (Maybe VersionRead) -> Expectation
assertCargoOwner readOwner = do
    selected <- readOwner >>= maybe (fail "selected Cargo owner is absent") pure
    snapshot <- either (fail . toString) pure (cargoSnapshot selected)
    csRecordCount snapshot `shouldBe` 1
    csEntry snapshot `shouldBe` RecordEntry 0
    stringField "vers" (csRow snapshot) `shouldBe` Just "1.0.0"
    lengthWord8 (csPayload snapshot) `shouldBe` 2 * 1024 * 1024
    T.all (== 'x') (csPayload snapshot) `shouldBe` True
    vrBodyBytes selected `shouldSatisfy` (> 2 * 1024 * 1024)

assertCargoReleased :: CargoRoots -> Expectation
assertCargoReleased roots = do
    performGC
    isNothing <$> deRefWeak (crContainerWeak roots) `shouldReturn` True
    isNothing <$> deRefWeak (crPayloadWeak roots) `shouldReturn` True

assertObservedReleased :: IORef (Maybe CargoRoots) -> Expectation
assertObservedReleased observed =
    readIORef observed >>= maybe (expectationFailure "cache insertion did not observe a successful raw row") assertCargoReleased

seedRetainedCargo :: MetricsPort -> MetadataCache -> Source -> IO (VersionRead, CargoRoots)
seedRetainedCargo metrics cache source = do
    (_, selected, served) <- retainedCargoFixture
    weighVersion selected `shouldSatisfy` (> 1024 * 1024)
    roots <- withCargoRoots selected $ \rooted roots _ -> do
        resolveVersion metrics cache source cargoTestName (cargoTestVersion "1.0.0") (pure (Right rooted)) `shouldReturn` Right rooted
        pure roots
    assertCargoRetained roots (cachedVersion noopMetricsPort cache source cargoTestName (cargoTestVersion "1.0.0"))
    pure (served, roots)

observedReadSpec :: Spec
observedReadSpec = describe "observed full origin reads" $ do
    it "reports actual input bytes even when native parsing rejects the response" $ do
        seen <- newIORef 0
        result <- readObserved (\bytes -> modifyIORef' seen (+ bytes))
        (case result of OriginUnresolved -> True; _ -> False) `shouldBe` True
        readIORef seen `shouldReturn` BS.length malformed
    it "retains a typed request-wide bound refusal instead of a transport outcome" $ do
        let fault = BodyTooLarge (MetadataBodyLimit 12)
        result <- readObserved (\_ -> throwIO (ObservedReadBoundExceeded fault))
        (case result of OriginBudgetExceeded observed -> observed == fault; _ -> False) `shouldBe` True
  where
    malformed = "{invalid native metadata that must still consume its bounded response}"
    readObserved observe = withSweeper $ \deadlines ->
        testWithApplication (pure (\_ respond -> respond (responseLBS status200 [] (fromStrict malformed)))) $ \port -> do
            runtime <- testServeRuntime deadlines noopMetricsPort
            logEnv <- newTestLogEnv
            let deps = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show port))) inertPackumentDeps
                binding = MountBinding ("npm" :| []) npmRouter npmCredential deps Nothing
            expectRight
                =<< withMemoryEntry
                    noopMetricsPort
                    (srMemoryMeter runtime)
                    ( \ticket -> do
                        part <- newChargePart ticket
                        runHandler logEnv mempty (RequestCtx runtime binding) (fetchPrivateOriginObserved observe deps runtime part Nothing leftpadName)
                    )
