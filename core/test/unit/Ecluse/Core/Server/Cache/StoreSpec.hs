-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

{- | Store bounds and single-flight lifecycle checks.
Weights exercise admission without allocating the reported byte counts.
-}
module Ecluse.Core.Server.Cache.StoreSpec (spec) where

import Control.Exception (getMaskingState, throw)
import Data.Time (NominalDiffTime)
import Test.Hspec
import UnliftIO (async, cancel, concurrently, mapConcurrently, timeout, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Server.Cache.Backend (BackendStorage (ExternalStorage, LocalStorage), Recency (..), retentionBackend)
import Ecluse.Test.Server.Cache (externalOperations, newSingleFlight)

import Ecluse.Core.Server.Cache.Store (
    CacheOccupancy (..),
    SingleFlight,
    newSingleFlightWithBackend,
    resolveSingleFlight,
 )
import Ecluse.Core.Server.Cache.Store qualified as Store
import Ecluse.Core.Telemetry.Metrics qualified as Metric

data StoreFault = StoreFault
    deriving stock (Eq, Show)

newtype UnexpectedFault = UnexpectedFault StoreFault
    deriving stock (Show)

instance Exception UnexpectedFault

data LeaderEscaped = LeaderEscaped
    deriving stock (Eq, Show)

instance Exception LeaderEscaped

flatWeight :: Int
flatWeight = 100

newStore :: NominalDiffTime -> Int -> Int -> IO (SingleFlight StoreFault Text Text)
newStore ttl maxEntries maxBytes = newSingleFlight ttl maxEntries maxBytes (const flatWeight)

roomyStore :: IO (SingleFlight StoreFault Text Text)
roomyStore = newStore 60 100 (100 * flatWeight)

resolve :: SingleFlight StoreFault Text Text -> Text -> IO (Either StoreFault Text) -> IO (Either StoreFault Text)
resolve = resolveSingleFlight (const pass) (const pass) pass

resolveWith :: IO () -> SingleFlight StoreFault Text Text -> Text -> IO (Either StoreFault Text) -> IO (Either StoreFault Text)
resolveWith afterClaim sf key fetch = resolveSingleFlight (const pass) (const pass) pass sf key (afterClaim >> fetch)

resolveWithRequests :: IORef [Metric.CacheResult] -> IO () -> SingleFlight StoreFault Text Text -> Text -> IO (Either StoreFault Text) -> IO (Either StoreFault Text)
resolveWithRequests seen afterClaim sf key fetch =
    resolveSingleFlight (\r -> atomicModifyIORef' seen (\rs -> (r : rs, ()))) (const pass) pass sf key (afterClaim >> fetch)

resolveOk :: SingleFlight StoreFault Text Text -> Text -> IO Text -> IO Text
resolveOk sf key fetch = either (throwIO . UnexpectedFault) pure =<< resolve sf key (Right <$> fetch)

resolveOkRecording :: IORef (Maybe CacheOccupancy) -> SingleFlight StoreFault Text Text -> Text -> IO Text -> IO Text
resolveOkRecording seen sf key fetch =
    either (throwIO . UnexpectedFault) pure
        =<< resolveSingleFlight (const pass) (writeIORef seen . Just) pass sf key (Right <$> fetch)

resolveOkAccumulating :: IORef [CacheOccupancy] -> SingleFlight StoreFault Text Text -> Text -> IO Text -> IO Text
resolveOkAccumulating seen sf key fetch =
    either (throwIO . UnexpectedFault) pure
        =<< resolveSingleFlight (const pass) (recordOccupancyHistory seen) pass sf key (Right <$> fetch)

recordOccupancyHistory :: IORef [CacheOccupancy] -> CacheOccupancy -> IO ()
recordOccupancyHistory seen occ = atomicModifyIORef' seen (\os -> (occ : os, ()))

lookupStore :: SingleFlight StoreFault Text Text -> Text -> IO (Maybe Text)
lookupStore = Store.lookupStore (const pass) pass PreserveRecency

lookupStoreTouching :: SingleFlight StoreFault Text Text -> Text -> IO (Maybe Text)
lookupStoreTouching = Store.lookupStore (const pass) pass RefreshRecency

countingFetch :: IORef Int -> Text -> IO Text
countingFetch calls value = atomicModifyIORef' calls (\n -> (n + 1, ())) $> value

spec :: Spec
spec = do
    describe "prepared request lifetime" $ do
        it "pins a local hit through eviction and reports only on execution" $ do
            sf <- newStore 60 1 flatWeight
            _ <- resolveOk sf "held" (pure "original")
            seen <- newIORef []
            prepared <- Store.prepareStore (\outcome -> modifyIORef' seen (outcome :)) (const pass) pass sf "held" (pure (Right "wrong"))
            Store.preparedReuse prepared `shouldBe` Store.KnownLocalReuse
            readIORef seen `shouldReturn` []
            _ <- resolveOk sf "replacement" (pure "other")
            lookupStore sf "held" `shouldReturn` Nothing
            Store.executePrepared prepared `shouldReturn` Right "original"
            readIORef seen `shouldReturn` [Metric.Hit]

        it "pins absence through expiry without a second lookup" $ do
            held <- newIORef (Just (Nothing :: Maybe Text))
            lookupCalls <- newIORef (0 :: Int)
            seen <- newIORef []
            let operations = externalOperations (\_ _ -> modifyIORef' lookupCalls (+ 1) >> readIORef held) (\_ _ -> pass)
            sf <- newSingleFlightWithBackend (Just (retentionBackend LocalStorage operations))
            prepared <- Store.prepareStore (\outcome -> modifyIORef' seen (outcome :)) (const pass) pass sf ("absent" :: Text) (pure (Left StoreFault))
            writeIORef held Nothing
            Store.preparedReuse prepared `shouldBe` Store.KnownLocalReuse
            Store.executePrepared prepared `shouldReturn` Right Nothing
            readIORef lookupCalls `shouldReturn` 1
            readIORef seen `shouldReturn` [Metric.Hit]

        for_ [Nothing, Just "external"] $ \held ->
            it ("defers external storage until execution: " <> show held) $ do
                lookupCalls <- newIORef (0 :: Int)
                let operations = externalOperations (\_ _ -> modifyIORef' lookupCalls (+ 1) $> held) (\_ _ -> pass)
                sf <- newSingleFlightWithBackend (Just (retentionBackend (ExternalStorage 1000000) operations))
                prepared <- Store.prepareStore (const pass) (const pass) pass sf ("key" :: Text) (pure (Right "fresh" :: Either StoreFault Text))
                Store.preparedReuse prepared `shouldBe` Store.NeedsMaterialisation
                readIORef lookupCalls `shouldReturn` 0
                Store.executePrepared prepared `shouldReturn` Right (fromMaybe "fresh" held)
                readIORef lookupCalls `shouldReturn` 1

        it "defers an external lookup failure and falls back during execution" $ do
            failRead <- newIORef True
            let lookupValue _ _ = do
                    failing <- readIORef failRead
                    if failing then throwIO LeaderEscaped else pure (Just ("recovered" :: Text))
                operations = externalOperations lookupValue (\_ _ -> pass)
            sf <- newSingleFlightWithBackend (Just (retentionBackend (ExternalStorage 1000000) operations))
            prepared <- Store.prepareStore (const pass) (const pass) pass sf ("key" :: Text) (pure (Left StoreFault))
            Store.executePrepared prepared `shouldReturn` Left StoreFault
            writeIORef failRead False
            timeout 1000000 (resolve sf "key" (pure (Left StoreFault))) `shouldReturn` Just (Right "recovered")

        it "leaves no flight behind when preparation is abandoned" $ do
            sf <- roomyStore
            _ <- Store.prepareStore (const pass) (const pass) pass sf "key" (pure (Right "abandoned"))
            timeout 1000000 (resolveOk sf "key" (pure "leader")) `shouldReturn` Just "leader"

        it "rechecks a deferred miss populated while admission waited" $ do
            sf <- roomyStore
            prepared <- Store.prepareStore (const pass) (const pass) pass sf "key" (pure (Right "wrong"))
            _ <- resolveOk sf "key" (pure "winner")
            Store.executePrepared prepared `shouldReturn` Right "winner"

        it "keeps prepared followers coalesced and retries a cancelled leader once" $ do
            result <- timeout 1000000 $ do
                sf <- newSingleFlightWithBackend Nothing
                started <- newEmptyMVar
                joined <- newEmptyMVar
                release <- newEmptyMVar
                seen <- newIORef []
                let observe outcome = do
                        modifyIORef' seen (outcome :)
                        when (outcome == Metric.Collapsed) (putMVar joined ())
                    blocked = putMVar started () >> takeMVar release $> Right ("abandoned" :: Text)
                    prepare = Store.prepareStore observe (const pass) pass sf ("key" :: Text)
                leader <- prepare blocked
                follower <- prepare (pure (Right "recovered" :: Either StoreFault Text))
                withAsync (Store.executePrepared leader) $ \runningLeader -> do
                    takeMVar started
                    withAsync (Store.executePrepared follower) $ \runningFollower -> do
                        takeMVar joined
                        cancel runningLeader
                        wait runningFollower `shouldReturn` Right "recovered"
                readIORef seen `shouldReturn` [Metric.Collapsed, Metric.Miss]
            result `shouldBe` Just ()

        it "does not reserve a flight when a prepared hit observer fails" $ do
            sf <- roomyStore
            _ <- resolveOk sf "key" (pure "held")
            prepared <- Store.prepareStore (const (throwIO LeaderEscaped)) (const pass) pass sf "key" (pure (Right "wrong"))
            try (Store.executePrepared prepared) `shouldReturn` Left LeaderEscaped
            timeout 1000000 (resolveOk sf "key" (pure "wrong")) `shouldReturn` Just "held"

    describe "local retained hits" $ do
        it "returns a second hit while the first hit's request callback is blocked" $ do
            result <- timeout 1000000 $ do
                sf <- roomyStore
                _ <- resolveOk sf "hot" (pure "held")
                started <- newEmptyMVar
                release <- newEmptyMVar
                seen <- newIORef []
                let blocked request = putMVar started request >> takeMVar release
                withAsync (resolveSingleFlight blocked (const pass) pass sf "hot" (pure (Right "wrong"))) $ \firstWorker -> do
                    takeMVar started `shouldReturn` Metric.Hit
                    resolveWithRequests seen pass sf "hot" (pure (Right "wrong")) `shouldReturn` Right "held"
                    readIORef seen `shouldReturn` [Metric.Hit]
                    putMVar release ()
                    wait firstWorker `shouldReturn` Right "held"
            result `shouldBe` Just ()

        it "rechecks retention after claiming a miss that raced with another completed fetch" $ do
            result <- timeout 1000000 $ do
                held <- newIORef Nothing
                firstRead <- newIORef True
                started <- newEmptyMVar
                release <- newEmptyMVar
                fetches <- newIORef (0 :: Int)
                let readValue _ _ = do
                        isFirst <- atomicModifyIORef' firstRead (False,)
                        if isFirst
                            then putMVar started () >> takeMVar release $> Nothing
                            else readIORef held
                    operations = externalOperations readValue (\_ value -> writeIORef held (Just value))
                sf <- newSingleFlightWithBackend (Just (retentionBackend LocalStorage operations))
                withAsync (resolveOk sf "key" (countingFetch fetches "wrong")) $ \firstWorker -> do
                    takeMVar started
                    resolveOk sf "key" (countingFetch fetches "winner") `shouldReturn` "winner"
                    putMVar release ()
                    wait firstWorker `shouldReturn` "winner"
                readIORef fetches `shouldReturn` 1
            result `shouldBe` Just ()

    describe "single-flight without retention" $ do
        for_ [Right "fresh", Left StoreFault] $ \outcome ->
            it ("shares an active result and drops completed history: " <> show outcome) $ do
                result <- timeout 1000000 $ do
                    sf <- newSingleFlightWithBackend Nothing
                    started <- newEmptyMVar
                    joined <- newEmptyMVar
                    release <- newEmptyMVar
                    let fetch = putMVar started () >> takeMVar release $> outcome
                        observe request = when (request == Metric.Collapsed) (putMVar joined ())
                        run = resolveSingleFlight observe (const pass) pass sf "key"
                    withAsync (run fetch) $ \leader -> do
                        takeMVar started
                        withAsync (run fetch) $ \follower -> do
                            takeMVar joined
                            putMVar release ()
                            wait leader `shouldReturn` outcome
                            wait follower `shouldReturn` outcome
                    lookupStore sf "key" `shouldReturn` Nothing
                    run (pure (Right "next")) `shouldReturn` Right "next"
                result `shouldBe` Just ()

        it "lets followers recover from cancellation without keeping their result" $ do
            result <- timeout 1000000 $ do
                sf <- newSingleFlightWithBackend Nothing
                started <- newEmptyMVar
                joined <- newEmptyMVar
                release <- newEmptyMVar
                let fetch = putMVar started () >> takeMVar release $> Right "cancelled"
                    observe request = when (request == Metric.Collapsed) (putMVar joined ())
                withAsync (resolve sf "key" fetch) $ \leader -> do
                    takeMVar started
                    withAsync (resolveSingleFlight observe (const pass) pass sf "key" (pure (Right "recovered"))) $ \follower -> do
                        takeMVar joined
                        cancel leader
                        wait follower `shouldReturn` Right "recovered"
                lookupStore sf "key" `shouldReturn` Nothing
                resolve sf "key" (pure (Right "next")) `shouldReturn` Right "next"
            result `shouldBe` Just ()

    describe "resolveSingleFlight -- collapse" $ do
        it "collapses concurrent resolutions of one key to a single fetch" $ do
            sf <- roomyStore
            calls <- newIORef (0 :: Int)
            started <- newEmptyMVar
            release <- newEmptyMVar

            let fetch = do
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    _ <- tryPutMVar started ()
                    takeMVar release
                    pure "raw"
            (results, ()) <-
                concurrently
                    (mapConcurrently (const (resolveOk sf "hot" fetch)) [1 .. 8 :: Int])
                    ( do
                        takeMVar started
                        threadDelay 30000 -- give the others time to coalesce
                        putMVar release ()
                    )
            results `shouldBe` replicate 8 ("raw" :: Text)
            readIORef calls `shouldReturn` 1

        for_ [(flatWeight, Metric.Hit, 0), (flatWeight - 1, Metric.Miss, 1)] $ \(budget, nextResult, refusalCount) ->
            it ("counts collapsed work once with byte budget " <> show budget) $ do
                result <- timeout 5_000_000 $ do
                    sf <- newStore 60 2 budget
                    seen <- newIORef []
                    refused <- newIORef (0 :: Int)
                    started <- newEmptyMVar
                    joined <- newEmptyMVar
                    release <- newEmptyMVar
                    let recordRequest request = do
                            atomicModifyIORef' seen (\requests -> (request : requests, ()))
                            when (request == Metric.Collapsed) (putMVar joined ())
                        fetch = putMVar started () >> takeMVar release $> Right "raw"
                        run = resolveSingleFlight recordRequest (const pass) (modifyIORef' refused (+ 1)) sf "shared"
                    withAsync (run fetch) $ \leader -> do
                        takeMVar started
                        withAsync (run fetch) $ \follower -> do
                            takeMVar joined
                            putMVar release ()
                            wait leader `shouldReturn` Right "raw"
                            wait follower `shouldReturn` Right "raw"
                    readIORef refused `shouldReturn` refusalCount
                    run (pure (Right "raw")) `shouldReturn` Right "raw"
                    readIORef refused `shouldReturn` (2 * refusalCount)
                    readIORef seen `shouldReturn` [nextResult, Metric.Collapsed, Metric.Miss]
                result `shouldBe` Just ()

        it "has the value in the store the instant the leader's fetch returns" $ do
            sf <- roomyStore
            _ <- resolveOk sf "fresh" (pure "raw")
            lookupStore sf "fresh" `shouldReturn` Just "raw"

        it "does not re-fetch for a caller arriving right after the fetch returns" $ do
            sf <- roomyStore
            calls <- newIORef 0
            _ <- resolveOk sf "back-to-back" (countingFetch calls "raw")
            _ <- resolveOk sf "back-to-back" (countingFetch calls "raw")
            readIORef calls `shouldReturn` 1

    describe "resolveSingleFlight -- typed failure channel" $ do
        it "hands the leader's Left to every coalesced follower, caching nothing" $ do
            sf <- roomyStore
            calls <- newIORef (0 :: Int)
            started <- newEmptyMVar
            release <- newEmptyMVar
            let failing = do
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    _ <- tryPutMVar started ()
                    takeMVar release
                    pure (Left StoreFault)
            (results, ()) <-
                concurrently
                    (mapConcurrently (const (resolve sf "shared-fault" failing)) [1 .. 8 :: Int])
                    ( do
                        takeMVar started
                        threadDelay 30000 -- give the others time to coalesce
                        putMVar release ()
                    )
            results `shouldBe` replicate 8 (Left StoreFault)
            readIORef calls `shouldReturn` 1
            lookupStore sf "shared-fault" `shouldReturn` Nothing

        it "re-raises a synchronously escaping leader to its followers (the invariant channel)" $ do
            result <- timeout 5_000_000 $ do
                sf <- roomyStore
                started <- newEmptyMVar
                release <- newEmptyMVar
                let escaping = do
                        putMVar started ()
                        () <- takeMVar release
                        throwIO LeaderEscaped
                leader <- async (try (resolveOk sf "escape" escaping) :: IO (Either LeaderEscaped Text))
                takeMVar started
                follower <- async (try (resolveOk sf "escape" escaping) :: IO (Either LeaderEscaped Text))
                threadDelay 30000 -- give the follower time to register on the marker
                putMVar release ()
                (,) <$> wait leader <*> wait follower
            case result of
                Nothing -> expectationFailure "wedged: an escaping leader parked its follower"
                Just (leaderOutcome, followerOutcome) -> do
                    leaderOutcome `shouldBe` Left LeaderEscaped
                    followerOutcome `shouldBe` Left LeaderEscaped

        it "releases the claim when leader request telemetry throws" $ do
            result <- timeout 5_000_000 $ do
                sf <- roomyStore
                calls <- newIORef (0 :: Int)
                seen <- newIORef []
                leaderReported <- newEmptyMVar
                followerReported <- newEmptyMVar
                let fetch = Right <$> countingFetch calls "raw"
                    reportRequest observed request = do
                        atomicModifyIORef' seen (\requests -> (request : requests, ()))
                        putMVar observed ()
                    leaderRequest request = do
                        reportRequest leaderReported request
                        readMVar followerReported
                        -- A callback fault must propagate through the exception channel.
                        throwIO LeaderEscaped
                    resolveReporting callback =
                        resolveSingleFlight callback (const pass) pass sf "reporter" fetch
                withAsync (try (resolveReporting leaderRequest)) $ \leader -> do
                    takeMVar leaderReported
                    withAsync (try (resolveReporting (reportRequest followerReported))) $ \follower -> do
                        wait leader `shouldReturn` Left LeaderEscaped
                        wait follower `shouldReturn` Left LeaderEscaped
                lookupStore sf "reporter" `shouldReturn` Nothing
                readIORef calls `shouldReturn` 0
                readIORef seen `shouldReturn` [Metric.Collapsed, Metric.Miss]
                resolveWithRequests seen (pure ()) sf "reporter" fetch `shouldReturn` Right "raw"
                resolveWithRequests seen (pure ()) sf "reporter" fetch `shouldReturn` Right "raw"
                lookupStore sf "reporter" `shouldReturn` Just "raw"
                readIORef calls `shouldReturn` 1
                readIORef seen `shouldReturn` [Metric.Hit, Metric.Miss, Metric.Collapsed, Metric.Miss]
            result `shouldBe` Just ()

    describe "resolveSingleFlight -- single-flight orphan window" $ do
        it "unblocks a waiting follower and lets a later caller re-lead when the leader is cancelled at the claim handoff" $ do
            result <- timeout 5_000_000 $ do
                sf <- roomyStore
                calls <- newIORef (0 :: Int)
                reached <- newEmptyMVar
                release <- newEmptyMVar
                armed <- newIORef True -- only the first (cancelled) leader parks
                let fetch = Right <$> countingFetch calls "raw"
                    afterClaim = do
                        wasArmed <- atomicModifyIORef' armed (False,)
                        when wasArmed $ do
                            putMVar reached () -- claimed the slot, parked at the handoff
                            takeMVar release -- block interruptibly so the cancel lands here
                leader <- async (resolveWith afterClaim sf "wedge" fetch)
                takeMVar reached

                follower <- async (try (resolve sf "wedge" fetch) :: IO (Either SomeException (Either StoreFault Text)))
                threadDelay 30000 -- give the follower time to register on the marker
                cancel leader -- cancel in the handoff window: the slot must still free
                wait follower
            case result of
                Nothing -> expectationFailure "wedged: a cancelled leader orphaned the in-flight slot"
                Just (Left _) -> expectationFailure "follower failed instead of recovering"
                Just (Right recovered) -> recovered `shouldBe` Right "raw"

        it "frees the slot for a later caller when the leader's fetch is cancelled mid-flight" $ do
            result <- timeout 5_000_000 $ do
                sf <- roomyStore
                calls <- newIORef (0 :: Int)
                started <- newEmptyMVar
                release <- newEmptyMVar
                let blockingFetch = do
                        atomicModifyIORef' calls (\n -> (n + 1, ()))
                        putMVar started ()
                        () <- takeMVar release
                        pure "unreached"
                leader <- async (resolveOk sf "midflight" blockingFetch)
                takeMVar started
                cancel leader
                recovered <- resolveOk sf "midflight" (countingFetch calls "raw")
                n <- readIORef calls
                pure (recovered, n)
            case result of
                Nothing -> expectationFailure "wedged: a mid-flight cancel orphaned the in-flight slot"
                Just (recovered, n) -> do
                    recovered `shouldBe` "raw"
                    n `shouldBe` 2 -- the cancelled fetch and the recovering re-lead, no caching of the failure
        it "keeps one outcome per request when a cancelled leader forces a follower to retry" $ do
            result <- timeout 5_000_000 $ do
                sf <- roomyStore
                seen <- newIORef []
                calls <- newIORef (0 :: Int)
                reached <- newEmptyMVar
                joined <- newEmptyMVar
                release <- newEmptyMVar
                armed <- newIORef True -- only the first (cancelled) leader parks
                let fetch = Right <$> countingFetch calls "raw"
                    followerRequest request = do
                        atomicModifyIORef' seen (\requests -> (request : requests, ()))
                        putMVar joined ()
                    afterClaim = do
                        wasArmed <- atomicModifyIORef' armed (False,)
                        when wasArmed $ do
                            putMVar reached () -- claimed the slot, parked at the handoff
                            takeMVar release -- block interruptibly so the cancel lands here
                leader <- async (resolveWithRequests seen afterClaim sf "wedge" fetch)
                takeMVar reached
                follower <- async (try (resolveSingleFlight followerRequest (const pass) pass sf "wedge" fetch) :: IO (Either SomeException (Either StoreFault Text)))
                takeMVar joined
                cancel leader -- cancel in the handoff window: the follower must re-resolve
                recovered <- wait follower
                recorded <- readIORef seen
                pure (recovered, recorded)
            case result of
                Nothing -> expectationFailure "wedged: a cancelled leader orphaned the in-flight slot"
                Just (Left _, _) -> expectationFailure "follower failed instead of recovering"
                Just (Right recovered, recorded) -> do
                    recovered `shouldBe` Right "raw" -- the follower recovered by re-leading
                    recorded `shouldBe` [Metric.Collapsed, Metric.Miss] -- leader + follower, never a third for the retry
    describe "the entry-count bound" $ do
        it "never exceeds the configured maximum entry count" $ do
            seen <- newIORef Nothing
            sf <- newStore 60 4 (1000 * flatWeight)
            for_ [1 .. 20 :: Int] $ \i ->
                resolveOkRecording seen sf (show i) (pure "raw")
            recordedOccupancy seen `shouldReturn` Just (4, 4 * flatWeight)

        it "keeps serving fresh resolutions even under eviction pressure" $ do
            sf <- newStore 60 2 (1000 * flatWeight)
            for_ [1 .. 10 :: Int] $ \i ->
                resolveOk sf (show i) (pure "raw")
            resolveOk sf "final" (pure "raw") `shouldReturn` "raw"

    describe "incremental occupancy" $ do
        it "reports only committed occupancy after an eviction replacement" $ do
            seen <- newIORef []
            sf <- newStore 60 1 flatWeight
            _ <- resolveOkAccumulating seen sf "first" (pure "raw")
            _ <- resolveOkAccumulating seen sf "second" (pure "raw")
            map occupancyPair <$> readIORef seen `shouldReturn` [(1, flatWeight)]
            lookupStore sf "first" `shouldReturn` Nothing
            lookupStore sf "second" `shouldReturn` Just "raw"

        for_ [("read-only", \record -> Store.lookupStore record pass PreserveRecency), ("touching", \record -> Store.lookupStore record pass RefreshRecency)] $ \(viewName, readEntry) ->
            it ("reports expiry immediately through the " <> viewName <> " view") $ do
                seen <- newIORef Nothing
                sf <- newStore 0 1 flatWeight
                _ <- resolveOkRecording seen sf "expired" (pure "raw")
                threadDelay 1000
                readEntry (writeIORef seen . Just) sf "expired" `shouldReturn` Nothing
                recordedOccupancy seen `shouldReturn` Just (0, 0)

        it "reports expiry even when the replacement fetch fails" $ do
            seen <- newIORef Nothing
            sf <- newStore 0 1 flatWeight
            _ <- resolveOkRecording seen sf "expired" (pure "raw")
            threadDelay 1000
            resolveSingleFlight (const pass) (writeIORef seen . Just) pass sf "expired" (pure (Left StoreFault))
                `shouldReturn` Left StoreFault
            recordedOccupancy seen `shouldReturn` Just (0, 0)

        it "restores the caller's masking state when expiry leads to a fetch" $ do
            sf <- newStore 0 1 flatWeight
            _ <- resolveOk sf "expired" (pure "old")
            threadDelay 1000
            callerState <- getMaskingState
            let fetch = do
                    getMaskingState `shouldReturn` callerState
                    pure "new"
            resolveOk sf "expired" fetch `shouldReturn` "new"

        it "does not claim a leader when the expiry callback throws" $ do
            result <- timeout 5_000_000 $ do
                sf <- newStore 0 1 flatWeight
                _ <- resolveOk sf "expired" (pure "raw")
                threadDelay 1000
                -- The telemetry callback exposes faults only through exceptions.
                outcome <- try (resolveSingleFlight (const pass) (const (throwIO LeaderEscaped)) pass sf "expired" (pure (Right "new")))
                outcome `shouldBe` Left LeaderEscaped
                resolveOk sf "expired" (pure "new") `shouldReturn` "new"
            result `shouldBe` Just ()

        it "serves fresh hits while another key's occupancy callback holds the mutation lock" $ do
            sf <- roomyStore
            _ <- resolveOk sf "hot" (pure "raw")
            started <- newEmptyMVar
            release <- newEmptyMVar
            let recordOccupancy _ = putMVar started () >> takeMVar release
                insertOther = resolveSingleFlight (const pass) recordOccupancy pass sf "other" (pure (Right "other"))
            withAsync insertOther $ \inserting -> do
                takeMVar started
                timeout 1_000_000 (resolveOk sf "hot" (pure "unexpected")) `shouldReturn` Just "raw"
                timeout 1_000_000 (lookupStore sf "hot") `shouldReturn` Just (Just "raw")
                timeout 1_000_000 (lookupStoreTouching sf "hot") `shouldReturn` Just (Just "raw")
                putMVar release ()
                wait inserting `shouldReturn` Right "other"

        it "keeps concurrent absolute gauge callbacks in mutation order" $ do
            result <- timeout 5_000_000 $ do
                sf <- newStore 60 2 (2 * flatWeight)
                seen <- newIORef Nothing
                started <- newEmptyMVar
                secondStarted <- newEmptyMVar
                release <- newEmptyMVar
                let recordOccupancy occ = do
                        when (occEntries occ == 1) (putMVar started () >> takeMVar release)
                        writeIORef seen (Just occ)
                    run key = resolveSingleFlight (const pass) recordOccupancy pass sf key (pure (Right "raw"))
                withAsync (run "first") $ \firstWorker -> do
                    takeMVar started
                    withAsync (putMVar secondStarted () >> run "second") $ \secondWorker -> do
                        takeMVar secondStarted
                        timeout 30000 (wait secondWorker) `shouldReturn` Nothing
                        putMVar release ()
                        wait firstWorker `shouldReturn` Right "raw"
                        wait secondWorker `shouldReturn` Right "raw"
                recordedOccupancy seen `shouldReturn` Just (2, 2 * flatWeight)
            result `shouldBe` Just ()

        it "matches retained values through varied-weight eviction and repeated keys" $ do
            seen <- newIORef Nothing
            let weigh value = if value == "large" then 170 else 30
                keys = map show [1 .. 7 :: Int]
            sf <- newSingleFlight 60 4 230 weigh :: IO (SingleFlight StoreFault Text Text)
            for_ (zip (concat (replicate 6 keys)) [1 .. 40 :: Int]) $ \(key, turn) -> do
                let value = if even turn then "large" else "small"
                _ <- resolveOkRecording seen sf key (pure value)
                held <- catMaybes <$> traverse (lookupStore sf) keys
                recordedOccupancy seen `shouldReturn` Just (length held, sum (map weigh held))

        it "purges expired entries before inserting and reuses their full budget" $ do
            seen <- newIORef Nothing
            sf <- newStore 0.01 2 (2 * flatWeight)
            _ <- resolveOk sf "old-a" (pure "raw")
            _ <- resolveOk sf "old-b" (pure "raw")
            threadDelay 30000
            _ <- resolveOkRecording seen sf "new" (pure "raw")
            recordedOccupancy seen `shouldReturn` Just (1, flatWeight)
            lookupStore sf "old-a" `shouldReturn` Nothing
            lookupStore sf "old-b" `shouldReturn` Nothing

        for_ [("read-only", lookupStore), ("touching", lookupStoreTouching)] $ \(viewName, readEntry) ->
            it ("accounts once for expiry through the " <> viewName <> " view") $ do
                seen <- newIORef Nothing
                sf <- newStore 0.01 2 (2 * flatWeight)
                _ <- resolveOk sf "expired" (pure "raw")
                threadDelay 30000
                readEntry sf "expired" `shouldReturn` Nothing
                readEntry sf "expired" `shouldReturn` Nothing
                _ <- resolveOkRecording seen sf "fresh" (pure "raw")
                recordedOccupancy seen `shouldReturn` Just (1, flatWeight)

        it "replaces an expired key with its new weight without retaining the old charge" $ do
            seen <- newIORef Nothing
            let weigh value = if value == "old" then 170 else 30
            sf <- newSingleFlight 0.01 2 230 weigh :: IO (SingleFlight StoreFault Text Text)
            _ <- resolveOk sf "same" (pure "old")
            threadDelay 30000
            _ <- resolveOkRecording seen sf "same" (pure "new")
            recordedOccupancy seen `shouldReturn` Just (1, 30)
            _ <- resolveOkRecording seen sf "other" (pure "new")
            recordedOccupancy seen `shouldReturn` Just (2, 60)

        it "keeps accounting after the occupancy callback throws" $ do
            seen <- newIORef Nothing
            sf <- roomyStore
            outcome <- try (resolveSingleFlight (const pass) (const (throwIO LeaderEscaped)) pass sf "first" (pure (Right "raw")))
            outcome `shouldBe` Left LeaderEscaped
            lookupStore sf "first" `shouldReturn` Just "raw"
            _ <- resolveOkRecording seen sf "second" (pure "raw")
            recordedOccupancy seen `shouldReturn` Just (2, 2 * flatWeight)

        it "does not change occupancy when the weigher throws" $ do
            seen <- newIORef Nothing
            let weigh value = if value == "fault" then throw LeaderEscaped else flatWeight
            sf <- newSingleFlight 60 2 (2 * flatWeight) weigh :: IO (SingleFlight StoreFault Text Text)
            _ <- resolveOk sf "first" (pure "raw")
            outcome <- try (resolveOk sf "failed" (pure "fault"))
            outcome `shouldBe` Left LeaderEscaped
            lookupStore sf "first" `shouldReturn` Just "raw"
            lookupStore sf "failed" `shouldReturn` Nothing
            _ <- resolveOkRecording seen sf "second" (pure "raw")
            recordedOccupancy seen `shouldReturn` Just (2, 2 * flatWeight)

        it "keeps committed occupancy bounded through concurrent expiry reads and inserts" $ do
            seen <- newIORef []
            sf <- newStore 0 3 (3 * flatWeight)
            let readEntry = Store.lookupStore (recordOccupancyHistory seen) pass RefreshRecency sf
            _ <- resolveOkAccumulating seen sf "expired" (pure "raw")
            threadDelay 1000
            (_, results) <-
                concurrently
                    (replicateM_ 20 (readEntry "expired"))
                    (mapConcurrently (\(key :: Int) -> resolveOkAccumulating seen sf (show key) (pure "raw")) [1 .. 8])
            results `shouldBe` replicate 8 "raw"
            threadDelay 1000
            traverse readEntry ("expired" : map show [1 .. 8 :: Int]) `shouldReturn` replicate 9 Nothing
            readings <- map occupancyPair <$> readIORef seen
            listToMaybe readings `shouldBe` Just (0, 0)
            readings `shouldSatisfy` all (`elem` [(0, 0), (1, flatWeight)])

        it "counts zero-weight entries against the entry limit" $ do
            seen <- newIORef Nothing
            sf <- newSingleFlight 60 2 1 (const 0) :: IO (SingleFlight StoreFault Text Text)
            for_ [1 .. 6 :: Int] $ \key ->
                resolveOkRecording seen sf (show key) (pure "raw")
            recordedOccupancy seen `shouldReturn` Just (2, 0)

    describe "oversized refusal telemetry" $ do
        for_ [flatWeight + 1, maxBound] $ \weight ->
            it ("counts one refusal per fetched value with weight " <> show weight) $ do
                sf <- newSingleFlight 60 2 flatWeight (const weight) :: IO (SingleFlight StoreFault Text Text)
                refused <- newIORef (0 :: Int)
                seen <- newIORef []
                let run = resolveSingleFlight (const pass) (\occ -> modifyIORef' seen (occ :)) (modifyIORef' refused (+ 1)) sf "large" (pure (Right "raw"))
                run `shouldReturn` Right "raw"
                run `shouldReturn` Right "raw"
                lookupStore sf "large" `shouldReturn` Nothing
                readIORef refused `shouldReturn` 2
                readIORef seen `shouldReturn` []

    describe "the resident-byte budget" $ do
        it "evicts to keep the resident estimate under the byte budget" $ do
            let held = 3
            seen <- newIORef Nothing
            sf <- newStore 60 1000 (held * flatWeight + flatWeight `div` 2)
            for_ [1 .. 20 :: Int] $ \i ->
                resolveOkRecording seen sf (show i) (pure "raw")
            recordedOccupancy seen `shouldReturn` Just (held, held * flatWeight)

        it "retains a repeatedly-accessed entry while evicting the one-shot tail" $ do
            let held = 3
            sf <- newStore 60 1000 (held * flatWeight + flatWeight `div` 2)
            _ <- resolveOk sf "hot" (pure "raw")
            for_ [1 .. 30 :: Int] $ \i -> do
                _ <- resolveOk sf "hot" (pure "unused")
                resolveOk sf ("cold-" <> show i) (pure "raw")
            lookupStore sf "hot" `shouldReturn` Just "raw"
            lookupStore sf "cold-1" `shouldReturn` Nothing

    describe "read recency -- the touching vs read-only views" $ do
        it "a touching read bumps recency, so eviction sheds an untouched entry, not the touched one" $ do
            sf <- newStore 60 2 (100 * flatWeight)
            _ <- resolveOk sf "old" (pure "raw")
            _ <- resolveOk sf "recent" (pure "raw")
            lookupStoreTouching sf "old" `shouldReturn` Just "raw"
            _ <- resolveOk sf "new" (pure "raw")
            lookupStore sf "old" `shouldReturn` Just "raw"
            lookupStore sf "recent" `shouldReturn` Nothing
            lookupStore sf "new" `shouldReturn` Just "raw"

        it "a read-only lookup leaves recency unchanged, so the insert-order-oldest entry still evicts" $ do
            sf <- newStore 60 2 (100 * flatWeight)
            _ <- resolveOk sf "old" (pure "raw")
            _ <- resolveOk sf "recent" (pure "raw")
            lookupStore sf "old" `shouldReturn` Just "raw"
            _ <- resolveOk sf "new" (pure "raw")
            lookupStore sf "old" `shouldReturn` Nothing
            lookupStore sf "recent" `shouldReturn` Just "raw"
            lookupStore sf "new" `shouldReturn` Just "raw"

    describe "the oversized pass-through" $ do
        it "never retains the saturated weight even with a maxBound budget" $ do
            sf <- newSingleFlight 60 100 maxBound (const maxBound) :: IO (SingleFlight StoreFault Text Text)
            resolveOk sf "overflow" (pure "value") `shouldReturn` "value"
            lookupStore sf "overflow" `shouldReturn` Nothing

        it "evicts before adding weights whose sum would overflow Int" $ do
            seen <- newIORef Nothing
            sf <- newSingleFlight 60 100 maxBound (const (maxBound - 1)) :: IO (SingleFlight StoreFault Text Text)
            _ <- resolveOkRecording seen sf "first" (pure "a")
            _ <- resolveOkRecording seen sf "second" (pure "b")
            lookupStore sf "first" `shouldReturn` Nothing
            lookupStore sf "second" `shouldReturn` Just "b"
            recordedOccupancy seen `shouldReturn` Just (1, maxBound - 1)

        it "serves a value larger than the whole byte budget without retaining it" $ do
            sf <- newStore 60 100 (flatWeight - 1)
            calls <- newIORef (0 :: Int)
            resolveOk sf "big" (countingFetch calls "huge") `shouldReturn` "huge"

            lookupStore sf "big" `shouldReturn` Nothing
            resolveOk sf "big" (countingFetch calls "huge") `shouldReturn` "huge"
            readIORef calls `shouldReturn` 2

        it "evicts nothing resident to make room that cannot exist" $ do
            let weigh v = if v == "pathological" then 3 * flatWeight else flatWeight
            sf <- newSingleFlight 60 100 (2 * flatWeight) weigh :: IO (SingleFlight StoreFault Text Text)
            _ <- resolveOk sf "a" (pure "resident")
            _ <- resolveOk sf "b" (pure "resident")
            _ <- resolveOk sf "big" (pure "pathological")
            lookupStore sf "a" `shouldReturn` Just "resident"
            lookupStore sf "b" `shouldReturn` Just "resident"
            lookupStore sf "big" `shouldReturn` Nothing

        it "reports no occupancy for a pass-through (the gauges describe the store, not the serve)" $ do
            seen <- newIORef Nothing
            sf <- newStore 60 100 (flatWeight - 1)
            _ <- resolveOkRecording seen sf "big" (pure "huge")
            (isNothing <$> readIORef seen) `shouldReturn` True

    describe "concurrent different-key leaders under the byte budget" $ do
        it "never lands the resident sum past the budget (the insert lock)" $ do
            let budget = 3 * flatWeight
            seen <- newIORef []
            sf <- newStore 60 1000 budget
            barrier <- newEmptyMVar
            leaders <- traverse (\(i :: Int) -> async (resolveOkAccumulating seen sf (show i) (readMVar barrier $> "v"))) [1 .. 8]
            putMVar barrier ()
            traverse_ wait leaders
            byteReadings <- map occBytes <$> readIORef seen
            byteReadings `shouldSatisfy` (not . null)
            byteReadings `shouldSatisfy` all (<= budget)
            held <- catMaybes <$> traverse (lookupStore sf . show) [1 .. 8 :: Int]
            length held `shouldBe` 3
            readings <- readIORef seen
            map occupancyPair readings `shouldSatisfy` all (\(entries, bytes) -> bytes == entries * flatWeight)

recordedOccupancy :: IORef (Maybe CacheOccupancy) -> IO (Maybe (Int, Int))
recordedOccupancy seen = fmap occupancyPair <$> readIORef seen

occupancyPair :: CacheOccupancy -> (Int, Int)
occupancyPair occ = (occEntries occ, occBytes occ)
