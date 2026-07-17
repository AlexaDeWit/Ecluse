-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

module Ecluse.Server.Cache.StoreSpec (spec) where

import Data.Time (NominalDiffTime)
import Test.Hspec
import UnliftIO (async, cancel, concurrently, mapConcurrently, timeout, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Server.Cache.Store (
    CacheOccupancy (..),
    SingleFlight,
    lookupStore,
    newSingleFlight,
    resolveSingleFlight,
 )

-- | The typed failure the fetches in these cases report through the store's channel.
data StoreFault = StoreFault
    deriving stock (Eq, Show)

-- | The typed wrapper for a 'Left' no success-path case expects (see 'resolveOk').
newtype UnexpectedFault = UnexpectedFault StoreFault
    deriving stock (Show)

instance Exception UnexpectedFault

-- | The typed escape the invariant-channel case throws from inside a leader's fetch.
data LeaderEscaped = LeaderEscaped
    deriving stock (Eq, Show)

instance Exception LeaderEscaped

{- | Every value in these cases is weighted at this flat figure, so a byte budget
reads as a count of entries.
-}
flatWeight :: Int
flatWeight = 100

{- | A store over 'Text' keys and values with the given TTL (seconds), entry-count
bound, and resident-byte budget, weighing every value at 'flatWeight'. The store is
generic in all three; these cases pin them to 'Text' so the machinery is exercised
without any domain vocabulary.
-}
newStore :: NominalDiffTime -> Int -> Int -> IO (SingleFlight StoreFault Text Text)
newStore ttl maxEntries maxBytes = newSingleFlight ttl maxEntries maxBytes (const flatWeight)

-- | A store with a generous TTL and ample room under both bounds.
roomyStore :: IO (SingleFlight StoreFault Text Text)
roomyStore = newStore 60 100 (100 * flatWeight)

{- | Resolve through the store with inert telemetry callbacks: these cases assert
behaviour through fetch counters and the read-only views, not the recordings.
-}
resolve :: SingleFlight StoreFault Text Text -> Text -> IO (Either StoreFault Text) -> IO (Either StoreFault Text)
resolve = resolveSingleFlight (pure ()) (const pass) (const pass)

-- | As 'resolve', threading the single-flight claim → fetch-runner handoff hook.
resolveWith :: IO () -> SingleFlight StoreFault Text Text -> Text -> IO (Either StoreFault Text) -> IO (Either StoreFault Text)
resolveWith afterClaim = resolveSingleFlight afterClaim (const pass) (const pass)

{- | The success-path adapter: these cases drive the store with fetches that cannot
fail, so the wrapper lifts them into the typed channel and a 'Left' is a test bug
surfaced as 'UnexpectedFault' (the typed-failure cases call 'resolve' directly).
-}
resolveOk :: SingleFlight StoreFault Text Text -> Text -> IO Text -> IO Text
resolveOk sf key fetch = either (throwIO . UnexpectedFault) pure =<< resolve sf key (Right <$> fetch)

{- | As 'resolveOk', but recording each leader insert's post-insert 'CacheOccupancy', so a
test observes the store's held-entry count and resident bytes through the same callback the
app wires to its occupancy gauges rather than polling the store directly.
-}
resolveOkRecording :: IORef (Maybe CacheOccupancy) -> SingleFlight StoreFault Text Text -> Text -> IO Text -> IO Text
resolveOkRecording seen sf key fetch =
    either (throwIO . UnexpectedFault) pure
        =<< resolveSingleFlight (pure ()) (const pass) (writeIORef seen . Just) sf key (Right <$> fetch)

{- | As 'resolveOkRecording', but accumulating __every__ post-insert occupancy, so a
concurrency case can assert that no insert, however interleaved, ever left the store
past its budget.
-}
resolveOkAccumulating :: IORef [CacheOccupancy] -> SingleFlight StoreFault Text Text -> Text -> IO Text -> IO Text
resolveOkAccumulating seen sf key fetch =
    either (throwIO . UnexpectedFault) pure
        =<< resolveSingleFlight (pure ()) (const pass) (\occ -> atomicModifyIORef' seen (\os -> (occ : os, ()))) sf key (Right <$> fetch)

-- | A counting fetch: bumps the call counter, then yields the given value.
countingFetch :: IORef Int -> Text -> IO Text
countingFetch calls value = atomicModifyIORef' calls (\n -> (n + 1, ())) $> value

spec :: Spec
spec = do
    describe "resolveSingleFlight -- collapse" $ do
        it "collapses concurrent resolutions of one key to a single fetch" $ do
            sf <- roomyStore
            calls <- newIORef (0 :: Int)
            started <- newEmptyMVar
            release <- newEmptyMVar
            -- The fetch blocks until released, so every concurrent resolver is in
            -- flight at once; if collapse fails, more than one enters the fetch and
            -- the call counter exceeds one.
            let fetch = do
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    _ <- tryPutMVar started () -- signal the first fetch has begun
                    takeMVar release -- block until the test releases it
                    pure "raw"
            (results, ()) <-
                concurrently
                    (mapConcurrently (const (resolveOk sf "hot" fetch)) [1 .. 8 :: Int])
                    ( do
                        takeMVar started -- wait until a fetch has begun
                        threadDelay 30000 -- give the others time to coalesce
                        putMVar release () -- let the single fetch complete
                    )
            results `shouldBe` replicate 8 ("raw" :: Text)
            readIORef calls `shouldReturn` 1

        it "has the value in the store the instant the leader's fetch returns" $ do
            -- The leader inserts into the store *before* de-registering its in-flight
            -- slot, so by the time the resolve returns the value is already
            -- discoverable via the store (not merely via the now-removed marker).
            -- A caller racing the de-register therefore finds the store entry rather
            -- than re-leading a redundant fetch. The window between insert and
            -- de-register is internal to the leader's run (under mask, no injection
            -- handle), so this asserts the observable post-condition the ordering
            -- guarantees: the store is populated as soon as the call completes.
            sf <- roomyStore
            _ <- resolveOk sf "fresh" (pure "raw")
            lookupStore sf "fresh" `shouldReturn` Just "raw"

        it "does not re-fetch for a caller arriving right after the fetch returns" $ do
            -- Sequential mirror of the collapse property at the post-fetch boundary:
            -- the second resolution lands after the first has fully returned, and is
            -- served from the store with no second fetch.
            sf <- roomyStore
            calls <- newIORef 0
            _ <- resolveOk sf "back-to-back" (countingFetch calls "raw")
            _ <- resolveOk sf "back-to-back" (countingFetch calls "raw")
            readIORef calls `shouldReturn` 1

    describe "resolveSingleFlight -- typed failure channel" $ do
        it "hands the leader's Left to every coalesced follower, caching nothing" $ do
            -- The leader's fetch reports a typed failure; every concurrent waiter must
            -- receive that same value (never an exception, never a re-lead), and the
            -- store must stay empty so the next resolution fetches afresh.
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
            -- The fetch's contract is total, so a synchronous escape is an invariant
            -- break: the follower must see the exception re-raised, not a value, and
            -- the slot must still free for a later caller.
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

    describe "resolveSingleFlight -- single-flight orphan window" $ do
        it "unblocks a waiting follower and lets a later caller re-lead when the leader is cancelled at the claim handoff" $ do
            -- Regression: an async exception (request timeout, killed handler thread)
            -- landing on the leader between claiming the in-flight slot and completing
            -- must still fill the marker with the error and free the slot, or the
            -- waiting follower parks on the marker forever and the key wedges until
            -- restart. The window is otherwise a smooth, interruptible point, so it
            -- is driven deterministically through the 'resolveWith' hook, which runs
            -- on the leading thread at exactly the claim -> fetch-runner handoff: it
            -- signals it has reached the window, then parks, so the test can cancel
            -- the leader there. Everything that could wedge is wrapped in a 'timeout'
            -- so a regression fails fast instead of hanging the suite.
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
                            putMVar reached () -- claimed the slot; parked at the handoff
                            takeMVar release -- block interruptibly so the cancel lands here
                            -- The leader claims the slot and parks at the handoff, holding it.
                leader <- async (resolveWith afterClaim sf "wedge" fetch)
                takeMVar reached
                -- A follower arrives while the slot is held; it must become a follower
                -- on the marker, not re-lead. (It will block on the marker until the
                -- cancelled leader fills it.)
                follower <- async (try (resolve sf "wedge" fetch) :: IO (Either SomeException (Either StoreFault Text)))
                threadDelay 30000 -- give the follower time to register on the marker
                cancel leader -- cancel in the handoff window; the slot must still free
                wait follower
            case result of
                Nothing -> expectationFailure "wedged: a cancelled leader orphaned the in-flight slot"
                Just (Left _) -> expectationFailure "follower failed instead of recovering"
                Just (Right recovered) -> recovered `shouldBe` Right "raw"

        it "frees the slot for a later caller when the leader's fetch is cancelled mid-flight" $ do
            -- The mid-fetch analogue: the async exception lands while the leader is
            -- inside the fetch (under restore). The marker fill and de-register must
            -- still run, so a subsequent caller re-leads and fetches rather than
            -- finding a stuck slot.
            result <- timeout 5_000_000 $ do
                sf <- roomyStore
                calls <- newIORef (0 :: Int)
                started <- newEmptyMVar
                release <- newEmptyMVar
                let blockingFetch = do
                        atomicModifyIORef' calls (\n -> (n + 1, ()))
                        putMVar started () -- in the fetch (slot claimed)
                        () <- takeMVar release -- block so the leader can be cancelled here
                        pure "unreached"
                leader <- async (resolveOk sf "midflight" blockingFetch)
                takeMVar started -- the leader holds the slot and is inside the fetch
                cancel leader -- async-cancel mid-fetch; the slot must still free
                -- A fresh caller must not wedge: with the slot freed it re-leads.
                recovered <- resolveOk sf "midflight" (countingFetch calls "raw")
                n <- readIORef calls
                pure (recovered, n)
            case result of
                Nothing -> expectationFailure "wedged: a mid-flight cancel orphaned the in-flight slot"
                Just (recovered, n) -> do
                    recovered `shouldBe` "raw"
                    n `shouldBe` 2 -- the cancelled fetch and the recovering re-lead, no caching of the failure
    describe "the entry-count bound" $ do
        it "never exceeds the configured maximum entry count" $ do
            seen <- newIORef Nothing
            sf <- newStore 60 4 (1000 * flatWeight)
            for_ [1 .. 20 :: Int] $ \i ->
                resolveOkRecording seen sf (show i) (pure "raw")
            occ <- readIORef seen
            fmap occEntries occ `shouldSatisfy` maybe False (<= 4)

        it "keeps serving fresh resolutions even under eviction pressure" $ do
            sf <- newStore 60 2 (1000 * flatWeight)
            for_ [1 .. 10 :: Int] $ \i ->
                resolveOk sf (show i) (pure "raw")
            resolveOk sf "final" (pure "raw") `shouldReturn` "raw"

    describe "the resident-byte budget" $ do
        it "evicts to keep the resident estimate under the byte budget" $ do
            -- A budget that holds three entries (the entry count is generous, so the
            -- byte budget is the binding bound): resolving many distinct keys must not
            -- let the resident estimate exceed it.
            let held = 3
            seen <- newIORef Nothing
            sf <- newStore 60 1000 (held * flatWeight + flatWeight `div` 2)
            for_ [1 .. 20 :: Int] $ \i ->
                resolveOkRecording seen sf (show i) (pure "raw")
            occ <- readIORef seen
            fmap occBytes occ `shouldSatisfy` maybe False (<= held * flatWeight + flatWeight `div` 2)
            fmap occEntries occ `shouldSatisfy` maybe False (<= held)

        it "retains a repeatedly-accessed entry while evicting the one-shot tail" $ do
            -- The hot head survives pressure: a budget that holds a few entries, a hot
            -- key re-accessed on every round, and a long tail of one-shot keys. The hot
            -- key is most-recently-used each round, so the least-recently-used eviction
            -- sheds the cold tail and never the head.
            let held = 3
            sf <- newStore 60 1000 (held * flatWeight + flatWeight `div` 2)
            _ <- resolveOk sf "hot" (pure "raw")
            for_ [1 .. 30 :: Int] $ \i -> do
                -- Touch the hot key (a hit, bumping its recency), then insert a one-shot.
                _ <- resolveOk sf "hot" (pure "unused")
                resolveOk sf ("cold-" <> show i) (pure "raw")
            lookupStore sf "hot" `shouldReturn` Just "raw"
            lookupStore sf "cold-1" `shouldReturn` Nothing

    describe "the oversized pass-through" $ do
        it "serves a value larger than the whole byte budget without retaining it" $ do
            sf <- newStore 60 100 (flatWeight - 1)
            calls <- newIORef (0 :: Int)
            resolveOk sf "big" (countingFetch calls "huge") `shouldReturn` "huge"
            -- Served, never retained: the next resolution re-leads its own fetch.
            lookupStore sf "big" `shouldReturn` Nothing
            resolveOk sf "big" (countingFetch calls "huge") `shouldReturn` "huge"
            readIORef calls `shouldReturn` 2

        it "evicts nothing resident to make room that cannot exist" $ do
            -- The budget fits exactly two flat entries; the weigher charges triple
            -- for the pathological value, so admitting it could only flush the store.
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
            -- Eight leaders on distinct keys with barrier-released fetches, so all
            -- eight evict-then-insert sequences collide; without the per-store
            -- insert lock two of them can both read the pre-insert resident sum
            -- and both admit, landing the store past its budget.
            let budget = 3 * flatWeight
            seen <- newIORef []
            sf <- newStore 60 1000 budget
            barrier <- newEmptyMVar
            leaders <- traverse (\(i :: Int) -> async (resolveOkAccumulating seen sf (show i) (readMVar barrier $> "v"))) [1 .. 8]
            putMVar barrier ()
            _ <- traverse wait leaders
            byteReadings <- map occBytes <$> readIORef seen
            byteReadings `shouldSatisfy` (not . null)
            byteReadings `shouldSatisfy` all (<= budget)
