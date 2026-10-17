-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

-- | Unit contracts for the module at the corresponding source path.
module Ecluse.Core.Credential.Refresh.InternalSpec (spec) where

import Control.Concurrent (ThreadId, myThreadId, throwTo)
import Control.Exception (AsyncException (ThreadKilled))
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, fromGregorian)
import Hedgehog (
    Callback (Ensure, Require, Update),
    Command (Command),
    FunctorB (..),
    TraversableB (..),
    annotateShow,
    (===),
 )
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import UnliftIO (async, cancel, mapConcurrently, timeout, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (finally, throwIO, try)

import Ecluse.Core.Breaker (Breaker, BreakerPhase (..), BreakerReporter (..), admit, breakerPhase, initialBreaker, noBreakerReporter, recordFailure)
import Ecluse.Core.Credential
import Ecluse.Core.Credential.Refresh
import Ecluse.Core.Credential.Refresh.Internal (
    CacheState (..),
    MintClaim,
    ServeAction (..),
    decide,
    onMintFailure,
    onMintSuccess,
    refreshDueAt,
    refreshingProviderWith,
    releaseSingleFlight,
 )
import Ecluse.Core.Rules.Effectful (Resilience (..), defaultEffectfulConfig, newBreaker, runResilient)
import Ecluse.Test.Poll (awaitUntil, threadEnded)
import Ecluse.Test.Support (newTestClock)

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 6 22) 0

tokenExpiringIn :: Text -> NominalDiffTime -> AuthToken
tokenExpiringIn s ttl =
    AuthToken{authSecret = mkSecret s, authExpiresAt = Just (addUTCTime ttl t0)}

testConfig :: IO UTCTime -> IO AuthToken -> RefreshConfig
testConfig clock mint =
    defaultRefreshConfig
        { rcClock = clock
        , rcMint = mint
        , rcJitter = pure 0
        , rcRefreshAt = 0.8
        , rcBreakerThreshold = 3
        , rcBreakerCooldown = 30
        }

waitUntil :: IO Bool -> IO Bool
waitUntil = awaitUntil 2_000_000 1_000

waitForCount :: IORef Int -> Int -> IO Bool
waitForCount ref n = waitUntil ((>= n) <$> readIORef ref)

aCacheState :: CacheState
aCacheState =
    CacheState
        { csToken = tokenExpiringIn "cached" 1000
        , csRefreshDue = Just (addUTCTime 800 t0)
        , csMintClaim = Nothing
        , csClaimCounter = 0
        , csBreaker = initialBreaker
        }

breakerCfg :: RefreshConfig
breakerCfg = defaultRefreshConfig{rcBreakerThreshold = 3, rcBreakerCooldown = 30}

data RefreshEvent = ReportedSuccess (Maybe UTCTime) | ReportedFailure (Maybe UTCTime)
    deriving stock (Eq, Show)

capturingReporters :: IO (IORef [Breaker], IORef [RefreshEvent], BreakerReporter, RefreshReporter)
capturingReporters = do
    breakerLog <- newIORef []
    refreshLog <- newIORef []
    let breakerR = BreakerReporter (\b -> modifyIORef' breakerLog (<> [b]))
        refreshR =
            RefreshReporter
                { onRefreshSucceeded = \mttl -> modifyIORef' refreshLog (<> [ReportedSuccess mttl])
                , onRefreshFailed = \mttl -> modifyIORef' refreshLog (<> [ReportedFailure mttl])
                }
    pure (breakerLog, refreshLog, breakerR, refreshR)

data MintBoom = MintBoom
    deriving stock (Eq, Show)

instance Exception MintBoom

scriptedMint :: IORef [IO AuthToken] -> IO AuthToken
scriptedMint ref = join (atomicModifyIORef' ref next)
  where
    next (a : rest) = (rest, a)
    next [] = ([], throwIO MintBoom)

spec :: Spec
spec = do
    recoverySpec
    describe "refreshingProvider" $ do
        it "mints once at construction and serves that token while well inside its life" $ do
            (clock, _setClock) <- newTestClock t0
            mintCount <- newIORef (0 :: Int)
            let mint = atomicModifyIORef' mintCount (\n -> (n + 1, ())) >> pure (tokenExpiringIn "tok-1" 3600)
            provider <- refreshingProvider (testConfig clock mint)
            -- One mint seeded the cache at construction.
            readIORef mintCount `shouldReturn` 1
            got <- currentToken provider
            unSecret (authSecret got) `shouldBe` "tok-1"
            -- Still inside the refresh threshold: no extra mint, no background refresh.
            _ <- currentToken provider
            _ <- currentToken provider
            readIORef mintCount `shouldReturn` 1

        it "refreshes proactively in the background once past the refresh threshold" $ do
            (clock, setClock) <- newTestClock t0
            tokenRef <- newIORef (tokenExpiringIn "tok-1" 1000)
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    readIORef tokenRef
            provider <- refreshingProvider (testConfig clock mint)
            readIORef mintCount `shouldReturn` 1
            -- The token the background refresh will pick up next.
            writeIORef tokenRef (tokenExpiringIn "tok-2" 1000)
            -- Cross the 80% threshold (refresh due at 800s, token still valid).
            setClock (addUTCTime 850 t0)
            stale <- currentToken provider
            -- Nothing blocks the caller: it still gets the valid (old) token.
            unSecret (authSecret stale) `shouldBe` "tok-1"
            -- The background refresh eventually swaps in the new token.
            waitUntil ((== "tok-2") . unSecret . authSecret <$> currentToken provider)
                `shouldReturn` True
            -- Exactly one background mint fired on top of the seeding mint.
            readIORef mintCount `shouldReturn` 2

        it "is single-flight: a cohort past the threshold triggers at most one refresh mint" $ do
            (clock, setClock) <- newTestClock t0
            -- The refresh mint blocks on the gate, so it is demonstrably in flight while the
            -- cohort piles in. The seed mint (call #1) does not block.
            gate <- newEmptyTMVarIO
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    when (n >= 2) (atomically (takeTMVar gate))
                    pure (tokenExpiringIn "tok-2" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            readIORef mintCount `shouldReturn` 1
            setClock (addUTCTime 850 t0)
            -- A whole cohort calls currentToken while a refresh is in flight.
            replicateM_ 5 (void (currentToken provider))
            -- Wait for the (single) refresh mint to start.
            waitForCount mintCount 2 `shouldReturn` True
            -- Give any erroneous extra mints a chance to register, then assert
            -- only one refresh is ever in flight.
            threadDelay 20_000
            readIORef mintCount `shouldReturn` 2
            atomically (putTMVar gate ())
            waitUntil ((== "tok-2") . unSecret . authSecret <$> currentToken provider)
                `shouldReturn` True

        it "releases the single-flight flag when a mint is cancelled mid-flight (no wedge)" $ do
            -- Cancellation after the claim must release the flag to unblock expired callers.
            (clock, setClock) <- newTestClock t0
            started <- newEmptyTMVarIO
            gate <- newEmptyTMVarIO
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    case n of
                        1 -> pure (tokenExpiringIn "seed" 1000)
                        2 -> do
                            -- In the mint (flag claimed). Block so the test can cancel
                            -- the caller here, mid-flight.
                            atomically (putTMVar started ())
                            (atomically (takeTMVar gate) :: IO ())
                            pure (tokenExpiringIn "unreached" 5000)
                        _ -> pure (tokenExpiringIn "recovered" 5000)
            provider <- refreshingProvider (testConfig clock mint)
            setClock (addUTCTime 2000 t0) -- expired: the next serve mints synchronously
            blocked <- async (currentToken provider)
            atomically (takeTMVar started) -- the caller holds the flag and is in the mint
            cancel blocked -- async-cancel mid-mint: the finally must release the flag
            -- A fresh caller must not wedge: with the flag released it mints (call #3).
            result <- timeout 1_000_000 (currentToken provider)
            (unSecret . authSecret <$> result) `shouldBe` Just "recovered"

        it "releases the single-flight flag when the serving thread is cancelled at the claim handoff (no wedge)" $ do
            -- Cancellation before the mint runner installs its handler must still release the flag.
            (clock, setClock) <- newTestClock t0
            reached <- newEmptyTMVarIO
            release <- newEmptyTMVarIO
            armed <- newIORef True -- only the first claim parks, recovery runs free
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    if n == 1
                        then pure (tokenExpiringIn "seed" 1000)
                        else pure (tokenExpiringIn "recovered" 5000)
                -- Runs on the serving thread in the claim -> mint-runner window. Only the first
                -- claim parks here, interruptibly, so the cancel lands inside the window.
                afterClaim = do
                    wasArmed <- atomicModifyIORef' armed (False,)
                    when wasArmed $ do
                        atomically (putTMVar reached ())
                        (atomically (takeTMVar release) :: IO ())
            provider <- refreshingProviderWith afterClaim (testConfig clock mint)
            setClock (addUTCTime 2000 t0) -- expired: the next serve mints synchronously
            blocked <- async (currentToken provider)
            atomically (takeTMVar reached) -- flag claimed, thread parked at the handoff
            cancel blocked -- cancel in the handoff window: the flag must still release
            -- A fresh expired caller must not wedge: with the flag released it mints.
            result <- timeout 1_000_000 (currentToken provider)
            (unSecret . authSecret <$> result) `shouldBe` Just "recovered"

        it "keeps serving the still-valid token when a background mint fails" $ do
            (clock, setClock) <- newTestClock t0
            failRef <- newIORef False
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    bad <- readIORef failRef
                    if bad then throwIO MintBoom else pure (tokenExpiringIn "tok-1" 1000)
            failed <- newEmptyMVar
            let reporters = noCredentialReporters{crRefreshReporter = noRefreshReporter{onRefreshFailed = \_ -> myThreadId >>= putMVar failed}}
            provider <- refreshingProvider (testConfig clock mint){rcReporters = reporters}
            writeIORef failRef True
            setClock (addUTCTime 850 t0)
            _ <- currentToken provider
            finished <- timeout 2_000_000 $ do
                worker <- takeMVar failed
                waitUntil (threadEnded worker) `shouldReturn` True
            finished `shouldBe` Just ()
            readIORef mintCount `shouldReturn` 2
            -- The failed refresh left the cached token and its expiry in place.
            tok <- currentToken provider
            unSecret (authSecret tok) `shouldBe` "tok-1"
            authExpiresAt tok `shouldBe` Just (addUTCTime 1000 t0)

        it "surfaces failure to the caller only once the token has expired and mint still fails" $ do
            (clock, setClock) <- newTestClock t0
            failRef <- newIORef False
            let mint = do
                    bad <- readIORef failRef
                    if bad then throwIO MintBoom else pure (tokenExpiringIn "tok-1" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            writeIORef failRef True
            -- Past expiry: no valid token left to serve, and mint fails.
            setClock (addUTCTime 2000 t0)
            currentToken provider `shouldThrow` anyException

        it "rethrows the mint's own exception on the synchronous path" $ do
            (clock, setClock) <- newTestClock t0
            script <- newIORef [pure (tokenExpiringIn "eager" 10), throwIO MintBoom]
            provider <- refreshingProvider (testConfig clock (scriptedMint script))
            setClock (addUTCTime 20 t0)
            currentToken provider `shouldThrow` (== MintBoom)

        it "trips the breaker after repeated failures, then recovers on a half-open probe" $ do
            (clock, setClock) <- newTestClock t0
            failRef <- newIORef False
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    bad <- readIORef failRef
                    if bad
                        then throwIO MintBoom
                        else
                            if n == 1
                                then pure (tokenExpiringIn "tok-2" 1000)
                                else pure (tokenExpiringIn "tok-2" 5000)
            -- Seed succeeds. Failures start afterwards.
            provider <- refreshingProvider (testConfig clock mint)
            readIORef mintCount `shouldReturn` 1
            writeIORef failRef True
            -- Expire the token so every currentToken must mint synchronously.
            setClock (addUTCTime 2000 t0)
            -- Drive enough synchronous failures to trip the breaker (threshold = 3).
            replicateM_ 3 (currentToken provider `shouldThrow` anyException)
            afterTrip <- readIORef mintCount
            afterTrip `shouldBe` 4 -- seed + 3 failing mints
            -- While the breaker is open, a call fast-fails (with 'BreakerOpen')
            -- without minting.
            currentToken provider `shouldThrow` (== BreakerOpen)
            readIORef mintCount `shouldReturn` afterTrip
            -- After the cooldown elapses, the breaker half-opens: it admits one probe
            -- mint, that mint succeeds, and the provider recovers.
            writeIORef failRef False
            setClock (addUTCTime 60 (addUTCTime 2000 t0))
            recovered <- currentToken provider
            unSecret (authSecret recovered) `shouldBe` "tok-2"
            readIORef mintCount `shouldReturn` (afterTrip + 1)

        it "re-opens the breaker when the half-open probe also fails" $ do
            (clock, setClock) <- newTestClock t0
            failRef <- newIORef False
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    bad <- readIORef failRef
                    if bad then throwIO MintBoom else pure (tokenExpiringIn "tok-1" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            writeIORef failRef True
            setClock (addUTCTime 2000 t0)
            -- Trip the breaker.
            replicateM_ 3 (currentToken provider `shouldThrow` anyException)
            -- Cooldown elapsed: the breaker admits a half-open probe. It fails, so the breaker
            -- re-opens and the next call fast-fails without minting.
            setClock (addUTCTime 60 (addUTCTime 2000 t0))
            currentToken provider `shouldThrow` anyException
            afterProbe <- readIORef mintCount
            currentToken provider `shouldThrow` (== BreakerOpen)
            readIORef mintCount `shouldReturn` afterProbe

        it "waits for an in-flight refresh rather than launching a second mint when expired" $ do
            (clock, setClock) <- newTestClock t0
            gate <- newEmptyTMVarIO
            mintCount <- newIORef (0 :: Int)
            -- An expired caller must wait for the in-flight refresh blocked on the gate.
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    if n >= 2
                        then do
                            atomically (takeTMVar gate)
                            pure (tokenExpiringIn "tok-2" 5000)
                        else pure (tokenExpiringIn "tok-1" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            -- Cross the threshold to launch the background refresh.
            setClock (addUTCTime 850 t0)
            _ <- currentToken provider
            waitForCount mintCount 2 `shouldReturn` True
            -- Now jump past expiry while the refresh is still gated.
            setClock (addUTCTime 1200 t0)
            waiter <- async (currentToken provider)
            -- Give the waiter a chance to (wrongly) start a second mint.
            threadDelay 20_000
            readIORef mintCount `shouldReturn` 2
            -- Release the in-flight refresh. The waiter gets its result.
            atomically (putTMVar gate ())
            served <- wait waiter
            unSecret (authSecret served) `shouldBe` "tok-2"
            readIORef mintCount `shouldReturn` 2

        it "stops hammering the mint once repeated background refreshes trip the breaker" $ do
            (clock, setClock) <- newTestClock t0
            -- Requests past the refresh threshold must respect the breaker after repeated mint failures.
            seeded <- newIORef True
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    firstTime <- readIORef seeded
                    if firstTime
                        then writeIORef seeded False >> pure (tokenExpiringIn "tok-1" 10000)
                        else throwIO MintBoom
            provider <- refreshingProvider (testConfig clock mint)
            -- Past the refresh threshold (0.8 * 10000 = 8000), token still valid.
            setClock (addUTCTime 8500 t0)
            -- Drive a burst of requests. Each tries to refresh in the background.
            replicateM_ 8 (currentToken provider >> threadDelay 5_000)
            -- Let things settle. The breaker caps the failing mints at the seed plus at most the
            -- threshold, rather than one per request.
            threadDelay 30_000
            final <- readIORef mintCount
            final `shouldSatisfy` (<= 4) -- seed + at most threshold (3) failures
            tok <- currentToken provider
            unSecret (authSecret tok) `shouldBe` "tok-1"

        it "drives the default policy knobs (jitter, refresh fraction, breaker) end to end" $ do
            -- defaultRefreshConfig with only the effectful leaves wired: exercises
            -- the shipped defaults rather than test overrides.
            (clock, setClock) <- newTestClock t0
            tokenRef <- newIORef (tokenExpiringIn "tok-1" 1000)
            mintCount <- newIORef (0 :: Int)
            let mint = atomicModifyIORef' mintCount (\n -> (n + 1, ())) >> readIORef tokenRef
                cfg = defaultRefreshConfig{rcClock = clock, rcMint = mint}
            provider <- refreshingProvider cfg
            readIORef mintCount `shouldReturn` 1
            writeIORef tokenRef (tokenExpiringIn "tok-2" 1000)
            -- Default refresh fraction is 0.8 (jitter only pulls it earlier), so
            -- by 95% of life a background refresh is certainly due.
            setClock (addUTCTime 950 t0)
            _ <- currentToken provider
            waitUntil ((== "tok-2") . unSecret . authSecret <$> currentToken provider)
                `shouldReturn` True

        it "never refreshes a token that has no expiry" $ do
            (clock, setClock) <- newTestClock t0
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    pure AuthToken{authSecret = mkSecret "forever", authExpiresAt = Nothing}
            provider <- refreshingProvider (testConfig clock mint)
            readIORef mintCount `shouldReturn` 1
            -- No expiry: nothing ever schedules a refresh instant, so advancing the
            -- clock arbitrarily far still serves the original token, never minting.
            setClock (addUTCTime 1_000_000 t0)
            tok <- currentToken provider
            unSecret (authSecret tok) `shouldBe` "forever"
            readIORef mintCount `shouldReturn` 1

        it "fails loudly when built from defaults without wiring the mint and clock" $ do
            -- defaultRefreshConfig leaves rcMint and rcClock unwired, so construction must fail
            -- loudly whichever leaf is the missing one.
            refreshingProvider defaultRefreshConfig `shouldThrow` (== Unconfigured "rcClock")
            (clock, _setClock) <- newTestClock t0
            refreshingProvider defaultRefreshConfig{rcClock = clock} `shouldThrow` (== Unconfigured "rcMint")

        it "trips at the default breaker threshold and cooldown" $ do
            (clock, setClock) <- newTestClock t0
            seeded <- newIORef True
            let mint = do
                    firstTime <- readIORef seeded
                    if firstTime
                        then writeIORef seeded False >> pure (tokenExpiringIn "tok-1" 1000)
                        else throwIO MintBoom
                cfg = defaultRefreshConfig{rcClock = clock, rcMint = mint}
            provider <- refreshingProvider cfg
            -- Expire the token so each call mints synchronously and fails.
            setClock (addUTCTime 2000 t0)
            -- The default threshold is 5: five failures, then the breaker is open
            -- and the sixth call fast-fails with BreakerOpen.
            replicateM_ 5 (currentToken provider `shouldThrow` anyException)
            currentToken provider `shouldThrow` (== BreakerOpen)
            -- The default cooldown is 60s: before it elapses the breaker is still
            -- open. Once it does, the breaker admits a half-open probe again.
            setClock (addUTCTime 30 (addUTCTime 2000 t0))
            currentToken provider `shouldThrow` (== BreakerOpen)

    -- Direct unit tests for the pure state-transition helpers, so a regression localises to
    -- the function rather than only surfacing through 'currentToken'.
    describe "refreshDueAt" $ do
        it "schedules the refresh at the configured fraction of the lifetime" $ do
            -- Default knobs: 0.8 fraction, 30s floor, no jitter. A 1000s token
            -- issued at t0 is due at 800s (well clear of the 30s floor).
            due <- refreshDueAt defaultRefreshConfig t0 (tokenExpiringIn "tok" 1000)
            due `shouldBe` Just (addUTCTime 800 t0)

        it "never schedules later than the floor before expiry" $ do
            -- A short 100s token: 0.8 * 100 = 80s would land 20s before expiry,
            -- inside the 30s floor, so the floor (70s) wins.
            due <- refreshDueAt defaultRefreshConfig t0 (tokenExpiringIn "tok" 100)
            due `shouldBe` Just (addUTCTime 70 t0)

        it "pulls the refresh earlier by the sampled jitter fraction" $ do
            -- Jitter 0.2 pulls the 0.8 fraction down to 0.6 of the lifetime.
            due <- refreshDueAt defaultRefreshConfig{rcJitter = pure 0.2} t0 (tokenExpiringIn "tok" 1000)
            due `shouldBe` Just (addUTCTime 600 t0)

        it "never schedules a refresh before the issue instant" $ do
            -- A fraction of 0 cannot pull the due time before issue.
            due <- refreshDueAt defaultRefreshConfig{rcRefreshAt = 0} t0 (tokenExpiringIn "tok" 1000)
            due `shouldBe` Just t0

        it "never schedules a refresh for a token with no expiry" $ do
            let noExpiryToken = AuthToken{authSecret = mkSecret "forever", authExpiresAt = Nothing}
            refreshDueAt defaultRefreshConfig t0 noExpiryToken `shouldReturn` Nothing

    describe "mint ownership folds" $ do
        it "installs a successful token and leaves mint release to the guard" $ do
            stateVar <- newTVarIO aCacheState
            claim <- claimExpired stateVar
            st <- readTVarIO stateVar
            let token = tokenExpiringIn "fresh" 5000
                due = Just (addUTCTime 4000 t0)
                settled = onMintSuccess claim token due st
            csToken settled `shouldBe` token
            csRefreshDue settled `shouldBe` due
            csMintClaim settled `shouldBe` Just claim
            breakerPhase (csBreaker settled) `shouldBe` Closed 0

        it "counts failures, trips at the threshold, and retains the cache" $ do
            stateVar <- newTVarIO aCacheState
            forM_ [1 .. 3 :: Int] $ \count -> do
                claim <- claimExpired stateVar
                previousState <- readTVarIO stateVar
                let settled = onMintFailure claim breakerCfg t0 previousState
                csToken settled `shouldBe` csToken previousState
                csRefreshDue settled `shouldBe` csRefreshDue previousState
                csMintClaim settled `shouldBe` Just claim
                breakerPhase (csBreaker settled)
                    `shouldBe` if count < 3 then Closed count else Open (addUTCTime 30 t0)
                atomically (writeTVar stateVar settled)
                releaseSingleFlight stateVar claim
            csMintClaim <$> readTVarIO stateVar `shouldReturn` Nothing

        it "fences stale settlement and cleanup from the next mint owner" $ do
            stateVar <- newTVarIO aCacheState
            old <- claimExpired stateVar
            releaseSingleFlight stateVar old
            current <- claimExpired stateVar
            owned <- readTVarIO stateVar
            let staleSuccess = onMintSuccess old (tokenExpiringIn "stale" 9000) Nothing owned
                staleFailure = onMintFailure old breakerCfg t0 owned
            csToken staleSuccess `shouldBe` csToken owned
            csBreaker staleSuccess `shouldBe` csBreaker owned
            csBreaker staleFailure `shouldBe` csBreaker owned
            releaseSingleFlight stateVar old
            csMintClaim <$> readTVarIO stateVar `shouldReturn` Just current
            releaseSingleFlight stateVar current
            releaseSingleFlight stateVar current
            csMintClaim <$> readTVarIO stateVar `shouldReturn` Nothing

    describe "refresh telemetry reporting" $ do
        it "reports each refresh outcome and the breaker trip → probe → reset transitions" $ do
            (clock, setClock) <- newTestClock t0
            (breakerLog, refreshLog, breakerR, refreshR) <- capturingReporters
            script <-
                newIORef
                    [ pure (tokenExpiringIn "eager" 10) -- the eager construction mint
                    , throwIO MintBoom -- the next mint fails, tripping the breaker
                    , pure (tokenExpiringIn "recovered" 200) -- the probe mint recovers it
                    ]
            let cfg =
                    (testConfig clock (scriptedMint script))
                        { rcBreakerThreshold = 1
                        , rcReporters = CredentialReporters breakerR refreshR
                        }
            provider <- refreshingProvider cfg
            -- Construction's eager mint records nothing: the reporters fire on refreshes.
            (map breakerPhase <$> readIORef breakerLog) `shouldReturn` []
            readIORef refreshLog `shouldReturn` []
            -- The eager token (expires t0+10) is expired at t0+20, so the serve mints synchronously
            -- and fails. The failure reports the cached expiry.
            setClock (addUTCTime 20 t0)
            currentToken provider `shouldThrow` anyException
            (map breakerPhase <$> readIORef breakerLog) `shouldReturn` [Open (addUTCTime 50 t0)]
            readIORef refreshLog `shouldReturn` [ReportedFailure (Just (addUTCTime 10 t0))]
            -- The 30s cooldown elapses by t0+51, so the breaker admits a half-open probe. It
            -- succeeds, resetting the breaker and recording the fresh token's expiry.
            setClock (addUTCTime 51 t0)
            recovered <- currentToken provider
            unSecret (authSecret recovered) `shouldBe` "recovered"
            (map breakerPhase <$> readIORef breakerLog) `shouldReturn` [Open (addUTCTime 50 t0), HalfOpen, Closed 0]
            readIORef refreshLog
                `shouldReturn` [ReportedFailure (Just (addUTCTime 10 t0)), ReportedSuccess (Just (addUTCTime 200 t0))]

        it "is silent and never throws on that account when wired with the default no-op reporters" $ do
            (clock, setClock) <- newTestClock t0
            script <- newIORef [pure (tokenExpiringIn "eager" 10), throwIO MintBoom]
            -- 'defaultRefreshConfig' (via 'testConfig') wires 'noCredentialReporters':
            -- a refresh that trips the breaker records nothing.
            provider <- refreshingProvider (testConfig clock (scriptedMint script)){rcBreakerThreshold = 1}
            setClock (addUTCTime 20 t0)
            currentToken provider `shouldThrow` anyException

    describe "decide" $ do
        it "serves a valid token without claiming a mint before its refresh instant" $ do
            stateVar <- newTVarIO aCacheState
            atomically (decide stateVar t0) `shouldReturn` ServeCached (csToken aCacheState)
            csMintClaim <$> readTVarIO stateVar `shouldReturn` Nothing

        it "claims due background work once and keeps serving the valid cache" $ do
            stateVar <- newTVarIO aCacheState
            atomically (decide stateVar (addUTCTime 850 t0)) >>= \case
                ServeAndRefresh token claim -> do
                    token `shouldBe` csToken aCacheState
                    csMintClaim <$> readTVarIO stateVar `shouldReturn` Just claim
                _ -> expectationFailure "expected a background mint claim"
            atomically (decide stateVar (addUTCTime 850 t0)) `shouldReturn` ServeCached (csToken aCacheState)

        it "waits for an ordinary in-flight mint after expiry" $ do
            stateVar <- newTVarIO aCacheState
            _ <- claimExpired stateVar
            timeout 50_000 (atomically (decide stateVar (addUTCTime 2000 t0))) `shouldReturn` Nothing

        it "refuses expired recovery followers, but serves valid tokens during recovery" $ do
            stateVar <- newTVarIO aCacheState{csBreaker = trippedAt t0}
            atomically (decide stateVar (addUTCTime 850 t0)) >>= \case
                ServeAndRefresh _ _ -> pass
                _ -> expectationFailure "expected owned background recovery"
            atomically (decide stateVar (addUTCTime 850 t0)) `shouldReturn` ServeCached (csToken aCacheState)
            atomically (decide stateVar (addUTCTime 2000 t0)) `shouldReturn` RefuseMint

        it "does not let an old recovery mint settle or release its replacement" $ do
            stateVar <- newTVarIO aCacheState{csBreaker = trippedAt t0}
            old <- claimExpired stateVar
            releaseSingleFlight stateVar old
            breakerPhase . csBreaker <$> readTVarIO stateVar `shouldReturn` RecoveryReady
            current <- claimExpired stateVar
            owned <- readTVarIO stateVar
            csBreaker (onMintSuccess old (tokenExpiringIn "stale" 9000) Nothing owned) `shouldBe` csBreaker owned
            csBreaker (onMintFailure old breakerCfg t0 owned) `shouldBe` csBreaker owned
            releaseSingleFlight stateVar old
            csMintClaim <$> readTVarIO stateVar `shouldReturn` Just current
            atomically (decide stateVar (addUTCTime 2000 t0)) `shouldReturn` RefuseMint

    describe "refresh completion observations" $
        it "rejects a delayed start and a reported refresh whose worker still holds the claim" $ do
            h <- newHarness t0
            claimed <- newEmptyMVar
            start <- newEmptyMVar
            reported <- newEmptyMVar
            finish <- newEmptyMVar
            provider <-
                harnessProviderWith
                    (putMVar claimed () >> takeMVar start)
                    (putMVar reported () >> takeMVar finish)
                    h
            writeIORef (hClock h) (addUTCTime 850 t0)
            let exercise = do
                    _ <- currentToken provider
                    takeMVar claimed
                    awaitRefreshes 10_000 h 2 `shouldReturn` False
                    readIORef (hMintCount h) `shouldReturn` 1
                    putMVar start ()
                    takeMVar reported
                    awaitRefreshes 10_000 h 2 `shouldReturn` False
                    readIORef (hMintCount h) `shouldReturn` 2
                    putMVar finish ()
                    awaitRefreshes 2_000_000 h 2 `shouldReturn` True
                releaseWorker = do
                    void (tryPutMVar start ())
                    void (tryPutMVar finish ())
                    awaitRefreshes 2_000_000 h 2 `shouldReturn` True
            result <- timeout 2_000_000 exercise `finally` releaseWorker
            result `shouldBe` Just ()

    describe "refreshingProvider (model-based)" $
        it "agrees with a pure cache/clock/breaker model under random operation sequences" $
            hedgehog refreshModelProperty

modelRefreshAt :: Double
modelRefreshAt = 0.8

modelRefreshFloor :: NominalDiffTime
modelRefreshFloor = 30

modelBreakerThreshold :: Int
modelBreakerThreshold = 3

modelBreakerCooldown :: NominalDiffTime
modelBreakerCooldown = 30

tokenLiving :: Text -> UTCTime -> NominalDiffTime -> AuthToken
tokenLiving s issuedAt ttl =
    AuthToken{authSecret = mkSecret s, authExpiresAt = Just (addUTCTime ttl issuedAt)}

data MBreaker
    = MClosed Int
    | MOpen UTCTime
    | MHalfOpen
    deriving stock (Eq, Show)

data RModel (v :: Type -> Type) = RModel
    { rmToken :: AuthToken
    -- ^ The token the cache currently holds (and will serve while valid).
    , rmRefreshDue :: Maybe UTCTime
    -- ^ When a proactive background refresh is due. 'Nothing' never refreshes.
    , rmBreaker :: MBreaker
    -- ^ The circuit-breaker state.
    , rmNow :: UTCTime
    -- ^ The model's clock (advanced by 'AdvanceClock').
    , rmFail :: Bool
    -- ^ Whether the next mint is set to fail (toggled by 'SetFail').
    , rmExpectedMints :: Int
    -- ^ How many mints the implementation should have performed so far.
    , rmNextToken :: Int
    -- ^ Index of the next distinct token the mint will hand out on success.
    }

mTokenValid :: UTCTime -> AuthToken -> Bool
mTokenValid now token = case authExpiresAt token of
    Nothing -> True
    Just expiry -> now < expiry

mRefreshNeeded :: UTCTime -> RModel v -> Bool
mRefreshNeeded now m = case rmRefreshDue m of
    Nothing -> False
    Just due -> now >= due

mRefreshDueAt :: UTCTime -> AuthToken -> Maybe UTCTime
mRefreshDueAt issuedAt token = case authExpiresAt token of
    Nothing -> Nothing
    Just expiry ->
        let lifetime = realToFrac (diffUTCTime expiry issuedAt) :: Double
            frac = clamp01 modelRefreshAt
            byFraction = addUTCTime (realToFrac (frac * lifetime)) issuedAt
            floorInstant = addUTCTime (negate modelRefreshFloor) expiry
         in Just (max issuedAt (min byFraction floorInstant))
  where
    clamp01 = max 0 . min 1

mAdmit :: UTCTime -> MBreaker -> (Bool, MBreaker)
mAdmit now = \case
    MOpen until'
        | now < until' -> (False, MOpen until')
        | otherwise -> (True, MHalfOpen)
    other -> (True, other)

mOnSuccess :: MBreaker
mOnSuccess = MClosed 0

mOnFailure :: UTCTime -> MBreaker -> MBreaker
mOnFailure now = \case
    MClosed n
        | n + 1 >= modelBreakerThreshold -> tripped
        | otherwise -> MClosed (n + 1)
    _ -> tripped
  where
    tripped = MOpen (addUTCTime modelBreakerCooldown now)

data RequestOutcome
    = ServedToken AuthToken
    | RaisedError
    deriving stock (Eq, Show)

stepRequest :: RModel v -> (RequestOutcome, RModel v)
stepRequest m
    | mTokenValid now (rmToken m) =
        if mRefreshNeeded now m
            then -- Valid but past the threshold: a background refresh fires (if the
            -- breaker admits). The caller still gets the current, valid token.
                (ServedToken (rmToken m), backgroundRefreshed)
            else (ServedToken (rmToken m), m)
    | otherwise = expiredPath
  where
    now = rmNow m

    backgroundRefreshed =
        let (admitted, br') = mAdmit now (rmBreaker m)
         in if not admitted
                then m{rmBreaker = br'}
                else mintInto m{rmBreaker = br'} (rmFail m)

    expiredPath =
        let (admitted, br') = mAdmit now (rmBreaker m)
         in if not admitted
                then (RaisedError, m{rmBreaker = br'})
                else
                    let m' = mintInto m{rmBreaker = br'} (rmFail m)
                     in if rmFail m
                            then (RaisedError, m')
                            else (ServedToken (rmToken m'), m')

    mintInto base failed
        | failed =
            base
                { rmBreaker = mOnFailure now (rmBreaker base)
                , rmExpectedMints = rmExpectedMints base + 1
                }
        | otherwise =
            let fresh = tokenLiving (mintName (rmNextToken base)) now 1000
             in base
                    { rmToken = fresh
                    , rmRefreshDue = mRefreshDueAt now fresh
                    , rmBreaker = mOnSuccess
                    , rmExpectedMints = rmExpectedMints base + 1
                    , rmNextToken = rmNextToken base + 1
                    }

mintName :: Int -> Text
mintName n = "tok-" <> show n

data RefreshHarness = RefreshHarness
    { hClock :: IORef UTCTime
    , hFail :: IORef Bool
    , hMintCount :: IORef Int
    , hNextToken :: IORef Int
    , hInFlight :: IORef Int
    , hMaxInFlight :: IORef Int
    , hReportedWorkers :: IORef [ThreadId]
    }

newHarness :: UTCTime -> IO RefreshHarness
newHarness start =
    RefreshHarness
        <$> newIORef start
        <*> newIORef False
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef []

harnessProvider :: RefreshHarness -> IO CredentialProvider
harnessProvider = harnessProviderWith (pure ()) (pure ())

harnessProviderWith :: IO () -> IO () -> RefreshHarness -> IO CredentialProvider
harnessProviderWith afterClaim afterReport h =
    refreshingProviderWith
        afterClaim
        defaultRefreshConfig
            { rcClock = readIORef (hClock h)
            , rcJitter = pure 0
            , rcRefreshAt = modelRefreshAt
            , rcRefreshFloor = modelRefreshFloor
            , rcBreakerThreshold = modelBreakerThreshold
            , rcBreakerCooldown = modelBreakerCooldown
            , rcMint = mint
            , rcReporters = noCredentialReporters{crRefreshReporter = RefreshReporter (const report) (const report)}
            }
  where
    report = do
        worker <- myThreadId
        atomicModifyIORef' (hReportedWorkers h) (\workers -> (worker : workers, ()))
        afterReport
    mint = do
        inFlight <- atomicModifyIORef' (hInFlight h) (\n -> (n + 1, n + 1))
        atomicModifyIORef' (hMaxInFlight h) (\hi -> (max hi inFlight, ()))
        _ <- atomicModifyIORef' (hMintCount h) (\n -> (n + 1, ()))
        now <- readIORef (hClock h)
        bad <- readIORef (hFail h)
        let leave = atomicModifyIORef' (hInFlight h) (\n -> (n - 1, ()))
        if bad
            then leave >> throwIO MintBoom
            else do
                idx <- atomicModifyIORef' (hNextToken h) (\n -> (n + 1, n))
                leave
                pure (tokenLiving (mintName idx) now 1000)

newtype RequestInput (v :: Type -> Type) = RequestInput Int
    deriving stock (Show)

instance FunctorB RequestInput where
    bmap _ (RequestInput count) = RequestInput count

instance TraversableB RequestInput where
    btraverse _ (RequestInput count) = pure (RequestInput count)

newtype AdvanceInput (v :: Type -> Type) = AdvanceInput NominalDiffTime
    deriving stock (Show)

instance FunctorB AdvanceInput where
    bmap _ (AdvanceInput d) = AdvanceInput d

instance TraversableB AdvanceInput where
    btraverse _ (AdvanceInput d) = pure (AdvanceInput d)

newtype SetFailInput (v :: Type -> Type) = SetFailInput Bool
    deriving stock (Show)

instance FunctorB SetFailInput where
    bmap _ (SetFailInput b) = SetFailInput b

instance TraversableB SetFailInput where
    btraverse _ (SetFailInput b) = pure (SetFailInput b)

advanceCommand :: RefreshHarness -> Command H.Gen (H.PropertyT IO) RModel
advanceCommand h =
    Command
        (const (Just (AdvanceInput . fromInteger <$> Gen.integral (Range.linear 0 600))))
        (\(AdvanceInput d) -> liftIO (atomicModifyIORef' (hClock h) (\now -> (addUTCTime d now, ()))))
        [ Update $ \m (AdvanceInput d) _out -> m{rmNow = addUTCTime d (rmNow m)}
        ]

setFailCommand :: RefreshHarness -> Command H.Gen (H.PropertyT IO) RModel
setFailCommand h =
    Command
        (const (Just (SetFailInput <$> Gen.bool)))
        (\(SetFailInput b) -> liftIO (writeIORef (hFail h) b))
        [ Update $ \m (SetFailInput b) _out -> m{rmFail = b}
        ]

requestCommand :: RefreshHarness -> CredentialProvider -> Command H.Gen (H.PropertyT IO) RModel
requestCommand h provider =
    Command
        (Just . pure . RequestInput . rmExpectedMints . snd . stepRequest)
        execute
        [ Require $ \m (RequestInput expected) -> expected == rmExpectedMints (snd (stepRequest m))
        , Update $ \m (RequestInput _) _out -> snd (stepRequest m)
        , Ensure $ \beforeState afterState (RequestInput _) (observed, maxInFlight, mintCount) -> do
            let (expected, _) = stepRequest beforeState
            annotateShow (rmNow beforeState)
            annotateShow expected
            outcomeMatches expected observed
            -- A served token is always valid at the clock it was served under
            -- (the wrapper never hands back an expired token).
            case observed of
                Right tok -> H.assert (mTokenValid (rmNow beforeState) tok)
                Left _ -> H.success
            H.assert (maxInFlight <= 1)
            mintCount === rmExpectedMints afterState
            -- Coverage is per step, including the asynchronous refresh arm.
            let tag = coverTag beforeState expected
            H.cover 1 "valid-bg-refresh" (tag == "valid-bg-refresh")
            H.cover 1 "expired-mint-ok" (tag == "expired-mint-ok")
            H.cover 1 "expired-error" (tag == "expired-error")
        ]
  where
    execute (RequestInput expected) = liftIO $ do
        result <- try (currentToken provider)
        awaitRefreshes 2_000_000 h expected `shouldReturn` True
        maxInFlight <- readIORef (hMaxInFlight h)
        mintCount <- readIORef (hMintCount h)
        pure (toObserved result, maxInFlight, mintCount)

    toObserved :: Either SomeException AuthToken -> Either Text AuthToken
    toObserved = first (const "error")

    coverTag :: RModel v -> RequestOutcome -> Text
    coverTag st expected =
        let valid = mTokenValid (rmNow st) (rmToken st)
            refreshDue = mRefreshNeeded (rmNow st) st
         in case (valid, refreshDue, expected) of
                (True, False, _) -> "serve-cached"
                (True, True, _) -> "valid-bg-refresh"
                (False, _, RaisedError) -> "expired-error"
                (False, _, ServedToken _) -> "expired-mint-ok"

    outcomeMatches expected observed = case (expected, observed) of
        (ServedToken tok, Right got) ->
            unSecret (authSecret got) === unSecret (authSecret tok)
        (RaisedError, Left _) -> H.success
        _ -> do
            annotateShow ("outcome mismatch" :: Text, expected, fmap (unSecret . authSecret) observed)
            H.failure

-- Reporters follow the fold. Worker exit also proves the single-flight finaliser returned.
awaitRefreshes :: Int -> RefreshHarness -> Int -> IO Bool
awaitRefreshes budget h expected = do
    caller <- myThreadId
    awaitUntil budget 1_000 $ do
        started <- readIORef (hMintCount h)
        workers <- readIORef (hReportedWorkers h)
        ended <- and <$> traverse threadEnded (filter (/= caller) workers)
        pure (started >= expected && length workers >= expected - 1 && ended)

refreshModelProperty :: H.PropertyT IO ()
refreshModelProperty = do
    h <- liftIO (newHarness t0)
    provider <- liftIO (harnessProvider h)
    -- Construction performed exactly one mint, installing tok-0 (lives 1000s).
    seedToken <- liftIO (currentToken provider)
    liftIO (awaitRefreshes 2_000_000 h 1 `shouldReturn` True)
    let initial =
            RModel
                { rmToken = seedToken
                , rmRefreshDue = mRefreshDueAt t0 seedToken
                , rmBreaker = MClosed 0
                , rmNow = t0
                , rmFail = False
                , rmExpectedMints = 1
                , rmNextToken = 1
                }
        commands =
            [ requestCommand h provider
            , advanceCommand h
            , setFailCommand h
            ]
    actions <- H.forAll (Gen.sequential (Range.linear 1 40) initial commands)
    H.executeSequential initial actions

claimExpired :: TVar CacheState -> IO MintClaim
claimExpired stateVar =
    atomically (decide stateVar (addUTCTime 2000 t0)) >>= \case
        MintNow claim -> pure claim
        _ -> throwIO MintBoom

trippedAt :: UTCTime -> Breaker
trippedAt clock = case admit clock initialBreaker of
    (Just ticket, claimed) -> recordFailure 1 30 clock ticket claimed
    (Nothing, unchanged) -> unchanged

recoverySpec :: Spec
recoverySpec = describe "owned credential recovery" $ do
    it "refuses expired followers before a synchronous recovery mint finishes" $ do
        (clock, setClock) <- newTestClock t0
        entered <- newEmptyTMVarIO
        gate <- newEmptyTMVarIO
        script <-
            newIORef
                [ pure (tokenExpiringIn "seed" 10)
                , throwIO MintBoom
                , atomically (putTMVar entered ()) >> atomically (takeTMVar gate) >> pure (tokenExpiringIn "fresh" 5000)
                ]
        (breakerLog, refreshLog, breakerR, refreshR) <- capturingReporters
        let cfg = (testConfig clock (scriptedMint script)){rcBreakerThreshold = 1, rcReporters = CredentialReporters breakerR refreshR}
        provider <- refreshingProvider cfg
        setClock (addUTCTime 20 t0)
        currentToken provider `shouldThrow` (== MintBoom)
        setClock (addUTCTime 50 t0)
        withAsync (currentToken provider) $ \winner -> do
            timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just ()
            timeout 1_000_000 (mapConcurrently (const (try (currentToken provider) :: IO (Either CredentialError AuthToken))) [1 .. 8 :: Int])
                `shouldReturn` Just (replicate 8 (Left BreakerOpen))
            readIORef refreshLog `shouldReturn` [ReportedFailure (Just (addUTCTime 10 t0))]
            (map breakerPhase <$> readIORef breakerLog) `shouldReturn` [Open (addUTCTime 50 t0), HalfOpen]
            atomically (putTMVar gate ())
            unSecret . authSecret <$> wait winner `shouldReturn` "fresh"
        unSecret . authSecret <$> currentToken provider `shouldReturn` "fresh"

    forM_ [(background, handoff) | background <- [False, True], handoff <- [False, True]] $ \(background, handoff) ->
        it ("releases cancellation, background = " <> show background <> ", handoff = " <> show handoff) $ do
            (clock, setClock) <- newTestClock t0
            failed <- newEmptyTMVarIO
            entered <- newEmptyTMVarIO
            gate <- newEmptyTMVarIO
            calls <- newIORef (0 :: Int)
            armed <- newIORef False
            refreshEvents <- newIORef (0 :: Int)
            let park = myThreadId >>= \tid -> atomically (putTMVar entered tid) >> atomically (takeTMVar gate)
                hook = whenM (readIORef armed) (when handoff park)
                mint = do
                    n <- atomicModifyIORef' calls (\count -> (count + 1, count + 1))
                    case n of
                        1 -> pure (tokenExpiringIn "seed" 1000)
                        2 -> throwIO MintBoom
                        3 -> whenM (readIORef armed) (unless handoff park) >> pure (tokenExpiringIn "fresh" 5000)
                        _ -> pure (tokenExpiringIn "fresh" 5000)
                reporter = noRefreshReporter{onRefreshFailed = \_ -> modifyIORef' refreshEvents (+ 1) >> myThreadId >>= atomically . putTMVar failed}
                cfg = (testConfig clock mint){rcBreakerThreshold = 1, rcReporters = CredentialReporters noBreakerReporter reporter}
            provider <- refreshingProviderWith hook cfg
            setClock (addUTCTime 850 t0)
            unSecret . authSecret <$> currentToken provider `shouldReturn` "seed"
            failedWorker <- timeout 1_000_000 (atomically (takeTMVar failed))
            forM_ failedWorker $ \worker -> waitUntil (threadEnded worker) `shouldReturn` True
            failedWorker `shouldSatisfy` isJust
            setClock (addUTCTime (if background then 880 else 1001) t0)
            writeIORef armed True
            withAsync (currentToken provider) $ \caller -> do
                tid <- timeout 1_000_000 (atomically (takeTMVar entered))
                when background $ do
                    unSecret . authSecret <$> wait caller `shouldReturn` "seed"
                    unSecret . authSecret <$> currentToken provider `shouldReturn` "seed"
                    setClock (addUTCTime 1001 t0)
                    timeout 1_000_000 (try (currentToken provider) :: IO (Either CredentialError AuthToken))
                        `shouldReturn` Just (Left BreakerOpen)
                writeIORef armed False
                case tid of
                    Just worker ->
                        if background
                            then throwTo worker ThreadKilled >> (waitUntil (threadEnded worker) `shouldReturn` True)
                            else cancel caller
                    Nothing -> expectationFailure "recovery did not reach its cancellation barrier"
            readIORef refreshEvents `shouldReturn` 1
            setClock (addUTCTime 1001 t0)
            timeout 1_000_000 (currentToken provider) >>= \case
                Just token -> unSecret (authSecret token) `shouldBe` "fresh"
                Nothing -> expectationFailure "cancelled recovery stranded its claim"
            readIORef calls `shouldReturn` if handoff then 3 else 4

    forM_ [False, True] $ \advisoryOwner ->
        it ("keeps consumers and providers independent, advisory owner = " <> show advisoryOwner) $ do
            (clock, setClock) <- newTestClock t0
            entered <- newEmptyTMVarIO
            gate <- newEmptyTMVarIO
            let park = atomically (putTMVar entered ()) >> atomically (takeTMVar gate)
            script <-
                newIORef
                    [ pure (tokenExpiringIn "seed" 10)
                    , throwIO MintBoom
                    , unless advisoryOwner park >> pure (tokenExpiringIn "fresh" 5000)
                    ]
            firstProvider <- refreshingProvider (testConfig clock (scriptedMint script)){rcBreakerThreshold = 1}
            otherScript <- newIORef [pure (tokenExpiringIn "other-seed" 10), throwIO MintBoom, pure (tokenExpiringIn "other-fresh" 5000)]
            other <- refreshingProvider (testConfig clock (scriptedMint otherScript)){rcBreakerThreshold = 1}
            advisory <- newBreaker
            atomically (writeTVar advisory (trippedAt t0))
            let res = Resilience defaultEffectfulConfig advisory noBreakerReporter clock
                owned = if advisoryOwner then void (runResilient res park) else void (currentToken firstProvider)
            setClock (addUTCTime 20 t0)
            currentToken firstProvider `shouldThrow` (== MintBoom)
            currentToken other `shouldThrow` (== MintBoom)
            setClock (addUTCTime 50 t0)
            withAsync owned $ \winner -> do
                timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just ()
                if advisoryOwner
                    then unSecret . authSecret <$> currentToken firstProvider `shouldReturn` "fresh"
                    else runResilient res (pure ()) `shouldReturn` Right ()
                unSecret . authSecret <$> currentToken other `shouldReturn` "other-fresh"
                atomically (putTMVar gate ())
                wait winner `shouldReturn` ()
