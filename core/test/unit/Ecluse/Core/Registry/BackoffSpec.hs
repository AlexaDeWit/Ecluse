-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Authority isolation, cooldown deadlines, and recovery ownership.
module Ecluse.Core.Registry.BackoffSpec (spec) where

import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Test.Hspec
import UnliftIO (cancel, mapConcurrently, timeout, wait, withAsync)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Registry.Backoff
import Ecluse.Core.Security.Authority (HostPort (HostPort))
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = do
    describe "authority cooldown" $ do
        it "refuses locally with a rounded positive remainder and isolates authorities" $
            fixture chooseCeiling $ \backoff setClock -> do
                open backoff 10
                refuses backoff alpha 10
                admitted backoff beta (const pass)
                setClock (addUTCTime 9.5 now)
                refuses backoff alpha 1
                setClock (addUTCTime 10 now)
                admitted backoff alpha (`recordPublicResponse` NonThrottling)
                admitted backoff alpha (const pass)

        it "does not let an older ordinary success close a newer cooldown" $
            fixture chooseCeiling $ \backoff _ ->
                admitted backoff alpha $ \older -> do
                    open backoff 20
                    recordPublicResponse older NonThrottling
                    refuses backoff alpha 20

        it "retains the longest concurrent throttle deadline in either completion order" $
            for_ [(10, 30), (30, 10)] $ \(earlierDelay, laterDelay) ->
                fixture chooseCeiling $ \backoff _ ->
                    admitted backoff alpha $ \earlierAttempt ->
                        admitted backoff alpha $ \laterAttempt -> do
                            recordPublicResponse earlierAttempt (Throttled (Just (RetryAfter earlierDelay)))
                            recordPublicResponse laterAttempt (Throttled (Just (RetryAfter laterDelay)))
                            refuses backoff alpha 30

        it "computes concurrent deadlines from each response's fresh time" $
            fixture chooseCeiling $ \backoff setClock ->
                admitted backoff alpha $ \earlierAttempt ->
                    admitted backoff alpha $ \laterAttempt -> do
                        recordPublicResponse earlierAttempt (Throttled (Just (RetryAfter 30)))
                        setClock (addUTCTime 20 now)
                        recordPublicResponse laterAttempt (Throttled (Just (RetryAfter 20)))
                        refuses backoff alpha 20

        it "settles one response once and ignores a retained attempt after callback exit" $
            fixture chooseCeiling $ \backoff _ -> do
                admitted backoff alpha $ \attempt -> do
                    recordPublicResponse attempt (Throttled (Just (RetryAfter 10)))
                    recordPublicResponse attempt (Throttled (Just (RetryAfter 30)))
                refuses backoff alpha 10
                late <- newIORef Nothing
                admitted backoff beta (writeIORef late . Just)
                readIORef late >>= traverse_ (\attempt -> recordPublicResponse attempt (Throttled (Just (RetryAfter 30))))
                admitted backoff beta (const pass)

    describe "recovery ownership" $ do
        it "admits one probe while concurrent callers refuse without waiting on its callback" $
            fixture chooseCeiling $ \backoff setClock -> do
                open backoff 10
                setClock (addUTCTime 10 now)
                entered <- newEmptyTMVarIO
                gate <- newEmptyTMVarIO
                let probe attempt = do
                        atomically (putTMVar entered ())
                        atomically (takeTMVar gate)
                        recordPublicResponse attempt NonThrottling
                withAsync (withPublicAttempt backoff alpha probe) $ \owner -> do
                    timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just ()
                    timeout 1_000_000 (mapConcurrently (const (withPublicAttempt backoff alpha (const pass))) [1 .. 8 :: Int])
                        `shouldReturn` Just (replicate 8 (Left (RetryAfter 1)))
                    admitted backoff beta (const pass)
                    atomically (putTMVar gate ())
                    wait owner `shouldReturn` Right ()
                admitted backoff alpha (const pass)

        it "releases a cancelled probe for immediate replacement" $
            fixture chooseCeiling $ \backoff setClock -> do
                open backoff 10
                setClock (addUTCTime 10 now)
                entered <- newEmptyTMVarIO
                gate <- newEmptyTMVarIO
                let probe _ = atomically (putTMVar entered ()) >> atomically (takeTMVar gate)
                withAsync (withPublicAttempt backoff alpha probe) $ \owner -> do
                    timeout 1_000_000 (atomically (takeTMVar entered)) `shouldReturn` Just ()
                    cancel owner
                admitted backoff alpha (`recordPublicResponse` NonThrottling)
                admitted backoff alpha (const pass)

        it "releases a pre-header exception without adding a cooldown" $
            fixture chooseCeiling $ \backoff setClock -> do
                open backoff 10
                setClock (addUTCTime 10 now)
                withPublicAttempt backoff alpha (\_ -> throwIO (TestContractEscape "before headers") :: IO ())
                    `shouldThrow` (== TestContractEscape "before headers")
                admitted backoff alpha (`recordPublicResponse` NonThrottling)

        it "fences ordinary outcomes after a newer recovery probe starts" $
            fixture chooseCeiling $ \backoff setClock ->
                admitted backoff alpha $ \oldSuccess ->
                    admitted backoff alpha $ \oldThrottle -> do
                        open backoff 10
                        setClock (addUTCTime 10 now)
                        admitted backoff alpha $ \probe -> do
                            recordPublicResponse oldSuccess NonThrottling
                            recordPublicResponse oldThrottle (Throttled (Just (RetryAfter 80)))
                            refuses backoff alpha 1
                            recordPublicResponse probe NonThrottling
                        admitted backoff alpha (const pass)

        it "fences a late throttle after a newer probe restores healthy traffic" $
            fixture chooseCeiling $ \backoff setClock ->
                admitted backoff alpha $ \old -> do
                    open backoff 10
                    setClock (addUTCTime 10 now)
                    admitted backoff alpha (`recordPublicResponse` NonThrottling)
                    recordPublicResponse old (Throttled (Just (RetryAfter 80)))
                    admitted backoff alpha (const pass)

        it "releases a probe when injected jitter throws before settlement" $
            fixture (\_ _ -> throwIO (TestContractEscape "jitter failed")) $ \backoff setClock -> do
                open backoff 10
                setClock (addUTCTime 10 now)
                withPublicAttempt backoff alpha (\attempt -> recordPublicResponse attempt (Throttled Nothing))
                    `shouldThrow` (== TestContractEscape "jitter failed")
                admitted backoff alpha (`recordPublicResponse` NonThrottling)

        it "fences released probe outcomes and cleanup from a newer owner" $
            fixture chooseCeiling $ \backoff setClock -> do
                open backoff 10
                setClock (addUTCTime 10 now)
                old <- newIORef Nothing
                admitted backoff alpha (writeIORef old . Just)
                admitted backoff alpha $ \probe -> do
                    readIORef old >>= traverse_ (\attempt -> recordPublicResponse attempt (Throttled (Just (RetryAfter 80))))
                    refuses backoff alpha 1
                    recordPublicResponse probe NonThrottling
                admitted backoff alpha (const pass)

    describe "fallback delay" $ do
        it "doubles failed recovery up to the fixed cap and resets after recovery" $
            fixture chooseCeiling $ \backoff setClock -> do
                for_ [(0, 60), (60, 120), (180, 240), (420, 480), (900, 600), (1500, 600)] $ \(offset, delay) -> do
                    setClock (addUTCTime offset now)
                    admitted backoff alpha (\attempt -> recordPublicResponse attempt (Throttled Nothing))
                    refuses backoff alpha delay
                setClock (addUTCTime 2100 now)
                admitted backoff alpha (`recordPublicResponse` NonThrottling)
                admitted backoff alpha (\attempt -> recordPublicResponse attempt (Throttled Nothing))
                refuses backoff alpha 60

        it "clamps injected jitter to the current floor and ceiling" $
            for_ [(RetryAfter (-10), 60), (RetryAfter 1000, 120)] $ \(sample, nextDelay) ->
                fixture (\_ _ -> pure sample) $ \backoff setClock -> do
                    admitted backoff alpha (\attempt -> recordPublicResponse attempt (Throttled Nothing))
                    refuses backoff alpha 60
                    setClock (addUTCTime 60 now)
                    admitted backoff alpha (\attempt -> recordPublicResponse attempt (Throttled Nothing))
                    refuses backoff alpha nextDelay

        it "uses a jitter sample inside the current fallback interval" $
            fixture (\_ _ -> pure (RetryAfter 90)) $ \backoff setClock -> do
                admitted backoff alpha (\attempt -> recordPublicResponse attempt (Throttled Nothing))
                refuses backoff alpha 60
                setClock (addUTCTime 60 now)
                admitted backoff alpha (\attempt -> recordPublicResponse attempt (Throttled Nothing))
                refuses backoff alpha 90

        it "stays at the production cap throughout sustained failed recovery" $
            fixture chooseCeiling $ \backoff setClock -> do
                for_ [(0, 60), (60, 120), (180, 240), (420, 480)] $ \(offset, delay) -> do
                    setClock (addUTCTime offset now)
                    admitted backoff alpha (\attempt -> recordPublicResponse attempt (Throttled Nothing))
                    refuses backoff alpha delay
                for_ [0 .. 20 :: Int] $ \cycleIndex -> do
                    setClock (addUTCTime (fromIntegral (900 + 600 * cycleIndex)) now)
                    admitted backoff alpha (\attempt -> recordPublicResponse attempt (Throttled Nothing))
                    refuses backoff alpha 600

fixture :: (RetryAfter -> RetryAfter -> IO RetryAfter) -> (PublicBackoff -> (UTCTime -> IO ()) -> Expectation) -> Expectation
fixture jitter action = do
    clock <- newIORef now
    backoff <- newPublicBackoff (readIORef clock) jitter
    action backoff (writeIORef clock)

chooseCeiling :: RetryAfter -> RetryAfter -> IO RetryAfter
chooseCeiling _ = pure

admitted :: PublicBackoff -> HostPort -> (PublicAttempt -> IO ()) -> Expectation
admitted backoff host action = withPublicAttempt backoff host action >>= either (expectationFailure . show) pure

open :: PublicBackoff -> Int -> Expectation
open backoff delay = admitted backoff alpha (\attempt -> recordPublicResponse attempt (Throttled (Just (RetryAfter delay))))

refuses :: PublicBackoff -> HostPort -> Int -> Expectation
refuses backoff host delay =
    withPublicAttempt backoff host (\_ -> expectationFailure "the cooling authority admitted a request") `shouldReturn` Left (RetryAfter delay)

alpha, beta :: HostPort
alpha = HostPort "public.example.test" 443
beta = HostPort "files.example.test" 443

now :: UTCTime
now = UTCTime (fromGregorian 2026 10 18) 0
