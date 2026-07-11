module Ecluse.QueueSpec (spec) where

import System.Timeout (timeout)
import Test.Hspec
import UnliftIO (withAsync)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Fault (TransportCause (TransportUnreachable))
import Ecluse.Core.Queue (MirrorJob, MirrorQueue (..), newEnqueueBuffer, queueFault)
import Ecluse.Queue.Support (otherJob, sampleJob, thirdJob, unwrap)

{- | Tests for the contract module's buffered producer hand-off. The two in-memory
backends' coverage lives beside them in "Ecluse.Queue.MemorySpec".
-}
spec :: Spec
spec = do
    describe "newEnqueueBuffer" $ do
        it "delivers handed-off jobs to the backend in order" $ do
            delivered <- newIORef []
            (q, drainLoop) <- newEnqueueBuffer 8 (const pass) (\_ _ -> pass) (recordingBackend delivered)
            withAsync drainLoop $ \_ -> do
                traverse_ (unwrap . enqueue q) [sampleJob, otherJob, thirdJob]
                awaitUntil ((== (3 :: Int)) . length <$> readIORef delivered)
            readIORef delivered `shouldReturn` [sampleJob, otherJob, thirdJob]

        it "drops the newest hand-off at the cap, reporting every drop's running total" $ do
            -- The drain loop is deliberately not running, so the buffer stays full
            -- once its depth is reached and every further hand-off is a drop. The
            -- callback fires per drop (metric-grade); rate-limiting is the caller's.
            delivered <- newIORef []
            drops <- newIORef []
            (q, _drainLoop) <- newEnqueueBuffer 2 (\n -> modifyIORef' drops (<> [n])) (\_ _ -> pass) (recordingBackend delivered)
            traverse_ (unwrap . enqueue q) [sampleJob, otherJob, thirdJob, thirdJob]
            readIORef drops `shouldReturn` [1, 2]
            readIORef delivered `shouldReturn` [] -- nothing drained, nothing delivered
        it "keeps draining past a backend delivery fault, reporting its total and detail" $ do
            delivered <- newIORef []
            failures <- newIORef []
            failFirst <- newIORef True
            let flaky job = do
                    failNow <- atomicModifyIORef' failFirst (False,)
                    if failNow
                        then pure (Left (queueFault TransportUnreachable "backend unavailable"))
                        else Right () <$ modifyIORef' delivered (<> [job])
            (q, drainLoop) <-
                newEnqueueBuffer
                    8
                    (const pass)
                    (\n detail -> modifyIORef' failures (<> [(n, detail)]))
                    (recordingBackend delivered){enqueue = flaky}
            withAsync drainLoop $ \_ -> do
                traverse_ (unwrap . enqueue q) [sampleJob, otherJob]
                awaitUntil ((== (1 :: Int)) . length <$> readIORef delivered)
            -- The typed fault's detail arrives verbatim on the failure callback.
            readIORef failures `shouldReturn` [(1, "backend unavailable")]
            readIORef delivered `shouldReturn` [otherJob] -- the loop survived the failure
  where
    -- A backend stub whose 'enqueue' appends to the given ref, so a test can
    -- observe exactly what the buffer's drain loop delivered and in what order.
    -- The consumer fields are inert (the buffer passes them through untouched).
    recordingBackend :: IORef [MirrorJob] -> MirrorQueue
    recordingBackend delivered =
        MirrorQueue
            { enqueue = \job -> Right () <$ modifyIORef' delivered (<> [job])
            , receive = pure (Right [])
            , ack = const (pure (Right ()))
            , extendVisibility = \_ _ -> pure (Right ())
            }

    -- Poll (1ms cadence) until the condition holds, bounded at 2s so a broken
    -- drain loop fails the test loudly rather than hanging the suite.
    awaitUntil :: IO Bool -> IO ()
    awaitUntil cond = do
        outcome <- timeout 2_000_000 wait
        outcome `shouldBe` Just ()
      where
        wait = unlessM cond (threadDelay 1_000 *> wait)
