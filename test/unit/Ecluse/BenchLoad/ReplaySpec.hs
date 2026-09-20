-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.BenchLoad.ReplaySpec (spec) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Types (status200, status503)
import Network.Wai (pathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Async (cancel, waitCatch, withAsync)

import Ecluse.BenchLoad.Harness (Driver (DriveReplay), warmUp)
import Ecluse.BenchLoad.Oha (OhaReport (..))
import Ecluse.BenchLoad.PatternReport (ReplayTotals (..))
import Ecluse.BenchLoad.Patterns (ClientTrace (..), RequestTrace (..))
import Ecluse.BenchLoad.Replay (Replay (..), ReplayReport (..), runReplay)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = describe "finite HTTP replay" $ do
    it "leaves a fresh HTTP fixture cold and never consumes the finite trace during warm-up" $ do
        seen <- newIORef ([] :: [[Text]])
        let app request respond = do
                atomicModifyIORef' seen (\paths -> (pathInfo request : paths, ()))
                respond (responseLBS status200 [] "{}")
        testWithApplication (pure app) $ \port -> do
            let replay =
                    Replay
                        (RequestTrace [ClientTrace 0 0 ["first", "second"]] ["first", "second"])
                        5_000_000
                        (\name -> [localhost port <> "/" <> name])
                        (pure "")
            warmUp (DriveReplay replay)
            readIORef seen `shouldReturn` []
            report <- replayHttp <$> runReplay replay
            reverse <$> readIORef seen `shouldReturn` [["first"], ["second"]]
            ohaStatusCounts report `shouldBe` Map.singleton "200" 2
            ohaSuccessRate report `shouldBe` 1
    it "retains refused responses while successful latency stays unavailable" $
        testWithApplication (pure (\_ respond -> respond (responseLBS status503 [] "refused"))) $ \port -> do
            let replay =
                    Replay
                        (RequestTrace [ClientTrace 0 0 ["one"]] ["one"])
                        5_000_000
                        (const [localhost port <> "/one"])
                        (pure "")
            report <- replayHttp <$> runReplay replay
            ohaStatusCounts report `shouldBe` Map.singleton "503" 1
            ohaSuccessRate report `shouldBe` 0
            ohaP99 report `shouldBe` Nothing
            ohaRequestsPerSec report `shouldBe` 0
    it "expands a listing into a selected-version request in order" $ do
        seen <- newIORef ([] :: [[Text]])
        let app request respond = do
                atomicModifyIORef' seen (\paths -> (pathInfo request : paths, ()))
                respond (responseLBS status200 [] "{}")
        testWithApplication (pure app) $ \port -> do
            let replay =
                    Replay
                        (RequestTrace [ClientTrace 0 0 ["pkg"]] ["pkg"])
                        5_000_000
                        (\name -> [localhost port <> "/" <> name, localhost port <> "/" <> name <> "/1.0.0"])
                        (pure "")
            _ <- runReplay replay
            reverse <$> readIORef seen `shouldReturn` [["pkg"], ["pkg", "1.0.0"]]

    it "cancels clients waiting for their scheduled start and counts unstarted work" $
        testWithApplication (pure (\_ respond -> respond (responseLBS status200 [] "{}"))) $ \port -> do
            let replay =
                    Replay
                        (RequestTrace [ClientTrace 10_000_000 0 ["later"]] ["later"])
                        50_000
                        (const [localhost port <> "/later"])
                        (pure "")
            result <- runReplay replay
            rtotalScheduled (replayTotals result) `shouldBe` 1
            rtotalCompleted (replayTotals result) `shouldBe` 0
            rtotalUnfinished (replayTotals result) `shouldBe` 1
            rtotalTransportFailed (replayTotals result) `shouldBe` 0
    it "bounds a blocked response read without misclassifying cancellation as a transport failure" $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let app _ respond = do
                putMVar entered ()
                takeMVar release
                respond (responseLBS status200 [] "{}")
        testWithApplication (pure app) $ \port -> do
            let replay =
                    Replay
                        (RequestTrace [ClientTrace 0 0 ["blocked"]] ["blocked"])
                        1_000_000
                        (const [localhost port <> "/blocked"])
                        (pure "")
            withAsync (runReplay replay) $ \worker -> do
                timeout 2_000_000 (takeMVar entered) `shouldReturn` Just ()
                outcome <- waitCatch worker
                putMVar release ()
                case outcome of
                    Left failure -> expectationFailure (show failure)
                    Right result -> do
                        rtotalUnfinished (replayTotals result) `shouldBe` 1
                        rtotalTransportFailed (replayTotals result) `shouldBe` 0
    it "propagates external cancellation instead of returning a successful report" $ do
        let replay =
                Replay
                    (RequestTrace [ClientTrace 60_000_000 0 ["later"]] ["later"])
                    120_000_000
                    (const ["http://localhost:1/later"])
                    (pure "")
        withAsync (runReplay replay) $ \worker -> do
            cancel worker
            outcome <- waitCatch worker
            outcome `shouldSatisfy` isLeft
    it "accounts for successes, refusals, transport failures and the scheduled denominator"
        $ testWithApplication
            ( pure
                ( \request respond ->
                    respond
                        ( responseLBS
                            (if pathInfo request == ["refused"] then status503 else status200)
                            []
                            "{}"
                        )
                )
            )
        $ \port -> do
            let replay =
                    Replay
                        (RequestTrace [ClientTrace 0 0 ["ok", "refused", "bad"]] ["ok", "refused", "bad"])
                        5_000_000
                        (\name -> [if name == "bad" then "not a URL" else localhost port <> "/" <> name])
                        (pure "")
            result <- runReplay replay
            let totals = replayTotals result
            rtotalScheduled totals `shouldBe` 3
            rtotalCompleted totals `shouldBe` 2
            rtotalSuccessful totals `shouldBe` 1
            rtotalRefused totals `shouldBe` 1
            rtotalTransportFailed totals `shouldBe` 1
            rtotalUnfinished totals `shouldBe` 0
            ohaSuccessRate (replayHttp result) `shouldBe` (1 / 3)
