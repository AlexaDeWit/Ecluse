-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.BenchLoad.ReplaySpec (spec) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Types (status200, status503)
import Network.Wai (pathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.Harness (Driver (DriveReplay), warmUp)
import Ecluse.BenchLoad.Oha (OhaReport (..))
import Ecluse.BenchLoad.Patterns (ClientTrace (..), RequestTrace (..))
import Ecluse.BenchLoad.Replay (Replay (..), runReplay)
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
                        (\name -> [localhost port <> "/" <> name])
                        (pure "")
            warmUp (DriveReplay replay)
            readIORef seen `shouldReturn` []
            report <- runReplay replay
            reverse <$> readIORef seen `shouldReturn` [["first"], ["second"]]
            ohaStatusCounts report `shouldBe` Map.singleton "200" 2
            ohaSuccessRate report `shouldBe` 1
    it "retains refused responses while successful latency stays unavailable" $
        testWithApplication (pure (\_ respond -> respond (responseLBS status503 [] "refused"))) $ \port -> do
            let replay =
                    Replay
                        (RequestTrace [ClientTrace 0 0 ["one"]] ["one"])
                        (const [localhost port <> "/one"])
                        (pure "")
            report <- runReplay replay
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
                        (\name -> [localhost port <> "/" <> name, localhost port <> "/" <> name <> "/1.0.0"])
                        (pure "")
            _ <- runReplay replay
            reverse <$> readIORef seen `shouldReturn` [["pkg"], ["pkg", "1.0.0"]]
