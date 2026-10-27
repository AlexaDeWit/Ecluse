-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Admission.ArtifactSpec (spec) where

import Test.Hspec
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Server.Admission.Artifact
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Support (expectRight)

data HeldBodyEscaped = HeldBodyEscaped
    deriving stock (Eq, Show)
instance Exception HeldBodyEscaped

spec :: Spec
spec = describe "verified body byte admission" $ do
    it "rejects impossible budgets and never clamps an overweight reservation" $ withSweeper $ \deadlines -> do
        invalid <- newVerifiedArtifactBudget deadlines 2 1
        isLeft invalid `shouldBe` True
        budget <- expectRight =<< newVerifiedArtifactBudget deadlines 2 2
        entered <- newIORef False
        result <- withVerifiedArtifactAdmission noopMetricsPort budget 3 (writeIORef entered True)
        result `shouldBe` Nothing
        readIORef entered `shouldReturn` False
    it "uses read metrics and returns its reservation after an exception" $ withSweeper $ \deadlines -> do
        budget <- expectRight =<< newVerifiedArtifactBudget deadlines 8 8
        deltas <- newIORef []
        writes <- newIORef (0 :: Int)
        let metrics =
                noopMetricsPort
                    { mpArtifactBodyInFlightBytes = \delta -> modifyIORef' deltas (<> [delta])
                    , mpPublishBodyInFlightBytes = \_ -> modifyIORef' writes (+ 1)
                    }
        result <- try (withVerifiedArtifactAdmission metrics budget 8 (throwIO HeldBodyEscaped)) :: IO (Either HeldBodyEscaped (Maybe ()))
        result `shouldBe` Left HeldBodyEscaped
        readIORef deltas `shouldReturn` [8, -8]
        readIORef writes `shouldReturn` 0
        withVerifiedArtifactAdmission metrics budget 8 (pure ()) `shouldReturn` Just ()
