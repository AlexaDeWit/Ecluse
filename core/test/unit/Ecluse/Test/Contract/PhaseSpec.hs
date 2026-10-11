-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Exclusive phase accounting survives nested fixtures and failures at each boundary.
module Ecluse.Test.Contract.PhaseSpec (spec) where

import Control.Exception qualified as Exception
import Test.Hspec

import Ecluse.Test.Contract.Phase

spec :: Spec
spec = describe "timedFixture" $ do
    it "charges nested setup and cleanup once, outside the enclosing execution interval" $ do
        (recorder, advance) <- controlledClock
        timedFixture recorder (provider advance 2 13) $ \() -> do
            timedFixture recorder (provider advance 3 7) $ \() -> advance 5
            advance 11
        readPhaseTimings recorder `shouldReturn` PhaseTimings 5 16 20
    it "retains setup time when acquisition fails" $ do
        (recorder, advance) <- controlledClock
        outcome <-
            Exception.try @PhaseFailure $
                timedFixture recorder (\_ -> advance 2 >> Exception.throwIO PhaseFailure) (const $ advance 9)
        outcome `shouldSatisfy` isLeft
        readPhaseTimings recorder `shouldReturn` PhaseTimings 2 0 0
    it "retains cleanup time when the example fails" $ do
        (recorder, advance) <- controlledClock
        outcome <- Exception.try @PhaseFailure $
            timedFixture recorder (provider advance 2 5) $
                \() -> advance 3 >> Exception.throwIO PhaseFailure
        outcome `shouldSatisfy` isLeft
        readPhaseTimings recorder `shouldReturn` PhaseTimings 2 3 5
    it "retains cleanup time when cleanup fails" $ do
        (recorder, advance) <- controlledClock
        let failingProvider action = Exception.bracket_ (advance 2) (advance 5 >> Exception.throwIO PhaseFailure) (action ())
        outcome <- Exception.try @PhaseFailure $ timedFixture recorder failingProvider (const $ advance 3)
        outcome `shouldSatisfy` isLeft
        readPhaseTimings recorder `shouldReturn` PhaseTimings 2 3 5

controlledClock :: IO (PhaseRecorder, Word64 -> IO ())
controlledClock = do
    clock <- newIORef 0
    recorder <- newPhaseRecorderWithClock $ readIORef clock
    pure (recorder, \seconds -> modifyIORef' clock (+ (seconds * 1000000000)))

provider :: (Word64 -> IO ()) -> Word64 -> Word64 -> (() -> IO ()) -> IO ()
provider advance setup cleanup action = Exception.bracket_ (advance setup) (advance cleanup) (action ())

data PhaseFailure = PhaseFailure deriving stock (Show)
instance Exception PhaseFailure
