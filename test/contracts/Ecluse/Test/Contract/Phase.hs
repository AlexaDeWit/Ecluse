-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Nested fixture clocks attribute time to the innermost lifecycle phase.
module Ecluse.Test.Contract.Phase (
    PhaseRecorder,
    PhaseTimings (..),
    newPhaseRecorder,
    readPhaseTimings,
    timedFixture,
) where

import Control.Exception qualified as Exception
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import GHC.Clock (getMonotonicTimeNSec)

-- | Durations are exclusive seconds, so nested providers do not double-count work.
data PhaseTimings = PhaseTimings
    { phaseSetupSeconds :: Double
    , phaseExecutionSeconds :: Double
    , phaseTeardownSeconds :: Double
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON PhaseTimings where
    toJSON timings =
        object
            [ "setupSeconds" .= phaseSetupSeconds timings
            , "executionSeconds" .= phaseExecutionSeconds timings
            , "teardownSeconds" .= phaseTeardownSeconds timings
            ]
instance FromJSON PhaseTimings where
    parseJSON = withObject "phase timings" $ \value ->
        PhaseTimings <$> value .: "setupSeconds" <*> value .: "executionSeconds" <*> value .: "teardownSeconds"

data Phase = Setup | Execution | Teardown

data ClockState = ClockState Word64 [Phase] PhaseTimings

-- | One recorder belongs to one serial lifecycle group, never process-global state.
data PhaseRecorder = PhaseRecorder (IO Word64) (IORef ClockState)

-- | Uninstrumented work, including role restarts inside a scenario, counts as execution.
newPhaseRecorder :: IO PhaseRecorder
newPhaseRecorder = do
    now <- getMonotonicTimeNSec
    PhaseRecorder getMonotonicTimeNSec <$> newIORef (ClockState now [Execution] $ PhaseTimings 0 0 0)

-- | Read accumulated exclusive timings after resource cleanup.
readPhaseTimings :: PhaseRecorder -> IO PhaseTimings
readPhaseTimings recorder@(PhaseRecorder _ clockRef) = do
    changePhase recorder id
    ClockState _ _ durations <- readIORef clockRef
    pure durations

-- | Wrap a bracketed provider, including acquisition and cleanup failures.
timedFixture :: PhaseRecorder -> ((a -> IO ()) -> IO ()) -> (a -> IO ()) -> IO ()
timedFixture recorder provider action = Exception.mask $ \restore -> do
    changePhase recorder (Setup :)
    restore
        ( provider $ \resource -> do
            changePhase recorder (replacePhase Execution)
            action resource `Exception.finally` changePhase recorder (replacePhase Teardown)
        )
        `Exception.finally` changePhase recorder (drop 1)

replacePhase :: Phase -> [Phase] -> [Phase]
replacePhase phase = \case
    [] -> [phase]
    _ : rest -> phase : rest

changePhase :: PhaseRecorder -> ([Phase] -> [Phase]) -> IO ()
changePhase (PhaseRecorder clock clockRef) update = do
    now <- clock
    atomicModifyIORef' clockRef $ \(ClockState previous phases durations) ->
        let elapsed = fromIntegral (now - min now previous) / 1000000000
            charged = case phases of
                Setup : _ -> durations{phaseSetupSeconds = phaseSetupSeconds durations + elapsed}
                Execution : _ -> durations{phaseExecutionSeconds = phaseExecutionSeconds durations + elapsed}
                Teardown : _ -> durations{phaseTeardownSeconds = phaseTeardownSeconds durations + elapsed}
                [] -> durations
         in (ClockState now (update phases) charged, ())
