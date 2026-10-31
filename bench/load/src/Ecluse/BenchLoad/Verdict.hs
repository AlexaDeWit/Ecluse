-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | Process endings, required successes, and the confirmation of a floor breach. A breach is a
count of at least one below its floor. It earns a re-run on a fresh proxy and fails the run only when
every attempt breaches. A job with more first-attempt breaches than its confirmation cap re-runs none,
and each of its breaches fails the run. Latency and memory figures stay informational.
-}
module Ecluse.BenchLoad.Verdict (
    ProxyEnding (..),
    classifyEnding,
    describeEnding,
    RunEvidence (..),
    runViolations,

    -- * Confirming a breach
    Outcome,
    Attempts,
    attemptCount,
    attemptClean,
    passViolations,
    firstAttemptBreaches,
    overCapViolations,
    rerunsDue,
    confirmBreaches,
    runVerdict,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.BenchLoad.Floors (
    Confirmation (cfCap, cfReruns),
    Enforcement (Enforced),
    FloorCheck (NoFloor, Unchecked),
    FloorKey,
    Trigger,
    checkedFloor,
    floorCheck,
    floorsPath,
    passKey,
    staleFloorViolations,
    unheldViolations,
 )

-- | How the proxy process ended, read from its exit status, its stderr, and its cgroup.
data ProxyEnding
    = -- | It drained and exited 0 after the harness asked it to stop.
      CleanShutdown
    | -- | The RTS heap ceiling (@-M@) was reached and the process exited on it.
      HeapOverflow
    | -- | The kernel killed it at the cgroup memory limit (@oom_kill@ counted).
      KernelOomKill
    | -- | It outlived the drain grace, so the harness killed it.
      StoppedByHarness
    | -- | It exited with this status for another reason.
      ExitedWith Int
    | -- | A signal other than the kernel's OOM kill ended it.
      KilledBySignal Int
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

{- | Classify an ending. The status is the process library's: negative for a signal. The heap
overflow is recognised from the process's own report or the RTS exit status, never assumed.
-}
classifyEnding :: Int -> Text -> Int -> Bool -> ProxyEnding
classifyEnding status stderrText oomKills harnessKilled
    | heapOverflowReported = HeapOverflow
    | status == 0 = CleanShutdown
    | status == -9 && oomKills > 0 && not harnessKilled = KernelOomKill
    | harnessKilled = StoppedByHarness
    | status < 0 = KilledBySignal (negate status)
    | otherwise = ExitedWith status
  where
    lowered = T.toLower stderrText
    -- 251 is the RTS's own exit status when it reports the heap exhausted.
    heapOverflowReported = "heap overflow" `T.isInfixOf` lowered || "heap exhausted" `T.isInfixOf` lowered || status == 251

-- | A proxy ending as report text.
describeEnding :: ProxyEnding -> Text
describeEnding = \case
    CleanShutdown -> "clean shutdown"
    HeapOverflow -> "heap overflow"
    KernelOomKill -> "kernel OOM kill"
    StoppedByHarness -> "killed after the drain grace"
    ExitedWith code -> "exited " <> show code
    KilledBySignal signal -> "killed by signal " <> show signal

-- | What one scenario run shows about the invariants.
data RunEvidence = RunEvidence
    { reScenario :: Text
    , reSuccesses :: [(Text, Int)]
    -- ^ Successful responses per required load or ramp step, labelled for the failure message.
    , reOomKills :: Int
    , reEnding :: Maybe ProxyEnding
    -- ^ 'Nothing' for a scenario that runs in the harness process.
    , reExitedEarly :: Bool
    -- ^ The proxy had exited before the harness asked it to stop.
    }
    deriving stock (Eq, Show)

-- | One line per invariant one attempt broke, with every required load held to the pass's floor.
runViolations :: FloorCheck -> RunEvidence -> [Text]
runViolations check evidence = concatMap (loadViolations check evidence) (reSuccesses evidence) <> processViolations check evidence

-- A load with no success keeps its own line, whatever its floor.
loadViolations :: FloorCheck -> RunEvidence -> (Text, Int) -> [Text]
loadViolations check evidence (label, count)
    | count <= 0 = [reScenario evidence <> ": no successful responses" <> labelled label]
    | Just least <- checkedFloor check
    , count < least =
        [reScenario evidence <> ": " <> show count <> " successful responses, below " <> boundName check <> labelled label]
    | otherwise = []

-- What a re-run cannot clear: a missing cell, an OOM kill, an unclean ending, and an early exit.
processViolations :: FloorCheck -> RunEvidence -> [Text]
processViolations check evidence =
    floorless
        <> [scenario <> ": the kernel OOM-killed the proxy (" <> show (reOomKills evidence) <> " oom_kill events)" | reOomKills evidence > 0]
        <> endingViolations
        <> [scenario <> ": the proxy exited before the harness stopped it" | reExitedEarly evidence]
  where
    floorless = case check of
        NoFloor whichPass -> [scenario <> ": no " <> passKey whichPass <> " cell for this pod shape in " <> toText floorsPath]
        _ -> []
    -- The OOM kill already has its own line from the cgroup's count.
    endingViolations = case reEnding evidence of
        Just HeapOverflow -> [scenario <> ": the proxy exited on heap overflow"]
        Just ending
            | ending `notElem` [CleanShutdown, KernelOomKill] ->
                [scenario <> ": the proxy did not shut down cleanly (" <> describeEnding ending <> ")"]
        _ -> []
    scenario = reScenario evidence

boundName :: FloorCheck -> Text
boundName = maybe "no floor" (\least -> "the floor of " <> show least) . checkedFloor

labelled :: Text -> Text
labelled label = if T.null label then "" else " (" <> label <> ")"

-- | One attempt at a scenario pass: its evidence, or why its child process gave none.
type Outcome = Either Text RunEvidence

-- | A scenario pass's first attempt, then its confirmation re-runs in order.
type Attempts = NonEmpty Outcome

-- | The count the floors hold an attempt to: its lowest required load. 'Nothing' without evidence.
attemptCount :: Outcome -> Maybe Int
attemptCount outcome = case map snd . reSuccesses <$> outcome of
    Right (count : more) -> Just (foldl' min count more)
    _ -> Nothing

-- Whether some load of the attempt is at least one and below the floor in force.
breached :: FloorCheck -> Outcome -> Bool
breached check = \case
    Left _ -> False
    Right evidence -> case checkedFloor check of
        Nothing -> False
        Just least -> any (\(_, count) -> count >= 1 && count < least) (reSuccesses evidence)

-- | Whether an attempt reported every load with a success and a proxy that ended cleanly, whatever its floor.
attemptClean :: Outcome -> Bool
attemptClean = \case
    Right evidence -> all ((>= 1) . snd) (reSuccesses evidence) && null (processViolations Unchecked evidence)
    Left _ -> False

-- A breach a re-run may clear: the attempt is otherwise clean.
breachedOnly :: FloorCheck -> Outcome -> Bool
breachedOnly check outcome = breached check outcome && attemptClean outcome

{- | Every invariant one scenario pass broke over its attempts. Each attempt's own failures count, and
a floor breach counts only when every attempt breached.
-}
passViolations :: FloorKey -> FloorCheck -> Attempts -> [Text]
passViolations (scenarioKey, _) check = \case
    only :| [] -> outcomeViolations only
    attempts -> concatMap unclearable (toList attempts) <> confirmed attempts
  where
    outcomeViolations = either (\failure -> [scenarioKey <> ": " <> failure]) (runViolations check)
    unclearable = \case
        Left failure -> [scenarioKey <> ": " <> failure]
        Right evidence -> [reScenario evidence <> ": no successful responses" <> labelled label | (label, count) <- reSuccesses evidence, count <= 0] <> processViolations check evidence
    confirmed attempts@(initial :| more)
        | all (breached check) attempts =
            [ either (const scenarioKey) reScenario initial
                <> ": "
                <> countOf initial
                <> " successful responses, then "
                <> case more of
                    [again] -> countOf again <> " on the re-run, both below " <> boundName check
                    _ -> T.intercalate " and " (map countOf more) <> " on the re-runs, all below " <> boundName check
            ]
        | otherwise = []
    countOf = maybe "no report" show . attemptCount

-- | Each scenario pass whose first attempt's count is at least 1 and below its floor, with the floor.
firstAttemptBreaches :: Enforcement -> [(FloorKey, Attempts)] -> [(FloorKey, Attempts, Int)]
firstAttemptBreaches enforcement passes =
    [ (key, attempts, least)
    | (key, attempts@(initial :| _)) <- passes
    , Just least <- [checkedFloor (floorCheck enforcement key)]
    , maybe False (\count -> count >= 1 && count < least) (attemptCount initial)
    ]

-- The count of first-attempt breaches when it exceeds the job's confirmation cap, which stops every re-run.
breachesOverCap :: Enforcement -> [(FloorKey, Attempts)] -> Maybe Int
breachesOverCap enforcement passes = case enforcement of
    Enforced _ confirmation _ | breaches > cfCap confirmation -> Just breaches
    _ -> Nothing
  where
    breaches = length (firstAttemptBreaches enforcement passes)

-- | The verdict's line for a job whose first-attempt breaches exceed its confirmation cap.
overCapViolations :: Enforcement -> [(FloorKey, Attempts)] -> [Text]
overCapViolations enforcement passes = case (enforcement, breachesOverCap enforcement passes) of
    (Enforced _ confirmation _, Just breaches) ->
        [show breaches <> " scenario passes breached their floors on their first attempt, more than the confirmation cap of " <> show (cfCap confirmation) <> ", so none was re-run"]
    _ -> []

{- | The scenario passes that earn one more attempt: every attempt so far breached its floor and nothing
else, and re-runs remain. None does when the first-attempt breaches exceed the confirmation cap.
-}
rerunsDue :: Enforcement -> [(FloorKey, Attempts)] -> [FloorKey]
rerunsDue enforcement passes = case enforcement of
    Enforced _ confirmation _
        | isNothing (breachesOverCap enforcement passes) ->
            [ key
            | (key, attempts) <- passes
            , length attempts <= cfReruns confirmation
            , all (breachedOnly (floorCheck enforcement key)) attempts
            ]
    _ -> []

-- | Re-run each pass 'rerunsDue' names, one round at a time, until none is due.
confirmBreaches :: (Monad m) => Enforcement -> (FloorKey -> m Outcome) -> [(FloorKey, Attempts)] -> m [(FloorKey, Attempts)]
confirmBreaches enforcement rerun = go
  where
    go passes = case rerunsDue enforcement passes of
        [] -> pure passes
        due -> do
            again <- Map.fromList <$> traverse (\key -> (key,) <$> rerun key) due
            go [(key, maybe attempts (\outcome -> attempts <> (outcome :| [])) (Map.lookup key again)) | (key, attempts) <- passes]

{- | Every invariant a run broke: a scheduled run that is not held, first-attempt breaches over the
confirmation cap, each scenario pass over its attempts, and a held run's stale cells.
-}
runVerdict :: Trigger -> Enforcement -> Set FloorKey -> [(FloorKey, Attempts)] -> [Text]
runVerdict trigger enforcement checked passes =
    unheldViolations trigger enforcement
        <> overCapViolations enforcement passes
        <> concatMap (\(key, attempts) -> passViolations key (floorCheck enforcement key) attempts) passes
        <> staleFloorViolations enforcement checked
