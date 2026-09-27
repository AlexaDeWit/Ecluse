-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | How a measured proxy ended, and the invariants that fail a load run. Throughput and latency
stay informational. A scenario with no successful response, a kernel OOM kill, or a heap overflow
is a broken run whatever the other figures say.
-}
module Ecluse.BenchLoad.Verdict (
    ProxyEnding (..),
    classifyEnding,
    RunEvidence (..),
    runViolations,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T

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

-- | What one scenario run shows about the invariants.
data RunEvidence = RunEvidence
    { reScenario :: Text
    , reSuccesses :: [(Text, Int)]
    -- ^ Successful responses per load the scenario drove, labelled for the failure message.
    , reOomKills :: Int
    , reEnding :: Maybe ProxyEnding
    -- ^ 'Nothing' for a scenario that runs in the harness process.
    }
    deriving stock (Eq, Show)

-- | One line per broken invariant, empty when the run holds.
runViolations :: RunEvidence -> [Text]
runViolations evidence =
    [ scenario <> ": no successful responses" <> labelled label
    | (label, count) <- reSuccesses evidence
    , count <= 0
    ]
        <> [scenario <> ": the kernel OOM-killed the proxy (" <> show (reOomKills evidence) <> " oom_kill events)" | reOomKills evidence > 0]
        <> [scenario <> ": the proxy exited on heap overflow" | reEnding evidence == Just HeapOverflow]
  where
    scenario = reScenario evidence
    labelled label = if T.null label then "" else " (" <> label <> ")"
