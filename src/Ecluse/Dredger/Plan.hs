-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The Dredger's pure decisions over its resolved configuration and its invocation, so
"Ecluse.Dredger" dispatches on their results instead of branching inside 'IO'.
-}
module Ecluse.Dredger.Plan (
    DredgerOptions (..),
    SweepMode (..),
    SweepRepetition (..),
    dredgerBootRole,
    sweepPacingFor,
    sweepReportFor,
    waitsForAdvisories,
    advisoryWaitAttempts,
    advisoryPollMicros,
    cycleEnding,
    haltDetail,
) where

import Data.Text qualified as T

import Ecluse.Composition.Sizing (resolveSized)
import Ecluse.Composition.Types (BootRole (BootStorePreview, BootStorePruner))
import Ecluse.Config (
    AppConfig (cfgDredger),
    DredgerSettings (drgChunkPause, drgChunkSize, drgCyclePause, drgDeletionCap, drgFullWalk),
 )
import Ecluse.Core.Registry.Sweep.Types (
    CycleHalt,
    CycleOutcome (outcomeEvidence, outcomeHalt),
    SweepMount (smConfigured),
    SweepPacing (SweepPacing, swpChunkPause, swpChunkSize, swpCyclePause, swpDeletionCap, swpShape),
    SweepReport (SweepReport, reportCapHalts, reportOpening, reportRemoval),
    SweepShape (SweepCandidates, SweepEverything),
    deletionCapPerStore,
    evidenceComplete,
    outcomeComplete,
    renderCycleHalt,
    renderEvidenceGaps,
 )
import Ecluse.Core.Rules.Types (readsAdvisories)
import Ecluse.Core.Supervision (secondsToMicros)
import Ecluse.Core.Telemetry.Metrics (SweepResult (SweepDeleted, SweepWouldDelete))

-- | Whether the run deletes, or previews what a run that deletes would reach.
data SweepMode
    = -- | Versions a named decisive deny condemns are deleted.
      SweepDeletes
    | -- | Nothing is deleted, because the run holds no capability that could.
      SweepPreviews
    deriving stock (Eq, Show)

{- | The role a Dredger invocation boots under, settled from its own flags before the boot's
vetting pass runs, so the pass and the runtime agree on what the process may do to a store.
-}
dredgerBootRole :: SweepMode -> BootRole
dredgerBootRole = \case
    SweepDeletes -> BootStorePruner
    SweepPreviews -> BootStorePreview

-- | Whether the role cycles for the life of the process, or runs one cycle and exits.
data SweepRepetition
    = -- | Cycle, pause, cycle again, under supervision. The shipped invocation.
      SweepContinuously
    | -- | One cycle, then exit with what that cycle did. @--once@, which the harness drives.
      SweepOnce
    deriving stock (Eq, Show)

-- | What @ecluse dredger@'s own flags settled, carried from the command line to the sweep.
data DredgerOptions = DredgerOptions
    { doMode :: SweepMode
    -- ^ Whether the sweep deletes (@--dry-run@ previews instead).
    , doRepetition :: SweepRepetition
    -- ^ Whether it cycles for the life of the process (@--once@ runs one cycle).
    }
    deriving stock (Eq, Show)

{- | The pacing, the per-cycle cap, and the shape the @dredger@ group settled over the stores a
cycle sweeps, beside the boot line naming where the cap came from.
-}
sweepPacingFor :: AppConfig -> Int -> (SweepPacing, Text)
sweepPacingFor appConfig stores =
    ( SweepPacing
        { swpChunkSize = drgChunkSize dredger
        , swpChunkPause = drgChunkPause dredger
        , swpCyclePause = drgCyclePause dredger
        , swpDeletionCap = cap
        , swpShape = if drgFullWalk dredger then SweepEverything else SweepCandidates
        }
    , capLine
    )
  where
    dredger = cfgDredger appConfig
    (cap, capLine) =
        resolveSized
            "dredger: deletion cap"
            (drgDeletionCap dredger)
            (deletionCapPerStore * stores)
            ("computed as " <> show deletionCapPerStore <> " per sweepable mirror store")

{- | The detail a halted one-shot run reports as its own non-zero ending, so a scheduler reads
the outcome from the status and the reason from the same line.
-}
haltDetail :: CycleHalt -> Text
haltDetail halt = "the mirror sweep cycle halted: " <> renderCycleHalt halt

{- | How a run reports what it removed. A preview counts under its own arm and past the cap, so it
reports the full reach a real run would have rather than stopping at the breaker.
-}
sweepReportFor :: SweepMode -> SweepReport
sweepReportFor = \case
    SweepDeletes -> SweepReport{reportRemoval = SweepDeleted, reportOpening = "deleting ", reportCapHalts = True}
    SweepPreviews ->
        SweepReport{reportRemoval = SweepWouldDelete, reportOpening = "dry run, would delete ", reportCapHalts = False}

{- | What a one-shot run reports as its own ending, or 'Nothing' where it ends cleanly. A run that
deletes ends on the halt its cycle raised, and a preview ends on completeness alone.
-}
cycleEnding :: SweepMode -> CycleOutcome -> Maybe Text
cycleEnding = \case
    SweepDeletes -> fmap haltDetail . outcomeHalt
    SweepPreviews -> previewEnding

{- The preview's own ending. Its counts describe part of the store, so a scheduler reads that from
the status rather than from the counts, which report every candidate the cycle did gather. -}
previewEnding :: CycleOutcome -> Maybe Text
previewEnding outcome
    | outcomeComplete outcome = Nothing
    | otherwise = Just ("the mirror sweep preview counted from part of the store: " <> partial)
  where
    gaps = outcomeEvidence outcome
    partial =
        T.intercalate
            "; "
            (catMaybes [renderCycleHalt <$> outcomeHalt outcome, renderEvidenceGaps gaps <$ guard (not (evidenceComplete gaps))])

{- | Whether a first cycle waits for the first advisory sync. A rule set with no advisory rule
never needs one, so it starts at once.
-}
waitsForAdvisories :: [SweepMount] -> Bool
waitsForAdvisories = any (any readsAdvisories . smConfigured)

{- | How many times a first cycle checks for the advisory sync before it starts anyway. The bound
is the cycle pause, so waiting never costs more than one cycle's worth of time.
-}
advisoryWaitAttempts :: SweepPacing -> Int
advisoryWaitAttempts pacing = max 1 (secondsToMicros (swpCyclePause pacing) `div` advisoryPollMicros)

-- | How long the first cycle waits between checks for the advisory sync.
advisoryPollMicros :: Int
advisoryPollMicros = 500_000
