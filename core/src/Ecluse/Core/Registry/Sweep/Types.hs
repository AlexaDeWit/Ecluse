-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror sweep's inputs, effects, and cycle state.
Store operations arrive through "Ecluse.Core.Registry.Maintenance" handles.
-}
module Ecluse.Core.Registry.Sweep.Types (
    -- * What a sweep runs over
    SweepMount (..),
    SweepStore (..),
    SweepCache (..),
    SweepExecution (..),
    deletingCache,
    previewCache,
    pairedStore,
    privateStore,
    countingAt,
    walkMarkerOf,
    SweepPacing (..),
    minimumChunkPause,
    deletionCapPerStore,
    SweepShape (..),
    SweepReport (..),
    SweepPorts (..),
    SweepAudit (..),
    sweepTargetOf,
    locatedPorts,


    -- * The cycle's running state
    SweepState (..),
    newSweepState,
    record,
    recordMetric,
    recordTally,
    recordGap,
    recordPrerequisites,
) where

import Data.Time (NominalDiffTime, UTCTime)

import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Adapter.Capability (ProjectName)
import Ecluse.Core.Registry.Sweep.Outcome (EvidenceGaps, SweepTally (..), TargetPrerequisites)
import Ecluse.Core.Registry.Maintenance (
    StoreCursor,
    StoreDeletion (dlCursor),
    StoreFacts (factBackend),
    StoreMaintenance,
    StoreObservation (obFacts),
    deletionOf,
    observationOf,
 )
import Ecluse.Core.Registry.Maintenance.Budget (BudgetPort)
import Ecluse.Core.Rules (PreparedRule, RuleDeps)
import Ecluse.Core.Rules.Types (Rule)
import Ecluse.Core.Telemetry.Metrics (SweepResult (..), SweepTarget (SweepMirror, SweepPrivate))
import Ecluse.Core.Telemetry.Record (DredgerMetricsPort (dmpSweptVersion))

-- | One mount's sweepable store, and everything that decides for it.
data SweepMount = SweepMount
    { smEcosystem :: Ecosystem
    -- ^ The mount's ecosystem, which names it in an audit line.
    , smStore :: SweepStore
    -- ^ The store's own calls. Every backend-varying fact is a value on it.
    , smRules :: [PreparedRule]
    -- ^ The mount's own prepared rule set, the one the serve and ingest gates evaluate.
    , smConfigured :: [Rule]
    {- ^ The same rules as configured values, which a prepared rule no longer carries. The
    candidate set reads an identity deny's names out of these.
    -}
    , smRuleDeps :: RuleDeps
    -- ^ Lookup acquisition, which candidate discovery and a later verdict may take at different generations.
    , smProjectName :: ProjectName
    -- ^ The ecosystem's own name parser, which both halves of the candidate set are read through.
    , smFirstParty :: PackageName -> Bool
    -- ^ Whether a name belongs to a namespace this deployment owns, derived once at the composition root.
    }

-- | One mount's sweepable store, with the private cache it is always swept beside.
data SweepStore = SweepStore
    { ssObserve :: StoreObservation
    , ssExecute :: SweepExecution
    , ssPrivate :: SweepCache
    {- ^ Carried by every view of the mount's stores, because the pairing is a fact about the
    mount rather than about the store in hand.
    -}
    , ssVersionLimit :: Int
    -- ^ Maximum distinct versions held for one package across both observations.
    }

-- | One store's own two halves. A cache is paired with no further store, so it carries none.
data SweepCache = SweepCache
    { scObserve :: StoreObservation
    , scExecute :: SweepExecution
    }

-- | What a run does with a condemned version. Only one arm carries a write.
data SweepExecution
    = -- | Hand the versions to the store's own delete, and record each completed bucket.
      SweepRemoves StoreDeletion
    | -- | Count the versions and reach nothing that could change the store.
      SweepCounts

-- | The whole handle as a deleting run holds it: its reads, and its writes as the execution.
deletingCache :: StoreMaintenance -> SweepCache
deletingCache handle = SweepCache{scObserve = observationOf handle, scExecute = SweepRemoves (deletionOf handle)}

-- | The observing calls alone, as a preview holds them.
previewCache :: StoreObservation -> SweepCache
previewCache observation = SweepCache{scObserve = observation, scExecute = SweepCounts}

-- | A mount's store: the mirror target's halves, its private cache, and the bound they share.
pairedStore :: Int -> SweepCache -> SweepCache -> SweepStore
pairedStore limit mirror cache =
    SweepStore{ssObserve = scObserve mirror, ssExecute = scExecute mirror, ssPrivate = cache, ssVersionLimit = limit}

-- | The mount's other store: its private cache, under the same pairing and bound.
privateStore :: SweepStore -> SweepStore
privateStore store = store{ssObserve = scObserve (ssPrivate store), ssExecute = scExecute (ssPrivate store)}

{- | The mount's store seen at one observation, counting only. A located view names its own
backend in an audit line and reaches nothing that could change a store.
-}
countingAt :: SweepStore -> StoreObservation -> SweepStore
countingAt store observation = store{ssObserve = observation, ssExecute = SweepCounts}

{- | The marker a full walk resumes from. A preview holds none, so its walk starts at the first
bucket and the recorded marker is neither read nor replaced.
-}
walkMarkerOf :: SweepStore -> Maybe StoreCursor
walkMarkerOf store = case ssExecute store of
    SweepRemoves deletion -> dlCursor deletion
    SweepCounts -> Nothing

-- | How a sweep paces itself, how much one cycle may delete, and which shape it runs.
data SweepPacing = SweepPacing
    { swpChunkSize :: Int
    -- ^ Candidate packages one chunk examines before the sweep pauses.
    , swpChunkPause :: NominalDiffTime
    -- ^ The wait between chunks, and the wait a fault whose advice names no delay takes.
    , swpCyclePause :: NominalDiffTime
    {- ^ The wait between the end of one cycle and the start of the next, a halted one included.
    The role's own loop applies it, so a cycle is one supervised step.
    -}
    , swpCycleWindow :: NominalDiffTime
    {- ^ The window an advisory is paced to reach every affected mirrored version inside. It
    covers the rest of the running cycle, the cycle pause, and the next whole cycle.
    -}
    , swpBudgetFraction :: Maybe Rational
    -- ^ The share of a store's request capacity one sweep may take, else computed per scope.
    , swpDeletionCap :: Int
    -- ^ Versions one cycle may hand over for deletion before it halts for good.
    , swpShape :: SweepShape
    -- ^ Which names the cycle carries to the rules.
    }
    deriving stock (Eq, Show)

{- | The shortest pause a sweep may be paced by, which the boot refuses beneath. Deletion is
permanent, and the pause between chunks is what leaves time to stop a mistaken run.
-}
minimumChunkPause :: NominalDiffTime
minimumChunkPause = 2

{- | Versions one cycle may hand over per sweepable store, which the cap's default is computed
from. A cycle covers every store in turn, so one pinned total would starve the later mounts.
-}
deletionCapPerStore :: Int
deletionCapPerStore = 100

{- | Which names one cycle decides. A full walk is a superset of a candidate cycle, so the
two never run beside each other.
-}
data SweepShape
    = {- | Every store name the advisory database covers or an identity deny pins. It is
      bounded by the listing for store size and by advisory hits for reads.
      -}
      SweepCandidates
    | {- | Every name in the store, bucket by bucket, resuming from the store's own cursor. It
      covers a rule-configuration change, which no candidate set can see.
      -}
      SweepEverything
    deriving stock (Eq, Show)

{- | How one run reports a removal, and whether its cap stops the cycle. A preview holds no delete
at all, so the loop reads these rather than asking which mode it is in.
-}
data SweepReport = SweepReport
    { reportRemoval :: SweepResult
    -- ^ What a removal the backend accepted counts as.
    , reportOpening :: Text
    -- ^ How a removal's audit line opens.
    , reportCapHalts :: Bool
    -- ^ Whether reaching the cap stops the cycle. A preview counts past it instead.
    }

{- | Where the sweep reports. The two severities are separate fields rather than a level
argument, so a caller cannot log a halt as routine.
-}
data SweepAudit = SweepAudit
    { auditInfo :: Text -> IO ()
    -- ^ One routine line: a deletion, a preview's own count, a completed cycle.
    , auditWarn :: Text -> IO ()
    -- ^ One line that may clear on its own: a store call being retried.
    , auditError :: Text -> IO ()
    -- ^ One line an operator must act on: a halt, a refused deletion, a store fault.
    }

-- | The effects the sweep reaches the running system through.
data SweepPorts = SweepPorts
    { sweepNow :: IO UTCTime
    -- ^ The clock the rules' evaluation context reads.
    , sweepAdvisoryEtag :: Ecosystem -> IO (Maybe DbEtag)
    -- ^ The active advisory generation for one ecosystem, for the audit line alone.
    , sweepDelay :: NominalDiffTime -> IO ()
    -- ^ The pause, injected so a spec observes pacing without waiting for it.
    , sweepTarget :: SweepTarget
    , sweepMetrics :: DredgerMetricsPort
    -- ^ Where each version's disposition is counted.
    , sweepAudit :: SweepAudit
    -- ^ Where the sweep's own lines go.
    , sweepReport :: SweepReport
    -- ^ How this run reports a removal, and whether its cap stops the cycle.
    , sweepBudget :: BudgetPort
    -- ^ Where the cycle's own request counts are measured and the next cycle's rate installed.
    }

{- | Which of a mount's two locations a store is. The decision keys on the backend name, so a
mount whose mirror store and private cache report one name reads as the mirror at both.
-}
sweepTargetOf :: SweepMount -> StoreObservation -> SweepTarget
sweepTargetOf mount store
    | factBackend (obFacts store) == factBackend (obFacts (ssObserve (smStore mount))) = SweepMirror
    | otherwise = SweepPrivate

-- | The ports an audit line from one located store is written through, labelled and targeted.
locatedPorts :: SweepMount -> StoreObservation -> SweepPorts -> SweepPorts
locatedPorts mount store ports =
    ports
        { sweepAudit = labelAudit (factBackend (obFacts store)) (sweepAudit ports)
        , sweepTarget = sweepTargetOf mount store
        }

-- Keep per-target audit messages distinct when one cycle sweeps associated stores.
labelAudit :: Text -> SweepAudit -> SweepAudit
labelAudit target audit =
    SweepAudit
        { auditInfo = labelled (auditInfo audit)
        , auditWarn = labelled (auditWarn audit)
        , auditError = labelled (auditError audit)
        }
  where
    labelled write = write . ((target <> ": ") <>)

-- | Cycle totals and pacing. The issued count bounds attempts, while the tally records outcomes.
data SweepState = SweepState
    { stTally :: IORef SweepTally
    , stIssued :: IORef Int
    , stChunkProgress :: IORef Int
    -- ^ Names examined in the current chunk, shared across pages, buckets, and mounts.
    , stEvidence :: IORef EvidenceGaps
    -- ^ What the cycle could not read, which decides whether its counts cover the whole store.
    , stPrerequisites :: IORef [TargetPrerequisites]
    -- ^ What a preview found of each target, newest first until the cycle reverses it.
    }

-- | Start a cycle with no counts or pending chunk pause.
newSweepState :: IO SweepState
newSweepState =
    SweepState
        <$> newIORef mempty
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef mempty
        <*> newIORef []

-- | Count one version's disposition, in the cycle tally and at the metrics port together.
record :: SweepPorts -> SweepState -> SweepResult -> IO ()
record ports counters result = do
    recordMetric ports result
    recordTally counters result

-- | Count a target operation separately from a deduplicated logical preview tally.
recordMetric :: SweepPorts -> SweepResult -> IO ()
recordMetric ports = dmpSweptVersion (sweepMetrics ports) (sweepTarget ports)

-- | Update the cycle tally without recording a second target operation.
recordTally :: SweepState -> SweepResult -> IO ()
recordTally counters result = modifyIORef' (stTally counters) (<> tallyOf result)

{- A previewed deletion counts under its own metric arm and in the cycle's deleted column, so
one dry run reports the reach a real run would have. -}
tallyOf :: SweepResult -> SweepTally
tallyOf = \case
    SweepExamined -> mempty{tallyExamined = 1}
    SweepDeleted -> mempty{tallyDeleted = 1}
    SweepWouldDelete -> mempty{tallyDeleted = 1}
    SweepKept -> mempty{tallyKept = 1}
    SweepGuardSkipped -> mempty{tallyGuardSkipped = 1}

-- | Record one gap in what this cycle could read.
recordGap :: SweepState -> EvidenceGaps -> IO ()
recordGap counters gaps = modifyIORef' (stEvidence counters) (<> gaps)

-- | Record one target's standing permissions, which only a preview reads rather than acts on.
recordPrerequisites :: SweepState -> TargetPrerequisites -> IO ()
recordPrerequisites counters target = modifyIORef' (stPrerequisites counters) (target :)
