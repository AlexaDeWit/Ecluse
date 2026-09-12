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
    SweepExecution (..),
    deletingStore,
    previewStore,
    walkMarkerOf,
    SweepPacing (..),
    minimumChunkPause,
    deletionCapPerStore,
    SweepShape (..),
    SweepReport (..),
    SweepPorts (..),
    SweepAudit (..),

    -- * What one cycle did
    SweepTally (..),
    CycleHalt (..),
    CycleOutcome (..),
    outcomeComplete,
    latches,
    renderCycleHalt,
    renderGeneration,
    renderTally,
    renderStoreFault,

    -- * What a preview found standing in a real sweep's way
    TargetPrerequisites (..),
    PrerequisiteStatus (..),
    prerequisitesMet,
    renderPrerequisites,

    -- * What a cycle could not read
    EvidenceGaps (..),
    unloadedGeneration,
    unreadManifest,
    evidenceComplete,
    renderEvidenceGaps,

    -- * The cycle's running state
    SweepState (..),
    newSweepState,
    record,
    recordGap,
    recordPrerequisites,
) where

import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime)

import Ecluse.Core.Cve (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Fault (TransportFault (tfCause, tfDetail), renderTransportCause)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Adapter.Capability (ProjectName)
import Ecluse.Core.Registry.Maintenance (
    StoreCursor,
    StoreDeletion (dlCursor),
    StoreFault (faultTransport),
    StoreMaintenance,
    StoreObservation,
    deletionOf,
    observationOf,
 )
import Ecluse.Core.Rules (PreparedRule, RuleDeps)
import Ecluse.Core.Rules.Types (Rule)
import Ecluse.Core.Telemetry.Metrics (SweepResult (..))
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
    candidate set reads the names an identity deny pins out of these.
    -}
    , smRuleDeps :: RuleDeps
    {- ^ The mount's dependencies for candidate discovery and per-rule lookup acquisition.
    Candidate discovery and a later verdict can use different generations.
    -}
    , smProjectName :: ProjectName
    -- ^ The ecosystem's own name parser, which both halves of the candidate set are read through.
    , smFirstParty :: PackageName -> Bool
    {- ^ Whether a name belongs to a namespace this deployment owns, the shared predicate
    derived once at the composition root.
    -}
    }

{- | One mount's store as the booting role holds it: the calls that observe it, and what this run
executes against a condemned version. Only the two builders below pair the halves.
-}
data SweepStore = SweepStore
    { ssObserve :: StoreObservation
    , ssExecute :: SweepExecution
    }

-- | What a run does with a condemned version. Only one arm carries a write.
data SweepExecution
    = -- | Hand the versions to the store's own delete, and record each completed bucket.
      SweepRemoves StoreDeletion
    | -- | Count the versions and reach nothing that could change the store.
      SweepCounts

-- | The whole handle as a deleting run holds it: its reads, and its writes as the execution.
deletingStore :: StoreMaintenance -> SweepStore
deletingStore handle =
    SweepStore{ssObserve = observationOf handle, ssExecute = SweepRemoves (deletionOf handle)}

-- | The observing calls alone, as a preview holds them.
previewStore :: StoreObservation -> SweepStore
previewStore observation = SweepStore{ssObserve = observation, ssExecute = SweepCounts}

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
    {- ^ The wait between the end of one cycle and the start of the next, a halted one
    included. The role's own loop applies it, so a cycle is one supervised step.
    -}
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
    | {- | Every name in the store, bucket by bucket, resuming from the store's own cursor.
      It covers a rule-configuration change, which no candidate set can see.
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
    {- ^ Whether reaching the cap stops the cycle. A preview counts past it instead, so it
    reports the full reach a real run would have.
    -}
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
    , sweepMetrics :: DredgerMetricsPort
    -- ^ Where each version's disposition is counted.
    , sweepAudit :: SweepAudit
    -- ^ Where the sweep's own lines go.
    , sweepReport :: SweepReport
    -- ^ How this run reports a removal, and whether its cap stops the cycle.
    }

-- | What one cycle did with the versions it examined.
data SweepTally = SweepTally
    { tallyExamined :: Int
    , tallyDeleted :: Int
    , tallyKept :: Int
    , tallyGuardSkipped :: Int
    }
    deriving stock (Eq, Show)

instance Semigroup SweepTally where
    left <> right =
        SweepTally
            { tallyExamined = tallyExamined left + tallyExamined right
            , tallyDeleted = tallyDeleted left + tallyDeleted right
            , tallyKept = tallyKept left + tallyKept right
            , tallyGuardSkipped = tallyGuardSkipped left + tallyGuardSkipped right
            }

instance Monoid SweepTally where
    mempty = SweepTally 0 0 0 0

-- | Why a cycle stopped before it finished.
data CycleHalt
    = -- | The store carries no consent marker, with the backend and its how-to-attach text.
      HaltConsentWithheld Ecosystem Text Text
    | -- | The store refills itself from elsewhere, so deleting from it changes nothing.
      HaltStorePreserved Ecosystem Text Text
    | {- | The cycle reached its deletion cap, carrying the cap, what it handed over, and the
      advisory generation of the exact denial that reached the cap. No later cycle runs.
      -}
      HaltDeletionCap Int Int (Maybe DbEtag)
    | -- | A store call produced no answer and its retry advice ran out, carrying the fault.
      HaltStoreFault Ecosystem Text Text
    | {- | A bucket outgrew the memory budget and nothing narrows it further, so the walk cannot
      read it without holding more than the budget allows.
      -}
      HaltBucketUnsplittable Ecosystem Text Text
    deriving stock (Eq, Show)

-- | One cycle's result: what it did, why it stopped early if it did, and what it could not read.
data CycleOutcome = CycleOutcome
    { outcomeHalt :: Maybe CycleHalt
    , outcomeTally :: SweepTally
    , outcomePrerequisites :: [TargetPrerequisites]
    {- ^ What a preview found of each target's standing permissions, in mount order. A run that
    refuses on them instead reports none.
    -}
    , outcomeEvidence :: EvidenceGaps
    -- ^ What the cycle could not read, which is separate from whether a real sweep may delete.
    }
    deriving stock (Eq, Show)

{- | Whether a cycle's counts cover what they claim to: it walked the whole store it was given,
and every rule that decided read the facts it needed. Nothing about permission enters here.
-}
outcomeComplete :: CycleOutcome -> Bool
outcomeComplete outcome =
    isNothing (outcomeHalt outcome) && evidenceComplete (outcomeEvidence outcome)

{- | What a preview could see of one standing permission. A preview exercises none of them, so an
unmet one is reported and never acted on.
-}
data PrerequisiteStatus
    = -- | The store answered, and a real sweep would pass this one.
      PrerequisiteMet
    | -- | The store answered, and a real sweep would stop here, carrying the backend's own text.
      PrerequisiteUnmet Text
    | -- | The store did not answer, so nothing the preview read settles it.
      PrerequisiteUnread Text
    deriving stock (Eq, Show)

-- | One target's standing permissions as a preview found them.
data TargetPrerequisites = TargetPrerequisites
    { tpEcosystem :: Ecosystem
    , tpBackend :: Text
    , tpConsent :: PrerequisiteStatus
    -- ^ Whether the store carries the operator's own deletion consent marker.
    , tpClassification :: PrerequisiteStatus
    -- ^ Whether deleting from the store destroys anything.
    }
    deriving stock (Eq, Show)

-- | Whether a real sweep of this target would pass both standing permissions.
prerequisitesMet :: TargetPrerequisites -> Bool
prerequisitesMet target = all (== PrerequisiteMet) [tpConsent target, tpClassification target]

{- | One target's line, which a preview prints above its counts. It closes on what no read
settles: a preview deletes nothing, so it proves no authority to delete.
-}
renderPrerequisites :: TargetPrerequisites -> Text
renderPrerequisites target =
    storeSubject (tpEcosystem target) (tpBackend target)
        <> ": deletion consent "
        <> renderPrerequisite (tpConsent target)
        <> ", and store classification "
        <> renderPrerequisite (tpClassification target)
        <> ". This preview deleted nothing, so it proves no authority to delete"

renderPrerequisite :: PrerequisiteStatus -> Text
renderPrerequisite = \case
    PrerequisiteMet -> "is met"
    PrerequisiteUnmet detail -> "is not met: " <> detail
    PrerequisiteUnread detail -> "could not be read: " <> detail

{- | What one cycle could not read. A count taken with a gap open describes part of the store, so
it is reported apart from the counts themselves rather than folded into them.
-}
data EvidenceGaps = EvidenceGaps
    { gapAdvisoryGeneration :: Int
    -- ^ Mounts that decided with no advisory generation loaded, so every advisory rule abstained.
    , gapManifests :: Int
    -- ^ Packages whose metadata the store did not serve, decided on identity alone.
    }
    deriving stock (Eq, Show)

instance Semigroup EvidenceGaps where
    left <> right =
        EvidenceGaps
            { gapAdvisoryGeneration = gapAdvisoryGeneration left + gapAdvisoryGeneration right
            , gapManifests = gapManifests left + gapManifests right
            }

instance Monoid EvidenceGaps where
    mempty = EvidenceGaps 0 0

-- | The gap one mount deciding without an advisory generation leaves.
unloadedGeneration :: EvidenceGaps
unloadedGeneration = mempty{gapAdvisoryGeneration = 1}

-- | The gap one package the store served no metadata for leaves.
unreadManifest :: EvidenceGaps
unreadManifest = mempty{gapManifests = 1}

-- | Whether a cycle read every fact its counts rest on.
evidenceComplete :: EvidenceGaps -> Bool
evidenceComplete gaps = gaps == mempty

-- | What a cycle could not read, as its closing line reports it, naming only what it did miss.
renderEvidenceGaps :: EvidenceGaps -> Text
renderEvidenceGaps gaps =
    T.intercalate
        ", "
        ( catMaybes
            [ counted (gapAdvisoryGeneration gaps) "mount" "decided without an advisory generation"
            , counted (gapManifests gaps) "package" "decided without the store's own metadata"
            ]
        )
  where
    counted count noun what
        | count <= 0 = Nothing
        | count == 1 = Just ("1 " <> noun <> " " <> what)
        | otherwise = Just (show count <> " " <> noun <> "s " <> what)

{- | Whether a halt stops the Dredger for the life of the process. Only the cap does, because a
breaker that re-closes itself is not a breaker; every other halt is re-read next cycle.
-}
latches :: CycleHalt -> Bool
latches = \case
    HaltDeletionCap{} -> True
    HaltConsentWithheld{} -> False
    HaltStorePreserved{} -> False
    HaltStoreFault{} -> False
    HaltBucketUnsplittable{} -> False

-- | The operator-facing text of a halt, naming the backend that raised it and what to fix.
renderCycleHalt :: CycleHalt -> Text
renderCycleHalt = \case
    HaltConsentWithheld eco backend descriptor ->
        storeSubject eco backend <> " carries no deletion consent marker: " <> descriptor
    HaltStorePreserved eco backend why ->
        storeSubject eco backend <> " refills itself, so a delete changes nothing: " <> why
    HaltDeletionCap cap issued etag ->
        "the cycle handed over "
            <> show issued
            <> " versions and reached its deletion cap of "
            <> show cap
            <> " under advisory generation "
            <> renderGeneration etag
            <> ", so the Dredger runs no further cycle until it is restarted deliberately"
    HaltStoreFault eco backend fault ->
        "a call against " <> storeSubject eco backend <> " produced no answer: " <> fault
    HaltBucketUnsplittable eco backend bucket ->
        "the walk over "
            <> storeSubject eco backend
            <> " cannot read the bucket of names beginning \""
            <> bucket
            <> "\": it holds more names than one bucket may, and no narrower bucket divides them"

-- | The advisory generation an audit line names, or that none was loaded.
renderGeneration :: Maybe DbEtag -> Text
renderGeneration = maybe "none" (\(DbEtag etag) -> etag)

storeSubject :: Ecosystem -> Text -> Text
storeSubject eco backend = "the " <> ecosystemName eco <> " mirror store on " <> backend

-- | One cycle's counts, as its closing line reports them.
renderTally :: SweepTally -> Text
renderTally tally =
    "examined "
        <> show (tallyExamined tally)
        <> ", deleted "
        <> show (tallyDeleted tally)
        <> ", kept "
        <> show (tallyKept tally)
        <> ", guard-skipped "
        <> show (tallyGuardSkipped tally)

-- | A store fault as an operator reads it: the transport's own cause and its bounded detail.
renderStoreFault :: StoreFault -> Text
renderStoreFault fault = renderTransportCause (tfCause transport) <> ": " <> tfDetail transport
  where
    transport = faultTransport fault

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

-- | Record one gap in what this cycle could read.
recordGap :: SweepState -> EvidenceGaps -> IO ()
recordGap counters gaps = modifyIORef' (stEvidence counters) (<> gaps)

-- | Record one target's standing permissions, which only a preview reads rather than acts on.
recordPrerequisites :: SweepState -> TargetPrerequisites -> IO ()
recordPrerequisites counters target = modifyIORef' (stPrerequisites counters) (target :)

-- | Count one version's disposition, in the cycle tally and at the metrics port together.
record :: SweepPorts -> SweepState -> SweepResult -> IO ()
record ports counters result = do
    dmpSweptVersion (sweepMetrics ports) result
    modifyIORef' (stTally counters) (<> tallyOf result)

{- A previewed deletion counts under its own metric arm and in the cycle's deleted column, so
one dry run reports the reach a real run would have. -}
tallyOf :: SweepResult -> SweepTally
tallyOf = \case
    SweepExamined -> mempty{tallyExamined = 1}
    SweepDeleted -> mempty{tallyDeleted = 1}
    SweepWouldDelete -> mempty{tallyDeleted = 1}
    SweepKept -> mempty{tallyKept = 1}
    SweepGuardSkipped -> mempty{tallyGuardSkipped = 1}
