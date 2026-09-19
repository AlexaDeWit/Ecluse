-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What one sweep cycle did, why it stopped early if it did, and what it could not read.

A count is only as good as the evidence behind it, so a cycle reports its halt and its gaps
beside its tally rather than folding them in. The rendered lines are the operator's, so their
wording is the reported behaviour.
-}
module Ecluse.Core.Registry.Sweep.Outcome (
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
    storeSubject,

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
) where

import Data.Text qualified as T

import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Fault (renderTransportCause, tfCause, tfDetail)
import Ecluse.Core.Registry.Maintenance (StoreFault (faultTransport))

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
      advisory generation of the denial that reached it. No later cycle runs.
      -}
      HaltDeletionCap Int Int (Maybe DbEtag)
    | -- | A store call produced no answer and its retry advice ran out, carrying the fault.
      HaltStoreFault Ecosystem Text Text
    | {- | A bucket outgrew the memory budget and nothing narrows it further, so the walk cannot
      read it within the budget.
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

{- | Whether a halt stops the Dredger for the life of the process. Only the cap does, because a
breaker that re-closes itself is not a breaker. Every other halt is re-read next cycle.
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

-- | Name a store without assuming whether it is the mirror or private target.
storeSubject :: Ecosystem -> Text -> Text
storeSubject eco backend = "the " <> ecosystemName eco <> " store on " <> backend

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

