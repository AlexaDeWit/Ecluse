-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Whether Pilot may replace the stored advisory artifact with a new compile.

Pilot writes only over the object revision it observed. It orders OSV and EPSS separately, each
by source identity, @Last-Modified@, and complete-stream digest, so an older run cannot replace
newer data. A new artifact may shrink only a bounded share below the durable size baseline, so a
damaged feed cannot replace good coverage. An operator's direct write bypasses these checks and
is the rollback authority. "Ecluse.Core.Osv.Publication.Envelope" carries the stored state.
-}
module Ecluse.Core.Osv.Publication (
    -- * The vocabulary
    module Ecluse.Core.Osv.Publication.Types,

    -- * The decision
    PublicationRequest (..),
    Precondition (..),
    PublicationPlan (..),
    SourceOrder (..),
    renderSourceOrder,
    OrderRefusal (..),
    ShrinkReading (..),
    PublicationRefusal (..),
    planPublication,
    renderPublicationRefusal,
) where

import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Core.Osv.Provenance (ProvenanceSource (EpssFeed, OsvExport))
import Ecluse.Core.Osv.Publication.Types
import Ecluse.Core.Osv.Schema (EpssStatus (EnrichmentAvailable, EnrichmentUnavailable), osvSchemaEpoch)
import Ecluse.Core.Text.Iso8601 (renderIso8601Utc)

-- | One publication attempt: the compile's facts, the artifact's size, and its nonce.
data PublicationRequest = PublicationRequest
    { prFacts :: CandidateFacts
    , prBytes :: Integer
    , prNonce :: PublicationNonce
    , prRebaseline :: Maybe Rebaseline
    }
    deriving stock (Eq, Show)

-- | The condition the store must hold for the write to land.
data Precondition
    = -- | Write only if no object holds the key.
      CreateOnly
    | -- | Write only over this exact revision.
      ReplaceRevision ObjectRevision
    deriving stock (Eq, Show)

-- | An accepted publication: the write's precondition, the state it records, and how it was ordered.
data PublicationPlan = PublicationPlan
    { plPrecondition :: Precondition
    , plState :: PublishedState
    , plOsvOrder :: SourceOrder
    , plEpssOrder :: Maybe SourceOrder
    -- ^ 'Nothing' when optional enrichment was unavailable, so EPSS was not compared.
    }
    deriving stock (Eq, Show)

-- | How one source's snapshot relates to the stored artifact's.
data SourceOrder
    = -- | No marker from this source identity was stored, so the snapshot starts its ordering.
      FreshScope
    | -- | The source declared a later @Last-Modified@.
      OrderedForward
    | -- | The same @Last-Modified@ and the same bytes, so the publication refreshes.
      EqualSnapshot
    | -- | The same bytes without comparable timestamps. The stored @Last-Modified@ carries forward.
      IdenticalBytes
    deriving stock (Eq, Show)

-- | The order as the publication log reads it.
renderSourceOrder :: SourceOrder -> Text
renderSourceOrder = \case
    FreshScope -> "new source"
    OrderedForward -> "newer snapshot"
    EqualSnapshot -> "same snapshot"
    IdenticalBytes -> "identical bytes"

-- | Why one source's snapshot cannot replace the stored one.
data OrderRefusal
    = -- | The candidate's @Last-Modified@ is older than the stored one, both carried.
      OlderSnapshot UTCTime UTCTime
    | -- | The same @Last-Modified@ with different bytes, so nothing orders them.
      AmbiguousSnapshot UTCTime
    | -- | Different bytes with no timestamp pair to order them.
      UnorderedSnapshot
    deriving stock (Eq, Show)

-- | The size comparison a refused shrink failed, all in bytes.
data ShrinkReading = ShrinkReading
    { srCandidateBytes :: Integer
    , srMinimumBytes :: Integer
    , srBaselineBytes :: Integer
    , srCreditBytes :: Integer
    }
    deriving stock (Eq, Show)

-- | Why Pilot will not replace the stored artifact. Consumers keep the stored one.
data PublicationRefusal
    = -- | The stored object's publication metadata is missing or unreadable.
      PredecessorUnreadable EnvelopeFault
    | -- | The stored metadata names another ecosystem or schema epoch, both carried.
      PredecessorOutOfScope Text Int
    | -- | One source's snapshot is not ordered after the stored one.
      SourceOutOfOrder ProvenanceSource OrderRefusal
    | -- | The artifact shrank past the baseline's allowance.
      ShrinkPastBaseline ShrinkReading
    | -- | The rebaseline names a revision the store does not hold. 'Nothing' is an empty store.
      RebaselineMismatch ObjectRevision (Maybe ObjectRevision)
    deriving stock (Eq, Show)

-- | Decide one attempt against one observation. A rebaseline replaces only the shrink allowance, never the ordering.
planPublication :: ShrinkPolicy -> BaselineCapability -> PublicationRequest -> StoreObservation -> Either PublicationRefusal PublicationPlan
planPublication policy capability request = \case
    StoreAbsent -> case prRebaseline request of
        Just approval -> Left (RebaselineMismatch (rbExpected approval) Nothing)
        Nothing -> Right (initialPlan request)
    StorePresent revision previousBytes stored -> do
        for_ (prRebaseline request) (approvedFor revision)
        previous <- first PredecessorUnreadable stored
        inScope facts previous
        osvOrder <- first (SourceOutOfOrder OsvExport) (orderSource (Just (psOsv previous)) (cfOsv facts))
        epssOrder <- first (SourceOutOfOrder EpssFeed) (traverse (orderSource (psLastEpss previous)) (cfEpss facts))
        sized <- first ShrinkPastBaseline (sizeAgainst policy capability request previous previousBytes)
        pure
            PublicationPlan
                { plPrecondition = ReplaceRevision revision
                , plState = recordedState request (snd osvOrder) (maybe (psLastEpss previous) (Just . snd) epssOrder) sized
                , plOsvOrder = fst osvOrder
                , plEpssOrder = fst <$> epssOrder
                }
  where
    facts = prFacts request

initialPlan :: PublicationRequest -> PublicationPlan
initialPlan request =
    PublicationPlan
        { plPrecondition = CreateOnly
        , plState = recordedState request (cfOsv facts) (cfEpss facts) (Sized (prBytes request) 0 InitialBaseline)
        , plOsvOrder = FreshScope
        , plEpssOrder = FreshScope <$ cfEpss facts
        }
  where
    facts = prFacts request

-- The ETag S3 reports is quoted, and an operator may copy it with or without the quotes.
approvedFor :: ObjectRevision -> Rebaseline -> Either PublicationRefusal ()
approvedFor observed approval
    | unquoted observed == unquoted (rbExpected approval) = Right ()
    | otherwise = Left (RebaselineMismatch (rbExpected approval) (Just observed))
  where
    unquoted (ObjectRevision tag) = T.dropAround (== '"') (T.strip tag)

inScope :: CandidateFacts -> PublishedState -> Either PublicationRefusal ()
inScope facts previous
    | psEcosystem previous == cfEcosystem facts && psSchemaEpoch previous == osvSchemaEpoch = Right ()
    | otherwise = Left (PredecessorOutOfScope (psEcosystem previous) (psSchemaEpoch previous))

-- A changed identity starts a fresh scope. Identical bytes keep the stored timestamp when the
-- candidate lacks one, so a later change is still ordered against it.
orderSource :: Maybe SourceMarker -> SourceMarker -> Either OrderRefusal (SourceOrder, SourceMarker)
orderSource Nothing candidate = Right (FreshScope, candidate)
orderSource (Just stored) candidate
    | smIdentity stored /= smIdentity candidate = Right (FreshScope, candidate)
    | otherwise = case (smLastModified candidate, smLastModified stored) of
        (Just new, Just old)
            | new > old -> Right (OrderedForward, candidate)
            | new < old -> Left (OlderSnapshot new old)
            | sameBytes -> Right (EqualSnapshot, candidate)
            | otherwise -> Left (AmbiguousSnapshot new)
        (new, old)
            | sameBytes -> Right (IdenticalBytes, candidate{smLastModified = new <|> old})
            | otherwise -> Left UnorderedSnapshot
  where
    sameBytes = smDigest stored == smDigest candidate

-- The baseline and credit an accepted size records, and how it set them.
data Sized = Sized Integer Integer Adjustment

-- The effective baseline is the baseline less any EPSS outage credit. A durable baseline keeps the
-- larger of itself and the accepted size, so neither small shrinks nor an overstated credit move it.
sizeAgainst :: ShrinkPolicy -> BaselineCapability -> PublicationRequest -> PublishedState -> Integer -> Either ShrinkReading Sized
sizeAgainst policy capability request previous previousBytes = case capability of
    NoBaseline -> Right (Sized candidate 0 OrdinaryBaseline)
    _
        | candidate < minimumBytes -> Left (ShrinkReading candidate minimumBytes baseline credit)
        | isJust (prRebaseline request) -> Right (Sized candidate 0 OperatorRebaseline)
        | capability == DurableBaseline -> Right (Sized (max baseline candidate) credit (creditAdjustment credit))
        | otherwise -> Right (Sized candidate credit (creditAdjustment credit))
  where
    candidate = prBytes request
    baseline = if capability == DurableBaseline then psBaselineBytes previous else previousBytes
    credit = epssCredit policy capability previous previousBytes (cfEpss (prFacts request))
    allowance = maybe (spMaxShrink policy) rbMaxShrink (prRebaseline request)
    minimumBytes = keptShare (max 0 (baseline - credit)) allowance
    creditAdjustment held = if held > 0 then EpssOutageCredit else OrdinaryBaseline

-- Only a fall from available to unavailable enrichment grants a credit, from the stored scored-row
-- count, never the candidate's. A durable baseline carries it, unchanged, through the outage.
epssCredit :: ShrinkPolicy -> BaselineCapability -> PublishedState -> Integer -> Maybe SourceMarker -> Integer
epssCredit policy capability previous previousBytes candidateEpss = case (psEpssStatus previous, candidateEpss) of
    (_, Just _) -> 0
    (EnrichmentAvailable, Nothing) -> min (epssBytesPerScoredRow * psScoredRows previous) (floorShare previousBytes (spEpssLossCap policy))
    (EnrichmentUnavailable, Nothing)
        | capability == DurableBaseline -> psEpssCreditBytes previous
        | otherwise -> 0

-- The smallest size that keeps the share left after the allowance, rounded up.
keptShare :: Integer -> Percent -> Integer
keptShare bytes allowance = ceiling (fromInteger bytes * (100 - unPercent allowance) / 100)

floorShare :: Integer -> Percent -> Integer
floorShare bytes share = floor (fromInteger bytes * unPercent share / 100)

recordedState :: PublicationRequest -> SourceMarker -> Maybe SourceMarker -> Sized -> PublishedState
recordedState request osv lastEpss (Sized baseline credit adjustment) =
    PublishedState
        { psEcosystem = cfEcosystem facts
        , psSchemaEpoch = osvSchemaEpoch
        , psOsv = osv
        , psEpssStatus = maybe EnrichmentUnavailable (const EnrichmentAvailable) (cfEpss facts)
        , psLastEpss = lastEpss
        , psRelevantRows = cfRelevantRows facts
        , psScoredRows = cfScoredRows facts
        , psBaselineBytes = baseline
        , psEpssCreditBytes = credit
        , psNonce = prNonce request
        , psAdjustment = adjustment
        }
  where
    facts = prFacts request

-- | The refusal as an operator reads it, ending with what stays in place.
renderPublicationRefusal :: PublicationRefusal -> Text
renderPublicationRefusal refusal = reason refusal <> ", so the stored artifact stays in place"
  where
    reason = \case
        PredecessorUnreadable fault -> "Pilot replaces only an object it can order against, and on the stored object " <> renderEnvelopeFault fault
        PredecessorOutOfScope ecosystem epoch -> "the stored object's publication metadata names ecosystem " <> ecosystem <> " at schema epoch " <> show epoch <> ", not this artifact's"
        SourceOutOfOrder source order -> sourceName source <> " " <> renderOrderRefusal order
        ShrinkPastBaseline reading -> renderShrink reading
        RebaselineMismatch (ObjectRevision expected) observed ->
            "the rebaseline expects revision " <> expected <> ", and the store holds " <> maybe "no object" (\(ObjectRevision tag) -> "revision " <> tag) observed

renderOrderRefusal :: OrderRefusal -> Text
renderOrderRefusal = \case
    OlderSnapshot new old -> "answered Last-Modified " <> renderIso8601Utc new <> ", older than the stored " <> renderIso8601Utc old
    AmbiguousSnapshot at -> "answered the stored Last-Modified " <> renderIso8601Utc at <> " with different bytes, so nothing orders them"
    UnorderedSnapshot -> "changed without a Last-Modified that orders it after the stored snapshot"

renderShrink :: ShrinkReading -> Text
renderShrink reading =
    "the artifact is "
        <> show (srCandidateBytes reading)
        <> " bytes, under the "
        <> show (srMinimumBytes reading)
        <> "-byte minimum from a "
        <> show (srBaselineBytes reading)
        <> "-byte baseline"
        <> credit
  where
    credit
        | srCreditBytes reading > 0 = " less a " <> show (srCreditBytes reading) <> "-byte EPSS outage credit"
        | otherwise = ""

sourceName :: ProvenanceSource -> Text
sourceName = \case
    OsvExport -> "the OSV export"
    EpssFeed -> "the EPSS feed"
