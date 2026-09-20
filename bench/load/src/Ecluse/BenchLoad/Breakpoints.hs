-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | Weighted full-store model over observed request intervals.
Completion times approximate insertion. This model does not measure decoding or heap residency.
-}
module Ecluse.BenchLoad.Breakpoints (
    Access (..),
    TraceRead (..),
    ModelBudget (..),
    Reuse (..),
    ModelReport (..),
    modelTrace,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map

-- | Artifact probes neither populate nor refresh the full store.
data Access = Listing | Artifact
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Times are monotonic microseconds from one run. Weights use production accounting.
data TraceRead = TraceRead
    { trKey :: Text
    , trStart :: Integer
    , trEnd :: Integer
    , trWeight :: Integer
    , trAccess :: Access
    , trSuccess :: Bool
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Full-store capacity, entry count, and insertion-based TTL, in microseconds.
data ModelBudget = ModelBudget
    { mbBytes :: Integer
    , mbEntries :: Int
    , mbTtlMicros :: Integer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Useful hits carry their insertion age and admitted bytes since insertion.
data Reuse = Reuse
    { reuseKey :: Text
    , reuseAccess :: Access
    , reuseDelayMicros :: Integer
    , reuseInterveningBytes :: Integer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Model outcomes stay separate from measured proxy telemetry.
data ModelReport = ModelReport
    { mrHits :: Int
    , mrMisses :: Int
    , mrCollapsed :: Int
    , mrCapacityEvictions :: Int
    , mrBytePressureEvictions :: Int
    , mrCountPressureEvictions :: Int
    , mrExpired :: Int
    , mrOversized :: Int
    , mrAdmittedBytes :: Integer
    , mrPeakBytes :: Integer
    , mrReuse :: [Reuse]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Held = Held
    { heldWeight :: Integer
    , heldInserted :: Integer
    , heldStamp :: Int
    , heldAdmitted :: Integer
    }

data Model = Model
    { modelHeld :: Map Text Held
    , modelPending :: Map Text TraceRead
    , modelStamp :: Int
    , modelReport :: ModelReport
    }

data Event = Begin TraceRead | Complete TraceRead

-- | Reject malformed observations before sweeping the same intervals under another budget.
modelTrace :: ModelBudget -> [TraceRead] -> Either Text ModelReport
modelTrace budget reads
    | mbBytes budget < 0 || mbEntries budget <= 0 || mbTtlMicros budget < 0 = Left "invalid model budget"
    | any invalid reads = Left "invalid request interval or weight"
    | otherwise = Right ((modelReport final){mrReuse = reverse (mrReuse (modelReport final))})
  where
    invalid r = trStart r < 0 || trEnd r <= trStart r || trWeight r <= 0
    events = sortOn eventOrder (concatMap (\r -> [Begin r, Complete r]) reads)
    eventOrder (Begin r) = (trStart r, 1 :: Int)
    eventOrder (Complete r) = (trEnd r, 0)
    initial = Model Map.empty Map.empty 0 (ModelReport 0 0 0 0 0 0 0 0 0 0 [])
    final = foldl' (step budget) initial events

step :: ModelBudget -> Model -> Event -> Model
step budget model event = case event of
    Begin r -> begin r (expire budget (trStart r) model)
    Complete r ->
        if Map.lookup (trKey r) (modelPending model) == Just r
            then
                let settled = model{modelPending = Map.delete (trKey r) (modelPending model)}
                 in if trSuccess r then insert budget r settled else settled
            else model

begin :: TraceRead -> Model -> Model
begin r model = case Map.lookup (trKey r) (modelHeld model) of
    Just held ->
        let report = modelReport model
            stamp = modelStamp model + 1
            reuse = Reuse (trKey r) (trAccess r) (trStart r - heldInserted held) (mrAdmittedBytes report - heldAdmitted held)
            held' = if trAccess r == Listing then held{heldStamp = stamp} else held
         in model
                { modelHeld = Map.insert (trKey r) held' (modelHeld model)
                , modelStamp = stamp
                , modelReport = report{mrHits = mrHits report + 1, mrReuse = reuse : mrReuse report}
                }
    Nothing ->
        let report = modelReport model
            follows = trAccess r == Listing && Map.member (trKey r) (modelPending model)
         in if follows
                then model{modelReport = report{mrCollapsed = mrCollapsed report + 1}}
                else
                    model
                        { modelPending = if trAccess r == Listing then Map.insert (trKey r) r (modelPending model) else modelPending model
                        , modelReport = report{mrMisses = mrMisses report + 1}
                        }

expire :: ModelBudget -> Integer -> Model -> Model
expire budget now model =
    let (dead, live) = Map.partition (\held -> heldInserted held + mbTtlMicros budget < now) (modelHeld model)
        report = modelReport model
     in model{modelHeld = live, modelReport = report{mrExpired = mrExpired report + Map.size dead}}

insert :: ModelBudget -> TraceRead -> Model -> Model
insert budget r model
    | trWeight r > mbBytes budget = model{modelReport = report{mrOversized = mrOversized report + 1}}
    | otherwise =
        let pruned = evict budget (trWeight r) (expire budget (trEnd r) model)
            before = modelReport pruned
            admitted = mrAdmittedBytes before + trWeight r
            stamp = modelStamp pruned + 1
            held = Held (trWeight r) (trEnd r) stamp admitted
            entries = Map.insert (trKey r) held (modelHeld pruned)
         in pruned
                { modelHeld = entries
                , modelStamp = stamp
                , modelReport = before{mrAdmittedBytes = admitted, mrPeakBytes = max (mrPeakBytes before) (occupied entries)}
                }
  where
    report = modelReport model

evict :: ModelBudget -> Integer -> Model -> Model
evict budget incoming model
    | occupied entries + incoming <= mbBytes budget && Map.size entries < mbEntries budget = model
    | otherwise = case sortOn (heldStamp . snd) (Map.toList entries) of
        [] -> model
        (key, _) : _ ->
            evict
                budget
                incoming
                model
                    { modelHeld = Map.delete key entries
                    , modelReport =
                        report
                            { mrCapacityEvictions = mrCapacityEvictions report + 1
                            , mrBytePressureEvictions = mrBytePressureEvictions report + fromEnum (occupied entries + incoming > mbBytes budget)
                            , mrCountPressureEvictions = mrCountPressureEvictions report + fromEnum (Map.size entries >= mbEntries budget)
                            }
                    }
  where
    entries = modelHeld model
    report = modelReport model

occupied :: Map Text Held -> Integer
occupied = sum . map heldWeight . Map.elems
