-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

-- | Cache outcome denominators and byte ratios for finite request patterns.
module Ecluse.BenchLoad.PatternReport (StoreEvidence (..), renderStoreEvidence, ReplayTotals (..), renderReplayTotals) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T

-- | A missing working set means no measurement of that store's accounted representation exists.
data StoreEvidence = StoreEvidence
    { seStore :: Text
    , seCapacity :: Int
    , seAccountedWorkingSet :: Maybe Int
    , seResidentBytes :: Int
    , seHits :: Int
    , seMisses :: Int
    , seCollapsed :: Int
    , seRefused :: Int
    }
    deriving stock (Eq, Show)

-- | Retention and collapse divide by all store resolutions, including failed leader fetches.
renderStoreEvidence :: [StoreEvidence] -> Text
renderStoreEvidence stores =
    T.unlines
        ( [ "| store | capacity resident B | working set accounted B | working set / capacity | observed resident B | retention hit fraction | collapsed fraction | hits / misses / collapsed | oversized refusals |"
          , "| --- | --: | --: | --: | --: | --: | --: | --- | --: |"
          ]
            <> map row stores
        )
  where
    row store =
        "| "
            <> T.intercalate
                " | "
                [ seStore store
                , show (seCapacity store)
                , maybe "unavailable" show (seAccountedWorkingSet store)
                , maybe "unavailable" (\bytes -> ratio bytes (seCapacity store)) (seAccountedWorkingSet store)
                , show (seResidentBytes store)
                , ratio (seHits store) (resolutions store)
                , ratio (seCollapsed store) (resolutions store)
                , show (seHits store) <> " / " <> show (seMisses store) <> " / " <> show (seCollapsed store)
                , show (seRefused store)
                ]
            <> " |"
    resolutions store = seHits store + seMisses store + seCollapsed store
    ratio count total
        | total <= 0 = "n/a"
        | otherwise = show (fromIntegral count / fromIntegral total :: Double)

-- | Terminal outcomes partition the scheduled requests. Unfinished includes in-flight and unstarted work.
data ReplayTotals = ReplayTotals
    { rtotalScheduled :: Int
    , rtotalCompleted :: Int
    , rtotalSuccessful :: Int
    , rtotalRefused :: Int
    , rtotalOtherHttpFailures :: Int
    , rtotalTransportFailed :: Int
    , rtotalUnfinished :: Int
    , rtotalElapsedSeconds :: Double
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Count and rate for each outcome, with success measured against the whole scheduled sequence.
renderReplayTotals :: ReplayTotals -> Text
renderReplayTotals totals =
    T.unlines
        ( [ "| finite replay outcome | requests | requests/s |"
          , "| --- | --: | --: |"
          ]
            <> map
                row
                [ ("scheduled", rtotalScheduled totals)
                , ("completed HTTP responses", rtotalCompleted totals)
                , ("successful HTTP (2xx/3xx)", rtotalSuccessful totals)
                , ("HTTP refused (429/503)", rtotalRefused totals)
                , ("other non-success HTTP", rtotalOtherHttpFailures totals)
                , ("transport failed", rtotalTransportFailed totals)
                , ("unfinished at deadline", rtotalUnfinished totals)
                ]
        )
  where
    row (label, count) =
        "| "
            <> label
            <> " | "
            <> show count
            <> " | "
            <> show (fromIntegral count / max 1e-9 (rtotalElapsedSeconds totals) :: Double)
            <> " |"
