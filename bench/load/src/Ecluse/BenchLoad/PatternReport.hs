-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cache outcome denominators and byte ratios for finite request patterns.
module Ecluse.BenchLoad.PatternReport (StoreEvidence (..), renderStoreEvidence) where

import Data.Text qualified as T

-- | Occupancy uses accounted resident bytes, while the working set uses captured wire bytes.
data StoreEvidence = StoreEvidence
    { seStore :: Text
    , seCapacity :: Int
    , seWireWorkingSet :: Int
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
        ( [ "| store | capacity resident B | working set wire B | wire B / resident capacity B | observed resident B | retention hit fraction | collapsed fraction | hits / misses / collapsed | oversized refusals |"
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
                , show (seWireWorkingSet store)
                , ratio (seWireWorkingSet store) (seCapacity store)
                , show (seResidentBytes store)
                , ratio (seHits store) (resolutions store)
                , ratio (seCollapsed store) (resolutions store)
                , show (seHits store) <> " / " <> show (seMisses store) <> " / " <> show (seCollapsed store)
                , show (seRefused store)
                ]
            <> " |"
    resolutions store = seHits store + seMisses store + seCollapsed store
    ratio numerator denominator
        | denominator <= 0 = "n/a"
        | otherwise = show (fromIntegral numerator / fromIntegral denominator :: Double)
