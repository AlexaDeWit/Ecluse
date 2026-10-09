-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixtures the load harness specs share: one pod shape, its floors for npm and PyPI, and a run a slow network keeps off them.
module Ecluse.BenchLoad.Support (
    twoCores,
    floorsAtTwoCores,
    slowNetwork,
) where

import Data.Map.Strict qualified as Map

import Ecluse.BenchLoad.Floors (Enforcement (NotHeld), FloorKey, Pass (ConcurrencyOne, Loaded), Unheld (LatencyAboveCeiling))
import Ecluse.BenchLoad.Pod (PodShape (Limited))

twoCores :: PodShape
twoCores = Limited 2 (1024 * 1024 * 1024)

floorsAtTwoCores :: Map FloorKey Int
floorsAtTwoCores =
    Map.fromList
        [ (("npm/merge-cold", Loaded), 338)
        , (("npm/merge-cold", ConcurrencyOne), 46)
        , (("npm/herd", Loaded), 10)
        , (("pypi/index-cold", Loaded), 964)
        , (("pypi/index-cold", ConcurrencyOne), 134)
        ]

slowNetwork :: Enforcement
slowNetwork = NotHeld (LatencyAboveCeiling 444 210 :| [])
