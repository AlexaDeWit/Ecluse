-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The common worker load window excludes preparation and evidence checks from each recorded interval.
module Ecluse.BenchLoad.Worker (runWorkerLoad) where

import GHC.Clock (getMonotonicTime)

import Ecluse.BenchLoad.Harness (LoadKnobs (lkDurationSeconds))

-- | Preparation and checks still consume the duration window, as enqueue and receive do for npm.
runWorkerLoad :: LoadKnobs -> IO a -> (a -> IO ()) -> (a -> IO ()) -> IO [Double]
runWorkerLoad knobs prepare work verify = do
    deadline <- (+ fromIntegral (lkDurationSeconds knobs)) <$> getMonotonicTime
    go deadline []
  where
    go deadline acc = do
        now <- getMonotonicTime
        if now >= deadline
            then pure (reverse acc)
            else do
                input <- prepare
                start <- getMonotonicTime
                work input
                end <- getMonotonicTime
                verify input
                go deadline ((end - start) : acc)
