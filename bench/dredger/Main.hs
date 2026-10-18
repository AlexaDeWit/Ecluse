-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Run the isolated Dredger preview comparison with a positive sample count.
module Main (main) where

import Ecluse.Core.Registry.SweepBench (measure)
import System.Environment (getArgs)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [raw] | Just count <- readMaybe raw, count > 0 -> measure count
        _ -> fail "usage: ecluse-dredger-bench POSITIVE_SAMPLE_COUNT"
