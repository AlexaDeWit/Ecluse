-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Retention under repeated input, independent of the reader's wire format.
module Ecluse.Test.Contract.Retention (boundedRetention) where

import Test.Hspec

-- | Eight times more repeated input must remain inside the existing live-byte allowance.
boundedRetention :: Integer -> (Int -> IO Integer) -> Expectation
boundedRetention allowance held = do
    few <- held 2000
    repeated <- held 16000
    repeated - few `shouldSatisfy` (< allowance)
