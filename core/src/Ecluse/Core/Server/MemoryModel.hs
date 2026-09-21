-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Retained-document accounting and fixed resource estimates.
Compact representation charges differ from original source bytes. The retained estimate
does not bound active input, parser, policy, merge, or output memory.
-}
module Ecluse.Core.Server.MemoryModel (
    expandWireBytes,
    mirrorJobEstimatedBytes,
) where

{- | Estimate retained bytes from a compact representation charge using the 7.5 factor.
This factor is independent of source-byte regression envelopes and active-work admission.
-}
expandWireBytes :: Int -> Int
expandWireBytes wireBytes = wireBytes * residentRatioNumerator `div` residentRatioDenominator

residentRatioNumerator :: Int
residentRatioNumerator = 15

residentRatioDenominator :: Int
residentRatioDenominator = 2

-- | The resident-byte allowance per in-memory mirror queue slot.
mirrorJobEstimatedBytes :: Int
mirrorJobEstimatedBytes = 1024
