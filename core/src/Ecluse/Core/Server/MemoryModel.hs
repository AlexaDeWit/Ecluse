-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The one wire-to-resident memory model the byte budgets share.

A fetched metadata document costs far more resident than its wire size: the
parsed structure, the retained raw 'Data.Aeson.Value', and their spines expand a
compact-encoded document by a near-constant factor. Every consumer that budgets
bytes against that expansion must use the __same__ factor, or the budgets drift
against each other (historically the cache weigher assumed 7.5x while the memory
budget assumed 4x, so the admission arithmetic under-counted what the cache
accounting would charge for the very same document). This module is that single
model: the cache weigher ("Ecluse.Core.Server.Cache") and the composition root's
memory plan both read it, so they can never disagree again.

The factor sits at the high end of the measured resident-to-encoded ratio, so
estimates upper-bound resident bytes and a budget never systematically
under-counts (a leaner document is over-estimated, which only over-evicts). A
measurement pass refining it is deliberately deferred to the architect's load
bench; until then the conservative bound stands.
-}
module Ecluse.Core.Server.MemoryModel (
    expandWireBytes,
) where

{- | Scale a wire (compact-encoded) byte count to its estimated resident
footprint: the 7.5x high-end ratio, applied as a halved integer to stay in 'Int'
arithmetic.
-}
expandWireBytes :: Int -> Int
expandWireBytes wireBytes = wireBytes * residentRatioNumerator `div` residentRatioDenominator

residentRatioNumerator :: Int
residentRatioNumerator = 15

residentRatioDenominator :: Int
residentRatioDenominator = 2
