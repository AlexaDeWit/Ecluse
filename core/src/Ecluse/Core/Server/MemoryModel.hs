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
    contractResidentBytes,
    packumentOriginFanout,
    mirrorJobEstimatedBytes,
) where

{- | Scale a wire (compact-encoded) byte count to its estimated resident
footprint: the 7.5x high-end ratio, applied as a halved integer to stay in 'Int'
arithmetic.
-}
expandWireBytes :: Int -> Int
expandWireBytes wireBytes = wireBytes * residentRatioNumerator `div` residentRatioDenominator

{- | Invert 'expandWireBytes': scale a resident-byte budget back to the wire
(compact-encoded) byte count it can hold, by the same ratio. A response cap carved
from a material share is derived through this, so the forward expansion and the
inverse contraction share one ratio and can never drift apart.
-}
contractResidentBytes :: Int -> Int
contractResidentBytes residentBytes = residentBytes * residentRatioDenominator `div` residentRatioNumerator

residentRatioNumerator :: Int
residentRatioNumerator = 15

residentRatioDenominator :: Int
residentRatioDenominator = 2

{- | How many origins one admitted materialisation holds concurrently: the private
and public packuments are fetched together ('Data.Functor.Concurrently'-style in
the packument pipeline), so an admission slot's envelope is this many wire+parsed
documents at once. The assembled encode and the public entry's cache residency
overlap this envelope and are covered by the material margin and the cache tenant
respectively, deliberately not double-counted here.
-}
packumentOriginFanout :: Int
packumentOriginFanout = 2

{- | The estimated resident footprint of one queued mirror job (a name, a version,
an artifact URL): what the in-memory queue's depth cap charges per slot.
-}
mirrorJobEstimatedBytes :: Int
mirrorJobEstimatedBytes = 1024
