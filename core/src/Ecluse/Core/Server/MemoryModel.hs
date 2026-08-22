-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The one wire-to-resident memory model the byte budgets share.

A fetched metadata document costs far more resident than its wire size. The parsed
structure, the retained raw 'Data.Aeson.Value', and their spines expand a
compact-encoded document by a near-constant factor. Every consumer that budgets bytes
against that expansion must use the __same__ factor, or the budgets drift against each
other. This module is that single model. The cache weigher
("Ecluse.Core.Server.Cache") and the composition root's memory plan both read it, so
they can never disagree.

The factor sits at the high end of the measured resident-to-encoded ratio, so estimates
upper-bound resident bytes and a budget never systematically under-counts. A leaner
document is over-estimated, which only over-evicts. A measurement pass refining the
factor is deliberately deferred to the load bench. Until then the conservative bound
stands.
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

{- | Invert 'expandWireBytes': scale a resident-byte budget back to the wire (compact-encoded)
byte count it can hold, by the same ratio, so the two can never drift apart.
-}
contractResidentBytes :: Int -> Int
contractResidentBytes residentBytes = residentBytes * residentRatioDenominator `div` residentRatioNumerator

residentRatioNumerator :: Int
residentRatioNumerator = 15

residentRatioDenominator :: Int
residentRatioDenominator = 2

{- | How many origins one admitted materialisation holds concurrently: the private and public
packuments are fetched together. The encode and the cache residency are covered by the material
margin and the cache tenant, so this deliberately does not count them.
-}
packumentOriginFanout :: Int
packumentOriginFanout = 2

{- | The estimated resident footprint of one queued mirror job (a name, a version,
an artifact URL): what the in-memory queue's depth cap charges per slot.
-}
mirrorJobEstimatedBytes :: Int
mirrorJobEstimatedBytes = 1024
