-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Keyed reference projections reuse the production entry-by-entry degradation rules.
module Ecluse.Test.Registry.WireSupport (partitionLenient) where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind)
import Ecluse.Core.Registry.WireSupport (partitionLenientList)

{- | The keyed-map form of 'partitionLenientList', for a document whose entries already carry
their keys. The dropped list is in ascending-key order, so it is deterministic.
-}
partitionLenient :: InvalidEntryKind -> (Value -> Either String a) -> Map Text Value -> (Map Text a, [InvalidEntry])
partitionLenient kind decode =
    first Map.fromDistinctAscList . partitionLenientList kind decode . Map.toAscList
