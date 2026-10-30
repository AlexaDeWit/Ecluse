-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Structural checks for the buffered reference decoder and diagnostic fixture trees.
module Ecluse.Test.Security.Limits (checkVersionCount, checkNestingDepth) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Ecluse.Core.Package (PackageInfo (infoVersions))
import Ecluse.Core.Security (LimitError (TooDeeplyNested), Limits (maxNestingDepth), checkVersionCountOf)

-- | Apply the production version ceiling to the buffered reference projection.
checkVersionCount :: Limits -> PackageInfo -> Either LimitError PackageInfo
checkVersionCount limits info = info <$ checkVersionCountOf limits (Map.size (infoVersions info))

-- | Preserve the former whole-tree depth check for baseline measurements.
checkNestingDepth :: Limits -> Value -> Either LimitError Value
checkNestingDepth limits value =
    if withinNestingBudget (maxNestingDepth limits) value
        then Right value
        else Left (TooDeeplyNested (maxNestingDepth limits))

{- | True iff @value@ nests no deeper than @budget@ levels: a scalar and an empty container are
depth @1@, and each enclosing 'Object' or 'Array' adds one.
-}
withinNestingBudget :: Int -> Value -> Bool
withinNestingBudget budget v =
    budget >= 1 && case v of
        Object o -> all (withinNestingBudget (budget - 1)) (KeyMap.elems o)
        Array xs -> V.all (withinNestingBudget (budget - 1)) xs
        String _ -> True
        Number _ -> True
        Bool _ -> True
        Null -> True
