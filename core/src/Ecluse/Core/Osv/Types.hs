-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory-range vocabulary of the OSV pipeline.

"Ecluse.Core.Osv.Advisory" and "Ecluse.Core.Osv.Compile" write these bounds into the
compiled artifact, and "Ecluse.Core.Cve" and "Ecluse.Core.Rules" read them back. The
vocabulary lives apart from both so the reading side never imports the writing side.

Nothing here evaluates. The module holds dependency-light data only.
-}
module Ecluse.Core.Osv.Types (
    UpperBound (..),
) where

{- | Where an affected interval closes. An advisory states one upper bound or none, so
no segment can carry two.
-}
data UpperBound
    = -- | Exclusive: affected below this version, which is itself the fix.
      FixedBefore Text
    | -- | Inclusive: affected up to and including this version.
      LastAffected Text
    | -- | The interval never closes.
      Unbounded
    deriving stock (Show, Eq)
