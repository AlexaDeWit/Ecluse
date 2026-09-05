-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cross-ecosystem scaffolding for projecting an untrusted registry wire document
into the domain model, shared by every ecosystem's projection
("Ecluse.Core.Registry.Npm.Project"):

* __Per-entry lenient degradation__. 'partitionLenient' splits a raw @key -> 'Value'@
  map into the entries that decode and the ones that do not. It drops each malformed
  entry and records it as an 'InvalidEntry' rather than failing the whole document.
  This is the one place that realises per-entry leniency and drop-tracking. Every
  ecosystem's element-wise-lenient axis layers its own decode on top: npm's
  @versions@\/@dist-tags@\/@time@, or another ecosystem's element-wise list.
* __Name agreement__. 'checkNameAgreement' checks that the name an upstream self-reports
  agrees with the name the proxy resolved from the route. The requested name is the
  validation authority, never a rewrite. A disagreement carries the reported name
  verbatim, so the caller can drop that origin's contribution as untrusted for this
  request.
-}
module Ecluse.Core.Registry.WireSupport (
    -- * Per-entry lenient degradation
    partitionLenient,

    -- * Name agreement
    NameAgreement (..),
    checkNameAgreement,
) where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (
    InvalidEntry,
    InvalidEntryKind,
    PackageName,
    mkInvalidEntry,
    renderPackageName,
 )

{- | Partition a raw @key -> 'Value'@ map into the entries that decode and the ones that do not.
Each dropped entry carries its key, the raw offending 'Value' verbatim, and the decode error as
the reason. The dropped list is in ascending-key order, so it is deterministic.
-}
partitionLenient :: InvalidEntryKind -> (Value -> Either String a) -> Map Text Value -> (Map Text a, [InvalidEntry])
partitionLenient kind decode =
    Map.foldrWithKey step (Map.empty, [])
  where
    step key value (kept, dropped) = case decode value of
        Right a -> (Map.insert key a kept, dropped)
        Left err -> (kept, mkInvalidEntry kind key value (toText err) : dropped)

{- | The outcome of checking an upstream's self-reported name against the requested name. The
requested name validates the document. It never rewrites it.
-}
data NameAgreement
    = -- | The self-reported name agreed with the request.
      NameAgrees
    | -- | The self-reported name __disagreed__, reporting this /different/ name (carried verbatim for the audit log).
      NameDisagrees Text
    deriving stock (Eq, Show)

{- | Check an upstream's self-reported 'PackageName' against the requested one through
ecosystem-aware 'PackageName' equality, never a byte compare an encoding variant could slip past.
A disagreement carries the reported name so the caller can drop that origin's contribution, and
the proxy never substitutes the name.
-}
checkNameAgreement :: PackageName -> PackageName -> NameAgreement
checkNameAgreement requestedName reportedName
    | reportedName == requestedName = NameAgrees
    | otherwise = NameDisagrees (renderPackageName reportedName)
