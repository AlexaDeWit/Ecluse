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
    InvalidEntry (InvalidEntry),
    InvalidEntryKind,
    PackageName,
    renderPackageName,
 )

{- | Partition a raw @key -> 'Value'@ map into the entries that decode and the ones
that do not. It drops each undecodable entry and records it as an 'InvalidEntry' of the
given 'InvalidEntryKind'. The entry carries its key, the __raw offending 'Value'__
(verbatim, for diagnostics), and the decode error as the reason. The dropped list is in
ascending-key order ('Map.foldrWithKey' visits keys ascending and each step prepends),
so it is deterministic. This is the one place that realises per-entry leniency and
drop-tracking, shared across every ecosystem's element-wise-lenient projection axes.
-}
partitionLenient :: InvalidEntryKind -> (Value -> Either String a) -> Map Text Value -> (Map Text a, [InvalidEntry])
partitionLenient kind decode =
    Map.foldrWithKey step (Map.empty, [])
  where
    step key value (kept, dropped) = case decode value of
        Right a -> (Map.insert key a kept, dropped)
        Left err -> (kept, InvalidEntry kind key value (toText err) : dropped)

{- | The outcome of checking an upstream's self-reported name against the requested
name (the identity the proxy resolved from the route). The requested name validates the
document. It never rewrites it.
-}
data NameAgreement
    = -- | The self-reported name agreed with the request.
      NameAgrees
    | -- | The self-reported name __disagreed__, reporting this /different/ name (carried verbatim for the audit log).
      NameDisagrees Text
    deriving stock (Eq, Show)

{- | Check an upstream's self-reported 'PackageName' against the requested one through
ecosystem-aware 'PackageName' equality. That equality honours npm's case sensitivity, so
the check is never a byte-for-byte compare an encoding variant could slip past. Agreement
is 'NameAgrees'. A disagreement is 'NameDisagrees' carrying the __reported__ name
(rendered), so the caller can treat that origin as untrusted for this request and drop
its contribution. The proxy never substitutes the name.
-}
checkNameAgreement :: PackageName -> PackageName -> NameAgreement
checkNameAgreement requestedName reportedName
    | reportedName == requestedName = NameAgrees
    | otherwise = NameDisagrees (renderPackageName reportedName)
