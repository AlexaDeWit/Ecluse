-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cross-ecosystem scaffolding for projecting an untrusted registry wire document
into the domain model, shared by every ecosystem's projection
("Ecluse.Core.Registry.Npm.Project"):

* __Per-entry lenient degradation.__ 'partitionLenient' splits a raw @key -> 'Value'@
  map into the entries that decode and the ones that do not, dropping each malformed
  entry and recording it as an 'InvalidEntry' rather than failing the whole document.
  This is the one place per-entry leniency and drop-tracking are realised; every
  ecosystem's element-wise-lenient axis (npm's @versions@\/@dist-tags@\/@time@, or another
  ecosystem's element-wise list) layers its own decode on top.
* __Name agreement.__ 'checkNameAgreement' is the anti-shadowing check that the name an
  upstream self-reports agrees with the name the proxy resolved from the route. The
  requested name is the validation authority, never a rewrite: a disagreement carries
  the reported name verbatim so the caller can drop that origin's contribution as
  untrusted for this request.
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
that do not: each undecodable entry is dropped and recorded as an 'InvalidEntry' of the
given 'InvalidEntryKind', carrying its key, the __raw offending 'Value'__ (verbatim, for
diagnostics), and the decode error as the reason. The dropped list is in ascending-key
order ('Map.foldrWithKey' visits keys ascending and each step prepends), so it is
deterministic. This is the one place per-entry leniency and drop-tracking are realised,
shared across every ecosystem's element-wise-lenient projection axes.
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
document; it never rewrites it.
-}
data NameAgreement
    = -- | The self-reported name agreed with the request.
      NameAgrees
    | -- | The self-reported name __disagreed__, reporting this /different/ name (carried verbatim for the audit log).
      NameDisagrees Text
    deriving stock (Eq, Show)

{- | Check an upstream's self-reported 'PackageName' against the requested one via
ecosystem-aware 'PackageName' equality (npm's case sensitivity is honoured, so this is
never a byte-for-byte compare an encoding variant could slip past). Agreement is
'NameAgrees'; a disagreement is 'NameDisagrees' carrying the __reported__ name
(rendered), so the caller can treat that origin as untrusted for this request and drop
its contribution. The name is never substituted.
-}
checkNameAgreement :: PackageName -> PackageName -> NameAgreement
checkNameAgreement requestedName reportedName
    | reportedName == requestedName = NameAgrees
    | otherwise = NameDisagrees (renderPackageName reportedName)
