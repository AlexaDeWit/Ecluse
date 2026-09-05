-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cross-ecosystem scaffolding for projecting an untrusted registry wire document
into the domain model, shared by every ecosystem's projection
("Ecluse.Core.Registry.Npm.Project"):

* __Per-entry lenient degradation__. 'partitionLenientList' splits a list of keyed raw
  entries into the ones that decode and the ones that do not, dropping each malformed
  entry as an 'InvalidEntry' rather than failing the whole document. 'partitionLenient'
  is the keyed-map form of it. This is the one place that realises per-entry leniency and
  drop-tracking, and the one place that builds an 'InvalidEntry' from a decode failure.
  Every ecosystem's element-wise-lenient axis layers its own decode on top: npm's
  @versions@\/@dist-tags@\/@time@ maps, or an array-shaped index's file list, which
  supplies each element's key itself.
* __The name floor__. 'parseNameComponent' is the non-empty, ASCII, path-safe trio every
  captured or wire-declared name component must clear before it reaches an interpolated
  upstream URL. Each ecosystem's grammar sits on it and adds only its own rules, so no
  parser can reach a URL having checked one of the three and forgotten another.
* __Name agreement__. 'checkNameAgreement' checks that the name an upstream self-reports
  agrees with the name the proxy resolved from the route, and carries what was projected
  through on agreement. The requested name is the validation authority, never a rewrite. A
  disagreement carries the reported name verbatim and no payload, so the caller cannot
  serve a contribution the origin is untrusted for.
-}
module Ecluse.Core.Registry.WireSupport (
    -- * Per-entry lenient degradation
    partitionLenient,
    partitionLenientList,

    -- * Name agreement
    Projection (..),
    checkNameAgreement,

    -- * The name floor
    NameRefusal (..),
    parseNameComponent,
) where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Package (
    InvalidEntry,
    InvalidEntryKind,
    PackageName,
    isAsciiNameComponent,
    mkInvalidEntry,
    renderPackageName,
 )
import Ecluse.Core.Server.Path (isSafeComponent)

{- | Partition a list of keyed raw entries into the ones that decode and the ones that do not,
in input order. An array-shaped format pairs each element with its own key first.
-}
partitionLenientList :: InvalidEntryKind -> (Value -> Either String a) -> [(Text, Value)] -> ([(Text, a)], [InvalidEntry])
partitionLenientList kind decode =
    foldr step ([], [])
  where
    step (key, value) (kept, dropped) = case decode value of
        Right a -> ((key, a) : kept, dropped)
        Left err -> (kept, mkInvalidEntry kind key value (toText err) : dropped)

{- | The keyed-map form of 'partitionLenientList', for a document whose entries already carry
their keys. The dropped list is in ascending-key order, so it is deterministic.
-}
partitionLenient :: InvalidEntryKind -> (Value -> Either String a) -> Map Text Value -> (Map Text a, [InvalidEntry])
partitionLenient kind decode =
    first Map.fromDistinctAscList . partitionLenientList kind decode . Map.toAscList

{- | What an upstream document projected into, once its self-reported name has been checked.
A mismatch carries no payload, so a disagreeing origin's contribution is unrepresentable.
-}
data Projection a
    = -- | The self-reported name agreed with the request, carrying what was projected.
      Projected a
    | -- | The document self-reported this /different/ name (carried verbatim for the audit log).
      NameMismatch Text
    deriving stock (Eq, Show)

{- | Check an upstream's self-reported 'PackageName' against the requested one through
ecosystem-aware 'PackageName' equality, never a byte compare an encoding variant could slip past.
A disagreement carries the reported name so the caller can drop that origin's contribution, and
the proxy never substitutes the name.
-}
checkNameAgreement :: PackageName -> PackageName -> a -> Projection a
checkNameAgreement requestedName reportedName projected
    | reportedName == requestedName = Projected projected
    | otherwise = NameMismatch (renderPackageName reportedName)

-- | Why a name component did not clear the floor every ecosystem's grammar sits on.
data NameRefusal
    = -- | The component was empty, so it names nothing.
      NameEmpty
    | -- | The component carried a non-ASCII or control codepoint, which renders two names as one.
      NameNotAscii
    | -- | The component was not a safe path component (a separator, a dot-dot, a control byte).
      NameUnsafeComponent
    deriving stock (Eq, Show)

{- | Parse one component of a package name against the floor every ecosystem shares: non-empty,
ASCII, and safe to interpolate into an upstream URL. A parser that clears one and skips another
still reaches that URL, which is why the three travel together.
-}
parseNameComponent :: Text -> Either NameRefusal Text
parseNameComponent component
    | T.null component = Left NameEmpty
    | not (isAsciiNameComponent component) = Left NameNotAscii
    | not (isSafeComponent component) = Left NameUnsafeComponent
    | otherwise = Right component
