-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The floor every ecosystem's projection of an untrusted registry document sits on: per-entry
lenient degradation, the shared name checks, and the upstream name-agreement test.

The three name checks travel together because skipping any one of them reaches an interpolated
upstream URL. An ecosystem's grammar layers its own rules on top and never replaces them.
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
    nameComponentWith,
    withinNameLimit,
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
import Ecluse.Core.Registry (ParseError (ParseError))
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
ASCII, and safe to interpolate into an upstream URL. The three travel together because one skip reaches that URL.
-}
parseNameComponent :: Text -> Either NameRefusal Text
parseNameComponent component
    | T.null component = Left NameEmpty
    | not (isAsciiNameComponent component) = Left NameNotAscii
    | not (isSafeComponent component) = Left NameUnsafeComponent
    | otherwise = Right component

{- | Clear the shared floor and then an ecosystem's own grammar. The noun names the component
in every refusal, so each ecosystem keeps its own wording ("npm name component").
-}
nameComponentWith :: Text -> (Text -> Bool) -> Text -> Either ParseError Text
nameComponentWith noun usable component = do
    onFloor <- first floorRefusal (parseNameComponent component)
    if usable onFloor
        then Right onFloor
        else Left unusable
  where
    floorRefusal :: NameRefusal -> ParseError
    floorRefusal = \case
        NameEmpty -> ParseError ("empty " <> noun)
        NameNotAscii -> ParseError ("non-ASCII " <> noun <> ": " <> show component)
        NameUnsafeComponent -> unusable

    unusable :: ParseError
    unusable = ParseError ("unusable " <> noun <> ": " <> show component)

{- | Refuse a name over an ecosystem's own cap, which the noun names in the refusal text.
'T.compareLength' stops at the cap without measuring the whole input.
-}
withinNameLimit :: Text -> Int -> Text -> Either ParseError ()
withinNameLimit noun limit raw
    | T.compareLength raw limit == GT = Left (ParseError overLong)
    | otherwise = Right ()
  where
    overLong :: Text
    overLong = noun <> " over " <> show limit <> " characters, starting " <> show (T.take 24 raw)
