-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Native Cargo SemVer with u64 release numbers and ASCII identifiers.
Numeric prerelease identifiers compare by length and text, without a machine-integer limit.
-}
module Ecluse.Core.Version.Cargo (
    CargoKey,
    parseCargo,
    renderCargo,
    isCargoStable,
    cargoUniquenessKey,
) where

import Data.Char (isDigit)
import Data.Text qualified as T

import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Text (isAsciiAlphaNum, readDecimalText)
import Ecluse.Core.Version.Token (withinVersionLength)

-- | A native ordering key. Equality and precedence exclude build metadata.
data CargoKey = CargoKey Word64 Word64 Word64 [Identifier] (Maybe Text)
    deriving stock (Show)

data Identifier = Numeric Text | Alphanumeric Text
    deriving stock (Eq, Show)

instance Eq CargoKey where
    a == b = compare a b == EQ

instance Ord CargoKey where
    compare (CargoKey a b c pre _) (CargoKey x y z other _) =
        compare (a, b, c) (x, y, z) <> comparePrerelease pre other

instance Ord Identifier where
    compare (Numeric a) (Numeric b) = compare (T.length a) (T.length b) <> compare a b
    compare (Alphanumeric a) (Alphanumeric b) = compare a b
    compare Numeric{} Alphanumeric{} = LT
    compare Alphanumeric{} Numeric{} = GT

-- | Parse three release numbers and native suffixes within the shared version-text bound.
parseCargo :: Text -> Maybe CargoKey
parseCargo raw = do
    guard (withinVersionLength raw)
    let (version, buildPart) = T.breakOn "+" raw
        (release, prePart) = T.breakOn "-" version
    (major, minor, patch) <- case T.splitOn "." release of
        [a, b, c] -> (,,) <$> releaseNumber a <*> releaseNumber b <*> releaseNumber c
        _ -> Nothing
    prerelease <- if T.null prePart then pure [] else traverse identifier (T.splitOn "." (T.drop 1 prePart))
    build <- if T.null buildPart then pure Nothing else Just <$> buildIdentifiers (T.drop 1 buildPart)
    pure (CargoKey major minor patch (strictElements prerelease) build)

releaseNumber :: Text -> Maybe Word64
releaseNumber raw = do
    guard (validNumber raw && T.compareLength raw 20 /= GT)
    number <- readDecimalText raw :: Maybe Integer
    guard (number <= toInteger (maxBound :: Word64))
    pure (fromInteger number)

validNumber :: Text -> Bool
validNumber raw =
    not (T.null raw)
        && T.all isDigit raw
        && (T.length raw == 1 || not (T.isPrefixOf "0" raw))

identifier :: Text -> Maybe Identifier
identifier raw = do
    guard (validIdentifier raw)
    if T.all isDigit raw
        then Numeric (T.copy raw) <$ guard (validNumber raw)
        else pure (Alphanumeric (T.copy raw))

buildIdentifiers :: Text -> Maybe Text
buildIdentifiers raw = T.copy raw <$ guard (all validIdentifier (T.splitOn "." raw))

validIdentifier :: Text -> Bool
validIdentifier raw = not (T.null raw) && T.all (\c -> isAsciiAlphaNum c || c == '-') raw

comparePrerelease :: [Identifier] -> [Identifier] -> Ordering
comparePrerelease [] [] = EQ
comparePrerelease [] _ = GT
comparePrerelease _ [] = LT
comparePrerelease a b = compare a b

-- | Render native spelling, including build metadata excluded from ordering.
renderCargo :: CargoKey -> Text
renderCargo key@(CargoKey _ _ _ _ build) = cargoUniquenessKey key <> maybe "" ("+" <>) build

-- | Whether the version has no prerelease suffix.
isCargoStable :: CargoKey -> Bool
isCargoStable (CargoKey _ _ _ pre _) = null pre

-- | The native uniqueness spelling, excluding build metadata and retaining prerelease identity.
cargoUniquenessKey :: CargoKey -> Text
cargoUniquenessKey (CargoKey major minor patch pre _) =
    T.intercalate "." (map show [major, minor, patch])
        <> if null pre then "" else "-" <> T.intercalate "." (map renderIdentifier pre)

renderIdentifier :: Identifier -> Text
renderIdentifier = \case
    Numeric raw -> raw
    Alphanumeric raw -> raw
