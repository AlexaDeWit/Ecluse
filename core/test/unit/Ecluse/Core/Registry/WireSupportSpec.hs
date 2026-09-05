-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.WireSupportSpec (spec) where

import Data.Aeson (Value (Number, String), parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    InvalidEntry (invalidKey, invalidKind, invalidValue),
    InvalidEntryKind (InvalidDistTag, InvalidVersionManifest),
    PackageName,
    mkPackageName,
    mkScope,
 )
import Ecluse.Core.Registry.WireSupport (
    NameRefusal (NameEmpty, NameNotAscii, NameUnsafeComponent),
    Projection (NameMismatch, Projected),
    checkNameAgreement,
    parseNameComponent,
    partitionLenient,
    partitionLenientList,
 )
import Ecluse.Test.Package (unscopedNpm)

{- | Direct tests for the cross-ecosystem wire-projection helpers the npm projection builds on.
"Ecluse.Core.Registry.Npm.ProjectSpec" covers the npm projection end to end.
-}
spec :: Spec
spec = do
    partitionLenientSpec
    partitionLenientListSpec
    checkNameAgreementSpec
    parseNameComponentSpec

partitionLenientSpec :: Spec
partitionLenientSpec = describe "partitionLenient" $ do
    it "keeps the entries that decode" $
        fst (partitionLenient InvalidVersionManifest decodeInt mixed)
            `shouldBe` Map.fromList [("1.0.0", 1), ("3.0.0", 3)]

    it "drops the undecodable entry, recording its kind, key, and raw value" $ do
        let dropped = snd (partitionLenient InvalidVersionManifest decodeInt mixed)
        map invalidKind dropped `shouldBe` [InvalidVersionManifest]
        map invalidKey dropped `shouldBe` ["2.0.0"]
        map invalidValue dropped `shouldBe` [String "nope"]

    it "lists dropped entries in ascending key order, deterministically" $
        -- "bravo" decodes. "alpha" and "charlie" do not, and must surface in that order.
        map invalidKey (snd (partitionLenient InvalidDistTag decodeInt manyBad))
            `shouldBe` ["alpha", "charlie"]

{- | The list form, driven over a PEP 691 @files@ array rather than an npm map. The caller
pairs each element with its own key, which for a file index is its @filename@.
-}
partitionLenientListSpec :: Spec
partitionLenientListSpec = describe "partitionLenientList" $ do
    it "keeps the entries that decode, in input order" $
        fst (partitionLenientList InvalidVersionManifest decodeInt keyedFiles)
            `shouldBe` [("acme-1.0.tar.gz", 1), ("acme-1.1-py3-none-any.whl", 3)]

    it "drops the undecodable entry, recording its kind, key, and value" $ do
        let dropped = snd (partitionLenientList InvalidVersionManifest decodeInt keyedFiles)
        map invalidKind dropped `shouldBe` [InvalidVersionManifest]
        map invalidKey dropped `shouldBe` ["acme-1.0-py3-none-any.whl"]
        map invalidValue dropped `shouldBe` [String "nope"]

    it "lists dropped entries in input order, not key order" $
        map invalidKey (snd (partitionLenientList InvalidDistTag decodeInt outOfOrderDrops))
            `shouldBe` ["charlie", "alpha"]

    it "reads an empty list as no entries either way" $
        partitionLenientList InvalidDistTag decodeInt [] `shouldBe` ([] :: [(Text, Int)], [])

checkNameAgreementSpec :: Spec
checkNameAgreementSpec = describe "checkNameAgreement" $ do
    it "carries the projected payload through when the reported name matches the request" $
        checkNameAgreement (unscopedNpm "left-pad") (unscopedNpm "left-pad") (1 :: Int)
            `shouldBe` Projected 1

    it "disagrees when the reported bare name differs, carrying the reported name" $
        checkNameAgreement (unscopedNpm "left-pad") (unscopedNpm "evil-pad") (1 :: Int)
            `shouldBe` NameMismatch "evil-pad"

    it "disagrees on a differing scope even when the bare name matches" $
        -- Ecosystem-aware equality compares the whole name, scope included, so the same
        -- bare name under a different scope is the anti-shadowing disagreement.
        checkNameAgreement (scoped "one" "x") (scoped "two" "x") (1 :: Int)
            `shouldBe` NameMismatch "@two/x"

{- | The floor every ecosystem's name grammar sits on. Each ecosystem adds its own rules on
top, so only the three shared refusals are pinned here.
-}
parseNameComponentSpec :: Spec
parseNameComponentSpec = describe "parseNameComponent" $ do
    it "admits an ordinary component unchanged" $
        parseNameComponent "left-pad" `shouldBe` Right "left-pad"

    it "refuses an empty component" $
        parseNameComponent "" `shouldBe` Left NameEmpty

    it "refuses a non-ASCII component" $
        parseNameComponent "caf\233" `shouldBe` Left NameNotAscii

    it "refuses an ASCII control character" $
        parseNameComponent "left\tpad" `shouldBe` Left NameNotAscii

    it "refuses a path separator, which would reach an upstream URL as structure" $
        parseNameComponent "a/b" `shouldBe` Left NameUnsafeComponent

    it "refuses a backslash separator" $
        parseNameComponent "a\\b" `shouldBe` Left NameUnsafeComponent

    it "refuses the traversal components" $ do
        parseNameComponent "." `shouldBe` Left NameUnsafeComponent
        parseNameComponent ".." `shouldBe` Left NameUnsafeComponent

-- | Decode a JSON value as an 'Int', the per-entry decode the partition drives.
decodeInt :: Value -> Either String Int
decodeInt = parseEither parseJSON

-- | A raw entry map with a healthy pair and one undecodable (string) entry between them.
mixed :: Map Text Value
mixed =
    Map.fromList
        [ ("1.0.0", Number 1)
        , ("2.0.0", String "nope")
        , ("3.0.0", Number 3)
        ]

{- | A PEP 691 file list, each element already paired with its @filename@ key, with one
element the per-entry decode rejects.
-}
keyedFiles :: [(Text, Value)]
keyedFiles =
    [ ("acme-1.0.tar.gz", Number 1)
    , ("acme-1.0-py3-none-any.whl", String "nope")
    , ("acme-1.1-py3-none-any.whl", Number 3)
    ]

-- | Two undecodable entries whose keys descend, so input order and key order disagree.
outOfOrderDrops :: [(Text, Value)]
outOfOrderDrops = [("charlie", String "x"), ("bravo", Number 2), ("alpha", String "y")]

-- | A raw entry map with two undecodable entries out of key order, to pin the drop order.
manyBad :: Map Text Value
manyBad =
    Map.fromList
        [ ("charlie", String "x")
        , ("alpha", String "y")
        , ("bravo", Number 2)
        ]

-- | A scoped npm 'PackageName' @\@scope\/base@.
scoped :: Text -> Text -> PackageName
scoped scope = mkPackageName Npm (Just (mkScope scope))
