-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.WireSupportSpec (spec) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, picosecondsToDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Hedgehog (Gen, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package (
    InvalidEntry (invalidKey, invalidKind, invalidValue),
    InvalidEntryKind (InvalidDistTag, InvalidIndexFile, InvalidVersionManifest),
 )
import Ecluse.Core.Registry.WireSupport (
    NameRefusal (NameEmpty, NameNotAscii, NameUnsafeComponent),
    Projection (NameMismatch, Projected),
    checkNameAgreement,
    parseNameComponent,
    parsePublishTime,
    partitionLenientList,
 )
import Ecluse.Core.Text.Iso8601 (readIso8601Utc)
import Ecluse.Test.Package (scopedNpm, unscopedNpm)
import Ecluse.Test.Registry.WireSupport (partitionLenient)

{- | Direct tests for the cross-ecosystem wire-projection helpers the npm projection builds on.
"Ecluse.Core.Registry.Npm.ProjectSpec" covers the npm projection end to end.
-}
spec :: Spec
spec = do
    partitionLenientSpec
    partitionLenientListSpec
    parsePublishTimeSpec
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
        fst (partitionLenientList InvalidIndexFile decodeInt keyedFiles)
            `shouldBe` [("acme-1.0.tar.gz", 1), ("acme-1.1-py3-none-any.whl", 3)]

    it "drops the undecodable entry, recording its kind, key, and value" $ do
        let dropped = snd (partitionLenientList InvalidIndexFile decodeInt keyedFiles)
        map invalidKind dropped `shouldBe` [InvalidIndexFile]
        map invalidKey dropped `shouldBe` ["acme-1.0-py3-none-any.whl"]
        map invalidValue dropped `shouldBe` [String "nope"]

    it "lists dropped entries in input order, not key order" $
        map invalidKey (snd (partitionLenientList InvalidDistTag decodeInt outOfOrderDrops))
            `shouldBe` ["charlie", "alpha"]

    it "reads an empty list as no entries either way" $
        partitionLenientList InvalidDistTag decodeInt [] `shouldBe` ([] :: [(Text, Int)], [])

parsePublishTimeSpec :: Spec
parsePublishTimeSpec = describe "parsePublishTime" $ do
    modifyMaxSuccess (const 2000) $
        it "decodes every JSON value to the instant or the refusal of the library's UTCTime decoder" $
            hedgehog $ do
                value <- forAll genTimeValue
                let library = parseEither parseJSON value :: Either String UTCTime
                    scanned = case value of
                        String raw -> isJust (readIso8601Utc raw)
                        _ -> False
                cover 20 "a stamp the scan reads" scanned
                cover 10 "a stamp only the library parser reads" (not scanned && isRight library)
                cover 10 "a string neither reads" (isString value && isLeft library)
                cover 5 "a value that is not a string" (not (isString value))
                parseEither parsePublishTime value === library

    it "refuses a string that is no stamp with the library's message" $
        parseEither parsePublishTime (String "last tuesday")
            `shouldBe` (parseEither parseJSON (String "last tuesday") :: Either String UTCTime)

isString :: Value -> Bool
isString = \case
    String _ -> True
    _ -> False

-- A publish time as a registry may write it: in the usual layout, in another the library reads, or malformed.
genTimeValue :: Gen Value
genTimeValue =
    Gen.frequency
        [ (4, String <$> genUsual)
        , (3, String <$> (Gen.element wider <*> genUsual))
        , (2, String <$> Gen.choice [Gen.text (Range.linear 0 40) Gen.unicode, T.replace "T" "t" <$> genUsual, T.dropEnd 1 <$> genUsual])
        , (1, Gen.element [Null, Bool True, Number 1_600_000_000, Array mempty, Object mempty])
        ]
  where
    -- 'iso8601Show' writes the usual layout, with a fraction of 0 to 12 digits.
    genUsual = do
        day <- fromGregorian <$> Gen.integral (Range.linear 0 9999) <*> Gen.int (Range.linear 1 12) <*> Gen.int (Range.linear 1 31)
        picos <- Gen.choice [(* 1_000_000_000_000) <$> Gen.integral (Range.linear 0 86_399), Gen.integral (Range.linear 0 86_399_999_999_999_999)]
        pure (toText (iso8601Show (UTCTime day (picosecondsToDiffTime picos))))
    -- Respellings the library reads and the scan declines.
    wider = [T.replace "T" " ", (<> "+00:00") . T.dropEnd 1, (<> "-0330") . T.dropEnd 1, T.cons '+']

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
        checkNameAgreement (scopedNpm "one" "x") (scopedNpm "two" "x") (1 :: Int)
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
