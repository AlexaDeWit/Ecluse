-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | 'parseSemver' held to the @versions@ library's parser, on generated text and on the npm captures.
module Ecluse.Core.Version.SemverSpec (spec) where

import Data.Aeson (Value (String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isAscii, isDigit)
import Data.Text qualified as T
import Data.Versions (Chunk (..), Release (..), SemVer (..))
import Data.Versions qualified as V
import Hedgehog (Gen, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Version.Semver (SemverKey (..), parseSemver)
import Ecluse.Core.Version.Token (digitRuns, withinVersionLength)
import Ecluse.Test.Corpus (corpusPackages, cpPath)
import Ecluse.Test.Json (keysAt, objectAt)
import Ecluse.Test.Support (decodeJsonOrFail)

spec :: Spec
spec = describe "parseSemver" $ do
    generatedSpec
    grammarSpec
    captureSpec

generatedSpec :: Spec
generatedSpec = describe "properties" $
    modifyMaxSuccess (const 20000) $
        it "returns what the library's parser returns under the two bounds, for generated text" $
            hedgehog $ do
                raw <- forAll genVersionText
                let expected = libraryParse raw
                    accepted = isJust expected
                    libraryAccepts = isRight (V.semver raw)
                    prerelease = [chunk | Just (_, _, _, Just (Release chunks), _) <- [expected], chunk <- toList chunks]
                cover 25 "accepted" accepted
                cover 25 "refused" (not accepted)
                cover 10 "accepted with a prerelease" (not (null prerelease))
                cover 10 "accepted with build metadata" (isJust (expected >>= \(_, _, _, _, build) -> build))
                cover 5 "a numeric identifier" (any isNumeric prerelease)
                cover 5 "an alphanumeric identifier" (not (all isNumeric prerelease))
                cover 2 "a hyphen inside an identifier" (any hyphenated prerelease)
                cover 1 "accepted with a character outside ASCII" (accepted && not (T.all isAscii raw))
                cover 0.5 "accepted with an 18-digit run" (accepted && any (digitRunIs EQ) (digitRuns raw))
                cover 0.5 "refused by the digit-run bound alone" (libraryAccepts && withinVersionLength raw && not accepted)
                cover 0.5 "refused by the length bound alone" (libraryAccepts && not (withinVersionLength raw))
                scanned raw === expected

-- | Each rule of the library's grammar that a reader of the semver specification could miss.
grammarSpec :: Spec
grammarSpec = describe "the library's grammar" $ do
    it "builds every field of a version with a prerelease and build metadata" $
        scanned "1.2.3-rc.1+build.5"
            `shouldBe` Just (1, 2, 3, Just (Release (Alphanum "rc" :| [Numeric 1])), Just "build.5")

    for_ grammarRules $ \(rule, taken, refused) ->
        it rule $ do
            filter (isNothing . scanned) taken `shouldBe` []
            filter (isJust . scanned) refused `shouldBe` []
            disagreements (taken <> refused) `shouldBe` []

captureSpec :: Spec
captureSpec = describe "on the npm captures" $
    for_ corpusPackages $ \package ->
        it ("returns what the library's parser returns for every version and dist-tag of " <> cpPath package) $ do
            document <- readFileBS (cpPath package) >>= decodeJsonOrFail
            let versions = keysAt "versions" document
                tags = [target | String target <- KeyMap.elems (objectAt "dist-tags" document)]
            any (isJust . scanned) versions `shouldBe` True
            tags `shouldNotBe` []
            disagreements (versions <> tags) `shouldBe` []

-- | Every field of a parsed version. The library's own equality leaves the build metadata out.
type Fields = (Word, Word, Word, Maybe Release, Maybe Text)

fields :: SemVer -> Fields
fields (SemVer major minor patch preRel build) = (major, minor, patch, preRel, build)

scanned :: Text -> Maybe Fields
scanned raw = (\(SemverKey parsed) -> fields parsed) <$> parseSemver raw

-- | The reference for 'parseSemver': the library's parser, under the length and 18-digit bounds.
libraryParse :: Text -> Maybe Fields
libraryParse raw = do
    guard (withinVersionLength raw)
    guard (not (any (digitRunIs GT) (digitRuns raw)))
    fields <$> rightToMaybe (V.semver raw)

-- | The first texts the two disagree on. A capture holds thousands, and a failure prints them.
disagreements :: [Text] -> [Text]
disagreements = take 5 . filter (\raw -> scanned raw /= libraryParse raw)

-- | Whether a run is all digits, and how its length compares with the 18 digits the bound allows.
digitRunIs :: Ordering -> Text -> Bool
digitRunIs comparison run = T.all isDigit run && T.compareLength run 18 == comparison

isNumeric :: Chunk -> Bool
isNumeric = \case
    Numeric _ -> True
    Alphanum _ -> False

hyphenated :: Chunk -> Bool
hyphenated = \case
    Numeric _ -> False
    Alphanum text -> T.any (== '-') text

-- | A rule's name, texts the library takes under it, and texts the library refuses under it.
grammarRules :: [(String, [Text], [Text])]
grammarRules =
    [
        ( "takes three numbers and nothing around them: no prefix, no space, no fourth number"
        , ["0.0.0", "1.2.3", "10.20.30"]
        , ["", "1", "1.2", "1.2.3.4", "1..3", ".1.2", "v1.2.3", "=1.2.3", " 1.2.3", "1.2.3 ", "1.2.3\n", "1.2.3x"]
        )
    ,
        ( "refuses a leading zero in a number"
        , ["0.0.0", "100.0.0"]
        , ["01.2.3", "1.02.3", "1.2.03", "00.0.0"]
        )
    ,
        ( "refuses a leading zero in a numeric identifier, and takes one beside a letter or a hyphen as text"
        , ["1.2.3-0", "1.2.3-10", "1.2.3-01a", "1.2.3-0-1", "1.2.3-00-"]
        , ["1.2.3-01", "1.2.3-00", "1.2.3-a.01"]
        )
    ,
        ( "takes a hyphen as an identifier character, so only the first one opens the prerelease"
        , ["1.2.3--", "1.2.3-a-b", "1.2.3---1", "1.2.3-x+-", "1.2.3+a-b"]
        , ["1-2.3", "1.2-3"]
        )
    ,
        ( "refuses an empty identifier"
        , []
        , ["1.2.3-", "1.2.3-a.", "1.2.3-.a", "1.2.3-a..b", "1.2.3+", "1.2.3+a.", "1.2.3+.a", "1.2.3+a..b", "1.2.3-+a", "1.2.3-a+"]
        )
    ,
        ( "reads build metadata after the prerelease, one of each, and holds its digits to no number rule"
        , ["1.2.3-a+b", "1.2.3+a-b.c", "1.2.3-a.b+c.d", "1.2.3+01"]
        , ["1.2.3+a+b", "1.2.3-a+b+c", "1.2.3-a_b", "1.2.3+a_b"]
        )
    ,
        ( "takes a letter or a numeral outside ASCII in an identifier, and reads no such numeral as a number"
        , ["1.2.3-\xE9", "1.2.3+\xDF", "1.2.3-a\x663", "1.2.3-\xB2-", "1.2.3+\x663"]
        , ["1.2.3-\x663", "1.2.3-1\xB2", "1.2.\x663", "\xFF11.\xFF12.\xFF13"]
        )
    ,
        ( "reads a number of 18 digits, and refuses a longer digit run wherever it stands"
        , ["999999999999999999.0.0", "1.0.0-999999999999999999", "1.0.0-a999999999999999999", "1.0.0+999999999999999999"]
        , ["1000000000000000000.0.0", "1.0.0-1000000000000000000", "1.0.0-a1000000000000000000", "1.0.0+1000000000000000000"]
        )
    ]

genVersionText :: Gen Text
genVersionText =
    Gen.frequency
        [ (5, genWellFormed)
        , (5, genMalformed)
        , (2, genWellFormed >>= wrapped)
        , (3, Gen.choice [genWellFormed, genMalformed] >>= edited)
        , (2, Gen.text (Range.linear 0 24) genVersionChar)
        , (1, Gen.text (Range.linear 0 12) Gen.unicode)
        , (2, genNearDigitBound)
        , (1, genNearLengthBound)
        ]

-- | A version from the given pieces: a dotted core, then a prerelease, build metadata, both or neither.
genAssembled :: Gen Int -> Gen Text -> Gen Text -> Gen Text
genAssembled coreSize number identifier = do
    size <- coreSize
    core <- dotted (Range.singleton size) number
    preRel <- Gen.choice [pure "", ("-" <>) <$> dotted (Range.linear 1 3) identifier]
    build <- Gen.choice [pure "", ("+" <>) <$> dotted (Range.linear 1 3) identifier]
    pure (core <> preRel <> build)
  where
    dotted size piece = T.intercalate "." <$> Gen.list size piece

genWellFormed :: Gen Text
genWellFormed = genAssembled (pure 3) genNumber genIdentifier

-- | A version where any number or identifier may be malformed, and the core may not hold three numbers.
genMalformed :: Gen Text
genMalformed =
    genAssembled
        (Gen.frequency [(6, pure 3), (1, Gen.int (Range.linear 1 5))])
        (Gen.frequency [(8, genNumber), (1, genBadDigits)])
        (Gen.frequency [(6, genIdentifier), (1, genBadDigits)])

-- | A well-formed version whose numbers, identifiers and build metadata may hold 17 to 19 digits.
genNearDigitBound :: Gen Text
genNearDigitBound =
    genAssembled
        (pure 3)
        (Gen.frequency [(3, genNumber), (1, genLongRun)])
        (Gen.frequency [(2, genIdentifier), (1, genLongRun), (1, (<>) <$> genAlphanumeric <*> genLongRun)])
  where
    genLongRun = T.cons <$> Gen.element ['1' .. '9'] <*> Gen.text (Range.constant 16 18) Gen.digit

-- | A version the library takes, from four characters under the length bound to four over it.
genNearLengthBound :: Gen Text
genNearLengthBound = do
    lead <- Gen.element ["1.0.0-", "1.0.0+", "1.0.0-a.", "1.0.0-0+"]
    total <- Gen.int (Range.constant 1020 1028)
    pure (lead <> T.replicate (total - T.length lead) "a")

genNumber :: Gen Text
genNumber = show <$> Gen.word (Range.exponential 0 99999)

genIdentifier :: Gen Text
genIdentifier = Gen.choice [genNumber, genAlphanumeric]

-- | Identifier characters around one letter or hyphen, so digits can lead it and hyphens can fill it.
genAlphanumeric :: Gen Text
genAlphanumeric = do
    front <- Gen.text (Range.linear 0 3) genIdentifierChar
    marker <- Gen.frequency [(3, Gen.alpha), (2, pure '-'), (1, Gen.element lettersOutsideAscii)]
    back <- Gen.text (Range.linear 0 3) genIdentifierChar
    pure (front <> T.singleton marker <> back)
  where
    genIdentifierChar = Gen.frequency [(6, Gen.alphaNum), (2, pure '-'), (1, Gen.element (lettersOutsideAscii <> numeralsOutsideAscii))]

-- | Digits that spell no semver number: none, a leading zero, or a numeral outside ASCII.
genBadDigits :: Gen Text
genBadDigits =
    Gen.choice
        [ pure ""
        , ("0" <>) <$> Gen.text (Range.linear 1 3) Gen.digit
        , Gen.text (Range.linear 1 2) (Gen.element numeralsOutsideAscii)
        , (<>) <$> genNumber <*> (T.singleton <$> Gen.element numeralsOutsideAscii)
        ]

-- | The version behind a prefix, inside whitespace, or with text after it.
wrapped :: Text -> Gen Text
wrapped version = do
    prefix <- Gen.element ["", "v", "V", "=", " ", "\t", "\n", "\xA0", "x", "1", "."]
    suffix <- Gen.element ["", " ", "\t", "\n", "\r\n", "\x3000", "x", ".", "-", "+", ".0"]
    pure (prefix <> version <> suffix)

-- | The version with one character deleted, inserted or replaced, or cut short.
edited :: Text -> Gen Text
edited version = do
    at <- Gen.int (Range.linear 0 (T.length version))
    c <- genVersionChar
    let (front, back) = T.splitAt at version
    Gen.element [front <> T.drop 1 back, front <> T.cons c back, front <> T.cons c (T.drop 1 back), front]

-- | The characters versions are written in, with the separators nearly as likely as the digits.
genVersionChar :: Gen Char
genVersionChar =
    Gen.frequency
        [ (6, Gen.digit)
        , (5, Gen.element ['.', '-', '+'])
        , (3, Gen.alpha)
        , (1, Gen.element [' ', '\t', 'v', '_'])
        , (1, Gen.element (lettersOutsideAscii <> numeralsOutsideAscii))
        ]

-- | A Latin, a German and a Cyrillic letter, which the library's Unicode-aware classes take.
lettersOutsideAscii :: [Char]
lettersOutsideAscii = ['\xE9', '\xDF', '\x416']

-- | An Arabic-Indic digit, a superscript, a Roman numeral and a fullwidth digit: numerals, not letters.
numeralsOutsideAscii :: [Char]
numeralsOutsideAscii = ['\x663', '\xB2', '\x2167', '\xFF11']
