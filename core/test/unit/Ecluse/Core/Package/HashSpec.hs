-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Package.HashSpec (spec) where

import Prelude hiding (universe)

import Crypto.Number.Serialize (i2ospOf)
import Data.ByteString qualified as BS
import Data.Char (toLower, toUpper)
import Data.JsonStream.Parser qualified as J
import Data.Text qualified as T
import Data.Universe.Class (universe)
import Hedgehog (Gen, LabelName, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package.Hash (
    Hash,
    HashAlg (..),
    base64DigestText,
    canonicalHashValue,
    hashAlg,
    hashValue,
    hexDigestText,
    mkHash,
    mkSriHashes,
    parseHashAlg,
    renderHashAlg,
    sriAlgorithm,
 )

import Ecluse.Test.Corpus (CorpusPackage (cpPath), captureTexts, corpusPackages, pypiCorpusPackages)
import Ecluse.Test.Package qualified as Package
import Ecluse.Test.Registry.JsonStream (sameTexts)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "mkHash" $ do
        it "accepts a well-formed 40-hex SHA-1 shasum" $
            (hashAlg <$> mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709") `shouldBe` Right SHA1

        it "accepts a well-formed sha512 SRI integrity" $
            (hashAlg <$> mkHash SRI "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==")
                `shouldBe` Right SRI

        it "rejects a multi-component integrity (one Hash holds exactly one component)" $
            mkHash
                SRI
                "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg== sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
                `shouldSatisfy` isLeft

        it "rejects an SRI component padded with surrounding whitespace" $
            -- The first-dash accessors read the stored value verbatim, so a padded
            -- component would corrupt the resolved algorithm and body.
            mkHash SRI " sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU= " `shouldSatisfy` isLeft

        it "accepts a well-formed sha384 SRI (a modelled algorithm)" $
            mkHash SRI "sha384-OLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"
                `shouldSatisfy` isRight

        it "accepts a well-formed 96-hex SHA-384 digest" $
            (hashAlg <$> mkHash SHA384 "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b")
                `shouldBe` Right SHA384

        it "rejects a 94-character (wrong-length) hex SHA-384" $
            mkHash SHA384 "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b" `shouldSatisfy` isLeft

        it "rejects a non-hex SHA-384" $
            mkHash SHA384 "zzb060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b" `shouldSatisfy` isLeft

        it "preserves the original (upper-case) hex value while validating case-insensitively" $
            (hashValue <$> mkHash SHA1 "DA39A3EE5E6B4B0D3255BFEF95601890AFD80709")
                `shouldBe` Right "DA39A3EE5E6B4B0D3255BFEF95601890AFD80709"

        it "rejects an empty digest" $
            mkHash SHA1 "" `shouldSatisfy` isLeft

        it "rejects a 39-character (odd-length) hex SHA-1" $
            mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd8070" `shouldSatisfy` isLeft

        it "rejects an over-long (21-byte) hex SHA-1" $
            mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709aa" `shouldSatisfy` isLeft

        it "rejects a non-hex SHA-1" $
            mkHash SHA1 "zz39a3ee5e6b4b0d3255bfef95601890afd80709" `shouldSatisfy` isLeft

        it "rejects a truncated SRI (alg prefix, no body)" $
            mkHash SRI "sha512-" `shouldSatisfy` isLeft

        it "rejects an SRI with a non-base64 body" $
            mkHash SRI "sha512-not base64!!" `shouldSatisfy` isLeft

        it "rejects an SRI whose base64 body is the wrong length for its algorithm" $
            -- Valid base64, but decodes to 6 bytes, not sha256's 32.
            mkHash SRI "sha256-Zm9vYmFy" `shouldSatisfy` isLeft

        it "rejects an SRI naming an algorithm outside the Subresource-Integrity set" $
            -- The body is the well-formed SHA-1 of no bytes, so only the prefix is refused.
            mkHash SRI "sha1-2jmj7l5rSw0yVb/vlWAYkK/YBwk=" `shouldSatisfy` isLeft

        it "never yields a Hash from non-digest text, for any algorithm" $
            hedgehog $ do
                alg <- forAll (Gen.element universe)
                junk <- forAll (Gen.text (Range.linear 0 80) (Gen.element ("!@#$%& *()" :: String)))
                isLeft (mkHash alg junk) === True

    decoderSpec

    describe "canonicalHashValue" $ do
        it "compares SHA-1 hex case without changing its original spelling" $ do
            let hex = Package.hexSha1Of "same bytes"
            forM_ [hex, T.toUpper hex] $ \wire -> do
                let parsed = mkHash SHA1 wire
                (canonicalHashValue <$> parsed) `shouldBe` Right (Just hex)
                (hashValue <$> parsed) `shouldBe` Right wire

        forM_
            [ (SHA256, Package.hexSha256Of, Package.sriSha256Of)
            , (SHA384, Package.hexSha384Of, Package.sriSha384Of)
            , (SHA512, Package.hexSha512Of, Package.sriSha512Of)
            ]
            $ \(alg, hexOf, sriOf) -> do
                it ("compares hex case and SRI by bytes for " <> show alg) $ do
                    let hex = hexOf "same bytes"
                        sri = sriOf "same bytes"
                    forM_ [(alg, hex), (alg, T.toUpper hex), (SRI, sri)] $ \(wireAlg, value) -> do
                        let parsed = mkHash wireAlg value
                        (canonicalHashValue <$> parsed) `shouldBe` Right (Just hex)
                        (hashValue <$> parsed) `shouldBe` Right value

                it ("is representation-invariant for arbitrary " <> show alg <> " digests") $
                    hedgehog $ do
                        bytes <- forAll (Gen.bytes (Range.linear 0 200))
                        let hex = hexOf bytes
                        forM_ [(alg, hex), (alg, T.toUpper hex), (SRI, sriOf bytes)] $ \(wireAlg, value) ->
                            (canonicalHashValue <$> mkHash wireAlg value) === Right (Just hex)

        it "rejects invalid record updates without changing their raw spelling" $ do
            let original = Package.unsafeHash SRI (Package.sriSha256Of "bytes")
            forM_ ["sha3-AAAA", "sha256-", "sha256-not-base64", " sha256-AAAA "] $ \value -> do
                let changed = original{hashValue = value}
                canonicalHashValue changed `shouldBe` Nothing
                hashValue changed `shouldBe` value

    describe "mkSriHashes" $ do
        it "splits a multi-component wire string into one Hash per component" $
            (fmap hashValue <$> mkSriHashes "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg== sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=")
                `shouldBe` Right
                    ( "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="
                        :| ["sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="]
                    )

        it "yields a singleton for the common single-component wire string" $
            (fmap hashValue <$> mkSriHashes "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==")
                `shouldBe` Right ("sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==" :| [])

        it "keeps a lone component as the input text itself, and a padded one as its own text" $ do
            let wire = Package.validSha512Sri
            lone :| _ <- expectRight (mkSriHashes wire)
            padded :| _ <- expectRight (mkSriHashes (" " <> wire))
            sameTexts [(hashValue lone, wire)] `shouldReturn` True
            sameTexts [(hashValue padded, wire)] `shouldReturn` False
            hashValue padded `shouldBe` wire

        it "rejects the whole wire string when any component is malformed" $
            -- All-or-nothing: a partly valid attacker-shaped value never yields a
            -- partial digest set for the gates to reason over.
            mkSriHashes "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg== sha256-short"
                `shouldSatisfy` isLeft

        it "rejects an empty or all-whitespace wire string" $ do
            mkSriHashes "" `shouldSatisfy` isLeft
            mkSriHashes "   " `shouldSatisfy` isLeft

    describe "algorithm vocabulary" $ do
        it "round-trips sha384 through render/parse" $ do
            renderHashAlg SHA384 `shouldBe` "sha384"
            parseHashAlg "sha384" `shouldBe` Right SHA384
            parseHashAlg "SHA-384" `shouldBe` Right SHA384
            parseHashAlg (renderHashAlg SHA384) `shouldBe` Right SHA384

        it "accepts canonical names and single-dash aliases, case- and whitespace-insensitively" $ do
            parseHashAlg "sha256" `shouldBe` Right SHA256
            parseHashAlg "SHA-256" `shouldBe` Right SHA256
            parseHashAlg "  Sha512  " `shouldBe` Right SHA512
            parseHashAlg "blake2b" `shouldBe` Right Blake2b
        it "rejects arbitrary internal dashes rather than masking a typo" $ do
            parseHashAlg "s-h-a--2-5-6" `shouldSatisfy` isLeft
            parseHashAlg "sha--256" `shouldSatisfy` isLeft
            parseHashAlg "sha-2-56" `shouldSatisfy` isLeft
        it "rejects the sri wrapper, which names no algorithm of its own" $
            parseHashAlg "sri" `shouldSatisfy` isLeft

        it "resolves a sha384 SRI prefix to SHA384 (was Nothing before it was modelled)" $
            sriAlgorithm "sha384-OLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"
                `shouldBe` Just SHA384

decoderSpec :: Spec
decoderSpec = describe "mkHash agrees with the digest decoder" $ do
    it "over a spelling of every algorithm" $
        sortNub (map spellingAlg spellings) `shouldBe` sortNub universe

    modifyMaxSuccess (const 3000) $
        for_ spellings $ \spelling ->
            it ("over " <> spellingName spelling <> " digests and their malformed neighbours") $
                hedgehog $ do
                    let alg = spellingAlg spelling
                        cases = casesFor spelling
                    wire <- forAll (genWire spelling)
                    (label, verdict, value) <- forAll (Gen.choice [(,,) name fixed <$> gen wire | Case name fixed gen <- cases])
                    for_ cases $ \(Case name _ _) -> cover 1 name (name == label)
                    for_ verdict (isRight (viaDecoder alg value) ===)
                    mkHash alg value === viaDecoder alg value

    it "with any ASCII character at any position of a well-formed digest" $
        firstDifferences
            [ (spellingAlg spelling, T.take at wire <> one c <> T.drop (at + 1) wire)
            | spelling <- spellings
            , let wire = render (plainWire spelling)
            , at <- [0 .. T.length wire - 1]
            , c <- ['\0' .. '\x7f']
            ]
            `shouldBe` []

    -- The decoder lowercases hex first, so a character also takes the place of as many digits as it lowers to.
    it "with any character at all in the place of hex digits" $
        firstDifferences
            [ (MD5, T.cons c (T.drop width Package.validMd5))
            | c <- [minBound .. maxBound]
            , width <- [1 .. T.length (T.toLower (one c))]
            ]
            `shouldBe` []

    for_ corpusPackages (agreesOnCapture 2 npmDigests)
    for_ pypiCorpusPackages (agreesOnCapture 1 pypiDigests)

-- 'mkHash' with the decoder as its test: 'canonicalHashValue' is 'Just' exactly when 'decodeHash' yields bytes.
viaDecoder :: HashAlg -> Text -> Either Text Hash
viaDecoder alg value
    | isJust (canonicalHashValue candidate) = Right candidate
    | otherwise = Left ("malformed " <> renderHashAlg alg <> " digest")
  where
    candidate = (Package.unsafeHash SHA1 Package.validSha1){hashAlg = alg, hashValue = value}

-- The first few inputs 'mkHash' and the decoder judge differently. A capture can hold thousands, and one names the fault.
firstDifferences :: [(HashAlg, Text)] -> [(HashAlg, Text)]
firstDifferences = take 5 . filter (\(alg, value) -> mkHash alg value /= viaDecoder alg value)

-- Each digest is read as every algorithm, whole and as the components 'mkSriHashes' splits it into.
agreesOnCapture :: Int -> J.Parser [Text] -> CorpusPackage -> Spec
agreesOnCapture fewest digestsOf package =
    it ("over every digest of the capture " <> cpPath package) $ do
        entries <- captureTexts fewest digestsOf package
        firstDifferences [(alg, reading) | digest <- concat entries, reading <- ordNub (digest : words digest), alg <- universe] `shouldBe` []

-- One list for each release of an npm packument (@dist.shasum@, @dist.integrity@) and for each file of a
-- PEP 691 index (every @hashes@ value). An entry with no digest gives the empty list.
npmDigests, pypiDigests :: J.Parser [Text]
npmDigests = "versions" J..: J.objectValues (many ("dist" J..: (("shasum" J..: J.string) <> ("integrity" J..: J.string))))
pypiDigests = "files" J..: J.arrayOf (many ("hashes" J..: J.objectValues J.string))

-- One spelling of a digest: the algorithm 'mkHash' is given, the SRI prefix if it has one, and the digest's length in bytes.
data Spelling = Spelling {spellingAlg :: HashAlg, spellingPrefix :: Maybe Text, spellingBytes :: Int}

spellings :: [Spelling]
spellings =
    [Spelling alg Nothing size | (alg, size) <- [(MD5, 16), (SHA1, 20), (SHA256, 32), (SHA384, 48), (SHA512, 64), (Blake2b, 64)]]
        <> sriSpellings

sriSpellings :: [Spelling]
sriSpellings = [Spelling SRI (Just prefix) size | (prefix, size) <- [("sha256-", 32), ("sha384-", 48), ("sha512-", 64)]]

spellingName :: Spelling -> String
spellingName spelling = show (spellingAlg spelling) <> maybe " hex" ((" " <>) . toString) (spellingPrefix spelling)

-- Every character the decoder takes as a digit, base64 in the order of the values it encodes.
digitsOf :: Spelling -> [Char]
digitsOf spelling
    | isJust (spellingPrefix spelling) = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "+/"
    | otherwise = ['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F']

-- A well-formed digest in parts: the SRI prefix (empty for hex), the digits, and the base64 padding.
data Wire = Wire {wirePrefix :: Text, wireDigits :: Text, wirePadding :: Text}
    deriving stock (Show)

render :: Wire -> Text
render wire = wirePrefix wire <> wireDigits wire <> wirePadding wire

encodeAs :: Spelling -> ByteString -> Wire
encodeAs spelling bytes = case spellingPrefix spelling of
    Nothing -> Wire "" (hexDigestText bytes) ""
    Just prefix -> uncurry (Wire prefix) (T.span (/= '=') (base64DigestText bytes))

genWire :: Spelling -> Gen Wire
genWire spelling = encodeAs spelling <$> genBytes (spellingBytes spelling)

-- A digest of all-zero bytes, for a reader that needs one well-formed digest of the spelling.
plainWire :: Spelling -> Wire
plainWire spelling = encodeAs spelling (BS.replicate (spellingBytes spelling) 0)

-- One draw for all the bytes, where 'Gen.bytes' draws each on its own. 'i2ospOf' refuses a length of zero.
genBytes :: Int -> Gen ByteString
genBytes size = fromMaybe BS.empty . i2ospOf size <$> Gen.integral (Range.constant 0 (256 ^ size - 1))

-- A class of input: its coverage label, the decoder's verdict where the class fixes one, and its generator.
data Case = Case LabelName (Maybe Bool) (Wire -> Gen Text)

accepted, refused, undecided :: LabelName -> (Wire -> Gen Text) -> Case
accepted name = Case name (Just True)
refused name = Case name (Just False)
undecided name = Case name Nothing

casesFor :: Spelling -> [Case]
casesFor spelling = commonCases spelling <> maybe (hexCases spelling) (sriCases spelling) (spellingPrefix spelling)

commonCases :: Spelling -> [Case]
commonCases spelling =
    [ accepted "a well-formed digest" (pure . render)
    , accepted "upper-case digits" (editDigits (pure . T.toUpper))
    , accepted "lower-case digits" (editDigits (pure . T.toLower))
    , accepted "mixed-case digits" (editDigits (fmap toText . traverse (\c -> Gen.element [toUpper c, toLower c]) . toString))
    , refused "one digit short" (editDigits (dropDigits 1))
    , refused "two digits short" (editDigits (dropDigits 2))
    , refused "one digit long" (editDigits (addDigits spelling 1))
    , refused "two digits long" (editDigits (addDigits spelling 2))
    , refused "a well-formed digest of another length" (const (render <$> genOtherLength))
    , refused "a digest in the other encoding" (const (render <$> (Gen.element otherEncoding >>= genWire)))
    , refused "an ASCII character outside the alphabet" (editDigits (replaceDigit (Gen.element outside)))
    , refused "a URL-safe hyphen" (editDigits (replaceDigit (pure '-')))
    , refused "a URL-safe underscore" (editDigits (replaceDigit (pure '_')))
    , refused "a non-ASCII character" (editDigits (replaceDigit genNonAscii))
    , refused "a non-ASCII character that lower-casing changes" (editDigits (replaceDigit (Gen.element lowerCaseChanges)))
    , refused "a non-ASCII character at the well-formed byte length" (editDigits fillBytes)
    , refused "leading white space" (\wire -> (<> render wire) <$> genSpace)
    , refused "trailing white space" (\wire -> (render wire <>) <$> genSpace)
    , refused "inner white space" (\wire -> insertWithin (render wire) =<< genSpace)
    , refused "the empty text" (const (pure ""))
    , undecided "digits of any length" (editDigits (const (Gen.text (Range.linear 0 140) (Gen.element (digitsOf spelling)))))
    , undecided "arbitrary text" (const (Gen.text (Range.linear 0 150) Gen.unicode))
    ]
  where
    size = spellingBytes spelling
    genOtherLength = do
        other <- Gen.element (filter (/= size) (ordNub ([size - 3 .. size + 3] <> [0, 16, 20, 28, 32, 48, 64])))
        encodeAs spelling <$> genBytes other
    otherEncoding = filter ((/= isJust (spellingPrefix spelling)) . isJust . spellingPrefix) spellings
    outside = filter (`notElem` digitsOf spelling) ['\0' .. '\x7f']

hexCases :: Spelling -> [Case]
hexCases spelling =
    [ refused "an odd length" $ editDigits $ \digits -> do
        count <- Gen.element [1, 3]
        Gen.choice [dropDigits count digits, addDigits spelling count digits]
    ]

sriCases :: Spelling -> Text -> [Case]
sriCases spelling prefix =
    [ refused "an unknown algorithm prefix" (withPrefix (Gen.element ["sha1-", "md5-", "sha224-", "sha3-256-", "sha-256-", "blake2b-", T.toUpper prefix, " " <> prefix]))
    , refused "another algorithm's prefix" (withPrefix (Gen.element (filter (/= prefix) (mapMaybe spellingPrefix sriSpellings))))
    , refused "a missing separator" (withPrefix (pure (T.dropEnd 1 prefix)))
    , refused "a doubled separator" (withPrefix (pure (prefix <> "-")))
    , refused "a prefix with no body" (const (Gen.element [prefix, T.dropEnd 1 prefix]))
    , refused "a body with no prefix" (withPrefix (pure ""))
    , refused "several space-separated components" $ \wire -> do
        others <- Gen.list (Range.constant 1 2) (Gen.element sriSpellings >>= genWire)
        pure (unwords (map render (wire : others)))
    , undecided "hex digits as the body" (const ((prefix <>) . hexDigestText <$> (Gen.element [16, 20, 32, 48, 64] >>= genBytes)))
    , refused "extra padding" (\wire -> (render wire <>) <$> Gen.element ["=", "=="])
    , refused "padding in place of a digit" (editDigits (replaceDigit (pure '=')))
    ]
        <> [refused "no padding" (\wire -> pure (render wire{wirePadding = ""})) | padCount > 0]
        <> [refused "one padding character of two" (\wire -> pure (render wire{wirePadding = "="})) | padCount == 2]
        <> [refused "padding before the last digit" (\wire -> pure (wirePrefix wire <> T.dropEnd 1 (wireDigits wire) <> wirePadding wire <> T.takeEnd 1 (wireDigits wire))) | padCount > 0]
        <> [accepted "spare bits set in the last digit" (editDigits setSpareBits) | padCount > 0]
  where
    padCount = T.length (wirePadding (plainWire spelling))
    withPrefix genPrefix wire = (\other -> render wire{wirePrefix = other}) <$> genPrefix
    -- Each padding character leaves two low bits of the last digit unused, so the digits that
    -- follow it in the alphabet spell the same bytes.
    setSpareBits digits = do
        let lastDigit = T.takeEnd 1 digits
        sameBytes <- Gen.element (take (4 ^ padCount - 1) (drop 1 (dropWhile ((/= lastDigit) . one) (digitsOf spelling))))
        pure (T.snoc (T.dropEnd 1 digits) sameBytes)

editDigits :: (Text -> Gen Text) -> Wire -> Gen Text
editDigits edit wire = (\digits -> render wire{wireDigits = digits}) <$> edit (wireDigits wire)

dropDigits :: Int -> Text -> Gen Text
dropDigits count digits = do
    at <- Gen.int (Range.constant 0 (T.length digits - count))
    pure (T.take at digits <> T.drop (at + count) digits)

addDigits :: Spelling -> Int -> Text -> Gen Text
addDigits spelling count digits = do
    at <- Gen.int (Range.constant 0 (T.length digits))
    extra <- Gen.text (Range.singleton count) (Gen.element (digitsOf spelling))
    pure (T.take at digits <> extra <> T.drop at digits)

replaceDigit :: Gen Char -> Text -> Gen Text
replaceDigit genChar digits = do
    at <- Gen.int (Range.constant 0 (T.length digits - 1))
    c <- genChar
    pure (T.take at digits <> one c <> T.drop (at + 1) digits)

-- One non-ASCII character in place of as many digits as its UTF-8 encoding has bytes.
fillBytes :: Text -> Gen Text
fillBytes digits = do
    c <- genNonAscii
    let width = BS.length (encodeUtf8 (one c :: Text))
    at <- Gen.int (Range.constant 0 (T.length digits - width))
    pure (T.take at digits <> one c <> T.drop (at + width) digits)

insertWithin :: Text -> Text -> Gen Text
insertWithin value inserted = do
    at <- Gen.int (Range.constant 1 (T.length value - 1))
    pure (T.take at value <> inserted <> T.drop at value)

genNonAscii :: Gen Char
genNonAscii = Gen.filter (> '\x7f') Gen.unicode

-- Among them U+0130, which lowers to two characters, and U+212A, which lowers to an ASCII letter.
lowerCaseChanges :: [Char]
lowerCaseChanges = filter (\c -> T.toLower (one c) /= one c) (['\x80' .. '\x24f'] <> "\x391\x410\x212a\xff21\xff26\x10400")

genSpace :: Gen Text
genSpace = Gen.element [" ", "\t", "\n", "\r", "\f", "\v", "\xa0", "\x1680", "\x2003", "\x2028", "\x3000", "  "]
