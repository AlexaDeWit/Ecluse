-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent validator bytes and output-charge estimates for admitted metadata.
module Ecluse.Core.Server.Pipeline.Packument.RepresentationSpec (spec) where

import Crypto.Hash (Context, Digest, SHA256, hash, hashFinalize, hashInit, hashUpdates)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, intDec, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAscii)
import Data.Text qualified as T
import Hedgehog (Gen, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (PackageName, mkPackageName, pkgEcosystem, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource), mergePackuments)
import Ecluse.Core.Registry.Metadata (ContentDigest, Manifest (manifestDigest, manifestInfo, manifestRaw), digestBytes)
import Ecluse.Core.Server.Conditional (ETag, mkStrongETag, renderETag)
import Ecluse.Core.Server.Encoding (ContentCoding (GzipCoding, IdentityCoding))
import Ecluse.Core.Server.Pipeline.Packument.Representation
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Version (Version)
import Ecluse.Test.Package (npmVersion, pypiVersion, requestsName, sampleManifest, thingName)
import Ecluse.Test.Snapshot (digestOf)

spec :: Spec
spec = do
    packumentETagSpec
    codedETagSpec
    outputBasisSpec

codedETagSpec :: Spec
codedETagSpec = describe "codedETag -- one validator per content coding" $ do
    it "keeps the identity tag for identity coding" $
        codedETag IdentityCoding (tagWith base) `shouldBe` tagWith base
    it "pins a weak gzip tag over the framed identity tag" $
        -- SHA-256 of "ecluse:packument-etag:gzip:v1\0" and the length-framed quoted identity tag.
        renderETag (codedETag GzipCoding (mkStrongETag (hash ("identity" :: ByteString) :: Digest SHA256)))
            `shouldBe` "W/\"34d4ae5c45518e8b9b30722d9a46d3628e64aca9df195e4351b9f3a702d44b3a\""
    it "changes the gzip tag with the identity tag" $
        codedETag GzipCoding (tagWith base{publicSurvivors = ["1.0.0"]}) `shouldNotBe` codedETag GzipCoding (tagWith base)
    it "never shares the identity tag's opaque value" $
        T.drop 2 (renderETag (codedETag GzipCoding (tagWith base))) `shouldNotBe` renderETag (tagWith base)

packumentETagSpec :: Spec
packumentETagSpec = describe "packumentETag -- the input-derived validator" $ do
    it "pins the v4 byte framing for every entry constructor" $ do
        let sources =
                [ (TrustedSource, privateDigest base, [("1\0é", [ArrayEntry 0, ArrayEntry 10, ArrayEntry (-1), ObjectEntry "é\0x", SingletonEntry])])
                , (GatedSource, publicDigest base, [])
                ]
        renderETag (packumentETag mountBase [] thingName sources)
            `shouldBe` "\"4319a6866882aedd076a71e0f8158f81138c10f4f18ff475446cd63a307a5fe6\""

    it "changes when the origin URL changes the source metadata pointer" $
        packumentETag mountBase ["https://source-one.example"] thingName (piecesOf base)
            `shouldNotBe` packumentETag mountBase ["https://source-two.example"] thingName (piecesOf base)

    it "changes when an origin body changes (same survivors)" $
        tagWith base{publicDigest = digestOf "public-bytes-v2"} `shouldNotBe` tagWith base

    it "changes when the private origin's body changes" $
        tagWith base{privateDigest = digestOf "private-bytes-v2"} `shouldNotBe` tagWith base

    it "changes when a version drops out of the survivor set" $
        tagWith base{publicSurvivors = ["1.0.0"]} `shouldNotBe` tagWith base

    it "changes when a version joins the survivor set" $
        tagWith base{publicSurvivors = ["1.0.0", "2.0.0", "3.0.0"]} `shouldNotBe` tagWith base

    it "changes when the mount base URL changes (rewritten tarball URLs differ)" $
        packumentETag "https://other.example/npm" [] thingName (piecesOf base)
            `shouldNotBe` tagWith base

    it "changes across packages" $
        packumentETag mountBase [] (mkPackageName Npm Nothing "other-thing") (piecesOf base)
            `shouldNotBe` tagWith base

    it "distinguishes provenance: the same digest as trusted vs gated" $
        tagWith base{privateProvenance = GatedSource} `shouldNotBe` tagWith base

    it "distinguishes source order (merge precedence is positional)" $
        packumentETag mountBase [] thingName (reverse (piecesOf base)) `shouldNotBe` tagWith base

    it "does not collide survivor lists on concatenation framing" $ do
        -- ["1.0", "0.2.0"] vs ["1.0.0", "2.0"] concatenate to the same characters.
        -- The per-field terminator must keep them distinct.
        tagWith base{publicSurvivors = ["1.0", "0.2.0"]}
            `shouldNotBe` tagWith base{publicSurvivors = ["1.0.0", "2.0"]}

    it "does not collide a survivor moved across the source boundary" $
        -- The same flat multiset of survivors, split differently between the two
        -- sources, must not collide. The source-block terminator keeps them apart.
        tagWith base{privateSurvivors = ["9.0.0", "1.0.0"], publicSurvivors = ["2.0.0"]}
            `shouldNotBe` tagWith base{privateSurvivors = ["9.0.0"], publicSurvivors = ["1.0.0", "2.0.0"]}

    it "changes when a whole source appears or disappears" $
        packumentETag mountBase [] thingName [publicPiece base] `shouldNotBe` tagWith base

    for_ [TrustedSource, GatedSource] $ \provenance -> do
        let tag entries = packumentETag mountBase [] thingName [(provenance, publicDigest base, [("1.0.0", entries)])]
        it ("tracks exact admitted coordinates for " <> show provenance) $
            tag [ArrayEntry 0, ArrayEntry 1] `shouldNotBe` tag [ArrayEntry 1]
        it ("distinguishes entry constructors for " <> show provenance) $ do
            tag [ArrayEntry 0] `shouldNotBe` tag [ObjectEntry "0"]
            tag [SingletonEntry] `shouldNotBe` tag [ObjectEntry "s"]
        it ("frames arbitrary object keys for " <> show provenance) $
            tag [ObjectEntry "a", ObjectEntry "b"] `shouldNotBe` tag [ObjectEntry "a\0b"]

    describe "properties" $
        modifyMaxSuccess (const 500) . it "writes the same fingerprint as the Builder encoding it replaced" $
            hedgehog $ do
                mount <- forAll genFingerprintText
                origins <- forAll (Gen.list (Range.constant 0 3) genFingerprintText)
                name <- forAll (mkPackageName <$> Gen.element [Npm, PyPI] <*> pure Nothing <*> genFingerprintText)
                sources <- forAll (Gen.list (Range.constant 0 3) genSourcePiece)
                let indices = [index | (_, _, survivors) <- sources, (_, entries) <- survivors, ArrayEntry index <- entries]
                    texts = mount : origins <> [text | (_, _, survivors) <- sources, (version, entries) <- survivors, text <- version : [key | ObjectEntry key <- entries]]
                cover 5 "an index at minBound" (minBound `elem` indices)
                cover 5 "an index at maxBound" (maxBound `elem` indices)
                cover 5 "another negative index" (any (\index -> index < 0 && index /= minBound) indices)
                cover 5 "a zero index" (0 `elem` indices)
                cover 5 "text with a NUL or a non-ASCII character" (any (T.any (\c -> c == '\0' || not (isAscii c))) texts)
                cover 5 "a source with no survivors" (any (\(_, _, survivors) -> null survivors) sources)
                cover 5 "no sources" (null sources)
                cover 5 "several origins" (length origins >= 2)
                packumentETag mount origins name sources === builderETag mount origins name sources

-- The v4 encoding through a Builder, frozen as it stood before the fingerprint moved to one buffer.
builderETag :: Text -> [Text] -> PackageName -> [(Provenance, ContentDigest, [(Text, [EntryKey])])] -> ETag
builderETag mountBaseUrl originBaseUrls name sources =
    mkStrongETag (hashFinalize (hashUpdates (hashInit :: Context SHA256) (LBS.toChunks (toLazyByteString fingerprint))))
  where
    frame bytes = intDec (BS.length bytes) <> ":" <> byteString bytes
    fingerprint =
        "ecluse:packument-etag:v4\0"
            <> foldMap (frame . encodeUtf8) originBaseUrls
            <> "\0"
            <> byteString (encodeUtf8 mountBaseUrl)
            <> "\0"
            <> byteString (encodeUtf8 (renderPackageName name))
            <> "\0"
            <> foldMap sourcePiece sources
    sourcePiece (provenance, digest, survivors) = provenanceTag provenance <> byteString (digestBytes digest) <> foldMap versionPiece survivors <> "\1"
    versionPiece (version, entries) = frame (encodeUtf8 version) <> foldMap entryPiece entries <> "\2"
    entryPiece :: EntryKey -> Builder
    entryPiece = \case
        ArrayEntry index -> "a" <> frame (show index)
        ObjectEntry key -> "o" <> frame (encodeUtf8 key)
        SingletonEntry -> "s"
    provenanceTag :: Provenance -> Builder
    provenanceTag = \case
        TrustedSource -> "t\0"
        GatedSource -> "g\0"

genSourcePiece :: Gen (Provenance, ContentDigest, [(Text, [EntryKey])])
genSourcePiece =
    (,,)
        <$> Gen.element [TrustedSource, GatedSource]
        <*> (digestOf <$> Gen.bytes (Range.constant 0 8))
        <*> Gen.list (Range.constant 0 3) ((,) <$> genFingerprintText <*> Gen.list (Range.constant 0 4) genEntryKey)

genEntryKey :: Gen EntryKey
genEntryKey = Gen.choice [ArrayEntry <$> genIndex, ObjectEntry <$> genFingerprintText, pure SingletonEntry]
  where
    genIndex = Gen.choice [Gen.element [minBound, maxBound, 0, -1], Gen.int (Range.constantFrom 0 minBound maxBound)]

-- Mostly ASCII, with NUL, the framing colon, two- and four-byte UTF-8 characters, and other Unicode characters.
genFingerprintText :: Gen Text
genFingerprintText = Gen.text (Range.constant 0 6) (Gen.frequency [(4, Gen.alphaNum), (1, Gen.element ['\0', ':', '\233', '\128512']), (1, Gen.unicode)])

outputBasisSpec :: Spec
outputBasisSpec = describe "outputBasisBytes -- the source bytes a listing's output charge scales" $
    forM_ [(thingName, npmVersion), (requestsName, pypiVersion)] $ \(name, version) ->
        describe (show (pkgEcosystem name)) $ do
            let source = listingSource name version
            it "is a single source's size" $
                basisOf [source GatedSource 900 ["1.0.0", "2.0.0"]] `shouldBe` Just 900

            it "counts one copy of sources that hold the same versions" $
                basisOf [source TrustedSource 900 ["1.0.0", "2.0.0"], source GatedSource 900 ["1.0.0", "2.0.0"]] `shouldBe` Just 900

            it "counts both sources when they share no version" $
                basisOf [source TrustedSource 400 ["1.0.0"], source GatedSource 900 ["2.0.0", "3.0.0"]] `shouldBe` Just 1300

            it "adds the smaller source's share for the versions the larger lacks" $
                basisOf [source TrustedSource 600 ["1.0.0", "2.0.0", "3.0.0"], source GatedSource 900 ["2.0.0", "3.0.0", "4.0.0"]] `shouldBe` Just 1100

            it "takes the larger source whole on either side of the merge" $
                basisOf [source TrustedSource 900 ["1.0.0", "2.0.0", "3.0.0"], source GatedSource 300 ["3.0.0", "4.0.0", "5.0.0"]] `shouldBe` Just 1100

            it "rounds a share up, anchored on the largest source when it gives more" $
                basisOf [source TrustedSource 100 ["1.0.0", "2.0.0", "3.0.0"], source GatedSource 900 ["2.0.0", "3.0.0", "4.0.0"]] `shouldBe` Just 934

            it "anchors on the base document when a larger source holds all its versions" $
                basisOf [source TrustedSource 900 ["1.0.0"], source GatedSource 1000 thousandVersions] `shouldBe` Just 1899

            it "anchors on the trusted source wherever it sits in the merge" $
                basisOf [source GatedSource 1000 thousandVersions, source TrustedSource 900 ["1.0.0"]] `shouldBe` Just 1899

-- A 1,000-version document that includes 1.0.0.
thousandVersions :: [Text]
thousandVersions = [show major <> ".0.0" | major <- [1 .. 1000 :: Int]]

-- A merge source holding the given versions, whose body is the given number of bytes.
listingSource :: PackageName -> (Text -> Version) -> Provenance -> Int -> [Text] -> Contribution
listingSource name version provenance bytes keys =
    Contribution provenance (manifestInfo manifest) (manifestRaw manifest) (manifestDigest manifest) bytes
  where
    manifest = sampleManifest name (map version keys)

basisOf :: [Contribution] -> Maybe Int
basisOf sources = (`outputBasisBytes` sources) <$> mergePackuments [(srcProvenance s, Snapshot (srcDigest s) (srcInfo s)) | s <- sources]

-- The fixture: a private (trusted) and a public (gated) source with distinct
-- bodies and survivor sets, varied one field at a time by each case.
data Fixture = Fixture
    { privateProvenance :: Provenance
    , privateDigest :: ContentDigest
    , privateSurvivors :: [Text]
    , publicDigest :: ContentDigest
    , publicSurvivors :: [Text]
    }

base :: Fixture
base =
    Fixture
        { privateProvenance = TrustedSource
        , privateDigest = digestOf "private-bytes-v1"
        , privateSurvivors = ["9.0.0"]
        , publicDigest = digestOf "public-bytes-v1"
        , publicSurvivors = ["1.0.0", "2.0.0"]
        }

piecesOf :: Fixture -> [(Provenance, ContentDigest, [(Text, [EntryKey])])]
piecesOf f = [(privateProvenance f, privateDigest f, map (,[SingletonEntry]) (privateSurvivors f)), publicPiece f]

publicPiece :: Fixture -> (Provenance, ContentDigest, [(Text, [EntryKey])])
publicPiece f = (GatedSource, publicDigest f, map (,[SingletonEntry]) (publicSurvivors f))

tagWith :: Fixture -> ETag
tagWith f = packumentETag mountBase [] thingName (piecesOf f)

mountBase :: Text
mountBase = "https://proxy.example/npm"
