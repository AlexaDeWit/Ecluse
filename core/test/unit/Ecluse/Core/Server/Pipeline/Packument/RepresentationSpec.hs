-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent validator bytes and output-charge estimates for admitted metadata.
module Ecluse.Core.Server.Pipeline.Packument.RepresentationSpec (spec) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, pkgEcosystem)
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource), mergePackuments)
import Ecluse.Core.Registry.Metadata (ContentDigest, Manifest (manifestDigest, manifestInfo, manifestRaw))
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
