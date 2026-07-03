{- | The derived packument validator ('packumentETag'): the input fingerprint that
stands in for a hash of the served bytes.

The correctness direction a validator must hold is __never call a changed document
unchanged__, so these cases pin that the tag moves whenever any input the served
document is a function of moves -- an origin body, a survivor set, the source's
provenance, the source order, the mount base URL, the package -- and that it is
bit-stable when nothing does. The tag is allowed to change spuriously; it is not
allowed to stand still. Framing cases guard the hash-input encoding: adjacent
variable-length fields must not be collapsible into a colliding split.
-}
module Ecluse.Server.Pipeline.PackumentSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource))
import Ecluse.Core.Registry.Metadata (ContentDigest, digestOf)
import Ecluse.Core.Server.Conditional (ETag)
import Ecluse.Core.Server.Pipeline.Packument (packumentETag)

spec :: Spec
spec = describe "packumentETag -- the input-derived validator" $ do
    it "is bit-stable across identical inputs" $
        tagWith base `shouldBe` tagWith base

    it "changes when an origin body changes (same survivors)" $
        tagWith base{publicDigest = digestOf "public-bytes-v2"} `shouldNotBe` tagWith base

    it "changes when the private origin's body changes" $
        tagWith base{privateDigest = digestOf "private-bytes-v2"} `shouldNotBe` tagWith base

    it "changes when a version drops out of the survivor set" $
        tagWith base{publicSurvivors = ["1.0.0"]} `shouldNotBe` tagWith base

    it "changes when a version joins the survivor set" $
        tagWith base{publicSurvivors = ["1.0.0", "2.0.0", "3.0.0"]} `shouldNotBe` tagWith base

    it "changes when the mount base URL changes (rewritten tarball URLs differ)" $
        packumentETag "https://other.example/npm" thing (piecesOf base)
            `shouldNotBe` tagWith base

    it "changes across packages" $
        packumentETag mountBase (mkPackageName Npm Nothing "other-thing") (piecesOf base)
            `shouldNotBe` tagWith base

    it "distinguishes provenance: the same digest as trusted vs gated" $
        tagWith base{privateProvenance = GatedSource} `shouldNotBe` tagWith base

    it "distinguishes source order (merge precedence is positional)" $
        packumentETag mountBase thing (reverse (piecesOf base)) `shouldNotBe` tagWith base

    it "does not collide survivor lists on concatenation framing" $ do
        -- ["1.0", "0.2.0"] vs ["1.0.0", "2.0"] concatenate to the same characters;
        -- the per-field terminator must keep them distinct.
        tagWith base{publicSurvivors = ["1.0", "0.2.0"]}
            `shouldNotBe` tagWith base{publicSurvivors = ["1.0.0", "2.0"]}

    it "does not collide a survivor moved across the source boundary" $
        -- The same flat multiset of survivors, split differently between the two
        -- sources, must not collide; the source-block terminator keeps them apart.
        tagWith base{privateSurvivors = ["9.0.0", "1.0.0"], publicSurvivors = ["2.0.0"]}
            `shouldNotBe` tagWith base{privateSurvivors = ["9.0.0"], publicSurvivors = ["1.0.0", "2.0.0"]}

    it "changes when a whole source appears or disappears" $
        packumentETag mountBase thing [publicPiece base] `shouldNotBe` tagWith base

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

piecesOf :: Fixture -> [(Provenance, ContentDigest, [Text])]
piecesOf f = [(privateProvenance f, privateDigest f, privateSurvivors f), publicPiece f]

publicPiece :: Fixture -> (Provenance, ContentDigest, [Text])
publicPiece f = (GatedSource, publicDigest f, publicSurvivors f)

tagWith :: Fixture -> ETag
tagWith f = packumentETag mountBase thing (piecesOf f)

mountBase :: Text
mountBase = "https://proxy.example/npm"

thing :: PackageName
thing = mkPackageName Npm Nothing "thing"
