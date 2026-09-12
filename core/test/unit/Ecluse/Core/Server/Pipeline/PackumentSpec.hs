-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Validator identity, framing, and first-party private misses.
module Ecluse.Core.Server.Pipeline.PackumentSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource))
import Ecluse.Core.Registry.Metadata (ContentDigest, digestOf)
import Ecluse.Core.Server.Conditional (ETag, renderETag)
import Ecluse.Core.Server.Pipeline.Internal (denialLabels, packumentServeDecision)
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss (MissAbsent, MissUnresolved))
import Ecluse.Core.Server.Pipeline.Packument (
    PackumentReplies (..),
    firstPartyMissDecision,
    firstPartyMissReply,
    packumentETag,
 )
import Ecluse.Core.Server.Response (
    RejectReason (Unavailable),
    ServeDecision,
    Transience (WillResolve),
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Test.Server.Response (reasonOf)

spec :: Spec
spec = do
    packumentETagSpec
    firstPartyMissSpec

packumentETagSpec :: Spec
packumentETagSpec = describe "packumentETag -- the input-derived validator" $ do
    it "is bit-stable across identical inputs" $
        tagWith base `shouldBe` tagWith base

    it "preserves the v2 byte framing for every entry constructor" $ do
        let sources =
                [ (TrustedSource, privateDigest base, [("1\0é", [ArrayEntry 0, ArrayEntry 10, ArrayEntry (-1), ObjectEntry "é\0x", SingletonEntry])])
                , (GatedSource, publicDigest base, [])
                ]
        renderETag (packumentETag mountBase thing sources)
            `shouldBe` "\"93f747ebd65d300c3cd90719d0394ddd77c87140342be84e3df6560d8f7fed26\""

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
        packumentETag mountBase thing [publicPiece base] `shouldNotBe` tagWith base

    for_ [TrustedSource, GatedSource] $ \provenance -> do
        let tag entries = packumentETag mountBase thing [(provenance, publicDigest base, [("1.0.0", entries)])]
        it ("tracks exact admitted coordinates for " <> show provenance) $
            tag [ArrayEntry 0, ArrayEntry 1] `shouldNotBe` tag [ArrayEntry 1]
        it ("keeps identical selections stable for " <> show provenance) $
            tag [ArrayEntry 1, ArrayEntry 2] `shouldBe` tag [ArrayEntry 1, ArrayEntry 2]
        it ("distinguishes entry constructors for " <> show provenance) $ do
            tag [ArrayEntry 0] `shouldNotBe` tag [ObjectEntry "0"]
            tag [SingletonEntry] `shouldNotBe` tag [ObjectEntry "s"]
        it ("frames arbitrary object keys for " <> show provenance) $
            tag [ObjectEntry "a", ObjectEntry "b"] `shouldNotBe` tag [ObjectEntry "a\0b"]

firstPartyMissSpec :: Spec
firstPartyMissSpec = describe "a first-party name whose private origin yielded nothing" $ do
    it "renders an origin that answered 404 as a 404" $
        replyFor MissAbsent `shouldBe` "not-found"

    it "renders an origin that was never read as a 503" $
        replyFor MissUnresolved `shouldBe` "unavailable"

    it "counts an absence as a denial the first-party rule decided" $ do
        packumentServeDecision [decisionFor MissAbsent] `shouldBe` Metric.Deny
        fmap denialLabels (reasonOf (decisionFor MissAbsent))
            `shouldBe` Just (Just "first-party", Metric.ReasonPolicy)

    it "counts an unread origin as an outage, suggesting no delay" $ do
        packumentServeDecision [decisionFor MissUnresolved] `shouldBe` Metric.Unavailable
        reasonOf (decisionFor MissUnresolved) `shouldBe` Just (Unavailable (WillResolve Nothing))

-- Each reply factory answers its own name, so a case reads back which one the pipeline chose.
namedReplies :: PackumentReplies Text
namedReplies =
    PackumentReplies
        { packumentOk = \_ _ -> "ok"
        , packumentNotModified = const "not-modified"
        , packumentUnauthorised = \_ _ -> "unauthorised"
        , packumentForbidden = \_ _ -> "forbidden"
        , packumentNotFound = \_ _ -> "not-found"
        , packumentInternal = \_ _ -> "internal"
        , packumentBadGateway = \_ _ -> "bad-gateway"
        , packumentUnavailable = \_ _ -> "unavailable"
        }

replyFor :: OriginMiss -> Text
replyFor = firstPartyMissReply namedReplies Nothing thing

decisionFor :: OriginMiss -> ServeDecision
decisionFor = firstPartyMissDecision thing

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
tagWith f = packumentETag mountBase thing (piecesOf f)

mountBase :: Text
mountBase = "https://proxy.example/npm"

thing :: PackageName
thing = mkPackageName Npm Nothing "thing"
