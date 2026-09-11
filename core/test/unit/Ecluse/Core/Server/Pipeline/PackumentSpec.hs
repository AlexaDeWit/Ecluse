-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The derived packument validator ('packumentETag') and the answer a first-party
name's private miss renders.

A validator must never call a changed document unchanged, so these cases pin that the
tag moves whenever an input of the served document moves, and that it is bit-stable
when nothing moves. The framing cases guard the hash-input encoding: adjacent
variable-length fields must not collapse into a colliding split.
-}
module Ecluse.Core.Server.Pipeline.PackumentSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource))
import Ecluse.Core.Registry.Metadata (ContentDigest, digestOf)
import Ecluse.Core.Server.Conditional (ETag)
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
    Rejection (rejectionReason),
    ServeDecision (Admit, Reject),
    Transience (WillResolve),
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric

spec :: Spec
spec = do
    packumentETagSpec
    firstPartyMissSpec

packumentETagSpec :: Spec
packumentETagSpec = describe "packumentETag -- the input-derived validator" $ do
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

reasonOf :: ServeDecision -> Maybe RejectReason
reasonOf = \case
    Admit -> Nothing
    Reject rejection -> Just (rejectionReason rejection)

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
