-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Which per-origin outcomes leave a caller nothing to serve ('originMiss').
A first-party name has one authority, so it answers on the strength of this split: a settled
absence refuses the request, and an origin that was never read invites a retry.
-}
module Ecluse.Core.Server.Pipeline.OriginSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Server.Pipeline.Origin (
    OriginMiss (MissAbsent, MissUnresolved),
    OriginResult (
        OriginAbsent,
        OriginAuthorisationFailure,
        OriginNameMismatch,
        OriginNotFound,
        OriginResolved,
        OriginUnresolved
    ),
    originManifest,
    originMiss,
 )
import Ecluse.Test.Package (sampleManifest)

spec :: Spec
spec = describe "originMiss -- the outcomes that leave nothing to serve" $ do
    it "reports no miss for a document, an access refusal, or an identity refusal" $
        map
            originMiss
            [ OriginResolved (sampleManifest thing [])
            , OriginAuthorisationFailure 401
            , OriginAuthorisationFailure 403
            , OriginNameMismatch
            ]
            `shouldBe` [Nothing, Nothing, Nothing, Nothing]

    it "separates an origin that answered 404 from one that was never read" $
        map originMiss [OriginNotFound, OriginUnresolved]
            `shouldBe` [Just MissAbsent, Just MissUnresolved]

    it "groups an unconfigured origin with an absence, because no retry reaches it" $
        originMiss OriginAbsent `shouldBe` Just MissAbsent

    it "contributes no manifest from any miss" $
        map (isNothing . originManifest) [OriginNotFound, OriginUnresolved, OriginAbsent]
            `shouldBe` [True, True, True]

thing :: PackageName
thing = mkPackageName Npm Nothing "thing"
