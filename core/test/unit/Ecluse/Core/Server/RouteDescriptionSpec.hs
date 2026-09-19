-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The documented-operation view of a route, held against a route with no ecosystem in it.

The projection is engine glue every ecosystem's table calls rather than restates, so this module
projects one route built out of nothing but the engine's own builders. It imports no registry
module: if one were needed, the projection would not be shared.
-}
module Ecluse.Core.Server.RouteDescriptionSpec (spec) where

import Network.HTTP.Types (status404)
import Network.HTTP.Types.Method (StdMethod (GET, HEAD, POST))
import Test.Hspec

import Ecluse.Core.Server.Contract (
    BodySchema (SchemaEmpty, SchemaText),
    ResponseContract,
    ResponseDoc (responseBodySchema, responseStatus),
    ResponseStatus (ExactResponse),
    ResponseValue,
    emptyContract,
    mediaContract,
    responseValue,
 )
import Ecluse.Core.Server.Route (
    MediaNegotiation (AcceptsAnything),
    MethodMatch (MethodPost),
    PatternSeg (SegLit),
    Route (Route),
    RouteName (RouteName),
    answering,
 )
import Ecluse.Core.Server.RouteDescription (
    ParamSpec (ParamSpec),
    PathSeg (Param),
    RouteSpec (rsMethod, rsName, rsOutcomes, rsPattern),
    catchAllSpecs,
    specsOf,
 )

spec :: Spec
spec = do
    describe "specsOf" $
        it "projects a POST route to one POST operation and no derived HEAD" $ do
            map rsMethod (specsOf uploadRoute) `shouldBe` [POST]
            map rsName (specsOf uploadRoute) `shouldBe` [RouteName "upload"]

    describe "catchAllSpecs" $ do
        it "documents the pair a mount needs, GET and its bodiless HEAD" $ do
            map rsMethod (toList catchAll) `shouldBe` [GET, HEAD]
            map rsName (toList catchAll)
                `shouldBe` [RouteName "unsupported", RouteName "unsupported.head"]

        it "carries the caller's path parameter on both" $
            map rsPattern (toList catchAll)
                `shouldBe` [[Param catchAllParam], [Param catchAllParam]]

        it "documents the refusal contract's status on both operations" $
            map (map responseStatus . rsOutcomes) (toList catchAll)
                `shouldBe` [[ExactResponse status404], [ExactResponse status404]]

        it "keeps the GET's body and drops the HEAD's" $
            map (map (isEmptyBody . responseBodySchema) . rsOutcomes) (toList catchAll)
                `shouldBe` [[False], [True]]

-- The route projected: a submission route over the engine's own builders, with no capture.
uploadRoute :: Route ()
uploadRoute =
    Route
        (RouteName "upload")
        MethodPost
        AcceptsAnything
        [SegLit "-", SegLit "upload"]
        (answering (responseValue [] ()))
        "Submit a file"
        "Answered locally."
        Nothing
        (emptyContract status404 "A refusal.")

catchAll :: NonEmpty RouteSpec
catchAll = catchAllSpecs refusalContract catchAllParam

refusalContract :: ResponseContract (ResponseValue LByteString)
refusalContract = mediaContract status404 "Unrecognised path; deny by default." (SchemaText "text/plain")

catchAllParam :: ParamSpec
catchAllParam = ParamSpec "unsupportedPath" "Any path under this mount no route claims."

isEmptyBody :: BodySchema -> Bool
isEmptyBody = \case
    SchemaEmpty -> True
    _ -> False
