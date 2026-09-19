-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The route-table builders, held against a table with no ecosystem in it.

Every definition under test is engine glue an ecosystem's table calls rather than restates, so
this module builds a three-route table out of nothing but the engine and asserts on that. It
imports no registry module: if one were needed, the glue would not be shared.
-}
module Ecluse.Core.Server.RouteSpec (spec) where

import Network.HTTP.Types (status404)
import Network.HTTP.Types.Method (Method, methodGet)
import Test.Hspec

import Ecluse.Core.Server.Context (ResponseAction (AnswerLocally))
import Ecluse.Core.Server.Contract (emptyContract, responseValue)
import Ecluse.Core.Server.Route (
    Capture (Capture),
    MediaNegotiation (AcceptsAnything),
    MethodMatch (MethodPost, MethodRead),
    PatternSeg (SegCap, SegLit),
    Route (Route),
    RouteName (RouteName),
    answering,
    isHead,
    safeSegment,
 )
import Ecluse.Test.Server.Route (claimedOn, everyMethod)

spec :: Spec
spec = do
    describe "matchRoute -- the route's method condition" $ do
        it "claims a read route on GET and HEAD, and on no other method" $ do
            let ping = Just (RouteName "ping")
            map (`claimed` ["-", "ping"]) everyMethod
                `shouldBe` [ping, ping, Nothing, Nothing, Nothing]

        it "claims a POST route on POST, and on no other method" $ do
            let upload = Just (RouteName "upload")
            map (`claimed` ["-", "upload"]) everyMethod
                `shouldBe` [Nothing, Nothing, upload, Nothing, Nothing]

    describe "safeSegment" $ do
        it "claims one leading segment and yields the tail" $
            safeSegment ToyFile ["report.txt", "rest"]
                `shouldBe` Just (ToyFile "report.txt", ["rest"])

        it "refuses a traversal, a separator, a control character, an empty segment, and an empty path" $
            map (safeSegment ToyFile) [[".."], ["a/b"], ["a\tb"], [""], []]
                `shouldBe` replicate 5 Nothing

        it "keeps an unsafe component out of the table it guards" $ do
            claimed methodGet ["thing", "-", "file.txt"] `shouldBe` Just (RouteName "file")
            claimed methodGet ["thing", "-", ".."] `shouldBe` Nothing

    describe "isHead" $
        it "holds for HEAD alone" $
            map isHead everyMethod `shouldBe` [False, True, False, False, False]

-- The table under test: three routes built from nothing but the engine's own builders.

data ToyCap
    = ToyName Text
    | ToyFile Text
    deriving stock (Eq, Show)

toyRoutes :: [Route ToyCap]
toyRoutes = [pingRoute, fileRoute, uploadRoute]

pingRoute :: Route ToyCap
pingRoute =
    Route
        (RouteName "ping")
        MethodRead
        AcceptsAnything
        [SegLit "-", SegLit "ping"]
        (answering (responseValue [] ()))
        "Liveness probe"
        "Answered locally."
        Nothing
        (emptyContract status404 "A refusal.")

fileRoute :: Route ToyCap
fileRoute =
    Route
        (RouteName "file")
        MethodRead
        AcceptsAnything
        [SegCap capName, SegLit "-", SegCap capFile]
        buildFile
        "Fetch a file"
        "Answered locally."
        Nothing
        (emptyContract status404 "A refusal.")
  where
    buildFile _method = \case
        [ToyName _, ToyFile _] -> Just (AnswerLocally (responseValue [] ()))
        _ -> Nothing

uploadRoute :: Route ToyCap
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

capName :: Capture ToyCap
capName = Capture "name" "The thing's name." (safeSegment ToyName) toySegment

capFile :: Capture ToyCap
capFile = Capture "file" "The file's name." (safeSegment ToyFile) toySegment

-- The name of the route that claims a request, or 'Nothing' when none does.
claimed :: Method -> [Text] -> Maybe RouteName
claimed = claimedOn toyRoutes

-- | The one segment a toy capture claims, written back out.
toySegment :: ToyCap -> [Text]
toySegment = \case
    ToyName name -> [name]
    ToyFile file -> [file]
