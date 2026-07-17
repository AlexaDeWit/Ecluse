-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- TupleSections: the frozen reference parser copies 'takeScoped''s (,rest) form.
{-# LANGUAGE TupleSections #-}

{- | npm's route table, held against an __independent reference__: a hand-written
implementation of the same grammar, sharing no code with the table under test, so the
equivalence properties are a genuine differential check of the routing engine rather than a
tautology.

The security-critical routing path is checked on two axes, because a route record's action
is a closure (there is nothing to compare) while its /decision/ and its /parse/ both are:

* __Which route claims a request__ ('matchRoute', by 'routeId', or nothing at all). This
  covers everything the table is responsible for: matching order, the method condition, the
  precedence of the reserved meta-routes, and the __denials__ (a path-confusion artifact
  name claims no route, and so falls through to the @404@).

* __What a route's captures parse to__ ('takePackage', 'tarballCoordinate'). This is where the
  scoped-name decoding, the component-safety gate, and the artifact coordinate live. They
  are named functions the table references, so they are asserted directly rather than
  through the router.

The reference encodes the __corrected__ grammar, and both corrections are also asserted
directly below:

* a lone @"-"@ is never a package name, on __any__ method (it is the reserved meta-route
  prefix), so a @PUT \/-@ denies rather than publishing a package called @"-"@; and
* only @GET@, @HEAD@, and @PUT@ are answered, so a @DELETE@ or @POST@ over a package path
  denies rather than being served a packument.
-}
module Ecluse.Registry.Npm.RouteTableSpec (spec) where

import Data.Text qualified as T
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Types.Method (Method, methodDelete, methodGet, methodHead, methodPost, methodPut)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, unscopedName)
import Ecluse.Core.Registry.Npm.Route (npmRoutes, takePackage, tarballCoordinate)
import Ecluse.Core.Server.Path (Filename (Filename), isSafeComponent)
import Ecluse.Core.Server.Route (Route (routeName), RouteName (RouteName), matchRoute)
import Ecluse.Core.Version (mkVersion)

{- | The route a request takes, named: the identifier of the first route to claim it, or
'Nothing' when none does (the deny-by-default @404@).

This is what the table decides. The action it names is a closure, exercised through the
serve path; what is asserted here is that the __right__ route claimed the request.
-}
matchedId :: Method -> [Text] -> Maybe RouteName
matchedId method segments = routeName . fst <$> matchRoute npmRoutes method segments

spec :: Spec
spec = do
    describe "npm's route table (differential against an independent reference)" $ do
        modifyMaxSuccess (const 5000) $
            it "claims the same route as the reference, over generated requests" $
                hedgehog $ do
                    method <- forAll genMethod
                    segments <- forAll genSegments
                    matchedId method segments === referenceRouteId method segments

        modifyMaxSuccess (const 5000) $
            it "parses a package unit exactly as the reference does" $
                hedgehog $ do
                    segments <- forAll genSegments
                    takePackage segments === refTakePackage segments

    describe "the routes it claims" $ do
        -- Worked examples: also documentation of the grammar the table encodes.
        it "GET /-/ping is the liveness probe" $
            matchedId methodGet ["-", "ping"] `shouldBe` Just (RouteName "ping")
        it "GET /-/v1/search is the (unsupported) search route" $
            matchedId methodGet ["-", "v1", "search"] `shouldBe` Just (RouteName "search")
        it "GET /{package} is a packument read" $
            matchedId methodGet ["lodash"] `shouldBe` Just (RouteName "packument")
        it "GET /{package}/-/{file}.tgz is an artifact read" $
            matchedId methodGet ["lodash", "-", "lodash-1.0.0.tgz"] `shouldBe` Just (RouteName "tarball")
        it "PUT /{package} is a publish" $
            matchedId methodPut ["lodash"] `shouldBe` Just (RouteName "publish")
        it "a HEAD reads like a GET" $
            matchedId methodHead ["lodash"] `shouldBe` Just (RouteName "packument")
        it "an unknown meta-route denies" $
            matchedId methodGet ["-", "bogus"] `shouldBe` Nothing

        -- The artifact route does not claim a file naming another package's artifact, so
        -- the request falls through to the 404 rather than being fabricated into a
        -- coordinate. Path confusion is a denial, not a mis-parse.
        it "an artifact whose basename is for another package is not claimed (path confusion)" $
            matchedId methodGet ["lodash", "-", "evil-1.0.0.tgz"] `shouldBe` Nothing

        -- A lone "-" is the reserved meta-route prefix, never a package name. The read
        -- path always denied it; the publish path used to take it as a package called
        -- "-" (a name npm cannot even hold). Both deny now.
        it "a lone \"-\" is never a package, on any method" $ do
            matchedId methodPut ["-"] `shouldBe` Nothing
            matchedId methodGet ["-"] `shouldBe` Nothing

        -- Only GET, HEAD, and PUT are answered. A read used to mean "any method that is
        -- not a PUT", so a DELETE over a package path was served a packument; it denies now.
        it "a method the front door does not answer denies" $ do
            matchedId methodDelete ["lodash"] `shouldBe` Nothing
            matchedId methodPost ["lodash"] `shouldBe` Nothing
            matchedId methodDelete ["lodash", "-", "lodash-1.0.0.tgz"] `shouldBe` Nothing

    describe "what its captures parse to" $ do
        it "normalises both scoped-name wire encodings to the same package" $
            takePackage ["@scope", "pkg"] `shouldBe` takePackage ["@scope/pkg"]
        it "parses an unscoped package unit" $
            takePackage ["lodash"] `shouldBe` Just (mkPackageName Npm Nothing "lodash", [])
        it "parses a scoped package unit, leaving the tail" $
            takePackage ["@babel/core", "-", "core-7.0.0.tgz"]
                `shouldBe` Just (mkPackageName Npm (Just (mkScope "babel")) "core", ["-", "core-7.0.0.tgz"])
        it "refuses a traversal component" $
            takePackage [".."] `shouldBe` Nothing

        it "reads the version out of an artifact name, preserving the file verbatim" $
            tarballCoordinate (mkPackageName Npm Nothing "lodash") "lodash-1.0.0.tgz"
                `shouldBe` Just (mkVersion Npm "1.0.0", Filename "lodash-1.0.0.tgz")
        it "drops the scope from a scoped package's artifact name, as npm does" $
            tarballCoordinate (mkPackageName Npm (Just (mkScope "babel")) "code-frame") "code-frame-7.0.0.tgz"
                `shouldBe` Just (mkVersion Npm "7.0.0", Filename "code-frame-7.0.0.tgz")
        it "refuses an artifact name for a different package (path confusion)" $
            tarballCoordinate (mkPackageName Npm Nothing "lodash") "evil-1.0.0.tgz" `shouldBe` Nothing
        it "refuses a bare .tgz with no version" $
            tarballCoordinate (mkPackageName Npm Nothing "lodash") "lodash-.tgz" `shouldBe` Nothing

-- Generators -----------------------------------------------------------------

genMethod :: Gen Method
genMethod = Gen.element [methodGet, methodPut, methodHead, methodPost, methodDelete]

{- | Mount-relative segment lists that exercise the tricky grammar: scoped names in
both encodings, the reserved @"-"@ prefix, tarball shapes, hostile components, and
random fuzz.
-}
genSegments :: Gen [Text]
genSegments = Gen.list (Range.linear 0 4) genSegment

genSegment :: Gen Text
genSegment =
    Gen.choice
        [ Gen.element
            [ "lodash"
            , "@scope/pkg"
            , "@scope"
            , "pkg"
            , "-"
            , "-foo"
            , "ping"
            , "v1"
            , "search"
            , ".."
            , "."
            , "foo/bar"
            , "lodash-1.0.0.tgz"
            , "code-frame-7.0.0.tgz"
            , ""
            , "@"
            , "@/x"
            , "@scope/"
            ]
        , Gen.text (Range.linear 0 8) (Gen.element ['a', 'b', 'c', 'n', 'p', 'm', '@', '-', '/', '.', '%', '1', '2', '3', '4'])
        ]

-- The independent reference ---------------------------------------------------
--
-- A hand-written implementation of the npm grammar, structured the way the router was
-- before it was derived from a route table. It shares no code with the implementation
-- under test, so the equivalence properties are a genuine differential check of the
-- routing engine, not a tautology. It encodes the CORRECTED grammar: only GET, HEAD, and
-- PUT are answered, and a lone "-" is never a package.

-- | Which route the reference grammar says claims a request.
referenceRouteId :: Method -> [Text] -> Maybe RouteName
referenceRouteId method segments
    | method == methodPut = refPublish segments
    | method == methodGet || method == methodHead = refRead segments
    -- Any other method matches no route: deny by default.
    | otherwise = Nothing

refRead :: [Text] -> Maybe RouteName
refRead ("-" : meta) = refMeta meta
refRead segments = refPackage segments

refPublish :: [Text] -> Maybe RouteName
-- A lone "-" is the reserved meta-route prefix, never a package name.
refPublish ("-" : _) = Nothing
refPublish segments = case refTakePackage segments of
    Just (_name, []) -> Just (RouteName "publish")
    _ -> Nothing

refMeta :: [Text] -> Maybe RouteName
refMeta = \case
    ["ping"] -> Just (RouteName "ping")
    ["v1", "search"] -> Just (RouteName "search")
    _ -> Nothing

refPackage :: [Text] -> Maybe RouteName
refPackage segments = case refTakePackage segments of
    Nothing -> Nothing
    Just (name, rest) -> refDispatch name rest
  where
    refDispatch name = \case
        [] -> Just (RouteName "packument")
        ["-", file] | isSafeComponent file -> refTarball name file
        _ -> Nothing

refTakePackage :: [Text] -> Maybe (PackageName, [Text])
refTakePackage [] = Nothing
refTakePackage (seg : rest)
    | "@" <- T.take 1 seg = refTakeScoped seg rest
    | isSafeComponent seg = Just (mkPackageName Npm Nothing seg, rest)
    | otherwise = Nothing

refTakeScoped :: Text -> [Text] -> Maybe (PackageName, [Text])
refTakeScoped seg rest =
    case T.breakOn "/" (T.drop 1 seg) of
        (scope, base)
            | not (T.null base) -> (,rest) <$> refScopedName scope (T.drop 1 base)
        _ -> case rest of
            (base : more) -> (,more) <$> refScopedName (T.drop 1 seg) base
            _ -> Nothing

refScopedName :: Text -> Text -> Maybe PackageName
refScopedName scope base
    | isSafeComponent scope && isSafeComponent base =
        Just (mkPackageName Npm (Just (mkScope scope)) base)
    | otherwise = Nothing

-- The artifact route claims a request only when the file name parses for that package.
refTarball :: PackageName -> Text -> Maybe RouteName
refTarball name file =
    case T.stripSuffix ".tgz" file >>= T.stripPrefix (unscopedName name <> "-") of
        Just version
            | not (T.null version) -> Just (RouteName "tarball")
        _ -> Nothing
