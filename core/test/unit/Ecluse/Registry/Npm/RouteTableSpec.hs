-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- TupleSections: the frozen reference classifier copies 'takeScoped''s (,rest) form.
{-# LANGUAGE TupleSections #-}

{- | The pattern-derived npm classifier is held against an __independent reference__: a
hand-written implementation of the same grammar, structured the way the classifier was
before it was derived from 'Ecluse.Core.Registry.Npm.Route.npmPatterns'. The equivalence
property drives both with generated requests and asserts they agree, so the pattern
engine is differential-tested against a second implementation on the security-critical
routing path.

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
import Ecluse.Core.Registry.Npm.Route (classify)
import Ecluse.Core.Server.Route (Filename (Filename), Route (..), isSafeComponent)
import Ecluse.Core.Version (mkVersion)

spec :: Spec
spec = do
    describe "classify is derived from npmPatterns (differential against an independent reference)" $ do
        modifyMaxSuccess (const 5000) $
            it "agrees with the reference classifier over generated requests" $
                hedgehog $ do
                    method <- forAll genMethod
                    segments <- forAll genSegments
                    classify method segments === referenceClassify method segments

        -- Worked examples: also documentation of the grammar the engine now derives.
        it "GET /-/ping is the liveness probe" $
            classify methodGet ["-", "ping"] `shouldBe` Ping
        it "GET /-/v1/search is the (unsupported) search route" $
            classify methodGet ["-", "v1", "search"] `shouldBe` Search
        it "GET /{package} is a packument" $
            classify methodGet ["lodash"] `shouldBe` Packument (mkPackageName Npm Nothing "lodash")
        it "both scoped-name wire encodings normalise to the same packument" $
            classify methodGet ["@scope", "pkg"] `shouldBe` classify methodGet ["@scope/pkg"]
        it "GET /{package}/-/{file}.tgz is a tarball" $
            classify methodGet ["lodash", "-", "lodash-1.0.0.tgz"]
                `shouldBe` Tarball (mkPackageName Npm Nothing "lodash") (mkVersion Npm "1.0.0") (Filename "lodash-1.0.0.tgz")
        it "a tarball whose basename is for another package is denied (path confusion)" $
            classify methodGet ["lodash", "-", "evil-1.0.0.tgz"] `shouldBe` Unsupported
        it "PUT /{package} is a publish" $
            classify methodPut ["lodash"] `shouldBe` Publish (mkPackageName Npm Nothing "lodash")
        it "an unknown meta-route denies" $
            classify methodGet ["-", "bogus"] `shouldBe` Unsupported
        it "a HEAD reads like a GET" $
            classify methodHead ["lodash"] `shouldBe` Packument (mkPackageName Npm Nothing "lodash")

        -- A lone "-" is the reserved meta-route prefix, never a package name. The read
        -- path always denied it; the publish path used to take it as a package called
        -- "-" (a name npm cannot even hold). Both deny now.
        it "a lone \"-\" is never a package, on any method" $ do
            classify methodPut ["-"] `shouldBe` Unsupported
            classify methodGet ["-"] `shouldBe` Unsupported

        -- Only GET, HEAD, and PUT are answered. A read used to mean "any method that is
        -- not a PUT", so a DELETE over a package path was served a packument; it denies now.
        it "a method the front door does not answer denies" $ do
            classify methodDelete ["lodash"] `shouldBe` Unsupported
            classify methodPost ["lodash"] `shouldBe` Unsupported
            classify methodDelete ["lodash", "-", "lodash-1.0.0.tgz"] `shouldBe` Unsupported

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

-- The independent reference classifier ----------------------------------------
--
-- A hand-written implementation of the npm grammar, structured the way the classifier
-- was before it was derived from 'npmPatterns'. It shares no code with the
-- implementation under test, so the equivalence property is a genuine differential
-- check of the pattern engine, not a tautology. It encodes the CORRECTED grammar: only
-- GET, HEAD, and PUT are answered, and a lone "-" is never a package.

referenceClassify :: Method -> [Text] -> Route
referenceClassify method segments
    | method == methodPut = refPublish segments
    | method == methodGet || method == methodHead = refRead segments
    -- Any other method matches no route: deny by default.
    | otherwise = Unsupported

refRead :: [Text] -> Route
refRead ("-" : meta) = refMeta meta
refRead segments = refPackage segments

refPublish :: [Text] -> Route
-- A lone "-" is the reserved meta-route prefix, never a package name.
refPublish ("-" : _) = Unsupported
refPublish segments = case refTakePackage segments of
    Just (name, []) -> Publish name
    _ -> Unsupported

refMeta :: [Text] -> Route
refMeta = \case
    ["ping"] -> Ping
    ["v1", "search"] -> Search
    _ -> Unsupported

refPackage :: [Text] -> Route
refPackage segments = case refTakePackage segments of
    Nothing -> Unsupported
    Just (name, rest) -> refDispatch name rest
  where
    refDispatch name = \case
        [] -> Packument name
        ["-", file] | isSafeComponent file -> refTarball name file
        _ -> Unsupported

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

refTarball :: PackageName -> Text -> Route
refTarball name file =
    case T.stripSuffix ".tgz" file >>= T.stripPrefix (unscopedName name <> "-") of
        Just version
            | not (T.null version) -> Tarball name (mkVersion Npm version) (Filename file)
        _ -> Unsupported
