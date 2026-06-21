module Ecluse.Server.RouteSpec (spec) where

import Data.Char (isControl)
import Data.Text qualified as T
import Hedgehog (Gen, forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (
    PackageName,
    mkPackageName,
    mkScope,
    pkgNamespace,
    renderPackageName,
    unScope,
 )
import Ecluse.Server.Route (Route (..), classify)

-- | An unscoped npm package identity, for building expected 'Route's.
unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

-- | A scoped npm package identity (scope, base name), for expected 'Route's.
scoped :: Text -> Text -> PackageName
scoped scope = mkPackageName Npm (Just (mkScope scope))

{- | The routing table, asserted as @pathInfo → Route@. WAI percent-decodes
@pathInfo@, so each scoped case appears in __both__ wire encodings (one decoded
segment @\@scope\/pkg@ and two segments @\@scope@,@pkg@); both must agree.
-}
spec :: Spec
spec = do
    describe "classify — packuments" $ do
        it "routes an unscoped package to its packument" $
            classify ["is-odd"] `shouldBe` Packument (unscoped "is-odd")
        it "routes a scoped package (two segments) to its packument" $
            classify ["@babel", "code-frame"]
                `shouldBe` Packument (scoped "babel" "code-frame")
        it "routes a scoped package (one decoded segment) to its packument" $
            classify ["@babel/code-frame"]
                `shouldBe` Packument (scoped "babel" "code-frame")
        it "agrees on the same Route for both scoped encodings" $
            classify ["@babel", "code-frame"] `shouldBe` classify ["@babel/code-frame"]

    describe "classify — tarballs" $ do
        it "routes an unscoped tarball to its artifact" $
            classify ["is-odd", "-", "is-odd-3.0.1.tgz"]
                `shouldBe` Tarball (unscoped "is-odd") "is-odd-3.0.1.tgz"
        it "routes a scoped tarball (two segments) to its artifact" $
            -- The basename drops the scope: @\@babel\/code-frame@ → @code-frame-7.0.0.tgz@.
            classify ["@babel", "code-frame", "-", "code-frame-7.0.0.tgz"]
                `shouldBe` Tarball (scoped "babel" "code-frame") "code-frame-7.0.0.tgz"
        it "routes a scoped tarball (one decoded segment) to its artifact" $
            classify ["@babel/code-frame", "-", "code-frame-7.0.0.tgz"]
                `shouldBe` Tarball (scoped "babel" "code-frame") "code-frame-7.0.0.tgz"

    describe "classify — meta-routes (matched before any package)" $ do
        it "routes /-/ping to Ping" $
            classify ["-", "ping"] `shouldBe` Ping
        it "routes /-/v1/search to Search" $
            classify ["-", "v1", "search"] `shouldBe` Search
        it "treats an unknown /-/… meta-route as Unsupported, never a package" $
            classify ["-", "whoami"] `shouldBe` Unsupported
        it "treats the dist-tags meta-route as Unsupported (not modelled yet)" $
            classify ["-", "package", "is-odd", "dist-tags"] `shouldBe` Unsupported

    describe "classify — unrecognised paths deny by default" $ do
        it "routes the empty path to Unsupported" $
            classify [] `shouldBe` Unsupported
        it "routes a bare slash (one empty segment) to Unsupported" $
            classify [""] `shouldBe` Unsupported
        it "routes a non-.tgz artifact-shaped path to Unsupported" $
            classify ["is-odd", "-", "is-odd-3.0.1.zip"] `shouldBe` Unsupported
        it "routes a bare \".tgz\" (no name before the suffix) to Unsupported" $
            -- Pins the isTarballFile length guard: the file must be longer than
            -- @.tgz@, so the exact suffix alone is not a tarball.
            classify ["is-odd", "-", ".tgz"] `shouldBe` Unsupported
        it "routes a version-manifest request to Unsupported (not modelled yet)" $
            -- @GET /{pkg}/{version}@ is a later slice; today it is not a packument.
            classify ["is-odd", "3.0.1"] `shouldBe` Unsupported
        it "routes a scope with no package name to Unsupported" $
            classify ["@babel"] `shouldBe` Unsupported
        it "routes a scope with an empty trailing name to Unsupported" $
            -- Reachable from @\/\@scope%2F@: WAI decodes @%2F@ to one segment
            -- @"\@babel\/"@, whose base name is empty — a degenerate scoped name.
            classify ["@babel/"] `shouldBe` Unsupported
        it "routes an empty scope (\"@\" then name) to Unsupported" $
            -- @mkScope "@"@ strips to @""@, which would render as @\/code-frame@.
            classify ["@", "code-frame"] `shouldBe` Unsupported
        it "routes a scoped name whose base still contains a slash to Unsupported" $
            -- An npm name never contains @\'\/\'@ beyond the scope separator.
            classify ["@babel/code/frame"] `shouldBe` Unsupported
        it "routes trailing junk after a package to Unsupported" $
            classify ["is-odd", "extra", "junk"] `shouldBe` Unsupported

    describe "classify — unsafe path components deny by default" $ do
        -- A single WAI-decoded segment can carry traversal/separator/control
        -- content; 'classify' must never accept it as a name, scope, or file,
        -- since the component is interpolated into the upstream URL downstream.
        it "rejects an unscoped name with an embedded slash" $
            -- Reachable from @\/foo%2Fbar@: WAI decodes @%2F@ to one segment
            -- @"foo\/bar"@; accepting it as a packument would smuggle a path.
            classify ["foo/bar"] `shouldBe` Unsupported
        it "rejects an unscoped name with an embedded backslash" $
            classify ["foo\\bar"] `shouldBe` Unsupported
        it "rejects the parent-directory name \"..\"" $
            classify [".."] `shouldBe` Unsupported
        it "rejects the current-directory name \".\"" $
            classify ["."] `shouldBe` Unsupported
        it "rejects an unscoped name with a tab (control) character" $
            classify ["foo\tbar"] `shouldBe` Unsupported
        it "rejects an unscoped name with a NUL character" $
            classify ["foo\0bar"] `shouldBe` Unsupported
        it "rejects a scope of \"..\" (one decoded segment)" $
            classify ["@../pkg"] `shouldBe` Unsupported
        it "rejects a scope of \"..\" (two segments)" $
            classify ["@..", "pkg"] `shouldBe` Unsupported
        it "rejects a tarball filename that escapes via traversal" $
            -- The filename ends in @.tgz@ yet contains @..\/@: the suffix guard
            -- alone is not enough, so the safe-component check must reject it.
            classify ["is-odd", "-", "../evil.tgz"] `shouldBe` Unsupported
        it "rejects a tarball filename with an embedded slash" $
            classify ["is-odd", "-", "sub/is-odd-3.0.1.tgz"] `shouldBe` Unsupported

    describe "classify — real names still classify (no over-rejection)" $ do
        -- Guard against the safe-component check rejecting plausibly-real names:
        -- interior dots, hyphens, and uppercase are all fine — this is a security
        -- boundary, not an npm-policy validator.
        it "accepts an unscoped name with interior dots" $
            classify ["lodash.merge"] `shouldBe` Packument (unscoped "lodash.merge")
        it "accepts another dotted unscoped name" $
            classify ["is.odd"] `shouldBe` Packument (unscoped "is.odd")
        it "accepts a hyphenated unscoped name" $
            classify ["is-odd"] `shouldBe` Packument (unscoped "is-odd")
        it "accepts a scoped name in two segments" $
            classify ["@babel", "code-frame"]
                `shouldBe` Packument (scoped "babel" "code-frame")
        it "accepts a scoped name in one decoded segment" $
            classify ["@babel/code-frame"]
                `shouldBe` Packument (scoped "babel" "code-frame")
        it "accepts the @types scope" $
            classify ["@types", "node"] `shouldBe` Packument (scoped "types" "node")

    describe "properties" $
        -- The safety invariant: no hostile path yields an accepted route whose
        -- structural components are unsafe. The generator frequently emits
        -- hostile fragments AND real-looking names, so it exercises both denied
        -- and accepted routes (the classification below proves it is not
        -- vacuous). For each accepted 'Packument'/'Tarball' we check every
        -- component — scope, base name, and (for tarballs) the file — with the
        -- same 'isSafeComponent' rule the router enforces. We split the rendered
        -- name back into its components rather than scanning it whole, because a
        -- legitimate scoped name renders as @\@scope\/base@ and so contains the
        -- structural @\'\/\'@ separator by design.
        it "an accepted route never carries an unsafe component" $
            hedgehog $ do
                segs <- forAll genSegments
                let route = classify segs
                -- Non-vacuity: the same generator must reach both arms often.
                H.cover 5 "accepted (Packument/Tarball)" (isAccepted route)
                H.cover 5 "denied (Unsupported/Ping/Search)" (not (isAccepted route))
                case route of
                    Packument pn ->
                        H.assert (all safe (nameComponents pn))
                    Tarball pn file ->
                        H.assert (all safe (file : nameComponents pn))
                    _ -> pure ()

-- | Whether a route is an accepted package route (the arms the invariant binds).
isAccepted :: Route -> Bool
isAccepted = \case
    Packument _ -> True
    Tarball _ _ -> True
    _ -> False

{- | The structural components of an accepted name — its scope (if any) and its
base name — recovered from the public surface. The base name is the rendered
display form with any @\@scope\/@ prefix stripped, so each component is checked
on its own rather than across the scope separator.
-}
nameComponents :: PackageName -> [Text]
nameComponents pn =
    case pkgNamespace pn of
        Nothing -> [renderPackageName pn]
        -- A scoped name renders as "@scope/base"; recover [scope, base] by
        -- dropping the "@scope/" prefix, so neither component is judged across
        -- the structural separator.
        Just s ->
            let scopeTxt = unScope s
                base = fromMaybe (renderPackageName pn) (T.stripPrefix ("@" <> scopeTxt <> "/") (renderPackageName pn))
             in [scopeTxt, base]

{- | The router's safety rule, restated here so the property pins the externally
observable guarantee independently of the implementation: a component is safe
iff it is non-empty, is not @"."@\/@".."@, and carries no @\'\/\'@, @\'\\\\\'@,
or control character.
-}
safe :: Text -> Bool
safe c =
    not (T.null c)
        && c /= "."
        && c /= ".."
        && T.all (\ch -> ch /= '/' && ch /= '\\' && not (isControl ch)) c

{- | A path generator that mixes real-looking segments with hostile fragments
(@"."@, @".."@, slashes, backslashes, control chars, empties, @"-"@, @"@"@), so
'classify' is driven down both its accepting and its denying paths.
-}
genSegments :: Gen [Text]
genSegments = Gen.list (Range.linear 0 4) genSegment

-- | One segment: usually a benign name, often a hostile or structural fragment.
genSegment :: Gen Text
genSegment =
    Gen.frequency
        [ (5, genName)
        , (2, genScopedSegment)
        , (4, Gen.element hostile)
        ]
  where
    hostile =
        [ ""
        , "."
        , ".."
        , "-"
        , "@"
        , "a/b"
        , "a\\b"
        , "a\tb"
        , "a\0b"
        , "../evil.tgz"
        , "x.tgz"
        ]

-- | A benign unscoped-style name: letters, digits, and the safe punctuation npm allows.
genName :: Gen Text
genName = Gen.text (Range.linear 1 8) (Gen.frequency [(8, Gen.alphaNum), (2, Gen.element ['.', '-', '_'])])

-- | A @\@scope\/name@ one-segment form (and bare-scope @\@scope@) to drive scoped parsing.
genScopedSegment :: Gen Text
genScopedSegment = do
    scope <- genName
    Gen.choice
        [ pure ("@" <> scope)
        , (\base -> "@" <> scope <> "/" <> base) <$> genName
        ]
