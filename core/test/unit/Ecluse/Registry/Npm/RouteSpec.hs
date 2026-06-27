module Ecluse.Registry.Npm.RouteSpec (spec) where

import Data.Char (isControl)
import Data.Text qualified as T
import Hedgehog (Gen, forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    PackageName,
    mkPackageName,
    mkScope,
    pkgNamespace,
    renderPackageName,
    unScope,
 )
import Network.HTTP.Types.Method (methodGet, methodPut)

import Ecluse.Core.Registry.Npm.Route qualified as Route
import Ecluse.Core.Server.Route (Filename (Filename), Route (..))
import Ecluse.Core.Version (Version, mkVersion)

{- | The read classification (a @GET@) of an npm path, the existing routing table's
subject: 'Route.classify' restricted to a read method, so each @pathInfo → Route@
assertion below reads as before now that the classifier is method-aware. A @HEAD@
classifies identically (the dispatcher answers it bodiless), so @GET@ stands for every
read method here.
-}
classify :: [Text] -> Route
classify = Route.classify methodGet

{- | The __publish__ classification (a @PUT@) of an npm path: 'Route.classify' at the
publish method, so the publish routing cases assert @PUT \/{pkg} → Publish@ apart from
the read table.
-}
publish :: [Text] -> Route
publish = Route.classify methodPut

-- | An unscoped npm package identity, for building expected 'Route's.
unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

-- | A scoped npm package identity (scope, base name), for expected 'Route's.
scoped :: Text -> Text -> PackageName
scoped scope = mkPackageName Npm (Just (mkScope scope))

-- | An npm version, for the parsed coordinate a 'Tarball' route carries.
npmVersion :: Text -> Version
npmVersion = mkVersion Npm

{- | The npm routing table, asserted as @pathInfo → Route@. The path is
percent-decoded before it reaches 'classify', so each scoped case appears in
__both__ wire encodings (one decoded segment @\@scope\/pkg@ and two segments
@\@scope@,@pkg@); both must agree.
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

    describe "classify — tarballs (the parsed artifact coordinate)" $ do
        it "routes an unscoped tarball to its artifact, parsing the version" $
            classify ["is-odd", "-", "is-odd-3.0.1.tgz"]
                `shouldBe` Tarball (unscoped "is-odd") (npmVersion "3.0.1") (Filename "is-odd-3.0.1.tgz")
        it "routes a scoped tarball (two segments) to its artifact" $
            -- The basename drops the scope: @\@babel\/code-frame@ → @code-frame-7.0.0.tgz@.
            classify ["@babel", "code-frame", "-", "code-frame-7.0.0.tgz"]
                `shouldBe` Tarball (scoped "babel" "code-frame") (npmVersion "7.0.0") (Filename "code-frame-7.0.0.tgz")
        it "routes a scoped tarball (one decoded segment) to its artifact" $
            classify ["@babel/code-frame", "-", "code-frame-7.0.0.tgz"]
                `shouldBe` Tarball (scoped "babel" "code-frame") (npmVersion "7.0.0") (Filename "code-frame-7.0.0.tgz")
        it "reads a prerelease-hyphen version out of the basename verbatim" $
            -- The version itself carries hyphens (@1.0.0-rc.1@); the parse must split
            -- on the FIRST @{name}-@ boundary, taking everything after as the version.
            classify ["pkg", "-", "pkg-1.0.0-rc.1.tgz"]
                `shouldBe` Tarball (unscoped "pkg") (npmVersion "1.0.0-rc.1") (Filename "pkg-1.0.0-rc.1.tgz")
        it "preserves the filename verbatim, not one rebuilt from (name, version)" $
            -- The file's parsed version round-trips and the Filename is byte-identical
            -- to what arrived — it, not a reconstruction, fetches the bytes.
            classify ["@babel/code-frame", "-", "code-frame-7.0.0.tgz"]
                `shouldBe` Tarball (scoped "babel" "code-frame") (npmVersion "7.0.0") (Filename "code-frame-7.0.0.tgz")
        it "denies a basename that does not match the requested package (path-confusion)" $
            -- The file names a DIFFERENT package's artifact under @is-odd@'s path; the
            -- basename does not begin with @is-odd-@, so it is denied, never coerced
            -- into a fabricated @is-odd@ coordinate.
            classify ["is-odd", "-", "is-even-3.0.1.tgz"] `shouldBe` Unsupported
        it "denies a basename that is the bare package name with no version" $
            -- @{name}.tgz@ has no @-{version}@ run, so there is no coordinate to parse.
            classify ["is-odd", "-", "is-odd.tgz"] `shouldBe` Unsupported
        it "denies a basename that is the name and a trailing hyphen but empty version" $
            classify ["is-odd", "-", "is-odd-.tgz"] `shouldBe` Unsupported

    describe "classify — meta-routes (matched before any package)" $ do
        it "routes /-/ping to Ping" $
            classify ["-", "ping"] `shouldBe` Ping
        it "routes /-/v1/search to Search" $
            classify ["-", "v1", "search"] `shouldBe` Search
        it "treats an unknown /-/… meta-route as Unsupported, never a package" $
            classify ["-", "whoami"] `shouldBe` Unsupported
        it "treats the dist-tags meta-route as Unsupported" $
            classify ["-", "package", "is-odd", "dist-tags"] `shouldBe` Unsupported

    describe "classify — publish (PUT /{pkg}, the method-aware write route)" $ do
        it "routes a PUT of an unscoped package to Publish" $
            publish ["is-odd"] `shouldBe` Publish (unscoped "is-odd")
        it "routes a PUT of a scoped package (two segments) to Publish" $
            publish ["@acme", "widget"] `shouldBe` Publish (scoped "acme" "widget")
        it "routes a PUT of a scoped package (one decoded segment) to Publish" $
            publish ["@acme/widget"] `shouldBe` Publish (scoped "acme" "widget")
        it "agrees on the same Publish route for both scoped encodings" $
            publish ["@acme", "widget"] `shouldBe` publish ["@acme/widget"]
        it "denies a PUT to a tarball slot (a publish is a bare-package path only)" $
            -- The version lives in the body, not the path; a PUT to /{pkg}/-/{file}.tgz
            -- is not a publish.
            publish ["is-odd", "-", "is-odd-3.0.1.tgz"] `shouldBe` Unsupported
        it "denies a PUT to a meta-route" $
            publish ["-", "ping"] `shouldBe` Unsupported
        it "denies a PUT with trailing junk after the package" $
            publish ["is-odd", "extra"] `shouldBe` Unsupported
        it "denies a PUT to the empty path" $
            publish [] `shouldBe` Unsupported
        it "denies a PUT of an unsafe name (embedded slash) — the same component gate as reads" $
            publish ["foo/bar"] `shouldBe` Unsupported
        it "denies a PUT of a bare scope with no package name" $
            publish ["@acme"] `shouldBe` Unsupported
        it "does not publish a GET of the same package (a GET /{pkg} is a Packument)" $
            -- The method, not just the path, decides: the same /{pkg} reads under GET and
            -- publishes under PUT.
            classify ["is-odd"] `shouldBe` Packument (unscoped "is-odd")

    describe "classify — unrecognised paths deny by default" $ do
        it "routes the empty path to Unsupported" $
            classify [] `shouldBe` Unsupported
        it "routes a bare slash (one empty segment) to Unsupported" $
            classify [""] `shouldBe` Unsupported
        it "routes a non-.tgz artifact-shaped path to Unsupported" $
            classify ["is-odd", "-", "is-odd-3.0.1.zip"] `shouldBe` Unsupported
        it "routes a bare \".tgz\" (no name before the suffix) to Unsupported" $
            -- The basename is empty, so it can never match @{name}-{version}@.
            classify ["is-odd", "-", ".tgz"] `shouldBe` Unsupported
        it "routes a version-manifest request to Unsupported" $
            -- @GET /{pkg}/{version}@ is not a packument: a bare package is, but a
            -- trailing version segment is not recognised.
            classify ["is-odd", "3.0.1"] `shouldBe` Unsupported
        it "routes a scope with no package name to Unsupported" $
            classify ["@babel"] `shouldBe` Unsupported
        it "routes a scope with an empty trailing name to Unsupported" $
            -- Reachable from @\/\@scope%2F@: percent-decoding @%2F@ yields one segment
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
        -- A single percent-decoded segment can carry traversal/separator/control
        -- content; 'classify' must never accept it as a name, scope, or file,
        -- since the component is interpolated into the upstream URL downstream.
        it "rejects an unscoped name with an embedded slash" $
            -- Reachable from @\/foo%2Fbar@: percent-decoding @%2F@ yields one segment
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
        -- same safe-component rule the router enforces. We split the rendered
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
                    Tarball pn _ (Filename file) ->
                        H.assert (all safe (file : nameComponents pn))
                    _ -> pure ()

-- | Whether a route is an accepted package route (the arms the invariant binds).
isAccepted :: Route -> Bool
isAccepted = \case
    Packument _ -> True
    Tarball{} -> True
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
