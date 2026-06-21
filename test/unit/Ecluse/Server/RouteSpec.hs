module Ecluse.Server.RouteSpec (spec) where

import Test.Hspec

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (PackageName, mkPackageName, mkScope)
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
