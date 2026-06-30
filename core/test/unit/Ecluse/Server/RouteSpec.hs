module Ecluse.Server.RouteSpec (spec) where

import Hedgehog (forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Types.Method (methodGet, methodPut)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Server.Route (Filename (Filename), Route (..), denyAll, encodeComponent, isSafeComponent)
import Ecluse.Core.Version (Version, mkVersion)

-- | An unscoped package identity, for building 'Tarball' routes in the assertions.
unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

-- | A version, for the parsed coordinate a 'Tarball' route carries.
npmVersion :: Text -> Version
npmVersion = mkVersion Npm

{- | The agnostic routing layer: the shared 'Route' set, the deny-by-default
classifier, and the ecosystem-independent component-safety gate.

No ecosystem path grammar lives here -- that is each adapter's classifier (e.g.
"Ecluse.Core.Registry.Npm.Route"). These specs pin the neutral routing boundary: the
default classifier denies everything, and 'isSafeComponent' is the shared
traversal gate.
-}
spec :: Spec
spec = do
    describe "denyAll -- the agnostic default classifier" $ do
        it "denies a package-shaped path (no ecosystem grammar is built in)" $
            denyAll methodGet ["is-odd"] `shouldBe` Unsupported
        it "denies a meta-route-shaped path" $
            denyAll methodGet ["-", "ping"] `shouldBe` Unsupported
        it "denies the empty path" $
            denyAll methodGet [] `shouldBe` Unsupported
        it "denies a PUT (a publish) too -- it knows no write grammar either" $
            denyAll methodPut ["is-odd"] `shouldBe` Unsupported
        it "denies every path it is given, under any method (deny by default)" $
            hedgehog $ do
                method <- forAll (Gen.element [methodGet, methodPut, "HEAD", "DELETE"])
                segs <- forAll (Gen.list (Range.linear 0 5) (Gen.text (Range.linear 0 8) Gen.unicode))
                denyAll method segs H.=== Unsupported

    describe "Filename -- the agnostic artifact-name type" $ do
        -- The verbatim on-the-wire name a 'Tarball' route carries is held as a
        -- distinct type (not a bare 'Text') because it is authoritative for fetching
        -- the bytes; its equality is by the wrapped name, so two routes naming the
        -- same artifact compare equal and two different files do not.
        it "distinguishes two artifact routes by their preserved filename" $
            Tarball (unscoped "is-odd") (npmVersion "3.0.1") (Filename "is-odd-3.0.1.tgz")
                `shouldBe` Tarball (unscoped "is-odd") (npmVersion "3.0.1") (Filename "is-odd-3.0.1.tgz")
        it "treats two routes with different preserved filenames as unequal" $
            Tarball (unscoped "is-odd") (npmVersion "3.0.1") (Filename "is-odd-3.0.1.tgz")
                `shouldNotBe` Tarball (unscoped "is-odd") (npmVersion "3.0.1") (Filename "is-odd-3.0.2.tgz")

    describe "isSafeComponent -- the shared traversal gate" $ do
        it "accepts an ordinary name" $
            isSafeComponent "is-odd" `shouldBe` True
        it "accepts a name with interior dots, hyphens, underscores, and digits" $
            isSafeComponent "lodash.merge_2" `shouldBe` True
        it "rejects the empty component" $
            isSafeComponent "" `shouldBe` False
        it "rejects \".\"" $
            isSafeComponent "." `shouldBe` False
        it "rejects \"..\"" $
            isSafeComponent ".." `shouldBe` False
        it "rejects a component with an embedded slash" $
            isSafeComponent "foo/bar" `shouldBe` False
        it "rejects a component with an embedded backslash" $
            isSafeComponent "foo\\bar" `shouldBe` False
        it "rejects a component with a control character" $
            isSafeComponent "foo\tbar" `shouldBe` False
        it "rejects a component with a NUL" $
            isSafeComponent "foo\0bar" `shouldBe` False

    describe "encodeComponent -- the shared component percent-encoder" $ do
        it "leaves an ordinary name unchanged (only unreserved characters)" $
            encodeComponent "is-odd" `shouldBe` "is-odd"
        it "leaves interior dots, hyphens, underscores, digits, and tildes unchanged" $
            encodeComponent "lodash.merge_2~x" `shouldBe` "lodash.merge_2~x"
        it "percent-encodes a literal percent sign (closing the once-decoded re-encode gap)" $
            -- The crux of the defect: a once-decoded segment carrying '%2e%2e%2f'
            -- must have its '%' re-encoded so the upstream never sees a live escape.
            encodeComponent "foo%2e%2e%2fbar" `shouldBe` "foo%252e%252e%252fbar"
        it "percent-encodes a literal slash" $
            encodeComponent "a/b" `shouldBe` "a%2Fb"
        it "percent-encodes the URL-reserved query, fragment, and sub-delimiter characters" $
            encodeComponent "a?b#c;d" `shouldBe` "a%3Fb%23c%3Bd"
        it "percent-encodes a space" $
            encodeComponent "a b" `shouldBe` "a%20b"
        it "percent-encodes a leading '@' (the scope sigil is added structurally, never within a component)" $
            encodeComponent "@scope" `shouldBe` "%40scope"
        it "encodes a multi-byte UTF-8 character byte-by-byte" $
            -- 'é' is U+00E9, two UTF-8 bytes C3 A9, each percent-encoded.
            encodeComponent "café" `shouldBe` "caf%C3%A9"
