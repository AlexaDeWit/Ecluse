module Ecluse.Server.RouteSpec (spec) where

import Hedgehog (forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Server.Route (Route (..), denyAll, isSafeComponent)

{- | The agnostic routing layer: the shared 'Route' set, the deny-by-default
classifier, and the ecosystem-independent component-safety gate.

No ecosystem path grammar lives here — that is each adapter's classifier (e.g.
"Ecluse.Registry.Npm.Route"). These specs pin the neutral routing boundary: the
default classifier denies everything, and 'isSafeComponent' is the shared
traversal gate.
-}
spec :: Spec
spec = do
    describe "denyAll — the agnostic default classifier" $ do
        it "denies a package-shaped path (no ecosystem grammar is built in)" $
            denyAll ["is-odd"] `shouldBe` Unsupported
        it "denies a meta-route-shaped path" $
            denyAll ["-", "ping"] `shouldBe` Unsupported
        it "denies the empty path" $
            denyAll [] `shouldBe` Unsupported
        it "denies every path it is given (deny by default)" $
            hedgehog $ do
                segs <- forAll (Gen.list (Range.linear 0 5) (Gen.text (Range.linear 0 8) Gen.unicode))
                denyAll segs H.=== Unsupported

    describe "isSafeComponent — the shared traversal gate" $ do
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
