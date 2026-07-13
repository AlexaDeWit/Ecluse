-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.PathSpec (spec) where

import Test.Hspec

import Ecluse.Core.Server.Path (Filename (Filename), encodeComponent, isSafeComponent)

{- | The shared URL-path vocabulary: the artifact-name type, and the
ecosystem-independent component-safety gate.

No ecosystem's routes live here -- each declares its own table (npm's is
"Ecluse.Core.Registry.Npm.Route"). What every registry shares is the /threat/: a decoded
path component interpolated into an upstream URL. These specs pin that boundary.
-}
spec :: Spec
spec = do
    describe "Filename -- the artifact name an artifact route carries" $ do
        -- The verbatim on-the-wire name is held as a distinct type (not a bare 'Text')
        -- because it is authoritative for fetching the bytes; its equality is by the
        -- wrapped name, so two routes naming the same artifact compare equal and two
        -- different files do not.
        it "compares equal for the same preserved file name" $
            Filename "is-odd-3.0.1.tgz" `shouldBe` Filename "is-odd-3.0.1.tgz"
        it "compares unequal for different preserved file names" $
            Filename "is-odd-3.0.1.tgz" `shouldNotBe` Filename "is-odd-3.0.2.tgz"

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
