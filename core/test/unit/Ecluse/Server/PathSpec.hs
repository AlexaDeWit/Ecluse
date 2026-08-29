-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.PathSpec (spec) where

import Test.Hspec

import Ecluse.Core.Server.Path (encodeComponent, isSafeComponent, mkFilename, unFilename)

{- | The shared URL-path vocabulary: the artifact-name type, and the ecosystem-independent
component-safety gate. No ecosystem's routes live here. Every registry shares the /threat/ of a
decoded path component interpolated into an upstream URL, and these specs pin that boundary.
-}
spec :: Spec
spec = do
    describe "Filename -- the artifact name an artifact route carries" $ do
        -- A refinement, not a bare 'Text': the name is authoritative for fetching the bytes, so
        -- only a component 'isSafeComponent' admits can become one.
        it "reads back the verbatim name it was built from" $
            fmap unFilename (mkFilename "is-odd-3.0.1.tgz") `shouldBe` Just "is-odd-3.0.1.tgz"
        it "compares equal for the same preserved file name" $
            mkFilename "is-odd-3.0.1.tgz" `shouldBe` mkFilename "is-odd-3.0.1.tgz"
        it "compares unequal for different preserved file names" $
            mkFilename "is-odd-3.0.1.tgz" `shouldNotBe` mkFilename "is-odd-3.0.2.tgz"
        it "refuses a name that is not a safe path component" $
            mkFilename "../etc/passwd" `shouldBe` Nothing

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
            -- A once-decoded segment carrying '%2e%2e%2f' must have its '%' re-encoded,
            -- so the upstream never sees a live escape.
            encodeComponent "foo%2e%2e%2fbar" `shouldBe` "foo%252e%252e%252fbar"
        it "percent-encodes a literal slash" $
            encodeComponent "a/b" `shouldBe` "a%2Fb"
        it "percent-encodes the URL-reserved query, fragment, and sub-delimiter characters" $
            encodeComponent "a?b#c;d" `shouldBe` "a%3Fb%23c%3Bd"
        it "percent-encodes the RFC 3986 sub-delimiters !'()* (guarding against any widening of the query-safe set)" $
            -- A widening of http-types' unreserved set could silently pass these bytes through.
            -- Pinning them keeps the "percent-encodes every other byte" contract honest.
            encodeComponent "a!b'c(d)e*f" `shouldBe` "a%21b%27c%28d%29e%2Af"
        it "percent-encodes a space" $
            encodeComponent "a b" `shouldBe` "a%20b"
        it "percent-encodes a leading '@' (the scope sigil is added structurally, never within a component)" $
            encodeComponent "@scope" `shouldBe` "%40scope"
        it "encodes a multi-byte UTF-8 character byte-by-byte" $
            -- 'é' is U+00E9, two UTF-8 bytes C3 A9, each percent-encoded.
            encodeComponent "café" `shouldBe` "caf%C3%A9"
