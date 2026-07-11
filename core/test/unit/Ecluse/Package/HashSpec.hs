-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Package.HashSpec (spec) where

import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Package.Hash (
    HashAlg (..),
    hashAlg,
    hashValue,
    mkHash,
    mkSriHashes,
    parseHashAlg,
    renderHashAlg,
    sriAlgorithm,
 )

spec :: Spec
spec = do
    describe "mkHash" $ do
        it "accepts a well-formed 40-hex SHA-1 shasum" $
            (hashAlg <$> mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709") `shouldBe` Right SHA1

        it "accepts a well-formed sha512 SRI integrity" $
            (hashAlg <$> mkHash SRI "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==")
                `shouldBe` Right SRI

        it "rejects a multi-component integrity (one Hash holds exactly one component)" $
            -- npm may serve "sha512-… sha256-…" on the wire; that shape is split by
            -- mkSriHashes into one Hash per component, never carried whole, so the
            -- floor ranking and the worker's byte verification read the same
            -- component. A joined string does not construct a single Hash.
            mkHash
                SRI
                "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg== sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
                `shouldSatisfy` isLeft

        it "rejects an SRI component padded with surrounding whitespace" $
            -- The stored value is read verbatim by the first-dash accessors, so a
            -- padded component would corrupt the resolved algorithm and body.
            mkHash SRI " sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU= " `shouldSatisfy` isLeft

        it "accepts a well-formed sha384 SRI (a modelled algorithm)" $
            -- sha384 is a real, modelled SRI algorithm: validated and accepted as
            -- well-formed, and it resolves to 'SHA384' for the strength/floor logic.
            mkHash SRI "sha384-OLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"
                `shouldSatisfy` isRight

        it "accepts a well-formed 96-hex SHA-384 digest" $
            (hashAlg <$> mkHash SHA384 "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b")
                `shouldBe` Right SHA384

        it "rejects a 94-character (wrong-length) hex SHA-384" $
            mkHash SHA384 "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b" `shouldSatisfy` isLeft

        it "rejects a non-hex SHA-384" $
            mkHash SHA384 "zzb060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b" `shouldSatisfy` isLeft

        it "preserves the original (upper-case) hex value while validating case-insensitively" $
            (hashValue <$> mkHash SHA1 "DA39A3EE5E6B4B0D3255BFEF95601890AFD80709")
                `shouldBe` Right "DA39A3EE5E6B4B0D3255BFEF95601890AFD80709"

        it "rejects an empty digest" $
            mkHash SHA1 "" `shouldSatisfy` isLeft

        it "rejects a 39-character (odd-length) hex SHA-1" $
            mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd8070" `shouldSatisfy` isLeft

        it "rejects an over-long (21-byte) hex SHA-1" $
            mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709aa" `shouldSatisfy` isLeft

        it "rejects a non-hex SHA-1" $
            mkHash SHA1 "zz39a3ee5e6b4b0d3255bfef95601890afd80709" `shouldSatisfy` isLeft

        it "rejects a truncated SRI (alg prefix, no body)" $
            mkHash SRI "sha512-" `shouldSatisfy` isLeft

        it "rejects an SRI with a non-base64 body" $
            mkHash SRI "sha512-not base64!!" `shouldSatisfy` isLeft

        it "rejects an SRI whose base64 body is the wrong length for its algorithm" $
            -- Valid base64, but decodes to 6 bytes, not sha256's 32.
            mkHash SRI "sha256-Zm9vYmFy" `shouldSatisfy` isLeft

        it "rejects an SRI naming an algorithm outside the Subresource-Integrity set" $
            -- The SRI set is sha256/sha384/sha512; a well-formed sha1 base64 body is still
            -- not a valid SRI (sha1 is not an SRI algorithm), so it does not construct.
            mkHash SRI "sha1-2jmj7l5rSw0yVb/vlWAYkK/YBwk=" `shouldSatisfy` isLeft

    describe "mkSriHashes" $ do
        it "splits a multi-component wire string into one Hash per component" $
            (fmap hashValue <$> mkSriHashes "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg== sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=")
                `shouldBe` Right
                    ( "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="
                        :| ["sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="]
                    )

        it "yields a singleton for the common single-component wire string" $
            (fmap hashValue <$> mkSriHashes "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==")
                `shouldBe` Right ("sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==" :| [])

        it "rejects the whole wire string when any component is malformed" $
            -- All-or-nothing: a partially-valid attacker-shaped value never yields a
            -- partial digest set the gates would then reason over.
            mkSriHashes "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg== sha256-short"
                `shouldSatisfy` isLeft

        it "rejects an empty or all-whitespace wire string" $ do
            mkSriHashes "" `shouldSatisfy` isLeft
            mkSriHashes "   " `shouldSatisfy` isLeft

        it "never yields a Hash from non-digest text, for any algorithm" $
            -- The fail-closed property: a value built only from characters outside the hex
            -- and base64 alphabets is never a well-formed digest of any algorithm.
            hedgehog $ do
                alg <- forAll (Gen.element [SHA1, SHA256, SHA384, SHA512, MD5, Blake2b, SRI])
                junk <- forAll (Gen.text (Range.linear 0 80) (Gen.element ("!@#$%& *()" :: String)))
                isLeft (mkHash alg junk) === True

    describe "algorithm vocabulary" $ do
        it "round-trips sha384 through render/parse" $ do
            renderHashAlg SHA384 `shouldBe` "sha384"
            parseHashAlg "sha384" `shouldBe` Right SHA384
            parseHashAlg "SHA-384" `shouldBe` Right SHA384
            parseHashAlg (renderHashAlg SHA384) `shouldBe` Right SHA384

        it "accepts canonical names and single-dash aliases, case- and whitespace-insensitively" $ do
            parseHashAlg "sha256" `shouldBe` Right SHA256
            parseHashAlg "SHA-256" `shouldBe` Right SHA256
            parseHashAlg "  Sha512  " `shouldBe` Right SHA512
            parseHashAlg "blake2b" `shouldBe` Right Blake2b
        it "rejects arbitrary internal dashes rather than masking a typo" $ do
            parseHashAlg "s-h-a--2-5-6" `shouldSatisfy` isLeft
            parseHashAlg "sha--256" `shouldSatisfy` isLeft
            parseHashAlg "sha-2-56" `shouldSatisfy` isLeft
        it "rejects the sri wrapper, which names no algorithm of its own" $
            parseHashAlg "sri" `shouldSatisfy` isLeft

        it "resolves a sha384 SRI prefix to SHA384 (was Nothing before it was modelled)" $
            sriAlgorithm "sha384-OLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"
                `shouldBe` Just SHA384
