module Ecluse.PackageSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (sampleDetails)

spec :: Spec
spec = do
    describe "mkHash" $ do
        it "accepts a well-formed 40-hex SHA-1 shasum" $
            (hashAlg <$> mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709") `shouldBe` Right SHA1

        it "accepts a well-formed sha512 SRI integrity" $
            (hashAlg <$> mkHash SRI "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg==")
                `shouldBe` Right SRI

        it "accepts a multi-component integrity, validating every component" $
            -- npm may serve "sha512-… sha256-…"; both components must be well-formed.
            mkHash
                SRI
                "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg== sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
                `shouldSatisfy` isRight

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

        it "rejects a multi-component integrity when any component is malformed" $
            mkHash SRI "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg== sha256-short"
                `shouldSatisfy` isLeft

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

        it "resolves a sha384 SRI prefix to SHA384 (was Nothing before it was modelled)" $
            sriAlgorithm "sha384-OLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"
                `shouldBe` Just SHA384

    describe "Scope" $ do
        it "mkScope strips a leading '@'" $
            unScope (mkScope "@myorg") `shouldBe` "myorg"
        it "mkScope leaves an unprefixed scope unchanged" $
            unScope (mkScope "myorg") `shouldBe` "myorg"
        it "renderScope adds the leading '@'" $
            renderScope (mkScope "@myorg") `shouldBe` "@myorg"
        it "normalises scopes regardless of a leading '@'" $
            hedgehog $ do
                s <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
                mkScope ("@" <> s) === mkScope s

    describe "mkPackageName" $ do
        it "renders a scoped npm package as @scope/name" $
            renderPackageName (mkPackageName Npm (Just (mkScope "myorg")) "thing")
                `shouldBe` "@myorg/thing"
        it "renders an unscoped package as just the name" $
            renderPackageName (mkPackageName Npm Nothing "thing") `shouldBe` "thing"
        it "keeps npm canonical names verbatim (case-sensitive)" $
            pkgCanonical (mkPackageName Npm Nothing "Thing") `shouldBe` "Thing"
        it "normalises PyPI names per PEP 503" $
            pkgCanonical (mkPackageName PyPI Nothing "Flask_Thing.X")
                `shouldBe` "flask-thing-x"
        it "treats PyPI names equal up to normalisation" $
            mkPackageName PyPI Nothing "Flask" `shouldBe` mkPackageName PyPI Nothing "flask"

    describe "PackageInfo" $ do
        -- A packument-level fixture: one package, one published version "1.0.0"
        -- tagged "latest", carrying its own publish time on the version snapshot. The
        -- map is keyed by the raw version string, as the type documents.
        let name = mkPackageName Npm Nothing "thing"
            version = mkVersion Npm "1.0.0"
            publishedAt = UTCTime (fromGregorian 2026 6 21) 0
            versionDetails = (sampleDetails name version){pkgLicenses = ["MIT"], pkgPublishedAt = Just publishedAt}
            info =
                PackageInfo
                    { infoName = name
                    , infoVersions = Map.singleton "1.0.0" versionDetails
                    , infoDistTags = Map.singleton "latest" version
                    , infoInvalidEntries = []
                    }
        it "round-trips the package identity through infoName" $
            infoName info `shouldBe` name
        it "retrieves a version's details by its raw version-string key" $
            -- The version put in under "1.0.0" is the one that comes back out, and
            -- it still carries its own parsed 'Version' (the map key is just Text).
            (pkgVersion <$> Map.lookup "1.0.0" (infoVersions info)) `shouldBe` Just version
        it "resolves a dist-tag to the version it points at" $
            Map.lookup "latest" (infoDistTags info) `shouldBe` Just version
        it "carries the per-version publish time on the version snapshot" $
            -- The publish time is folded onto the version's own 'PackageDetails', not a
            -- sibling map; the npm wire @time@ object is reconstructed at serialisation.
            (pkgPublishedAt <$> Map.lookup "1.0.0" (infoVersions info)) `shouldBe` Just (Just publishedAt)
        it "is equal exactly when every field agrees" $ do
            -- Equality is structural over all fields: an identically-built document is
            -- equal, and changing a single field (here the version a dist-tag resolves
            -- to) makes it unequal.
            info `shouldBe` info{infoDistTags = Map.singleton "latest" version}
            info `shouldNotBe` info{infoDistTags = Map.singleton "latest" (mkVersion Npm "2.0.0")}
