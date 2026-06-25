module Ecluse.Package.IntegritySpec (spec) where

import Test.Hspec

import Ecluse.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Hash (Hash),
    HashAlg (Blake2b, MD5, SHA1, SHA256, SHA512, SRI),
 )
import Ecluse.Package.Integrity (
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    assertedAlg,
    classifyArtifacts,
    defaultMinIntegrity,
    integrityStrength,
    meetsFloor,
    mkMinIntegrity,
    parseMinIntegrity,
    renderMinIntegrity,
    unMinIntegrity,
 )

-- A tarball carrying a chosen set of integrity digests; everything else is inert.
artifactWith :: [Hash] -> Artifact
artifactWith hs =
    Artifact
        { artFilename = "thing.tgz"
        , artUrl = "https://example.test/thing.tgz"
        , artKind = Tarball
        , artHashes = hs
        , artSize = Nothing
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }

spec :: Spec
spec = do
    describe "integrityStrength" $ do
        it "ranks the broken algorithms below SHA-256" $ do
            (integrityStrength SHA1 < integrityStrength SHA256) `shouldBe` True
            (integrityStrength MD5 < integrityStrength SHA256) `shouldBe` True

        it "ranks the modern long digests at or above SHA-256" $ do
            (integrityStrength SHA512 >= integrityStrength SHA256) `shouldBe` True
            (integrityStrength Blake2b >= integrityStrength SHA256) `shouldBe` True
            (integrityStrength SHA512 > integrityStrength SHA256) `shouldBe` True

        it "ranks a bare SRI below every real algorithm (resolve it first)" $
            (integrityStrength SRI < integrityStrength MD5) `shouldBe` True

    describe "assertedAlg" $ do
        it "reads a plain tag directly" $
            assertedAlg (Hash SHA256 "abc") `shouldBe` Just SHA256

        it "resolves an SRI to its inner algorithm (sha512, sha256)" $ do
            assertedAlg (Hash SRI "sha512-abc") `shouldBe` Just SHA512
            assertedAlg (Hash SRI "sha256-abc") `shouldBe` Just SHA256

        it "yields Nothing for an SRI whose inner algorithm is unrecognised" $ do
            assertedAlg (Hash SRI "sha384-abc") `shouldBe` Nothing
            assertedAlg (Hash SRI "garbage") `shouldBe` Nothing

    describe "meetsFloor" $ do
        it "admits an algorithm at or above the default (SHA-256) floor" $ do
            meetsFloor defaultMinIntegrity SHA256 `shouldBe` True
            meetsFloor defaultMinIntegrity SHA512 `shouldBe` True
            meetsFloor defaultMinIntegrity Blake2b `shouldBe` True

        it "rejects an algorithm below the default floor (SHA-1, MD5)" $ do
            meetsFloor defaultMinIntegrity SHA1 `shouldBe` False
            meetsFloor defaultMinIntegrity MD5 `shouldBe` False

        it "rejects SHA-256 when the floor is raised to SHA-512" $ do
            sha512Floor <- expectRight (mkMinIntegrity SHA512)
            meetsFloor sha512Floor SHA256 `shouldBe` False
            meetsFloor sha512Floor SHA512 `shouldBe` True
            meetsFloor sha512Floor Blake2b `shouldBe` True

    describe "mkMinIntegrity / parseMinIntegrity" $ do
        it "defaults to SHA-256" $
            unMinIntegrity defaultMinIntegrity `shouldBe` SHA256

        it "accepts an algorithm at or above the hard SHA-256 floor" $ do
            (unMinIntegrity <$> mkMinIntegrity SHA256) `shouldBe` Right SHA256
            (unMinIntegrity <$> mkMinIntegrity SHA512) `shouldBe` Right SHA512
            (unMinIntegrity <$> mkMinIntegrity Blake2b) `shouldBe` Right Blake2b

        it "rejects a floor below SHA-256 (a sub-floor is a configuration error)" $ do
            mkMinIntegrity SHA1 `shouldSatisfy` isLeft
            mkMinIntegrity MD5 `shouldSatisfy` isLeft

        it "parses algorithm names, case- and separator-insensitively" $ do
            (unMinIntegrity <$> parseMinIntegrity "sha256") `shouldBe` Right SHA256
            (unMinIntegrity <$> parseMinIntegrity "SHA-512") `shouldBe` Right SHA512
            (unMinIntegrity <$> parseMinIntegrity "blake2b") `shouldBe` Right Blake2b

        it "rejects a below-floor name and an unknown name distinctly" $ do
            parseMinIntegrity "sha1" `shouldSatisfy` isLeft
            parseMinIntegrity "frobnicate" `shouldSatisfy` isLeft

        it "round-trips render and parse" $
            parseMinIntegrity (renderMinIntegrity defaultMinIntegrity) `shouldBe` Right defaultMinIntegrity

    describe "classifyArtifacts" $ do
        let classify floorAlg hs =
                classifyArtifacts floorAlg (artifactWith hs :| [])

        it "MeetsFloor when a digest clears the floor (SHA-256, sha512-SRI)" $ do
            classify defaultMinIntegrity [Hash SHA256 "x"] `shouldBe` MeetsFloor
            classify defaultMinIntegrity [Hash SRI "sha512-x"] `shouldBe` MeetsFloor

        it "MeetsFloor when a strong digest sits beside a weak one" $
            classify defaultMinIntegrity [Hash SHA1 "x", Hash SHA256 "y"] `shouldBe` MeetsFloor

        it "BelowFloor for a SHA-1-only version (a digest, but too weak)" $
            classify defaultMinIntegrity [Hash SHA1 "x"] `shouldBe` BelowFloor

        it "NoIntegrity for a version carrying no digest at all" $
            classify defaultMinIntegrity [] `shouldBe` NoIntegrity

        it "BelowFloor for a SHA-256-only version when the floor is SHA-512" $ do
            sha512Floor <- expectRight (mkMinIntegrity SHA512)
            classify sha512Floor [Hash SHA256 "x"] `shouldBe` BelowFloor

-- Local: assert a 'Right' and return its value, failing the example otherwise.
expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> fail ("expected Right, got Left " <> show e)) pure
