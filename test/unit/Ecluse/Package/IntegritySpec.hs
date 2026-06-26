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

        it "ranks SHA-512 and Blake2b as EQUAL — the modern long digests share the top tier" $
            -- The load-bearing invariant of the tier representation: a naive
            -- one-constructor-per-algorithm enum would make these distinct and
            -- silently change which digest wins a strongest-digest comparison. The
            -- worker's tamper gate picks the strongest present digest, so a spurious
            -- tie-break here could prefer the wrong algorithm. They must compare EQ.
            (integrityStrength SHA512 `compare` integrityStrength Blake2b) `shouldBe` EQ

        it "is strictly increasing weakest-to-strongest: SRI < MD5 < SHA1 < SHA256 < SHA512" $ do
            -- Pins the whole ranking in one assertion, so the tier representation can
            -- never drift from the order the tamper gate and the admission floor rely
            -- on. (Blake2b shares SHA-512's tier, asserted above, so it is left out of
            -- this strict chain.)
            let ranks = map integrityStrength [SRI, MD5, SHA1, SHA256, SHA512]
            and (zipWith (<) ranks (drop 1 ranks)) `shouldBe` True

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

        it "rejects a floor below SHA-256 with a precise message (a sub-floor is a config error)" $ do
            -- Asserted by value (not just isLeft) so the operator-facing message — and
            -- the rejected algorithm's rendered name — is pinned.
            mkMinIntegrity SHA1 `shouldBe` Left "the minimum public integrity algorithm must be SHA-256 or stronger, not sha1"
            mkMinIntegrity MD5 `shouldBe` Left "the minimum public integrity algorithm must be SHA-256 or stronger, not md5"
            mkMinIntegrity SRI `shouldBe` Left "the minimum public integrity algorithm must be SHA-256 or stronger, not sri"

        it "parses algorithm names, case- and separator-insensitively" $ do
            (unMinIntegrity <$> parseMinIntegrity "sha256") `shouldBe` Right SHA256
            (unMinIntegrity <$> parseMinIntegrity "SHA-512") `shouldBe` Right SHA512
            (unMinIntegrity <$> parseMinIntegrity "blake2b") `shouldBe` Right Blake2b

        it "rejects a below-floor name and an unknown name with distinct messages" $ do
            -- A recognised-but-weak name fails the floor; an unrecognised name fails the
            -- parse — the two error texts are distinct so a misconfiguration is precise.
            parseMinIntegrity "sha1" `shouldBe` Left "the minimum public integrity algorithm must be SHA-256 or stronger, not sha1"
            parseMinIntegrity "md5" `shouldBe` Left "the minimum public integrity algorithm must be SHA-256 or stronger, not md5"
            parseMinIntegrity "frobnicate" `shouldBe` Left "unknown integrity algorithm: frobnicate"

        it "round-trips render and parse for every floor-eligible algorithm" $ do
            sha512Floor <- expectRight (mkMinIntegrity SHA512)
            blake2bFloor <- expectRight (mkMinIntegrity Blake2b)
            parseMinIntegrity (renderMinIntegrity defaultMinIntegrity) `shouldBe` Right defaultMinIntegrity
            parseMinIntegrity (renderMinIntegrity sha512Floor) `shouldBe` Right sha512Floor
            parseMinIntegrity (renderMinIntegrity blake2bFloor) `shouldBe` Right blake2bFloor

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

        it "BelowFloor for a version whose only digest is an unrecognised SRI (cannot clear the floor)" $
            -- An SRI that resolves to no known algorithm asserts no floor-clearing digest,
            -- so it does not clear the floor (the conservative, fail-closed reading).
            classify defaultMinIntegrity [Hash SRI "sha384-x"] `shouldBe` BelowFloor

        it "NoIntegrity for a version carrying no digest at all" $
            classify defaultMinIntegrity [] `shouldBe` NoIntegrity

        it "BelowFloor for a SHA-256-only version when the floor is SHA-512" $ do
            sha512Floor <- expectRight (mkMinIntegrity SHA512)
            classify sha512Floor [Hash SHA256 "x"] `shouldBe` BelowFloor

-- Local: assert a 'Right' and return its value, failing the example otherwise.
expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> fail ("expected Right, got Left " <> show e)) pure
