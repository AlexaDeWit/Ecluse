module Ecluse.Package.IntegritySpec (spec) where

import Test.Hspec

import Ecluse.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Hash,
    HashAlg (Blake2b, MD5, SHA1, SHA256, SHA512, SRI),
    mkHash,
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
            assertedAlg (unsafeHash SHA256 validSha256) `shouldBe` Just SHA256

        it "resolves an SRI to its inner algorithm (sha512, sha256)" $ do
            assertedAlg (unsafeHash SRI validSha512Sri) `shouldBe` Just SHA512
            assertedAlg (unsafeHash SRI validSha256Sri) `shouldBe` Just SHA256

        it "yields Nothing for an SRI whose inner algorithm is unrecognised (a well-formed sha384)" $
            -- A sha384 SRI is well-formed (it constructs), but sha384 is not a modelled
            -- algorithm, so it asserts none and clears no floor — the fail-closed reading.
            -- (A malformed SRI prefix cannot occur: 'mkHash' makes it unrepresentable.)
            assertedAlg (unsafeHash SRI validSha384Sri) `shouldBe` Nothing

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
            classify defaultMinIntegrity [unsafeHash SHA256 validSha256] `shouldBe` MeetsFloor
            classify defaultMinIntegrity [unsafeHash SRI validSha512Sri] `shouldBe` MeetsFloor

        it "MeetsFloor when a strong digest sits beside a weak one" $
            classify defaultMinIntegrity [unsafeHash SHA1 validSha1, unsafeHash SHA256 validSha256] `shouldBe` MeetsFloor

        it "BelowFloor for a SHA-1-only version (a digest, but too weak)" $
            classify defaultMinIntegrity [unsafeHash SHA1 validSha1] `shouldBe` BelowFloor

        it "BelowFloor for a version whose only digest is an unrecognised SRI (cannot clear the floor)" $
            -- An SRI that resolves to no known algorithm asserts no floor-clearing digest,
            -- so it does not clear the floor (the conservative, fail-closed reading).
            classify defaultMinIntegrity [unsafeHash SRI validSha384Sri] `shouldBe` BelowFloor

        it "NoIntegrity for a version carrying no digest at all" $
            classify defaultMinIntegrity [] `shouldBe` NoIntegrity

        it "BelowFloor for a SHA-256-only version when the floor is SHA-512" $ do
            sha512Floor <- expectRight (mkMinIntegrity SHA512)
            classify sha512Floor [unsafeHash SHA256 validSha256] `shouldBe` BelowFloor

-- Local: assert a 'Right' and return its value, failing the example otherwise.
expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> fail ("expected Right, got Left " <> show e)) pure

{- HLINT ignore unsafeHash "Avoid restricted function" -}

{- | Build a 'Hash' from a known-valid digest; only the algorithm (or SRI prefix)
matters to the strength/floor logic under test here, so the value is a canonical
well-formed digest. Errors on a malformed one, so a fixture typo fails loudly.
-}
unsafeHash :: HashAlg -> Text -> Hash
unsafeHash alg = either error id . mkHash alg

-- Canonical well-formed digests (each the empty-input digest of its algorithm).
validSha1, validSha256, validSha256Sri, validSha384Sri, validSha512Sri :: Text
validSha1 = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
validSha256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
validSha256Sri = "sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
validSha384Sri = "sha384-OLBgp1GsljhM2TJ+sbHjaiH9txEUvgdDTAzHv2P24donTt6/529l+9Ua0vFImLlb"
validSha512Sri = "sha512-z4PhNX7vuL3xVChQ1m2AB9Yg5AULVxXcg/SpIdNs6c5H0NE8XYXysP+DGNKHfuwvY7kxvUdBeoGlODJ6+SfaPg=="
