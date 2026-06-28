module Ecluse.Package.IntegritySpec (spec) where

import Test.Hspec

import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Hash,
    HashAlg (Blake2b, MD5, SHA1, SHA256, SHA384, SHA512, SRI),
    isComputable,
 )
import Ecluse.Core.Package.Integrity (
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    assertedAlg,
    classifyArtifacts,
    defaultMinIntegrity,
    defaultMinTrustedIntegrity,
    integrityStrength,
    meetsFloor,
    mkMinIntegrity,
    mkMinTrustedIntegrity,
    parseMinIntegrity,
    parseMinTrustedIntegrity,
    renderMinIntegrity,
    renderMinTrustedIntegrity,
    unMinIntegrity,
    unMinTrustedIntegrity,
 )
import Ecluse.Test.Package (
    unsafeHash,
    validSha1,
    validSha256,
    validSha256Sri,
    validSha384Sri,
    validSha512Sri,
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
    describe "the worker verifies what the public floor admits (the #409 invariant)" $ do
        it "every algorithm that meets the default public floor is computable" $
            -- The load-bearing cross-module invariant: the floor admits by strength
            -- ('meetsFloor'\/'integrityStrength'), the worker verifies by computation
            -- ('isComputable'\/'Ecluse.Core.Package.computeDigest'), and the second set must
            -- cover the first or an admitted public artifact is enqueued then permanently
            -- dropped (issue #409). 'computeDigest''s totality forces a compute arm for any
            -- new 'HashAlg'; this pins that the arm is actually present for every
            -- floor-clearing algorithm. Enumerated over the whole 'HashAlg' set, so a new
            -- constructor is checked automatically.
            [alg | alg <- [minBound .. maxBound], meetsFloor defaultMinIntegrity alg, not (isComputable alg)]
                `shouldBe` []

        it "the bare SRI wrapper neither clears the floor nor is computable (it names no algorithm)" $ do
            -- The one constructor that is correctly excluded from both sides: SRI is a
            -- wrapper resolved via 'assertedAlg', never a floor candidate or a compute target.
            meetsFloor defaultMinIntegrity SRI `shouldBe` False
            isComputable SRI `shouldBe` False

    describe "integrityStrength" $ do
        it "ranks the broken algorithms below SHA-256" $ do
            (integrityStrength SHA1 < integrityStrength SHA256) `shouldBe` True
            (integrityStrength MD5 < integrityStrength SHA256) `shouldBe` True

        it "ranks the modern long digests at or above SHA-256" $ do
            (integrityStrength SHA512 >= integrityStrength SHA256) `shouldBe` True
            (integrityStrength Blake2b >= integrityStrength SHA256) `shouldBe` True
            (integrityStrength SHA512 > integrityStrength SHA256) `shouldBe` True

        it "ranks SHA-384 strictly between SHA-256 and SHA-512" $ do
            -- SHA-384 is SHA-512 truncated: its collision resistance sits above SHA-256's
            -- and below SHA-512's, so it earns a tier of its own between them.
            (integrityStrength SHA256 < integrityStrength SHA384) `shouldBe` True
            (integrityStrength SHA384 < integrityStrength SHA512) `shouldBe` True

        it "ranks a bare SRI below every real algorithm (resolve it first)" $
            (integrityStrength SRI < integrityStrength MD5) `shouldBe` True

        it "ranks SHA-512 and Blake2b as EQUAL — the modern long digests share the top tier" $
            -- The load-bearing invariant of the tier representation: a naive
            -- one-constructor-per-algorithm enum would make these distinct and
            -- silently change which digest wins a strongest-digest comparison. The
            -- worker's tamper gate picks the strongest present digest, so a spurious
            -- tie-break here could prefer the wrong algorithm. They must compare EQ.
            (integrityStrength SHA512 `compare` integrityStrength Blake2b) `shouldBe` EQ

        it "is strictly increasing weakest-to-strongest: SRI < MD5 < SHA1 < SHA256 < SHA384 < SHA512" $ do
            -- Pins the whole ranking in one assertion, so the tier representation can
            -- never drift from the order the tamper gate and the admission floor rely
            -- on. (Blake2b shares SHA-512's tier, asserted above, so it is left out of
            -- this strict chain.)
            let ranks = map integrityStrength [SRI, MD5, SHA1, SHA256, SHA384, SHA512]
            and (zipWith (<) ranks (drop 1 ranks)) `shouldBe` True

    describe "assertedAlg" $ do
        it "reads a plain tag directly" $
            assertedAlg (unsafeHash SHA256 validSha256) `shouldBe` Just SHA256

        it "resolves an SRI to its inner algorithm (sha512, sha384, sha256)" $ do
            assertedAlg (unsafeHash SRI validSha512Sri) `shouldBe` Just SHA512
            assertedAlg (unsafeHash SRI validSha384Sri) `shouldBe` Just SHA384
            assertedAlg (unsafeHash SRI validSha256Sri) `shouldBe` Just SHA256

    describe "meetsFloor" $ do
        it "admits an algorithm at or above the default (SHA-256) floor" $ do
            meetsFloor defaultMinIntegrity SHA256 `shouldBe` True
            meetsFloor defaultMinIntegrity SHA384 `shouldBe` True
            meetsFloor defaultMinIntegrity SHA512 `shouldBe` True
            meetsFloor defaultMinIntegrity Blake2b `shouldBe` True

        it "rejects SHA-384 when the floor is raised to SHA-512 (SHA-384 is below it)" $ do
            sha512Floor <- expectRight (mkMinIntegrity SHA512)
            meetsFloor sha512Floor SHA384 `shouldBe` False

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
            (unMinIntegrity <$> parseMinIntegrity "sha384") `shouldBe` Right SHA384
            (unMinIntegrity <$> parseMinIntegrity "SHA-384") `shouldBe` Right SHA384
            (unMinIntegrity <$> parseMinIntegrity "SHA-512") `shouldBe` Right SHA512
            (unMinIntegrity <$> parseMinIntegrity "blake2b") `shouldBe` Right Blake2b

        it "rejects a below-floor name and an unknown name with distinct messages" $ do
            -- A recognised-but-weak name fails the floor; an unrecognised name fails the
            -- parse — the two error texts are distinct so a misconfiguration is precise.
            parseMinIntegrity "sha1" `shouldBe` Left "the minimum public integrity algorithm must be SHA-256 or stronger, not sha1"
            parseMinIntegrity "md5" `shouldBe` Left "the minimum public integrity algorithm must be SHA-256 or stronger, not md5"
            parseMinIntegrity "frobnicate" `shouldBe` Left "unknown integrity algorithm: frobnicate"

        it "round-trips render and parse for every floor-eligible algorithm" $ do
            sha384Floor <- expectRight (mkMinIntegrity SHA384)
            sha512Floor <- expectRight (mkMinIntegrity SHA512)
            blake2bFloor <- expectRight (mkMinIntegrity Blake2b)
            parseMinIntegrity (renderMinIntegrity defaultMinIntegrity) `shouldBe` Right defaultMinIntegrity
            parseMinIntegrity (renderMinIntegrity sha384Floor) `shouldBe` Right sha384Floor
            parseMinIntegrity (renderMinIntegrity sha512Floor) `shouldBe` Right sha512Floor
            parseMinIntegrity (renderMinIntegrity blake2bFloor) `shouldBe` Right blake2bFloor

    describe "mkMinTrustedIntegrity / parseMinTrustedIntegrity (the loosenable trusted floor)" $ do
        it "defaults to SHA-256, the same secure default as the public floor" $
            unMinTrustedIntegrity defaultMinTrustedIntegrity `shouldBe` SHA256

        it "accepts any concrete algorithm — including the broken SHA-1 and MD5 (loosenable)" $ do
            -- The trusted floor has NO hard minimum: an operator may loosen it below
            -- SHA-256 for a legacy private mirror, where trust substitutes for strength.
            (unMinTrustedIntegrity <$> mkMinTrustedIntegrity SHA1) `shouldBe` Right SHA1
            (unMinTrustedIntegrity <$> mkMinTrustedIntegrity MD5) `shouldBe` Right MD5
            (unMinTrustedIntegrity <$> mkMinTrustedIntegrity SHA256) `shouldBe` Right SHA256
            (unMinTrustedIntegrity <$> mkMinTrustedIntegrity SHA512) `shouldBe` Right SHA512

        it "rejects the bare SRI wrapper (it names no concrete algorithm)" $
            mkMinTrustedIntegrity SRI
                `shouldBe` Left "the minimum trusted integrity algorithm must name a concrete algorithm, not a bare SRI"

        it "parses sub-SHA-256 names (sha1, md5) that the public floor would reject" $ do
            (unMinTrustedIntegrity <$> parseMinTrustedIntegrity "sha1") `shouldBe` Right SHA1
            (unMinTrustedIntegrity <$> parseMinTrustedIntegrity "md5") `shouldBe` Right MD5
            (unMinTrustedIntegrity <$> parseMinTrustedIntegrity "SHA-256") `shouldBe` Right SHA256

        it "rejects an unknown algorithm name" $
            parseMinTrustedIntegrity "frobnicate" `shouldBe` Left "unknown integrity algorithm: frobnicate"

        it "round-trips render and parse" $ do
            sha1Floor <- expectRight (mkMinTrustedIntegrity SHA1)
            parseMinTrustedIntegrity (renderMinTrustedIntegrity defaultMinTrustedIntegrity)
                `shouldBe` Right defaultMinTrustedIntegrity
            parseMinTrustedIntegrity (renderMinTrustedIntegrity sha1Floor) `shouldBe` Right sha1Floor

    describe "meetsFloor / classifyArtifacts over the trusted floor (one ranking backs both floors)" $ do
        it "a loosened (SHA-1) trusted floor admits SHA-1 but not MD5" $ do
            sha1Floor <- expectRight (mkMinTrustedIntegrity SHA1)
            meetsFloor sha1Floor SHA1 `shouldBe` True
            meetsFloor sha1Floor SHA256 `shouldBe` True
            meetsFloor sha1Floor MD5 `shouldBe` False

        it "the default (SHA-256) trusted floor rejects a SHA-1 digest" $
            meetsFloor defaultMinTrustedIntegrity SHA1 `shouldBe` False

        it "classifies a SHA-1-only version BelowFloor by default, MeetsFloor when loosened to SHA-1" $ do
            sha1Floor <- expectRight (mkMinTrustedIntegrity SHA1)
            classifyArtifacts defaultMinTrustedIntegrity (artifactWith [unsafeHash SHA1 validSha1] :| [])
                `shouldBe` BelowFloor
            classifyArtifacts sha1Floor (artifactWith [unsafeHash SHA1 validSha1] :| [])
                `shouldBe` MeetsFloor

        it "a hashless version is NoIntegrity under any trusted floor (no digest can meet a floor)" $ do
            sha1Floor <- expectRight (mkMinTrustedIntegrity SHA1)
            classifyArtifacts defaultMinTrustedIntegrity (artifactWith [] :| []) `shouldBe` NoIntegrity
            classifyArtifacts sha1Floor (artifactWith [] :| []) `shouldBe` NoIntegrity

    describe "classifyArtifacts" $ do
        let classify floorAlg hs =
                classifyArtifacts floorAlg (artifactWith hs :| [])

        it "MeetsFloor when a digest clears the floor (SHA-256, sha512-SRI)" $ do
            classify defaultMinIntegrity [unsafeHash SHA256 validSha256] `shouldBe` MeetsFloor
            classify defaultMinIntegrity [unsafeHash SRI validSha512Sri] `shouldBe` MeetsFloor

        it "MeetsFloor when the only digest is a sha384 SRI (clears the SHA-256 floor)" $
            -- A sha384 SRI resolves to the modelled SHA384, which ranks above the SHA-256
            -- floor, so a version carrying only it is admissible from a public upstream.
            classify defaultMinIntegrity [unsafeHash SRI validSha384Sri] `shouldBe` MeetsFloor

        it "MeetsFloor when a strong digest sits beside a weak one" $
            classify defaultMinIntegrity [unsafeHash SHA1 validSha1, unsafeHash SHA256 validSha256] `shouldBe` MeetsFloor

        it "BelowFloor for a SHA-1-only version (a digest, but too weak)" $
            classify defaultMinIntegrity [unsafeHash SHA1 validSha1] `shouldBe` BelowFloor

        it "NoIntegrity for a version carrying no digest at all" $
            classify defaultMinIntegrity [] `shouldBe` NoIntegrity

        it "BelowFloor for a SHA-256-only version when the floor is SHA-512" $ do
            sha512Floor <- expectRight (mkMinIntegrity SHA512)
            classify sha512Floor [unsafeHash SHA256 validSha256] `shouldBe` BelowFloor

-- Local: assert a 'Right' and return its value, failing the example otherwise.
expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> fail ("expected Right, got Left " <> show e)) pure
