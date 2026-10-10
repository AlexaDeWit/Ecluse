-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Package.IntegritySpec (spec) where

import Prelude hiding (universe)

import Data.List.NonEmpty qualified as NE
import Data.Universe.Class (Universe (..))
import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package (
    Artifact (artFilename, artHashes),
    Hash,
    HashAlg (Blake2b, MD5, SHA1, SHA256, SHA384, SHA512, SRI),
    isComputable,
 )
import Ecluse.Core.Package.Integrity (
    IntegrityFloor (..),
    MinIntegrity,
    MinTrustedIntegrity,
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    assertedAlg,
    classifyDigests,
    meetsFloor,
    mkMinIntegrity,
    mkMinTrustedIntegrity,
    parseMinIntegrity,
    parseMinTrustedIntegrity,
    partitionByFloor,
    unMinIntegrity,
    unMinTrustedIntegrity,
 )
import Ecluse.Test.Package (
    artifactWith,
    defaultMinIntegrity,
    defaultMinTrustedIntegrity,
    hexSha384Of,
    hexSha512Of,
    unsafeHash,
    validBlake2b,
    validMd5,
    validSha1,
    validSha256,
    validSha256Sri,
    validSha384Sri,
    validSha512Sri,
 )
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "the worker verifies what the public floor admits (the #409 invariant)" $ do
        it "every algorithm that meets the default public floor is computable" $
            -- The floor admits by algorithm authority ('meetsFloor') and the worker verifies by
            -- computation ('isComputable'), so the computable set must cover the floor-clearing
            -- set. Otherwise the mirror enqueues an admitted public artifact and then drops it
            -- permanently.
            [alg | alg <- universe, meetsFloor defaultMinIntegrity alg, not (isComputable alg)]
                `shouldBe` []

        it "the bare SRI wrapper neither clears the floor nor is computable (it names no algorithm)" $ do
            -- The one constructor both sides exclude: SRI is a wrapper that
            -- 'assertedAlg' resolves, never a floor candidate or a compute target.
            meetsFloor defaultMinIntegrity SRI `shouldBe` False
            isComputable SRI `shouldBe` False

    describe "HashAlg Ord" $ do
        it "uses the explicit checksum-authority order, not constructor order" $ do
            let ranks = [SRI, MD5, SHA1, SHA256, SHA384, Blake2b, SHA512]
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

        it "rejects an algorithm below the default floor (SHA-1, MD5)" $ do
            meetsFloor defaultMinIntegrity SHA1 `shouldBe` False
            meetsFloor defaultMinIntegrity MD5 `shouldBe` False

        it "admits only SHA-512 once the floor is raised to it" $ do
            sha512Floor <- expectRight (mkMinIntegrity SHA512)
            meetsFloor sha512Floor SHA512 `shouldBe` True
            meetsFloor sha512Floor SHA384 `shouldBe` False
            meetsFloor sha512Floor Blake2b `shouldBe` False
            meetsFloor sha512Floor SHA256 `shouldBe` False

    describe "mkMinIntegrity / parseMinIntegrity" $ do
        it "defaults to SHA-256" $
            unMinIntegrity defaultMinIntegrity `shouldBe` SHA256

        it "accepts an algorithm at or above the hard SHA-256 floor" $ do
            (unMinIntegrity <$> mkMinIntegrity SHA256) `shouldBe` Right SHA256
            (unMinIntegrity <$> mkMinIntegrity SHA512) `shouldBe` Right SHA512
            (unMinIntegrity <$> mkMinIntegrity Blake2b) `shouldBe` Right Blake2b

        it "rejects a floor below SHA-256 with a precise message (a sub-floor is a config error)" $ do
            -- Compared by value rather than by isLeft alone, so the case pins the
            -- operator-facing message and the rejected algorithm's rendered name.
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
            -- A recognised but weak name fails the floor. An unrecognised name fails the parse.
            -- The distinct texts tell the operator which mistake they made.
            parseMinIntegrity "sha1" `shouldBe` Left "the minimum public integrity algorithm must be SHA-256 or stronger, not sha1"
            parseMinIntegrity "md5" `shouldBe` Left "the minimum public integrity algorithm must be SHA-256 or stronger, not md5"
            parseMinIntegrity "frobnicate" `shouldBe` Left "unknown integrity algorithm: frobnicate"

    describe "mkMinTrustedIntegrity / parseMinTrustedIntegrity (the loosenable trusted floor)" $ do
        it "defaults to SHA-256, the same secure default as the public floor" $
            unMinTrustedIntegrity defaultMinTrustedIntegrity `shouldBe` SHA256

        it "accepts any concrete algorithm -- including the broken SHA-1 and MD5 (loosenable)" $ do
            -- The trusted floor has no hard minimum: an operator may loosen it below
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

    describe "meetsFloor / classifyDigests over the trusted floor (one ranking backs both floors)" $ do
        it "a loosened (SHA-1) trusted floor admits SHA-1 but not MD5" $ do
            sha1Floor <- expectRight (mkMinTrustedIntegrity SHA1)
            meetsFloor sha1Floor SHA1 `shouldBe` True
            meetsFloor sha1Floor SHA256 `shouldBe` True
            meetsFloor sha1Floor MD5 `shouldBe` False

        it "the default (SHA-256) trusted floor rejects a SHA-1 digest" $
            meetsFloor defaultMinTrustedIntegrity SHA1 `shouldBe` False

        it "classifies a SHA-1-only version BelowFloor by default, MeetsFloor when loosened to SHA-1" $ do
            sha1Floor <- expectRight (mkMinTrustedIntegrity SHA1)
            classifyDigests defaultMinTrustedIntegrity [unsafeHash SHA1 validSha1] `shouldBe` BelowFloor
            classifyDigests sha1Floor [unsafeHash SHA1 validSha1] `shouldBe` MeetsFloor

        it "a hashless version is NoIntegrity under any trusted floor (no digest can meet a floor)" $ do
            sha1Floor <- expectRight (mkMinTrustedIntegrity SHA1)
            classifyDigests defaultMinTrustedIntegrity [] `shouldBe` NoIntegrity
            classifyDigests sha1Floor [] `shouldBe` NoIntegrity

    describe "classifyDigests" $ do
        for_
            -- sha384 is the middle SRI algorithm: it clears the SHA-256 floor as sha512 does.
            [ ("a plain SHA-256 digest", [unsafeHash SHA256 validSha256], MeetsFloor)
            , ("a sha512 SRI", [unsafeHash SRI validSha512Sri], MeetsFloor)
            , ("a sha384 SRI", [unsafeHash SRI validSha384Sri], MeetsFloor)
            , ("a strong digest beside a weak one", [unsafeHash SHA1 validSha1, unsafeHash SHA256 validSha256], MeetsFloor)
            , ("a SHA-1 digest alone", [unsafeHash SHA1 validSha1], BelowFloor)
            , ("no digest at all", [], NoIntegrity)
            ]
            $ \(label, hashes, expected) ->
                it ("reads " <> label <> " as " <> show expected <> " at the default floor") $
                    classifyDigests defaultMinIntegrity hashes `shouldBe` expected

        it "BelowFloor for a SHA-256-only version when the floor is SHA-512" $ do
            sha512Floor <- expectRight (mkMinIntegrity SHA512)
            classifyDigests sha512Floor [unsafeHash SHA256 validSha256] `shouldBe` BelowFloor

    describe "partitionByFloor (the per-artifact gate)" $ do
        let named filename hs = (artifactWith hs){artFilename = filename}
            partition :: NonEmpty Artifact -> Either VersionIntegrity (NonEmpty Text)
            partition arts = fmap (fmap artFilename) (partitionByFloor defaultMinIntegrity artHashes arts)

        it "keeps the files that clear the floor and drops the ones that do not" $
            -- A release loses only the files that cannot be tied to a tamper-evident
            -- fingerprint, rather than disappearing whole.
            partition (named "ok.whl" [unsafeHash SHA256 validSha256] :| [named "legacy.tar.gz" [unsafeHash SHA1 validSha1]])
                `shouldBe` Right ("ok.whl" :| [])

        it "keeps every file when every file clears the floor" $
            partition (named "a.whl" [unsafeHash SHA256 validSha256] :| [named "b.whl" [unsafeHash SRI validSha512Sri]])
                `shouldBe` Right ("a.whl" :| ["b.whl"])

        it "reports BelowFloor when no file clears it but some carry a digest" $
            partition (named "legacy.tar.gz" [unsafeHash SHA1 validSha1] :| [])
                `shouldBe` Left BelowFloor

        it "reports NoIntegrity when no file carries a digest at all" $
            partition (named "bare.whl" [] :| [named "also-bare.tar.gz" []])
                `shouldBe` Left NoIntegrity

        it "reports BelowFloor when a digest-carrying file sits beside a hashless one" $
            partition (named "legacy.tar.gz" [unsafeHash SHA1 validSha1] :| [named "bare.whl" []])
                `shouldBe` Left BelowFloor

        it "either keeps or drops a singleton entire, matching the whole-version verdict" $ do
            -- npm's artifact set is always a singleton, so the partition is exactly the
            -- classification it replaces and npm's behaviour does not move.
            partition (named "one.tgz" [unsafeHash SHA256 validSha256] :| []) `shouldBe` Right ("one.tgz" :| [])
            partition (named "one.tgz" [unsafeHash SHA1 validSha1] :| []) `shouldBe` Left BelowFloor

    describe "the floor over a version's typed artifacts (against its reference)" $
        modifyMaxSuccess (const 500) $ do
            it "keeps the files the reference keeps, and refuses a version as the reference refuses it" $
                hedgehog $ do
                    flr <- forAll genFloor
                    files <- forAll genFiles
                    let expected = referencePartition flr files
                    H.cover 5 "every file kept" (fmap length expected == Right (length files))
                    H.cover 5 "some files dropped" (either (const False) ((< length files) . length) expected)
                    H.cover 5 "refused below the floor" (expected == Left BelowFloor)
                    H.cover 2 "refused with no digest" (expected == Left NoIntegrity)
                    H.cover 10 "one file, the npm shape" (length files == 1)
                    H.cover 10 "several files, the PyPI shape" (length files > 1)
                    floorPartition flr files === expected

            it "reads a version's digests as the reference reads them" $
                hedgehog $ do
                    flr <- forAll genFloor
                    files <- forAll genFiles
                    let expected = referenceClassify flr files
                    H.cover 10 "meets the floor" (expected == MeetsFloor)
                    H.cover 5 "below the floor" (expected == BelowFloor)
                    H.cover 2 "no digest" (expected == NoIntegrity)
                    floorVerdict flr files === expected

-- The production floor over a version's typed artifacts: the files it keeps, and its verdict.
floorPartition :: (IntegrityFloor floor) => floor -> NonEmpty Artifact -> Either VersionIntegrity (NonEmpty Artifact)
floorPartition flr = partitionByFloor flr artHashes

floorVerdict :: (IntegrityFloor floor) => floor -> NonEmpty Artifact -> VersionIntegrity
floorVerdict flr = classifyDigests flr . foldMap artHashes

-- The floor as it read a version's typed artifacts, held as the reference for the production floor.
referencePartition :: (IntegrityFloor floor) => floor -> NonEmpty Artifact -> Either VersionIntegrity (NonEmpty Artifact)
referencePartition flr arts = case nonEmpty (NE.filter (referenceMeetsFloor flr) arts) of
    Just survivors -> Right survivors
    Nothing -> Left (referenceClassify flr arts)

referenceMeetsFloor :: (IntegrityFloor floor) => floor -> Artifact -> Bool
referenceMeetsFloor flr art = any (maybe False (meetsFloor flr) . assertedAlg) (artHashes art)

referenceClassify :: (IntegrityFloor floor) => floor -> NonEmpty Artifact -> VersionIntegrity
referenceClassify flr arts
    | any (referenceMeetsFloor flr) arts = MeetsFloor
    | all (null . artHashes) arts = NoIntegrity
    | otherwise = BelowFloor

-- A public or a trusted floor, so one property ranks against both.
data AnyFloor
    = PublicFloor MinIntegrity
    | TrustedFloor MinTrustedIntegrity
    deriving stock (Show)

instance IntegrityFloor AnyFloor where
    floorAlgorithm = \case
        PublicFloor flr -> floorAlgorithm flr
        TrustedFloor flr -> floorAlgorithm flr

-- Every floor either constructor accepts.
genFloor :: Gen AnyFloor
genFloor = Gen.element (map PublicFloor (rights (map mkMinIntegrity universe)) <> map TrustedFloor (rights (map mkMinTrustedIntegrity universe)))

-- One to four files of a version, each under its own name with up to three digests.
genFiles :: Gen (NonEmpty Artifact)
genFiles = NE.zipWith named (0 :| [1 ..]) <$> Gen.nonEmpty (Range.constant 1 4) (Gen.list (Range.constant 0 3) (Gen.element digestPool))
  where
    named :: Int -> [Hash] -> Artifact
    named position hs = (artifactWith hs){artFilename = "file-" <> show position}

-- One digest of each algorithm, and an SRI of each algorithm an SRI names.
digestPool :: [Hash]
digestPool =
    [ unsafeHash MD5 validMd5
    , unsafeHash SHA1 validSha1
    , unsafeHash SHA256 validSha256
    , unsafeHash SHA384 (hexSha384Of "")
    , unsafeHash Blake2b validBlake2b
    , unsafeHash SHA512 (hexSha512Of "")
    , unsafeHash SRI validSha256Sri
    , unsafeHash SRI validSha384Sri
    , unsafeHash SRI validSha512Sri
    ]
