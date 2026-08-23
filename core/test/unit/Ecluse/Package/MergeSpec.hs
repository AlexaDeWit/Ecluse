-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- This spec deliberately writes out the Monoid identity laws (@mempty <> a@ and
-- @a <> mempty@) to /assert/ them. hlint would otherwise "simplify" the exact
-- expressions under test. The silence is file-wide because proving the laws is the
-- file's purpose, not an oversight.
{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

module Ecluse.Package.MergeSpec (spec) where

import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime (..), fromGregorian)
import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package
import Ecluse.Core.Package.Merge
import Ecluse.Core.Version (mkVersion, unVersion)
import Ecluse.Test.Package (hexSha1Of, hexSha256Of, sriSha256Of, sriSha512Of, unsafeHash)
import Ecluse.Test.WireVocab (wireRoundTrips)

name :: PackageName
name = mkPackageName Npm Nothing "thing"

{- | One tarball artifact carrying the given integrity digests, so a test varies only
integrity.
-}
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

{- | A per-version snapshot carrying the given integrity digests. The merge reads only
the version key, the parsed version (for @latest@), and artifact integrity.
-}
detailsWith :: Text -> [Hash] -> PackageDetails
detailsWith rawVer hs =
    PackageDetails
        { pkgName = name
        , pkgVersion = mkVersion Npm rawVer
        , pkgPublishedAt = Just t0
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = artifactWith hs :| []
        , pkgLicenses = ["MIT"]
        , pkgPublisher = Nothing
        }

{- | The fixed publish instant every 'detailsWith' version carries. 'withPublishedAt'
overrides it where a test needs distinct cross-source instants.
-}
t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

{- | Build a single-package packument from @(rawVersion, integrityDigests)@ pairs. Its
@latest@ points at the lexically-highest version, so a lone source is already a fixed
point of the merge's @latest@ reconciliation.
-}
packumentWith :: [(Text, [Hash])] -> PackageInfo
packumentWith vs =
    PackageInfo
        { infoName = name
        , infoVersions = Map.fromList [(v, detailsWith v hs) | (v, hs) <- vs]
        , infoDistTags = case sortOn Down (map fst vs) of
            [] -> Map.empty
            (hi : _) -> Map.singleton "latest" (mkVersion Npm hi)
        , infoInvalidEntries = []
        }

{- | Override the publish instant every version of a packument carries, for tests
pinning cross-source instants.
-}
withPublishedAt :: UTCTime -> PackageInfo -> PackageInfo
withPublishedAt t info =
    info{infoVersions = Map.map (\d -> d{pkgPublishedAt = Just t}) (infoVersions info)}

{- | Build a packument whose every version carries a single SRI digest, the uniform
algorithm set the collision and reconciliation tests need.
-}
packument :: [(Text, Text)] -> PackageInfo
packument vs = packumentWith [(v, [unsafeHash SRI d]) | (v, d) <- vs]

-- A well-formed sha512 SRI derived from a mnemonic label. Distinct labels yield distinct
-- digests, and the same label always yields the same digest.
validSriOf :: Text -> Text
validSriOf = sriSha512Of . encodeUtf8

-- Well-formed hex SHA-1 / SHA-256 digests derived from a label (40- / 64-hex).
validSha1Of, validSha256Of :: Text -> Text
validSha1Of = hexSha1Of . encodeUtf8
validSha256Of = hexSha256Of . encodeUtf8

-- A well-formed sha256 SRI (@sha256-\<base64\>@) derived from a label. A recomputing
-- mirror serves this in place of, or beside, npm's sha512.
validSha256SriOf :: Text -> Text
validSha256SriOf = sriSha256Of . encodeUtf8

-- Mnemonic SRI tokens (each a distinct, well-formed sha512 SRI), named for the role
-- each plays in the collision / divergence tests.
sriAaa, sriBbb, sriCcc, sriPriv, sriPrivate, sriPub, sriPublic, sriSame :: Text
sriAaa = validSriOf "aaa"
sriBbb = validSriOf "bbb"
sriCcc = validSriOf "ccc"
sriPriv = validSriOf "priv"
sriPrivate = validSriOf "private"
sriPub = validSriOf "pub"
sriPublic = validSriOf "public"
sriSame = validSriOf "same"

sriX, sriY, sriT, sriG1, sriG2, sriCapA, sriCapB, sriLowA, sriLowB, sriLowC :: Text
sriX = validSriOf "X"
sriY = validSriOf "Y"
sriT = validSriOf "T"
sriG1 = validSriOf "G1"
sriG2 = validSriOf "G2"
sriCapA = validSriOf "A"
sriCapB = validSriOf "B"
sriLowA = validSriOf "a"
sriLowB = validSriOf "b"
sriLowC = validSriOf "c"

-- SHA-1 / SHA-256 mnemonic digests for the shared-algorithm cross-check tests.
sha1Abc, sha1Dead, sha256Def :: Text
sha1Abc = validSha1Of "abc"
sha1Dead = validSha1Of "deadbeef"
sha256Def = validSha256Of "def"

-- The fingerprint triple an SRI resolves to under the merge's keying: the fixture file
-- name, the embedded algorithm, and the base64 body, as 'integrityHashes' reads them back.
sriPair :: Text -> (Text, Maybe HashAlg, Text)
sriPair s = ("thing.tgz", sriAlgorithm s, sriBody s)

-- The surviving version keys (the merged union), sorted.
survivorKeys :: MergePlan -> [Text]
survivorKeys = sort . Map.keys . mpSurvivors

-- The 'SourceId' that won a given surviving key, if it survived.
winnerOf :: Text -> MergePlan -> Maybe SourceId
winnerOf key = Map.lookup key . mpSurvivors

-- The resolved @latest@ tag's raw text, if present.
latestKey :: MergePlan -> Maybe Text
latestKey p = unVersion <$> Map.lookup "latest" (mpDistTags p)

{- | The winning provenance per surviving version key, the order-independent decision
beneath the order-dependent 'SourceId'. A 'SourceId' is a list index, so this maps each
winning index to the 'Provenance' of the input at that position.
-}
winnerProvenances :: [(Provenance, PackageInfo)] -> MergePlan -> Map Text Provenance
winnerProvenances inputs plan =
    -- Index the inputs by 'SourceId' (their list position) up front, so the lookup
    -- is total, with no partial indexing into the list.
    Map.mapMaybe (`Map.lookup` byId) (mpSurvivors plan)
  where
    byId = Map.fromList (zip [0 ..] (map fst inputs))

genDigest :: Gen Text
genDigest = validSriOf <$> Gen.text (Range.singleton 6) Gen.alphaNum

-- | An arbitrary 40-hex-character SHA-1 shasum (npm's @dist.shasum@ wire form).
genSha1 :: Gen Text
genSha1 = Gen.text (Range.singleton 40) Gen.hexit

-- | A simple numeric semver so generated versions always parse and order.
genVersionStr :: Gen Text
genVersionStr = do
    a <- Gen.int (Range.linear 0 9)
    b <- Gen.int (Range.linear 0 9)
    c <- Gen.int (Range.linear 0 9)
    pure (show a <> "." <> show b <> "." <> show c)

genSource :: Gen (Provenance, PackageInfo)
genSource = do
    prov <- Gen.element [TrustedSource, GatedSource]
    n <- Gen.int (Range.linear 0 5)
    vers <- Gen.list (Range.singleton n) genVersionStr
    let distinct = nub vers
    pairs <- forM distinct (\v -> (,) v <$> genDigest)
    pure (prov, packument pairs)

genSources :: Gen [(Provenance, PackageInfo)]
genSources = Gen.list (Range.linear 1 4) genSource

{- | An arbitrary 'Merge' accumulator, a 'foldMap' of 'contribute' over a small source
list. The empty list yields 'mempty', so the laws are tested over the identity too.
-}
genMerge :: Gen Merge
genMerge = foldMap (uncurry contribute) <$> Gen.list (Range.linear 0 3) genSource

spec :: Spec
spec = do
    describe "mergePackuments" $ do
        it "returns Nothing on an empty input (nothing to serve)" $
            mergePackuments [] `shouldBe` Nothing

        it "names the plan after the first input" $ do
            let info = packument [("1.0.0", sriAaa)]
            (mpName <$> mergePackuments [(GatedSource, info)]) `shouldBe` Just name

        it "carries mpName from a contribution, never a manufactured value" $ do
            -- Name validation runs upstream of the merge, so every contribution shares one
            -- validated identity.
            let a = packument [("1.0.0", sriAaa)]
                b = packument [("2.0.0", sriBbb)]
                inputs = [(TrustedSource, a), (GatedSource, b)]
            (mpName <$> mergePackuments inputs) `shouldBe` Just (infoName a)

        it "is the identity on a single input (survivors, tags, time)" $ do
            -- A lone source: every version survives, all won by source 0, with its
            -- own latest kept and its times carried whole.
            let info = packument [("1.0.0", sriAaa), ("2.0.0", sriBbb)]
                plan = mergePackuments [(GatedSource, info)]
            (Map.keys . mpSurvivors <$> plan) `shouldBe` Just ["1.0.0", "2.0.0"]
            (Map.elems . mpSurvivors <$> plan) `shouldBe` Just [0, 0]
            (latestKey =<< plan) `shouldBe` Just "2.0.0"
            (sort . Map.keys . mpTime <$> plan) `shouldBe` Just ["1.0.0", "2.0.0"]

        it "reports no divergences for a single input" $ do
            let info = packument [("1.0.0", sriAaa)]
            (mpDivergences <$> mergePackuments [(TrustedSource, info)]) `shouldBe` Just Set.empty

        it "unions versions across sources" $ do
            let trusted = packument [("1.0.0", sriAaa)]
                gated = packument [("2.0.0", sriBbb)]
            (survivorKeys <$> mergePackuments [(TrustedSource, trusted), (GatedSource, gated)])
                `shouldBe` Just ["1.0.0", "2.0.0"]

        it "private wins a collision: the survivor points at the trusted source" $ do
            -- Same version key in both, with differing integrity. The plan points the key at the
            -- trusted 'SourceId', so the serve layer takes the object from the private raw Value.
            let gated = packument [("1.0.0", sriPublic)] -- source 0
                trusted = packument [("1.0.0", sriPrivate)] -- source 1
            (winnerOf "1.0.0" =<< mergePackuments [(GatedSource, gated), (TrustedSource, trusted)])
                `shouldBe` Just 1

        it "detects a divergence when the same version's integrity differs" $ do
            let trusted = packument [("1.0.0", sriPrivate)]
                gated = packument [("1.0.0", sriPublic)]
                plan = mergePackuments [(TrustedSource, trusted), (GatedSource, gated)]
            (map divVersion . Set.toList . mpDivergences <$> plan) `shouldBe` Just ["1.0.0"]

        it "reports no divergence when a collision's integrity agrees" $ do
            let trusted = packument [("1.0.0", sriSame)]
                gated = packument [("1.0.0", sriSame)]
                plan = mergePackuments [(TrustedSource, trusted), (GatedSource, gated)]
            (mpDivergences <$> plan) `shouldBe` Just Set.empty

        it "repoints latest to the highest surviving version when the chosen tag is gone" $ do
            -- The trusted source's chosen latest (9.9.9) is absent from the union, so selectLatest
            -- repoints to the highest stable survivor (3.0.0).
            let trusted =
                    (packument [("1.0.0", sriAaa)])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "9.9.9")
                        }
                gated = packument [("3.0.0", sriBbb), ("2.0.0", sriCcc)]
            (latestKey =<< mergePackuments [(TrustedSource, trusted), (GatedSource, gated)])
                `shouldBe` Just "3.0.0"

        it "drops a dist-tag whose target is absent from the union" $ do
            -- A source advertises a "next" tag pointing at a version it does not
            -- actually carry. The merge drops it rather than serving a dangling tag.
            let info =
                    (packument [("1.0.0", sriAaa)])
                        { infoDistTags =
                            Map.fromList
                                [ ("latest", mkVersion Npm "1.0.0")
                                , ("next", mkVersion Npm "9.9.9")
                                ]
                        }
            (Map.keys . mpDistTags <$> mergePackuments [(GatedSource, info)])
                `shouldBe` Just ["latest"]

        it "restricts time to surviving versions" $ do
            let trusted = packument [("1.0.0", sriAaa)]
                gated = packument [("2.0.0", sriBbb)]
            (sort . Map.keys . mpTime <$> mergePackuments [(TrustedSource, trusted), (GatedSource, gated)])
                `shouldBe` Just ["1.0.0", "2.0.0"]

    describe "collision resolution & divergence correction (the supply-chain signal)" $ do
        -- A version in both a trusted and a gated source is a collision, and the trusted copy
        -- wins. Disagreeing integrity is flagged as a tampering signal, never dropped here.
        -- Fail-closed is the caller's decision.
        let trusted = packument [("1.0.0", sriPrivate)] -- source 0
            gated = packument [("1.0.0", sriPublic)] -- source 1
            plan = mergePackuments [(TrustedSource, trusted), (GatedSource, gated)]

        it "keeps the divergent version, won by the trusted source (flags, does not drop)" $ do
            (survivorKeys <$> plan) `shouldBe` Just ["1.0.0"]
            (winnerOf "1.0.0" =<< plan) `shouldBe` Just 0

        it "records the winning (trusted) and losing (gated) integrity for the audit trail" $
            case Set.toList . mpDivergences <$> plan of
                Just [d] -> do
                    divVersion d `shouldBe` "1.0.0"
                    integrityHashes (divWinning d) `shouldBe` [sriPair sriPrivate]
                    integrityHashes (divLosing d) `shouldBe` [sriPair sriPublic]
                other -> expectationFailure ("expected exactly one divergence, got " <> show other)

    describe "applyDivergencePolicy (the caller's fail-closed projection)" $ do
        -- 2.0.0 (the @latest@) diverges across sources and 1.0.0 agrees. The serve
        -- layer runs the projection AFTER logging and metering the divergence, so a
        -- fail-closed operator withholds only the contested version, coherently.
        let trusted = packument [("1.0.0", sriSame), ("2.0.0", sriPrivate)]
            gated = packument [("1.0.0", sriSame), ("2.0.0", sriPublic)]
            plan = mergePackuments [(TrustedSource, trusted), (GatedSource, gated)]

        it "warn is the identity: every version and its dist-tag survive" $ do
            (survivorKeys . applyDivergencePolicy Warn <$> plan) `shouldBe` Just ["1.0.0", "2.0.0"]
            (Map.lookup "latest" . mpDistTags . applyDivergencePolicy Warn <$> plan)
                `shouldBe` Just (Just (mkVersion Npm "2.0.0"))

        it "fail-closed withholds the contested version, keeps the agreeing one" $
            (survivorKeys . applyDivergencePolicy FailClosed <$> plan) `shouldBe` Just ["1.0.0"]

        it "fail-closed drops the dist-tag and time entry that pointed at the contested version" $ do
            let served = applyDivergencePolicy FailClosed <$> plan
            (Map.lookup "latest" . mpDistTags <$> served) `shouldBe` Just Nothing
            (Map.member "2.0.0" . mpTime <$> served) `shouldBe` Just False

        it "fail-closed leaves the audit record (mpDivergences) intact" $
            (Set.null . mpDivergences . applyDivergencePolicy FailClosed <$> plan) `shouldBe` Just False

        it "fail-closed empties the listing when every surviving version is contested" $ do
            let onlyDivergent =
                    mergePackuments
                        [ (TrustedSource, packument [("1.0.0", sriPrivate)])
                        , (GatedSource, packument [("1.0.0", sriPublic)])
                        ]
            (Map.null . mpSurvivors . applyDivergencePolicy FailClosed <$> onlyDivergent) `shouldBe` Just True

    wireRoundTrips @DivergencePolicy

    describe "parseDivergencePolicy (the ECLUSE_INTEGRITY__DIVERGENCE_POLICY value)" $ do
        it "parses warn and fail-closed, case- and spelling-tolerant" $ do
            parseDivergencePolicy "warn" `shouldBe` Right Warn
            parseDivergencePolicy "fail-closed" `shouldBe` Right FailClosed
            parseDivergencePolicy "FAIL_CLOSED" `shouldBe` Right FailClosed
            parseDivergencePolicy "  FailClosed  " `shouldBe` Right FailClosed

        it "rejects an unknown policy" $
            parseDivergencePolicy "drop" `shouldSatisfy` isLeft

    describe "divergence compares on shared algorithms, not the whole digest set" $ do
        -- A divergence needs two copies to contradict on an algorithm they both carry. An
        -- asymmetric digest set is not a contradiction: an older registry may serve only a legacy
        -- shasum while npmjs serves that shasum plus a modern SRI, over the same bytes.
        let sha1 = unsafeHash SHA1
            sri = unsafeHash SRI

        it "agreeing on the shared SRI is not a divergence though one also carries SHA-1" $ do
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri sriX, sha1 sha1Dead])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri sriX])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "contradicting on the shared SRI is a divergence even when SHA-1 agrees" $ do
            -- A SHA-1 agreement never rescues a contradicting secure digest, so the merge flags the
            -- SRI contradiction anyway.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri sriX, sha1 sha1Abc])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri sriY, sha1 sha1Abc])])
                plan = mergePackuments [trusted, gated]
            (map divVersion . Set.toList . mpDivergences <$> plan) `shouldBe` Just ["1.0.0"]

        it "private SHA-1 vs public SHA-1+SHA-256 cross-checks on the shared SHA-1 (not a divergence)" $ do
            -- The sanctioned asymmetric-trust case: the copies share SHA-1 and it agrees, so the
            -- cross-check passes. The public copy clears the admission floor on its SHA-256 alone.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sha1 sha1Abc])])
                gated = (GatedSource, packumentWith [("1.0.0", [sha1 sha1Abc, unsafeHash SHA256 sha256Def])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "SRI+SHA-1 vs SHA-1-only, agreeing on the shared SHA-1, is not a divergence" $ do
            -- The single shared algorithm agrees, so there is no divergence. Whether a weak-only
            -- agreement is itself suspicious is a stricter policy this fold does not decide.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri sriX, sha1 sha1Abc])])
                gated = (GatedSource, packumentWith [("1.0.0", [sha1 sha1Abc])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        -- A version may carry several artifacts, each with its own hashes, so one algorithm can
        -- hold several digests. Copies agree on a shared algorithm only when those sets match.
        it "agrees when a shared algorithm carries the same set of digests in any order" $ do
            -- The same two SRI digests on both copies, listed in opposite order: the
            -- per-algorithm set is identical, so this is not a divergence.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri sriX, sri sriY])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri sriY, sri sriX])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "contradicts when a shared algorithm's set of digests differs" $
            -- One copy offers two SRI digests for the key, the other only one of them.
            -- The digest sets for the shared algorithm differ, so the merge flags it.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri sriX, sri sriY])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri sriX])])
             in case Set.toList . mpDivergences <$> mergePackuments [trusted, gated] of
                    Just [d] -> do
                        divVersion d `shouldBe` "1.0.0"
                        integrityHashes (divWinning d) `shouldBe` sort [sriPair sriX, sriPair sriY]
                        integrityHashes (divLosing d) `shouldBe` [sriPair sriX]
                    other -> expectationFailure ("expected exactly one divergence, got " <> show other)

    describe "divergence keys per artifact, not per version (#739)" $ do
        -- A multi-artifact ecosystem (PyPI sdist plus wheels) spreads a version's digests across
        -- files. The fingerprint keys each digest by its file, so a differing file set cannot read
        -- as tampering and only a shared file's shared algorithm can contradict.
        let sri = unsafeHash SRI
            withArtifacts arts info =
                info{infoVersions = Map.map (\d -> d{pkgArtifacts = arts}) (infoVersions info)}
            wheelWith fileName hs =
                (artifactWith hs){artFilename = fileName, artUrl = "https://example.test/" <> fileName}

        it "a mirror carrying fewer files than the index is availability, not a divergence" $ do
            -- No shared file contradicts, so the extra wheel is availability, not substituted
            -- bytes.
            let sharedFile = artifactWith [sri sriX]
                extraWheel = wheelWith "thing-extra.whl" [sri sriY]
                trusted = (TrustedSource, withArtifacts (one sharedFile) (packumentWith [("1.0.0", [])]))
                gated = (GatedSource, withArtifacts (sharedFile :| [extraWheel]) (packumentWith [("1.0.0", [])]))
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "a shared file contradicting under a shared algorithm diverges even amid differing file sets" $ do
            -- The tampering signal survives the per-artifact keying. The shared thing.tgz
            -- disagrees, so the version diverges though the file sets also differ.
            let extraWheel = wheelWith "thing-extra.whl" [sri sriY]
                trusted = (TrustedSource, withArtifacts (one (artifactWith [sri sriX])) (packumentWith [("1.0.0", [])]))
                gated = (GatedSource, withArtifacts (artifactWith [sri sriY] :| [extraWheel]) (packumentWith [("1.0.0", [])]))
            (map divVersion . Set.toList . mpDivergences <$> mergePackuments [trusted, gated])
                `shouldBe` Just ["1.0.0"]

        it "the same digest under a renamed file is asymmetric, not a divergence" $ do
            -- A file renamed across the two sides shares no (file, algorithm) key, so nothing can
            -- contradict. Absence reads fail-open, as with an asymmetric algorithm.
            let trusted = (TrustedSource, withArtifacts (one (artifactWith [sri sriX])) (packumentWith [("1.0.0", [])]))
                gated = (GatedSource, withArtifacts (one (wheelWith "renamed.tgz" [sri sriY])) (packumentWith [("1.0.0", [])]))
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

    describe "divergence keys on the resolved algorithm, not the raw digest tag" $ do
        -- The comparison buckets each digest by the algorithm it asserts, not by the opaque SRI
        -- wrapper tag. That closes a false positive (two algorithms over the same bytes) and a
        -- false negative (one algorithm expressed two ways).

        it "a sha256 SRI and a sha512 SRI for the same bytes are asymmetric, not a divergence" $ do
            -- A private mirror may recompute integrity as sha256 while the public copy serves
            -- sha512 over the same bytes. The two share no resolved algorithm, so this is
            -- asymmetry.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [unsafeHash SRI (validSha256SriOf "same")])])
                gated = (GatedSource, packumentWith [("1.0.0", [unsafeHash SRI (validSriOf "same")])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "a hex SHA-256 and an sha256 SRI that disagree are a divergence (same resolved algorithm)" $ do
            -- Hex SHA-256 on one side and an sha256 SRI on the other, with different digests. Both
            -- resolve to SHA-256, so the comparison catches the contradiction.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [unsafeHash SHA256 (validSha256Of "aaa")])])
                gated = (GatedSource, packumentWith [("1.0.0", [unsafeHash SRI (validSha256SriOf "bbb")])])
                plan = mergePackuments [trusted, gated]
            (map divVersion . Set.toList . mpDivergences <$> plan) `shouldBe` Just ["1.0.0"]

    describe "precedence is by provenance, not input order" $ do
        -- dist-tags and time must resolve collisions by provenance (trusted wins),
        -- so the plan is identical whichever order the caller passes the upstreams.
        let trusted =
                ( TrustedSource
                , withPublishedAt tTrusted $
                    (packument [("1.0.0", sriPriv)])
                        { infoDistTags = Map.fromList [("latest", mkVersion Npm "1.0.0"), ("beta", mkVersion Npm "1.0.0")]
                        }
                )
            gated =
                ( GatedSource
                , withPublishedAt tGated $
                    (packument [("1.0.0", sriPub)])
                        { infoDistTags = Map.fromList [("latest", mkVersion Npm "1.0.0"), ("beta", mkVersion Npm "1.0.0")]
                        }
                )
            tTrusted = UTCTime (fromGregorian 2026 3 3) 0
            tGated = UTCTime (fromGregorian 2020 1 1) 0

        it "resolves identically whichever order trusted/gated is passed" $ do
            -- Every provenance-resolved decision is order-independent: survivors, tags, times, and
            -- divergences. Only the winner's 'SourceId' differs, pointing at the trusted position.
            let forward = mergePackuments [trusted, gated]
                backward = mergePackuments [gated, trusted]
            (Map.keys . mpSurvivors <$> forward) `shouldBe` (Map.keys . mpSurvivors <$> backward)
            (mpDistTags <$> forward) `shouldBe` (mpDistTags <$> backward)
            (mpTime <$> forward) `shouldBe` (mpTime <$> backward)
            (mpDivergences <$> forward) `shouldBe` (mpDivergences <$> backward)

        it "a non-latest tag resolves to the trusted target regardless of order" $ do
            -- Both sources tag 'beta' at 1.0.0, with different integrity behind that key.
            let forward = winnerOf "1.0.0" =<< mergePackuments [trusted, gated]
                backward = winnerOf "1.0.0" =<< mergePackuments [gated, trusted]
            -- trusted is index 0 forward and index 1 backward. Both must name trusted.
            forward `shouldBe` Just 0
            backward `shouldBe` Just 1

        it "time resolves to the trusted source's instant regardless of order" $ do
            (Map.lookup "1.0.0" . mpTime =<< mergePackuments [trusted, gated])
                `shouldBe` Just tTrusted
            (Map.lookup "1.0.0" . mpTime =<< mergePackuments [gated, trusted])
                `shouldBe` Just tTrusted

    describe "a version's served time comes from the source that won its manifest" $ do
        -- The served publish time comes off the SAME winning candidate that supplies the
        -- manifest, so no other source can stamp those bytes.
        it "does not borrow a losing source's time for a winning manifest (no false time)" $ do
            -- Trusted wins 1.0.0's manifest but knows no publish time for it, and the
            -- gated copy carries a date. The served time must not be that gated date.
            let trustedNoTime =
                    ( TrustedSource
                    , (packument [("1.0.0", sriPriv)]){infoVersions = noTime (infoVersions (packument [("1.0.0", sriPriv)]))}
                    )
                gatedDated = (GatedSource, withPublishedAt tGated (packument [("1.0.0", sriPub)]))
                tGated = UTCTime (fromGregorian 2019 9 9) 0
                noTime = Map.map (\d -> d{pkgPublishedAt = Nothing})
            (Map.lookup "1.0.0" . mpTime =<< mergePackuments [trustedNoTime, gatedDated])
                `shouldBe` Nothing
            (Map.lookup "1.0.0" . mpTime =<< mergePackuments [gatedDated, trustedNoTime])
                `shouldBe` Nothing

        it "serves the winning manifest's own time when it has one (not the loser's)" $ do
            -- Both sources carry a date. Trusted wins the manifest, so the plan serves its date.
            let tWin = UTCTime (fromGregorian 2026 4 4) 0
                tLose = UTCTime (fromGregorian 2018 2 2) 0
                trustedDated = (TrustedSource, withPublishedAt tWin (packument [("1.0.0", sriPriv)]))
                gatedDated = (GatedSource, withPublishedAt tLose (packument [("1.0.0", sriPub)]))
            (Map.lookup "1.0.0" . mpTime =<< mergePackuments [trustedDated, gatedDated])
                `shouldBe` Just tWin
            (Map.lookup "1.0.0" . mpTime =<< mergePackuments [gatedDated, trustedDated])
                `shouldBe` Just tWin

    describe "latest via the shared selector" $ do
        -- 'Ecluse.Core.Version.selectLatest' resolves latest and has its own exhaustive spec.
        -- These cases only check that the merge wires it in.
        it "keeps the chosen latest when it still survives (no promotion)" $ do
            -- The trusted source tags latest at 1.0.0 and that version survives, so
            -- latest stays 1.0.0 even though 2.0.0 exists in the union.
            let trusted =
                    ( TrustedSource
                    , (packument [("1.0.0", sriAaa)])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "1.0.0")
                        }
                    )
                gated = (GatedSource, packument [("2.0.0", sriBbb)])
            (latestKey =<< mergePackuments [trusted, gated]) `shouldBe` Just "1.0.0"

        it "chooses the chosen-latest by provenance (trusted's tag wins)" $ do
            -- Both sources survive and both tag a latest. The trusted source's latest
            -- is the chosen one, even though it is the lower version.
            let trusted =
                    ( TrustedSource
                    , (packument [("1.0.0", sriAaa)])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "1.0.0")
                        }
                    )
                gated =
                    ( GatedSource
                    , (packument [("2.0.0", sriBbb)])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "2.0.0")
                        }
                    )
            (latestKey =<< mergePackuments [trusted, gated]) `shouldBe` Just "1.0.0"

        it "repoints to the highest stable survivor over a prerelease when chosen is gone" $ do
            -- The chosen latest (5.0.0) was denied or absent. Among the survivors, a
            -- stable release wins over a higher prerelease.
            let info =
                    (packument [("2.0.0", sriAaa), ("3.0.0-rc.1", sriBbb)])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "5.0.0")
                        }
            (latestKey =<< mergePackuments [(GatedSource, info)]) `shouldBe` Just "2.0.0"

        it "falls back to a surviving prerelease when no stable survivor exists" $ do
            let info =
                    (packument [("3.0.0-rc.1", sriAaa), ("3.0.0-beta", sriBbb)])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "5.0.0")
                        }
            (latestKey =<< mergePackuments [(GatedSource, info)]) `shouldBe` Just "3.0.0-rc.1"

    describe "properties" $ do
        it "the survivors are exactly the union of every source's keys" $
            hedgehog $ do
                sources <- forAll genSources
                plan <- H.evalMaybe (mergePackuments sources)
                let expected = sort (nub (concatMap (Map.keys . infoVersions . snd) sources))
                survivorKeys plan === expected

        it "every surviving dist-tag target is a surviving version key" $
            hedgehog $ do
                sources <- forAll genSources
                plan <- H.evalMaybe (mergePackuments sources)
                let keys = Map.keys (mpSurvivors plan)
                    targets = map unVersion (Map.elems (mpDistTags plan))
                H.assert (all (`elem` keys) targets)

        it "latest, when present, is a surviving key" $
            hedgehog $ do
                sources <- forAll genSources
                plan <- H.evalMaybe (mergePackuments sources)
                case latestKey plan of
                    Nothing -> H.success
                    Just k -> H.assert (k `elem` Map.keys (mpSurvivors plan))

        it "time keys are a subset of the survivors" $
            hedgehog $ do
                sources <- forAll genSources
                plan <- H.evalMaybe (mergePackuments sources)
                let keys = Map.keys (mpSurvivors plan)
                H.assert (all (`elem` keys) (Map.keys (mpTime plan)))

        it "a single input is the identity over the plan" $
            hedgehog $ do
                src@(_, info) <- forAll genSource
                plan <- H.evalMaybe (mergePackuments [src])
                Map.keys (mpSurvivors plan) === Map.keys (infoVersions info)
                nub (Map.elems (mpSurvivors plan)) === ([0 | not (Map.null (infoVersions info))])
                -- Every test version carries a folded publish time, so the reconstructed
                -- served @time@ keys are exactly the surviving version keys.
                Map.keys (mpTime plan) === Map.keys (infoVersions info)
                mpDivergences plan === Set.empty

        it "the surviving set and time union are order-independent" $
            hedgehog $ do
                -- Disjoint sources never collide, so the survivor set and time union are a pure
                -- set operation any permutation reproduces. A permutation relabels 'SourceId'.
                sources <- forAll genDisjointSources
                perm <- forAll (Gen.shuffle sources)
                a <- H.evalMaybe (mergePackuments sources)
                b <- H.evalMaybe (mergePackuments perm)
                Map.keys (mpSurvivors a) === Map.keys (mpSurvivors b)
                mpTime a === mpTime b

        it "the private copy wins the tiebreak regardless of source order" $
            hedgehog $ do
                ver <- forAll genVersionStr
                privDigest <- forAll genDigest
                pubDigest <- forAll (Gen.filter (/= privDigest) genDigest)
                let trusted = (TrustedSource, packument [(ver, privDigest)])
                    gated = (GatedSource, packument [(ver, pubDigest)])
                -- trusted at index 0 forward and index 1 backward. The survivor must
                -- name the trusted source either way.
                forward <- H.evalMaybe (winnerOf ver =<< mergePackuments [trusted, gated])
                backward <- H.evalMaybe (winnerOf ver =<< mergePackuments [gated, trusted])
                forward === 0
                backward === 1

        it "a divergence is detected iff a shared version's integrity differs" $
            hedgehog $ do
                ver <- forAll genVersionStr
                d1 <- forAll genDigest
                d2 <- forAll genDigest
                let trusted = (TrustedSource, packument [(ver, d1)])
                    gated = (GatedSource, packument [(ver, d2)])
                plan <- H.evalMaybe (mergePackuments [trusted, gated])
                let diverged = not (Set.null (mpDivergences plan))
                diverged === (d1 /= d2)

        it "an extra SHA-1 on one copy never diverges while the shared SRI agrees" $
            hedgehog $ do
                -- The asymmetric-digest invariant: whatever legacy SHA-1 one mirror adds, two
                -- copies that agree on the shared SRI are the same bytes and never diverge.
                ver <- forAll genVersionStr
                sri <- forAll genDigest
                extra <- forAll genSha1
                let trusted = (TrustedSource, packumentWith [(ver, [unsafeHash SRI sri, unsafeHash SHA1 extra])])
                    gated = (GatedSource, packumentWith [(ver, [unsafeHash SRI sri])])
                plan <- H.evalMaybe (mergePackuments [trusted, gated])
                mpDivergences plan === Set.empty

    describe "the merge accumulator is a lawful Monoid" $ do
        -- The 'Merge' accumulator is associative and identity-respecting, but deliberately
        -- not commutative, because the 'SourceId' tiebreak is positional.
        it "is associative: (a <> b) <> c === a <> (b <> c)" $
            hedgehog $ do
                a <- forAll genMerge
                b <- forAll genMerge
                c <- forAll genMerge
                (a <> b) <> c === a <> (b <> c)

        it "has mempty as a left identity: mempty <> a === a" $
            hedgehog $ do
                a <- forAll genMerge
                mempty <> a === a

        it "has mempty as a right identity: a <> mempty === a" $
            hedgehog $ do
                a <- forAll genMerge
                a <> mempty === a

        it "is intentionally NOT commutative (SourceId labels are positional)" $ do
            -- 'SourceId' must name the input's position so the serve layer can index back to a
            -- raw Value. Swapping operands swaps the labels while the decision stays the same.
            let trusted = contribute TrustedSource (packument [("1.0.0", sriPriv)])
                gated = contribute GatedSource (packument [("1.0.0", sriPub)])
                forward = planFrom (trusted <> gated)
                backward = planFrom (gated <> trusted)
            -- Same decision (trusted wins) but opposite positional label, so the two
            -- plans, and the two accumulators, are genuinely not equal.
            (trusted <> gated == gated <> trusted) `shouldBe` False
            (winnerOf "1.0.0" =<< forward) `shouldBe` Just 0
            (winnerOf "1.0.0" =<< backward) `shouldBe` Just 1

        it "mergePackuments is planFrom . foldMap contribute" $
            hedgehog $ do
                sources <- forAll genSources
                mergePackuments sources === planFrom (foldMap (uncurry contribute) sources)

    describe "the laws do not erode the trust hierarchy" $ do
        it "the trust order IS the hierarchy: TrustedSource < GatedSource (keystone -- do not reorder)" $
            -- Not a trivial Ord check. 'TrustedSource' sorts before 'GatedSource', and "smallest
            -- wins" in 'Set.findMin' and 'keepBetter' is what gives the private upstream its
            -- authority. Reorder those constructors and a tampered public copy wins every
            -- collision.
            compare TrustedSource GatedSource `shouldBe` LT

        it "trusted wins a collision; the divergence's winner is the trusted copy" $ do
            let trusted = (TrustedSource, packument [("1.0.0", sriPriv)])
                gated = (GatedSource, packument [("1.0.0", sriPub)])
                plan = mergePackuments [gated, trusted] -- trusted at index 1
            (winnerOf "1.0.0" =<< plan) `shouldBe` Just 1
            case Set.toList . mpDivergences <$> plan of
                Just [d] -> do
                    divVersion d `shouldBe` "1.0.0"
                    integrityHashes (divWinning d) `shouldBe` [sriPair sriPriv]
                    integrityHashes (divLosing d) `shouldBe` [sriPair sriPub]
                other -> expectationFailure ("expected one divergence, got " <> show other)

        it "the merged set is the mixed-provenance union trusted ∪ filtered(public)" $ do
            -- Versions unique to each upstream are all present. The trust split does
            -- not drop a side, it unions them.
            let trusted = (TrustedSource, packument [("1.0.0", sriLowA), ("1.1.0", sriLowB)])
                gated = (GatedSource, packument [("2.0.0", sriLowC), ("1.1.0", sriLowB)])
            (survivorKeys <$> mergePackuments [trusted, gated])
                `shouldBe` Just ["1.0.0", "1.1.0", "2.0.0"]

        it "identical integrity across sources yields no divergence" $ do
            let trusted = (TrustedSource, packument [("1.0.0", sriSame)])
                gated = (GatedSource, packument [("1.0.0", sriSame)])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "a 3+-copy collision fans the winner out against each distinct loser" $ do
            -- Three copies of one key with three distinct fingerprints, one trusted and two
            -- gated. The winner is recorded against each distinct loser exactly once.
            let t = (TrustedSource, packument [("1.0.0", sriT)]) -- index 0, wins
                g1 = (GatedSource, packument [("1.0.0", sriG1)]) -- index 1
                g2 = (GatedSource, packument [("1.0.0", sriG2)]) -- index 2
                plan = mergePackuments [t, g1, g2]
            (winnerOf "1.0.0" =<< plan) `shouldBe` Just 0
            let expected =
                    Set.fromList
                        [ ("1.0.0", [sriPair sriT], [sriPair sriG1])
                        , ("1.0.0", [sriPair sriT], [sriPair sriG2])
                        ]
                actual =
                    Set.map
                        (\d -> (divVersion d, integrityHashes (divWinning d), integrityHashes (divLosing d)))
                        . mpDivergences
                        <$> plan
            actual `shouldBe` Just expected

        it "a 3+-copy collision's divergences are associativity-stable (regroup the fold)" $ do
            -- A pairwise winner-versus-loser fold would break this: regrouping would change the
            -- pairs.
            let t = contribute TrustedSource (packument [("1.0.0", sriT)])
                g1 = contribute GatedSource (packument [("1.0.0", sriG1)])
                g2 = contribute GatedSource (packument [("1.0.0", sriG2)])
                left = planFrom ((t <> g1) <> g2)
                right = planFrom (t <> (g1 <> g2))
            (mpDivergences <$> left) `shouldBe` (mpDivergences <$> right)

        it "dist-tags: keep-unless-denied, absent-target dropped, by provenance" $ do
            -- The merge drops 'next' because its target 9.9.9 is absent from the union.
            let trusted =
                    ( TrustedSource
                    , (packument [("1.0.0", sriLowA)])
                        { infoDistTags =
                            Map.fromList
                                [ ("latest", mkVersion Npm "1.0.0")
                                , ("next", mkVersion Npm "9.9.9")
                                ]
                        }
                    )
                gated = (GatedSource, packument [("2.0.0", sriLowB)])
                plan = mergePackuments [gated, trusted]
            (latestKey =<< plan) `shouldBe` Just "1.0.0"
            (sort . Map.keys . mpDistTags <$> plan) `shouldBe` Just ["latest"]

        it "single source is the degenerate identity: all survive, won by source 0" $
            hedgehog $ do
                src@(_, info) <- forAll genSource
                plan <- H.evalMaybe (mergePackuments [src])
                Map.keys (mpSurvivors plan) === Map.keys (infoVersions info)
                nub (Map.elems (mpSurvivors plan)) === ([0 | not (Map.null (infoVersions info))])
                -- Every test version carries a folded publish time, so the reconstructed
                -- served @time@ keys are exactly the surviving version keys.
                Map.keys (mpTime plan) === Map.keys (infoVersions info)
                mpDivergences plan === Set.empty

        it "the always-invariant decisions survive any permutation of any inputs" $
            hedgehog $ do
                -- 'genSources' collides keys freely, same-provenance included, so only the
                -- surviving key set and the winning provenance are order-independent. 'SourceId'
                -- labels move.
                sources <- forAll genSources
                perm <- forAll (Gen.shuffle sources)
                base <- H.evalMaybe (mergePackuments sources)
                shuffled <- H.evalMaybe (mergePackuments perm)
                sort (Map.keys (mpSurvivors base)) === sort (Map.keys (mpSurvivors shuffled))
                winnerProvenances sources base === winnerProvenances perm shuffled

        it "every decision is order-independent in the npm (1 trusted, 1 gated) topology" $
            hedgehog $ do
                -- The architecture's topology is exactly one trusted and one gated upstream, so
                -- every collision is cross-provenance and every decision is order-independent.
                trusted <- forAll (snd <$> genSource)
                gated <- forAll (snd <$> genSource)
                let fwd = [(TrustedSource, trusted), (GatedSource, gated)]
                    bwd = [(GatedSource, gated), (TrustedSource, trusted)]
                forward <- H.evalMaybe (mergePackuments fwd)
                backward <- H.evalMaybe (mergePackuments bwd)
                sort (Map.keys (mpSurvivors forward)) === sort (Map.keys (mpSurvivors backward))
                winnerProvenances fwd forward === winnerProvenances bwd backward
                mpDivergences forward === mpDivergences backward
                mpDistTags forward === mpDistTags backward
                mpTime forward === mpTime backward

        it "within one provenance, the divergence winner is positional (documented boundary)" $ do
            -- Provenance cannot break a same-provenance tie, so the lower 'SourceId' (the earlier
            -- position) wins. Collisions in the npm topology always cross provenance, so this is
            -- the documented boundary of the order-independence guarantee.
            let a = (GatedSource, packument [("1.0.0", sriCapA)]) -- earlier wins
                b = (GatedSource, packument [("1.0.0", sriCapB)])
                forward = Set.toList . mpDivergences <$> mergePackuments [a, b]
                backward = Set.toList . mpDivergences <$> mergePackuments [b, a]
            (map (integrityHashes . divWinning) <$> forward) `shouldBe` Just [[sriPair sriCapA]]
            (map (integrityHashes . divWinning) <$> backward) `shouldBe` Just [[sriPair sriCapB]]

{- | Sources with pairwise-disjoint version keys, so the merge is a pure set union
with no collisions: the regime in which order cannot matter at all.
-}
genDisjointSources :: Gen [(Provenance, PackageInfo)]
genDisjointSources = do
    n <- Gen.int (Range.linear 1 4)
    pure [oneSource i | i <- [1 .. n]]
  where
    -- Source @i@ owns the single version @i.0.0@, so no two sources share a key.
    oneSource i =
        let ver = show (i :: Int) <> ".0.0"
         in (if even i then TrustedSource else GatedSource, packument [(ver, validSriOf ver)])
