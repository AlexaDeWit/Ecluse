-- This spec deliberately writes out the Monoid identity laws (@mempty <> a@ and
-- @a <> mempty@) to /assert/ them; hlint would otherwise "simplify" the very
-- expressions under test. Silenced file-wide because proving the laws is the
-- file's purpose, not an oversight.
{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

module Ecluse.Package.MergeSpec (spec) where

import Crypto.Hash (Digest, SHA1, SHA256, SHA512, hash)
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
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
import Ecluse.Test.Package (unsafeHash)

name :: PackageName
name = mkPackageName Npm Nothing "thing"

{- | One tarball carrying a chosen set of integrity digests, so two copies of the
same version key can be made to agree, contradict, or merely expose asymmetric
algorithm sets purely on integrity.
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

{- | A per-version snapshot for a raw version string, carrying a chosen set of
integrity digests. Everything else is inert -- the merge reads only the version
key, the parsed version (for @latest@), and artifact integrity (for divergence).
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

{- | The fixed publish instant every 'detailsWith' version carries (the folded
per-version @time@); overridden per-version by 'withPublishedAt' where a test needs
distinct cross-source instants.
-}
t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

{- | Build a single-package packument from @(rawVersion, integrityDigests)@ pairs,
each version carrying the given set of integrity hashes (so two copies of one key
can expose asymmetric algorithm sets). @latest@ is pointed at the lexically-highest
version (a coherent packument always tags its newest release), so a lone source is
already a fixed point of the merge's @latest@ reconciliation; every version carries the
fixed publish instant 't0' on its own snapshot.
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

{- | Override the publish instant carried by every version of a packument: the per-version
counterpart of the old sibling @time@ override, for tests pinning cross-source instants.
-}
withPublishedAt :: UTCTime -> PackageInfo -> PackageInfo
withPublishedAt t info =
    info{infoVersions = Map.map (\d -> d{pkgPublishedAt = Just t}) (infoVersions info)}

{- | Build a packument whose every version carries a single SRI digest -- the common
case for the collision and reconciliation tests, where the algorithm set is uniform
and only the digest value varies.
-}
packument :: [(Text, Text)] -> PackageInfo
packument vs = packumentWith [(v, [unsafeHash SRI d]) | (v, d) <- vs]

-- A well-formed sha512 SRI deterministically derived from a mnemonic label, so distinct
-- labels yield distinct well-formed digests and the same label always yields the same one.
validSriOf :: Text -> Text
validSriOf label =
    "sha512-" <> decodeUtf8 (convertToBase Base64 (hash (encodeUtf8 label :: ByteString) :: Digest SHA512) :: ByteString)

-- Well-formed hex SHA-1 / SHA-256 digests derived from a label (40- / 64-hex).
validSha1Of, validSha256Of :: Text -> Text
validSha1Of label =
    decodeUtf8 (convertToBase Base16 (hash (encodeUtf8 label :: ByteString) :: Digest SHA1) :: ByteString)
validSha256Of label =
    decodeUtf8 (convertToBase Base16 (hash (encodeUtf8 label :: ByteString) :: Digest SHA256) :: ByteString)

-- A well-formed sha256 SRI (@sha256-\<base64\>@) deterministically derived from a label --
-- the SRI encoding of a sha256 digest a recomputing mirror serves in place of, or beside,
-- npm's sha512.
validSha256SriOf :: Text -> Text
validSha256SriOf label =
    "sha256-" <> decodeUtf8 (convertToBase Base64 (hash (encodeUtf8 label :: ByteString) :: Digest SHA256) :: ByteString)

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

-- The fingerprint triple an SRI digest resolves to under the merge's keying: the
-- fixture artifact's filename, its embedded algorithm (via 'sriAlgorithm'), and its
-- base64 body (via 'sriBody'), as 'integrityHashes' reads them back. The mnemonic
-- fixtures are all well-formed SRIs, so the algorithm is always 'Just'; the file is
-- the single fixture artifact every 'artifactWith' version carries.
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

{- | The winning __provenance__ per surviving version key, resolved back through the
inputs the plan was built from. A 'SourceId' is a list index, so this maps each
survivor's winning index to the 'Provenance' of the input at that position -- the
order-/independent/ decision the merge owns, beneath the order-/dependent/ label.
-}
winnerProvenances :: [(Provenance, PackageInfo)] -> MergePlan -> Map Text Provenance
winnerProvenances inputs plan =
    -- Index the inputs by 'SourceId' (their list position) up front, so the lookup
    -- is total -- no partial indexing into the list.
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

{- | An arbitrary 'Merge' accumulator: a 'foldMap' of 'contribute' over a small
list of sources, so the value carries internal 'SourceId's @0..n-1@ and exercises
collisions, tags, and times -- the realistic inputs the laws must hold over. The
empty list yields 'mempty', so the identity is in the generated range too.
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
            -- Every contribution shares the validated identity (name validation runs
            -- upstream of the merge), so the plan's mpName originates from an input's
            -- own 'infoName' -- it is never substituted or fabricated.
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
            -- Same version key in both, with differing integrity. The plan records
            -- the surviving key against the trusted input's 'SourceId', so the serve
            -- layer takes that version's object from the private source's raw Value.
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
            -- The trusted source's chosen latest (9.9.9) is not actually carried,
            -- so selectLatest repoints across the union to the highest stable
            -- survivor (3.0.0).
            let trusted =
                    (packument [("1.0.0", sriAaa)])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "9.9.9")
                        }
                gated = packument [("3.0.0", sriBbb), ("2.0.0", sriCcc)]
            (latestKey =<< mergePackuments [(TrustedSource, trusted), (GatedSource, gated)])
                `shouldBe` Just "3.0.0"

        it "drops a dist-tag whose target is absent from the union" $ do
            -- A source advertises a "next" tag pointing at a version it does not
            -- actually carry; the merge drops it rather than serving a dangling tag.
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
        -- A version present in both a trusted (private) and a gated (public) source
        -- is a collision: the trusted copy wins. If the two copies' artifact
        -- integrity disagrees the merge *flags* it as a tampering signal -- and
        -- flags without dropping the version, leaving fail-closed to the caller.
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
        -- 2.0.0 (the @latest@) diverges across sources; 1.0.0 agrees. The projection is
        -- what the serve layer runs AFTER logging and metering the divergence, so a
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

    describe "parseDivergencePolicy (the ECLUSE_DIVERGENCE_POLICY value)" $ do
        it "parses warn and fail-closed, case- and spelling-tolerant" $ do
            parseDivergencePolicy "warn" `shouldBe` Right Warn
            parseDivergencePolicy "fail-closed" `shouldBe` Right FailClosed
            parseDivergencePolicy "FAIL_CLOSED" `shouldBe` Right FailClosed
            parseDivergencePolicy "  FailClosed  " `shouldBe` Right FailClosed

        it "rejects an unknown policy" $
            parseDivergencePolicy "drop" `shouldSatisfy` isLeft

    describe "divergence compares on shared algorithms, not the whole digest set" $ do
        -- A divergence is reported only when two copies *contradict* on an algorithm
        -- they both carry. An asymmetric digest set -- one mirror also serving a digest
        -- the other omits -- is not, on its own, a contradiction: an older registry
        -- exposing only a legacy shasum while npmjs serves shasum + a modern SRI
        -- describes the same bytes and must not be flagged.
        let sha1 = unsafeHash SHA1
            sri = unsafeHash SRI

        it "agreeing on the shared SRI is not a divergence though one also carries SHA-1" $ do
            -- Both expose the same sha512 SRI; the private copy additionally carries a
            -- legacy SHA-1 shasum the public copy lacks. The shared algorithm (SRI)
            -- agrees, so this is the same bytes -- not a divergence.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri sriX, sha1 sha1Dead])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri sriX])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "contradicting on the shared SRI is a divergence even when SHA-1 agrees" $ do
            -- Both carry the same SHA-1 but a *different* sha512 SRI. A SHA-1 agreement
            -- can never rescue a contradicting secure digest, so the SRI contradiction
            -- is flagged regardless of the matching weak digest beside it.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri sriX, sha1 sha1Abc])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri sriY, sha1 sha1Abc])])
                plan = mergePackuments [trusted, gated]
            (map divVersion . Set.toList . mpDivergences <$> plan) `shouldBe` Just ["1.0.0"]

        it "private SHA-1 vs public SHA-1+SHA-256 cross-checks on the shared SHA-1 (not a divergence)" $ do
            -- The blessed asymmetric-trust case: a private (trusted) upstream serving
            -- only a legacy SHA-1 shasum, a public one serving that shasum plus a modern
            -- SHA-256. They share SHA-1 and it agrees, so the cross-check passes -- the
            -- public copy independently clears the admission floor on its SHA-256, and
            -- the asymmetric SHA-256 is no contradiction.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sha1 sha1Abc])])
                gated = (GatedSource, packumentWith [("1.0.0", [sha1 sha1Abc, unsafeHash SHA256 sha256Def])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "SRI+SHA-1 vs SHA-1-only, agreeing on the shared SHA-1, is not a divergence" $ do
            -- One copy carries sha512 + sha1, the other only the legacy sha1, and that
            -- single shared algorithm agrees. With no contradiction on a shared
            -- algorithm this is not a divergence; the comparison only ever flags a
            -- shared algorithm whose digests disagree. (Pinned so the current behaviour
            -- is explicit: whether a weak-only agreement should itself be treated as
            -- suspicious is a separate, stricter policy not decided by this fold.)
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri sriX, sha1 sha1Abc])])
                gated = (GatedSource, packumentWith [("1.0.0", [sha1 sha1Abc])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        -- A single version can carry several digests of *one* algorithm -- the domain
        -- model allows many artifacts ('pkgArtifacts' is a 'NonEmpty'), each with its
        -- own hashes (a PyPI sdist + wheels may each carry a SHA-256), and 'fingerprint'
        -- gathers them all. For a shared algorithm the copies therefore agree only when
        -- the set of digests they each offer for it matches.
        it "agrees when a shared algorithm carries the same set of digests in any order" $ do
            -- The same two SRI digests on both copies, listed in opposite order: the
            -- per-algorithm set is identical, so this is not a divergence.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri sriX, sri sriY])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri sriY, sri sriX])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "contradicts when a shared algorithm's set of digests differs" $
            -- One copy offers two SRI digests for the key, the other only one of them:
            -- the digest sets for the shared algorithm differ, so it is flagged.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri sriX, sri sriY])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri sriX])])
             in case Set.toList . mpDivergences <$> mergePackuments [trusted, gated] of
                    Just [d] -> do
                        divVersion d `shouldBe` "1.0.0"
                        integrityHashes (divWinning d) `shouldBe` sort [sriPair sriX, sriPair sriY]
                        integrityHashes (divLosing d) `shouldBe` [sriPair sriX]
                    other -> expectationFailure ("expected exactly one divergence, got " <> show other)

    describe "divergence keys per artifact, not per version (#739)" $ do
        -- A multi-artifact ecosystem (PyPI: an sdist plus wheels) spreads a version's
        -- digests across files. Collapsing them into one per-algorithm set made two
        -- mirrors with a different file *set* read as tampering; the fingerprint keys
        -- each digest by its file, so only a shared file's shared algorithm can
        -- contradict.
        let sri = unsafeHash SRI
            withArtifacts arts info =
                info{infoVersions = Map.map (\d -> d{pkgArtifacts = arts}) (infoVersions info)}
            wheelWith fileName hs =
                (artifactWith hs){artFilename = fileName, artUrl = "https://example.test/" <> fileName}

        it "a mirror carrying fewer files than the index is availability, not a divergence" $ do
            -- Both serve thing.tgz with the same digest; the public index additionally
            -- carries a wheel the mirror lacks. No shared file contradicts, so no
            -- divergence -- the differing file set describes availability, not
            -- substituted bytes.
            let sharedFile = artifactWith [sri sriX]
                extraWheel = wheelWith "thing-extra.whl" [sri sriY]
                trusted = (TrustedSource, withArtifacts (one sharedFile) (packumentWith [("1.0.0", [])]))
                gated = (GatedSource, withArtifacts (sharedFile :| [extraWheel]) (packumentWith [("1.0.0", [])]))
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "a shared file contradicting under a shared algorithm diverges even amid differing file sets" $ do
            -- The tampering signal survives the per-artifact keying: the shared
            -- thing.tgz disagrees on its sha512 body, so the version diverges even
            -- though the sides also differ in which files they carry.
            let extraWheel = wheelWith "thing-extra.whl" [sri sriY]
                trusted = (TrustedSource, withArtifacts (one (artifactWith [sri sriX])) (packumentWith [("1.0.0", [])]))
                gated = (GatedSource, withArtifacts (artifactWith [sri sriY] :| [extraWheel]) (packumentWith [("1.0.0", [])]))
            (map divVersion . Set.toList . mpDivergences <$> mergePackuments [trusted, gated])
                `shouldBe` Just ["1.0.0"]

        it "the same digest under a renamed file is asymmetric, not a divergence" $ do
            -- A file served under different names on the two sides shares no
            -- (file, algorithm) key, so nothing can contradict: the fail-open reading
            -- on absence, consistent with the asymmetric-algorithm stance above.
            let trusted = (TrustedSource, withArtifacts (one (artifactWith [sri sriX])) (packumentWith [("1.0.0", [])]))
                gated = (GatedSource, withArtifacts (one (wheelWith "renamed.tgz" [sri sriY])) (packumentWith [("1.0.0", [])]))
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

    describe "divergence keys on the resolved algorithm, not the raw digest tag" $ do
        -- The comparison resolves each digest to the algorithm it asserts and compares
        -- the digest body under that resolved key, so an SRI is bucketed by its embedded
        -- algorithm rather than the opaque SRI wrapper tag. This closes a live false
        -- positive (different algorithms over the same bytes) and a latent false negative
        -- (one algorithm expressed two ways).

        it "a sha256 SRI and a sha512 SRI for the same bytes are asymmetric, not a divergence" $ do
            -- A private mirror that recomputes integrity as sha256 and a public copy
            -- serving sha512 over the same bytes share NO resolved algorithm, so the
            -- digest sets are asymmetric -- not a contradiction. (Keying on the raw SRI tag
            -- bucketed both under one tag with differing strings and spuriously diverged.)
            let trusted = (TrustedSource, packumentWith [("1.0.0", [unsafeHash SRI (validSha256SriOf "same")])])
                gated = (GatedSource, packumentWith [("1.0.0", [unsafeHash SRI (validSriOf "same")])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "a hex SHA-256 and an sha256 SRI that disagree are a divergence (same resolved algorithm)" $ do
            -- One upstream expresses SHA-256 as a hex Hash, the other as an sha256 SRI,
            -- with different digests. Both resolve to SHA-256, so the contradiction is
            -- caught. (Keying on the raw tag put them under different tags -- SHA256 vs SRI
            -- -- so a genuine same-algorithm contradiction was silently missed.)
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
            -- Every provenance-resolved decision must be order-independent: the
            -- surviving keys, the reconciled tags (incl. latest), the time union,
            -- and the divergences. The only thing that legitimately differs is the
            -- winner's 'SourceId' -- a faithful pointer to the trusted input's
            -- position, asserted to name the trusted source below.
            let forward = mergePackuments [trusted, gated]
                backward = mergePackuments [gated, trusted]
            (Map.keys . mpSurvivors <$> forward) `shouldBe` (Map.keys . mpSurvivors <$> backward)
            (mpDistTags <$> forward) `shouldBe` (mpDistTags <$> backward)
            (mpTime <$> forward) `shouldBe` (mpTime <$> backward)
            (mpDivergences <$> forward) `shouldBe` (mpDivergences <$> backward)

        it "a non-latest tag resolves to the trusted target regardless of order" $ do
            -- 'beta' is a non-'latest' tag; both sources tag it at 1.0.0 but with
            -- different integrity behind that key. The plan keeps the tag, and the
            -- survivor for 1.0.0 is the trusted copy either ordering.
            let forward = winnerOf "1.0.0" =<< mergePackuments [trusted, gated]
                backward = winnerOf "1.0.0" =<< mergePackuments [gated, trusted]
            -- trusted is index 0 forward, index 1 backward; both must name trusted.
            forward `shouldBe` Just 0
            backward `shouldBe` Just 1

        it "time resolves to the trusted source's instant regardless of order" $ do
            (Map.lookup "1.0.0" . mpTime =<< mergePackuments [trusted, gated])
                `shouldBe` Just tTrusted
            (Map.lookup "1.0.0" . mpTime =<< mergePackuments [gated, trusted])
                `shouldBe` Just tTrusted

    describe "a version's served time comes from the source that won its manifest" $ do
        -- The correctness fix: the served publish time is read off the SAME winning
        -- candidate whose manifest is served, so it can never be fabricated from a
        -- different source than the bytes it stamps. The decisive case is a manifest
        -- whose winning source carries NO time while a losing source does: the served
        -- time must be ABSENT, not the loser's date applied to bytes it never described.
        it "does not borrow a losing source's time for a winning manifest (no false time)" $ do
            -- Trusted wins 1.0.0's manifest but knows no publish time for it; the gated
            -- copy carries a date. The served time must not be that gated date.
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
            -- Both sources carry a date; trusted wins the manifest, so its date is served.
            let tWin = UTCTime (fromGregorian 2026 4 4) 0
                tLose = UTCTime (fromGregorian 2018 2 2) 0
                trustedDated = (TrustedSource, withPublishedAt tWin (packument [("1.0.0", sriPriv)]))
                gatedDated = (GatedSource, withPublishedAt tLose (packument [("1.0.0", sriPub)]))
            (Map.lookup "1.0.0" . mpTime =<< mergePackuments [trustedDated, gatedDated])
                `shouldBe` Just tWin
            (Map.lookup "1.0.0" . mpTime =<< mergePackuments [gatedDated, trustedDated])
                `shouldBe` Just tWin

    describe "latest via the shared selector" $ do
        -- latest is resolved by Ecluse.Core.Version.selectLatest, so the merge inherits
        -- keep-unless-denied + stable-preferring + unparseable-safe behaviour.
        -- selectLatest is exhaustively unit-tested in its own spec; these only
        -- check that it is wired into the merge correctly.
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
            -- Both sources survive and both tag a latest; the trusted source's
            -- latest is the chosen one, even though it is the lower version.
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
            -- The chosen latest (5.0.0) was denied/absent; among survivors a stable
            -- release is preferred over a higher prerelease.
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
                -- A lone source survives whole: every version is a survivor, all won
                -- by source 0, no collisions, and every carried tag targets one of
                -- its own versions.
                Map.keys (mpSurvivors plan) === Map.keys (infoVersions info)
                nub (Map.elems (mpSurvivors plan)) === ([0 | not (Map.null (infoVersions info))])
                -- Every test version carries a folded publish time, so the reconstructed
                -- served @time@ keys are exactly the surviving version keys.
                Map.keys (mpTime plan) === Map.keys (infoVersions info)
                mpDivergences plan === Set.empty

        it "the surviving set and time union are order-independent" $
            hedgehog $ do
                -- On disjoint sources there are no cross-source collisions, so the
                -- survivor set and the time union are a pure set operation and any
                -- permutation yields the same ones. (The winning SourceId of a key
                -- is its source's index, which a permutation relabels by design; and
                -- the provenance precedence of a *colliding* tag/time is checked
                -- deterministically in "precedence is by provenance" above, the
                -- two-source split the architecture defines.)
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
                -- trusted at index 0 forward, index 1 backward; the survivor must
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
                -- The asymmetric-digest invariant, generalised: whatever legacy SHA-1
                -- one mirror adds, two copies that agree on the shared SRI are the same
                -- bytes and never diverge on the asymmetry alone.
                ver <- forAll genVersionStr
                sri <- forAll genDigest
                extra <- forAll genSha1
                let trusted = (TrustedSource, packumentWith [(ver, [unsafeHash SRI sri, unsafeHash SHA1 extra])])
                    gated = (GatedSource, packumentWith [(ver, [unsafeHash SRI sri])])
                plan <- H.evalMaybe (mergePackuments [trusted, gated])
                mpDivergences plan === Set.empty

    describe "the merge accumulator is a lawful Monoid" $ do
        -- The fold is realised over the 'Merge' accumulator; its laws are what make
        -- 'mergePackuments' associative and identity-respecting. The instance is
        -- deliberately associative + identity but *not* commutative (the 'SourceId'
        -- tiebreak is positional); the decision-order-independence the business
        -- rules need is proved separately below, over input permutations.
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
            -- This is documented behaviour, not a defect: 'SourceId' must name the
            -- input's *position* so the serve layer can index back to a raw Value,
            -- so swapping operands swaps the labels. We assert the asymmetry rather
            -- than papering over it: two single-input merges of *different*
            -- provenance at the *same* version key, combined both ways. The decision
            -- (trusted wins) is identical; the winning SourceId label flips with
            -- the order -- which is exactly why commutativity is the wrong law.
            let trusted = contribute TrustedSource (packument [("1.0.0", sriPriv)])
                gated = contribute GatedSource (packument [("1.0.0", sriPub)])
                forward = planFrom (trusted <> gated)
                backward = planFrom (gated <> trusted)
            -- Same decision (trusted wins) but opposite positional label, so the
            -- two plans -- and the two accumulators -- are genuinely not equal.
            (trusted <> gated == gated <> trusted) `shouldBe` False
            (winnerOf "1.0.0" =<< forward) `shouldBe` Just 0
            (winnerOf "1.0.0" =<< backward) `shouldBe` Just 1

        it "mergePackuments is planFrom . foldMap contribute" $
            hedgehog $ do
                sources <- forAll genSources
                mergePackuments sources === planFrom (foldMap (uncurry contribute) sources)

    describe "the laws do not erode the trust hierarchy" $ do
        -- The architect's explicit requirement: prove the lawful refactor still
        -- enforces the business rules -- trusted-wins precedence, the union, the
        -- divergence signal, and (the core property) order-independence of every
        -- decision a caller can observe except the positional 'SourceId' label.

        it "the trust order IS the hierarchy: TrustedSource < GatedSource (keystone -- do not reorder)" $
            -- DO NOT read this as a trivial Enum/Ord check. This single line is the
            -- keystone of the entire merge. Every "the private registry always wins"
            -- decision the module makes -- which copy survives a version collision,
            -- whose integrity is recorded as the divergence winner, and which
            -- source's dist-tags and time are kept -- is resolved by 'Set.findMin' /
            -- 'keepBetter' over the @(Provenance, SourceId)@ rank, whose precedence
            -- is governed entirely by this 'Ord Provenance'. 'TrustedSource' is
            -- declared before 'GatedSource', so the derived 'Ord' makes it the
            -- smaller value, and "smallest wins" is what gives the private upstream
            -- authority.
            --
            -- If a future edit reorders the 'Provenance' constructors (or otherwise
            -- inverts this comparison), the trust relationship flips SILENTLY: the
            -- public upstream would win every collision, and a tampered public copy
            -- could shadow the vetted private one -- the precise supply-chain failure
            -- Écluse exists to prevent -- with nothing else in the types objecting.
            -- A failure here therefore means "the trust hierarchy has been inverted,"
            -- NOT "update the expected value." This assertion is the tripwire; keep it.
            compare TrustedSource GatedSource `shouldBe` LT

        it "trusted wins a collision; the divergence's winner is the trusted copy" $ do
            -- Trusted and gated collide at 1.0.0 with differing integrity. The
            -- survivor is the trusted copy and the recorded divergence's *winning*
            -- fingerprint is the trusted integrity -- the hierarchy, intact.
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
            -- Versions unique to each upstream are all present; the trust split does
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
            -- THREE copies of one key with three distinct fingerprints: one trusted
            -- (wins), two gated. A non-associative pairwise divergence definition
            -- would miss or double-count one of the losing pairs; the set-of-distinct
            -- -fingerprints definition records the trusted winner against *each* of
            -- the two distinct losers, exactly once.
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
            -- The same three copies, folded in two different associativity groupings
            -- of 'contribute', must yield the same divergence fingerprint-pairs -- the
            -- property a pairwise winner-vs-loser fold would violate.
            let t = contribute TrustedSource (packument [("1.0.0", sriT)])
                g1 = contribute GatedSource (packument [("1.0.0", sriG1)])
                g2 = contribute GatedSource (packument [("1.0.0", sriG2)])
                left = planFrom ((t <> g1) <> g2)
                right = planFrom (t <> (g1 <> g2))
            (mpDivergences <$> left) `shouldBe` (mpDivergences <$> right)

        it "dist-tags: keep-unless-denied, absent-target dropped, by provenance" $ do
            -- 'latest' kept at the trusted source's surviving tag; a 'next' tag whose
            -- target is absent from the union is dropped; resolution is by provenance.
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
                -- Over arbitrary mixed-provenance inputs ('genSources' freely collides
                -- keys, including *same-provenance* collisions), two decisions are
                -- order-independent without qualification: the surviving key *set* and
                -- the winning *provenance* per key. (A same-provenance collision's
                -- concrete winner is positional -- provenance cannot break that tie --
                -- so the value-level targets are asserted in the npm topology below,
                -- and the positional boundary is documented after.) Only the 'SourceId'
                -- labels move; the provenance beneath them does not.
                sources <- forAll genSources
                perm <- forAll (Gen.shuffle sources)
                base <- H.evalMaybe (mergePackuments sources)
                shuffled <- H.evalMaybe (mergePackuments perm)
                sort (Map.keys (mpSurvivors base)) === sort (Map.keys (mpSurvivors shuffled))
                winnerProvenances sources base === winnerProvenances perm shuffled

        it "every decision is order-independent in the npm (1 trusted, 1 gated) topology" $
            hedgehog $ do
                -- The architecture's defined two-source topology -- exactly one trusted
                -- and one gated upstream -- is where the merge actually runs today. Every
                -- collision there is *cross-provenance*, so provenance (trusted wins)
                -- resolves it regardless of position and EVERY decision is fully
                -- order-independent: survivors, winning provenance, divergence
                -- fingerprint-pairs, dist-tags, and time. Only the winner's 'SourceId'
                -- label tracks position. This is the "behaviour preserved" anchor.
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
            -- The boundary of the order-independence guarantee, asserted not hidden:
            -- when two same-provenance copies of a key carry differing integrity,
            -- \*provenance cannot break the tie*, so the lower 'SourceId' (earlier
            -- position) wins -- the same positional tiebreak that makes the Semigroup
            -- non-commutative. This case never arises in the npm topology (collisions
            -- there are always cross-provenance); it is the multi-same-provenance
            -- topology 'SourceId' exists for, where the winner legitimately tracks
            -- order. The *surviving set* and *winning provenance* stay invariant; only
            -- the winner/loser fingerprint labels flip.
            let a = (GatedSource, packument [("1.0.0", sriCapA)]) -- earlier wins
                b = (GatedSource, packument [("1.0.0", sriCapB)])
                forward = Set.toList . mpDivergences <$> mergePackuments [a, b]
                backward = Set.toList . mpDivergences <$> mergePackuments [b, a]
            (map (integrityHashes . divWinning) <$> forward) `shouldBe` Just [[sriPair sriCapA]]
            (map (integrityHashes . divWinning) <$> backward) `shouldBe` Just [[sriPair sriCapB]]

{- | Sources with pairwise-disjoint version keys, so the merge is a pure set union
with no collisions -- the regime in which order cannot matter at all.
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
