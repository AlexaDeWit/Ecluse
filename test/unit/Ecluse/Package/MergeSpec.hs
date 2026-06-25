-- This spec deliberately writes out the Monoid identity laws (@mempty <> a@ and
-- @a <> mempty@) to /assert/ them; hlint would otherwise "simplify" the very
-- expressions under test. Silenced file-wide because proving the laws is the
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

import Ecluse.Ecosystem (Ecosystem (..))
import Ecluse.Package
import Ecluse.Package.Merge
import Ecluse.Version (mkVersion, unVersion)

-- ── fixtures ─────────────────────────────────────────────────────────────────

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
integrity digests. Everything else is inert — the merge reads only the version
key, the parsed version (for @latest@), and artifact integrity (for divergence).
-}
detailsWith :: Text -> [Hash] -> PackageDetails
detailsWith rawVer hs =
    PackageDetails
        { pkgName = name
        , pkgVersion = mkVersion Npm rawVer
        , pkgPublishedAt = Nothing
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = artifactWith hs :| []
        , pkgLicenses = ["MIT"]
        , pkgPublisher = Nothing
        , pkgMaintainers = []
        , pkgDependencies = []
        }

{- | Build a single-package packument from @(rawVersion, integrityDigests)@ pairs,
each version carrying the given set of integrity hashes (so two copies of one key
can expose asymmetric algorithm sets). @latest@ is pointed at the lexically-highest
version (a coherent packument always tags its newest release), so a lone source is
already a fixed point of the merge's @latest@ reconciliation; @time@ gives each
version a fixed instant.
-}
packumentWith :: [(Text, [Hash])] -> PackageInfo
packumentWith vs =
    PackageInfo
        { infoName = name
        , infoVersions = Map.fromList [(v, detailsWith v hs) | (v, hs) <- vs]
        , infoDistTags = case sortOn Down (map fst vs) of
            [] -> Map.empty
            (hi : _) -> Map.singleton "latest" (mkVersion Npm hi)
        , infoPublishedAt = Map.fromList [(v, t0) | (v, _) <- vs]
        }
  where
    t0 = UTCTime (fromGregorian 2026 1 1) 0

{- | Build a packument whose every version carries a single SRI digest — the common
case for the collision and reconciliation tests, where the algorithm set is uniform
and only the digest value varies.
-}
packument :: [(Text, Text)] -> PackageInfo
packument vs = packumentWith [(v, [Hash SRI d]) | (v, d) <- vs]

-- ── small accessors ──────────────────────────────────────────────────────────

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
survivor's winning index to the 'Provenance' of the input at that position — the
order-/independent/ decision the merge owns, beneath the order-/dependent/ label.
-}
winnerProvenances :: [(Provenance, PackageInfo)] -> MergePlan -> Map Text Provenance
winnerProvenances inputs plan =
    -- Index the inputs by 'SourceId' (their list position) up front, so the lookup
    -- is total — no partial indexing into the list.
    Map.mapMaybe (`Map.lookup` byId) (mpSurvivors plan)
  where
    byId = Map.fromList (zip [0 ..] (map fst inputs))

-- ── generators ───────────────────────────────────────────────────────────────

genDigest :: Gen Text
genDigest = ("sha512-" <>) <$> Gen.text (Range.singleton 6) Gen.alphaNum

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
collisions, tags, and times — the realistic inputs the laws must hold over. The
empty list yields 'mempty', so the identity is in the generated range too.
-}
genMerge :: Gen Merge
genMerge = foldMap (uncurry contribute) <$> Gen.list (Range.linear 0 3) genSource

-- ── spec ─────────────────────────────────────────────────────────────────────

spec :: Spec
spec = do
    describe "mergePackuments" $ do
        it "returns Nothing on an empty input (nothing to serve)" $
            mergePackuments [] `shouldBe` Nothing

        it "names the plan after the first input" $ do
            let info = packument [("1.0.0", "sha512-aaa")]
            (mpName <$> mergePackuments [(GatedSource, info)]) `shouldBe` Just name

        it "carries mpName from a contribution, never a manufactured value" $ do
            -- Every contribution shares the validated identity (name validation runs
            -- upstream of the merge), so the plan's mpName originates from an input's
            -- own 'infoName' — it is never substituted or fabricated.
            let a = packument [("1.0.0", "sha512-aaa")]
                b = packument [("2.0.0", "sha512-bbb")]
                inputs = [(TrustedSource, a), (GatedSource, b)]
            (mpName <$> mergePackuments inputs) `shouldBe` Just (infoName a)

        it "is the identity on a single input (survivors, tags, time)" $ do
            -- A lone source: every version survives, all won by source 0, with its
            -- own latest kept and its times carried whole.
            let info = packument [("1.0.0", "sha512-aaa"), ("2.0.0", "sha512-bbb")]
                plan = mergePackuments [(GatedSource, info)]
            (Map.keys . mpSurvivors <$> plan) `shouldBe` Just ["1.0.0", "2.0.0"]
            (Map.elems . mpSurvivors <$> plan) `shouldBe` Just [0, 0]
            (latestKey =<< plan) `shouldBe` Just "2.0.0"
            (sort . Map.keys . mpTime <$> plan) `shouldBe` Just ["1.0.0", "2.0.0"]

        it "reports no divergences for a single input" $ do
            let info = packument [("1.0.0", "sha512-aaa")]
            (mpDivergences <$> mergePackuments [(TrustedSource, info)]) `shouldBe` Just Set.empty

        it "unions versions across sources" $ do
            let trusted = packument [("1.0.0", "sha512-aaa")]
                gated = packument [("2.0.0", "sha512-bbb")]
            (survivorKeys <$> mergePackuments [(TrustedSource, trusted), (GatedSource, gated)])
                `shouldBe` Just ["1.0.0", "2.0.0"]

        it "private wins a collision: the survivor points at the trusted source" $ do
            -- Same version key in both, with differing integrity. The plan records
            -- the surviving key against the trusted input's 'SourceId', so the serve
            -- layer takes that version's object from the private source's raw Value.
            let gated = packument [("1.0.0", "sha512-public")] -- source 0
                trusted = packument [("1.0.0", "sha512-private")] -- source 1
            (winnerOf "1.0.0" =<< mergePackuments [(GatedSource, gated), (TrustedSource, trusted)])
                `shouldBe` Just 1

        it "detects a divergence when the same version's integrity differs" $ do
            let trusted = packument [("1.0.0", "sha512-private")]
                gated = packument [("1.0.0", "sha512-public")]
                plan = mergePackuments [(TrustedSource, trusted), (GatedSource, gated)]
            (map divVersion . Set.toList . mpDivergences <$> plan) `shouldBe` Just ["1.0.0"]

        it "reports no divergence when a collision's integrity agrees" $ do
            let trusted = packument [("1.0.0", "sha512-same")]
                gated = packument [("1.0.0", "sha512-same")]
                plan = mergePackuments [(TrustedSource, trusted), (GatedSource, gated)]
            (mpDivergences <$> plan) `shouldBe` Just Set.empty

        it "repoints latest to the highest surviving version when the chosen tag is gone" $ do
            -- The trusted source's chosen latest (9.9.9) is not actually carried,
            -- so selectLatest repoints across the union to the highest stable
            -- survivor (3.0.0).
            let trusted =
                    (packument [("1.0.0", "sha512-aaa")])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "9.9.9")
                        }
                gated = packument [("3.0.0", "sha512-bbb"), ("2.0.0", "sha512-ccc")]
            (latestKey =<< mergePackuments [(TrustedSource, trusted), (GatedSource, gated)])
                `shouldBe` Just "3.0.0"

        it "drops a dist-tag whose target is absent from the union" $ do
            -- A source advertises a "next" tag pointing at a version it does not
            -- actually carry; the merge drops it rather than serving a dangling tag.
            let info =
                    (packument [("1.0.0", "sha512-aaa")])
                        { infoDistTags =
                            Map.fromList
                                [ ("latest", mkVersion Npm "1.0.0")
                                , ("next", mkVersion Npm "9.9.9")
                                ]
                        }
            (Map.keys . mpDistTags <$> mergePackuments [(GatedSource, info)])
                `shouldBe` Just ["latest"]

        it "restricts time to surviving versions" $ do
            let trusted = packument [("1.0.0", "sha512-aaa")]
                gated = packument [("2.0.0", "sha512-bbb")]
            (sort . Map.keys . mpTime <$> mergePackuments [(TrustedSource, trusted), (GatedSource, gated)])
                `shouldBe` Just ["1.0.0", "2.0.0"]

    describe "collision resolution & divergence correction (the supply-chain signal)" $ do
        -- A version present in both a trusted (private) and a gated (public) source
        -- is a collision: the trusted copy wins. If the two copies' artifact
        -- integrity disagrees the merge *flags* it as a tampering signal — and
        -- flags without dropping the version, leaving fail-closed to the caller.
        let trusted = packument [("1.0.0", "sha512-private")] -- source 0
            gated = packument [("1.0.0", "sha512-public")] -- source 1
            plan = mergePackuments [(TrustedSource, trusted), (GatedSource, gated)]

        it "keeps the divergent version, won by the trusted source (flags, does not drop)" $ do
            (survivorKeys <$> plan) `shouldBe` Just ["1.0.0"]
            (winnerOf "1.0.0" =<< plan) `shouldBe` Just 0

        it "records the winning (trusted) and losing (gated) integrity for the audit trail" $
            case Set.toList . mpDivergences <$> plan of
                Just [d] -> do
                    divVersion d `shouldBe` "1.0.0"
                    integrityHashes (divWinning d) `shouldBe` [(SRI, "sha512-private")]
                    integrityHashes (divLosing d) `shouldBe` [(SRI, "sha512-public")]
                other -> expectationFailure ("expected exactly one divergence, got " <> show other)

    describe "divergence compares on shared algorithms, not the whole digest set" $ do
        -- A divergence is reported only when two copies *contradict* on an algorithm
        -- they both carry. An asymmetric digest set — one mirror also serving a digest
        -- the other omits — is not, on its own, a contradiction: an older registry
        -- exposing only a legacy shasum while npmjs serves shasum + a modern SRI
        -- describes the same bytes and must not be flagged.
        let sha1 = Hash SHA1
            sri = Hash SRI

        it "agreeing on the shared SRI is not a divergence though one also carries SHA-1" $ do
            -- Both expose the same sha512 SRI; the private copy additionally carries a
            -- legacy SHA-1 shasum the public copy lacks. The shared algorithm (SRI)
            -- agrees, so this is the same bytes — not a divergence.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri "sha512-X", sha1 "deadbeef"])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri "sha512-X"])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "contradicting on the shared SRI is a divergence even when SHA-1 agrees" $ do
            -- Both carry the same SHA-1 but a *different* sha512 SRI. A SHA-1 agreement
            -- can never rescue a contradicting secure digest, so the SRI contradiction
            -- is flagged regardless of the matching weak digest beside it.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri "sha512-X", sha1 "abc"])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri "sha512-Y", sha1 "abc"])])
                plan = mergePackuments [trusted, gated]
            (map divVersion . Set.toList . mpDivergences <$> plan) `shouldBe` Just ["1.0.0"]

        it "SRI+SHA-1 vs SHA-1-only, agreeing on the shared SHA-1, is not a divergence" $ do
            -- One copy carries sha512 + sha1, the other only the legacy sha1, and that
            -- single shared algorithm agrees. With no contradiction on a shared
            -- algorithm this is not a divergence; the comparison only ever flags a
            -- shared algorithm whose digests disagree. (Pinned so the current behaviour
            -- is explicit: whether a weak-only agreement should itself be treated as
            -- suspicious is a separate, stricter policy not decided by this fold.)
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri "sha512-X", sha1 "abc"])])
                gated = (GatedSource, packumentWith [("1.0.0", [sha1 "abc"])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        -- A single version can carry several digests of *one* algorithm — the domain
        -- model allows many artifacts ('pkgArtifacts' is a 'NonEmpty'), each with its
        -- own hashes (a PyPI sdist + wheels may each carry a SHA-256), and 'fingerprint'
        -- gathers them all. For a shared algorithm the copies therefore agree only when
        -- the set of digests they each offer for it matches.
        it "agrees when a shared algorithm carries the same set of digests in any order" $ do
            -- The same two SRI digests on both copies, listed in opposite order: the
            -- per-algorithm set is identical, so this is not a divergence.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri "sha512-X", sri "sha512-Y"])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri "sha512-Y", sri "sha512-X"])])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "contradicts when a shared algorithm's set of digests differs" $
            -- One copy offers two SRI digests for the key, the other only one of them:
            -- the digest sets for the shared algorithm differ, so it is flagged.
            let trusted = (TrustedSource, packumentWith [("1.0.0", [sri "sha512-X", sri "sha512-Y"])])
                gated = (GatedSource, packumentWith [("1.0.0", [sri "sha512-X"])])
             in case Set.toList . mpDivergences <$> mergePackuments [trusted, gated] of
                    Just [d] -> do
                        divVersion d `shouldBe` "1.0.0"
                        integrityHashes (divWinning d) `shouldBe` [(SRI, "sha512-X"), (SRI, "sha512-Y")]
                        integrityHashes (divLosing d) `shouldBe` [(SRI, "sha512-X")]
                    other -> expectationFailure ("expected exactly one divergence, got " <> show other)

    describe "precedence is by provenance, not input order" $ do
        -- dist-tags and time must resolve collisions by provenance (trusted wins),
        -- so the plan is identical whichever order the caller passes the upstreams.
        let trusted =
                ( TrustedSource
                , (packument [("1.0.0", "sha512-priv")])
                    { infoDistTags = Map.fromList [("latest", mkVersion Npm "1.0.0"), ("beta", mkVersion Npm "1.0.0")]
                    , infoPublishedAt = Map.singleton "1.0.0" tTrusted
                    }
                )
            gated =
                ( GatedSource
                , (packument [("1.0.0", "sha512-pub")])
                    { infoDistTags = Map.fromList [("latest", mkVersion Npm "1.0.0"), ("beta", mkVersion Npm "1.0.0")]
                    , infoPublishedAt = Map.singleton "1.0.0" tGated
                    }
                )
            tTrusted = UTCTime (fromGregorian 2026 3 3) 0
            tGated = UTCTime (fromGregorian 2020 1 1) 0

        it "resolves identically whichever order trusted/gated is passed" $ do
            -- Every provenance-resolved decision must be order-independent: the
            -- surviving keys, the reconciled tags (incl. latest), the time union,
            -- and the divergences. The only thing that legitimately differs is the
            -- winner's 'SourceId' — a faithful pointer to the trusted input's
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

    describe "latest via the shared selector" $ do
        -- latest is resolved by Ecluse.Version.selectLatest, so the merge inherits
        -- keep-unless-denied + stable-preferring + unparseable-safe behaviour.
        -- selectLatest is exhaustively unit-tested in its own spec; these only
        -- check that it is wired into the merge correctly.
        it "keeps the chosen latest when it still survives (no promotion)" $ do
            -- The trusted source tags latest at 1.0.0 and that version survives, so
            -- latest stays 1.0.0 even though 2.0.0 exists in the union.
            let trusted =
                    ( TrustedSource
                    , (packument [("1.0.0", "sha512-aaa")])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "1.0.0")
                        }
                    )
                gated = (GatedSource, packument [("2.0.0", "sha512-bbb")])
            (latestKey =<< mergePackuments [trusted, gated]) `shouldBe` Just "1.0.0"

        it "chooses the chosen-latest by provenance (trusted's tag wins)" $ do
            -- Both sources survive and both tag a latest; the trusted source's
            -- latest is the chosen one, even though it is the lower version.
            let trusted =
                    ( TrustedSource
                    , (packument [("1.0.0", "sha512-aaa")])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "1.0.0")
                        }
                    )
                gated =
                    ( GatedSource
                    , (packument [("2.0.0", "sha512-bbb")])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "2.0.0")
                        }
                    )
            (latestKey =<< mergePackuments [trusted, gated]) `shouldBe` Just "1.0.0"

        it "repoints to the highest stable survivor over a prerelease when chosen is gone" $ do
            -- The chosen latest (5.0.0) was denied/absent; among survivors a stable
            -- release is preferred over a higher prerelease.
            let info =
                    (packument [("2.0.0", "sha512-aaa"), ("3.0.0-rc.1", "sha512-bbb")])
                        { infoDistTags = Map.singleton "latest" (mkVersion Npm "5.0.0")
                        }
            (latestKey =<< mergePackuments [(GatedSource, info)]) `shouldBe` Just "2.0.0"

        it "falls back to a surviving prerelease when no stable survivor exists" $ do
            let info =
                    (packument [("3.0.0-rc.1", "sha512-aaa"), ("3.0.0-beta", "sha512-bbb")])
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
                Map.keys (mpTime plan) === Map.keys (infoPublishedAt info)
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
                let trusted = (TrustedSource, packumentWith [(ver, [Hash SRI sri, Hash SHA1 extra])])
                    gated = (GatedSource, packumentWith [(ver, [Hash SRI sri])])
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
            -- the order — which is exactly why commutativity is the wrong law.
            let trusted = contribute TrustedSource (packument [("1.0.0", "sha512-priv")])
                gated = contribute GatedSource (packument [("1.0.0", "sha512-pub")])
                forward = planFrom (trusted <> gated)
                backward = planFrom (gated <> trusted)
            -- Same decision (trusted wins) but opposite positional label, so the
            -- two plans — and the two accumulators — are genuinely not equal.
            (trusted <> gated == gated <> trusted) `shouldBe` False
            (winnerOf "1.0.0" =<< forward) `shouldBe` Just 0
            (winnerOf "1.0.0" =<< backward) `shouldBe` Just 1

        it "mergePackuments is planFrom . foldMap contribute" $
            hedgehog $ do
                sources <- forAll genSources
                mergePackuments sources === planFrom (foldMap (uncurry contribute) sources)

    describe "the laws do not erode the trust hierarchy" $ do
        -- The architect's explicit requirement: prove the lawful refactor still
        -- enforces the business rules — trusted-wins precedence, the union, the
        -- divergence signal, and (the core property) order-independence of every
        -- decision a caller can observe except the positional 'SourceId' label.

        it "the trust order IS the hierarchy: TrustedSource < GatedSource (keystone — do not reorder)" $
            -- DO NOT read this as a trivial Enum/Ord check. This single line is the
            -- keystone of the entire merge. Every "the private registry always wins"
            -- decision the module makes — which copy survives a version collision,
            -- whose integrity is recorded as the divergence winner, and which
            -- source's dist-tags and time are kept — is resolved by 'Set.findMin' /
            -- 'keepBetter' over the @(Provenance, SourceId)@ rank, whose precedence
            -- is governed entirely by this 'Ord Provenance'. 'TrustedSource' is
            -- declared before 'GatedSource', so the derived 'Ord' makes it the
            -- smaller value, and "smallest wins" is what gives the private upstream
            -- authority.
            --
            -- If a future edit reorders the 'Provenance' constructors (or otherwise
            -- inverts this comparison), the trust relationship flips SILENTLY: the
            -- public upstream would win every collision, and a tampered public copy
            -- could shadow the vetted private one — the precise supply-chain failure
            -- Écluse exists to prevent — with nothing else in the types objecting.
            -- A failure here therefore means "the trust hierarchy has been inverted,"
            -- NOT "update the expected value." This assertion is the tripwire; keep it.
            compare TrustedSource GatedSource `shouldBe` LT

        it "trusted wins a collision; the divergence's winner is the trusted copy" $ do
            -- Trusted and gated collide at 1.0.0 with differing integrity. The
            -- survivor is the trusted copy and the recorded divergence's *winning*
            -- fingerprint is the trusted integrity — the hierarchy, intact.
            let trusted = (TrustedSource, packument [("1.0.0", "sha512-priv")])
                gated = (GatedSource, packument [("1.0.0", "sha512-pub")])
                plan = mergePackuments [gated, trusted] -- trusted at index 1
            (winnerOf "1.0.0" =<< plan) `shouldBe` Just 1
            case Set.toList . mpDivergences <$> plan of
                Just [d] -> do
                    divVersion d `shouldBe` "1.0.0"
                    integrityHashes (divWinning d) `shouldBe` [(SRI, "sha512-priv")]
                    integrityHashes (divLosing d) `shouldBe` [(SRI, "sha512-pub")]
                other -> expectationFailure ("expected one divergence, got " <> show other)

        it "the merged set is the mixed-provenance union trusted ∪ filtered(public)" $ do
            -- Versions unique to each upstream are all present; the trust split does
            -- not drop a side, it unions them.
            let trusted = (TrustedSource, packument [("1.0.0", "sha512-a"), ("1.1.0", "sha512-b")])
                gated = (GatedSource, packument [("2.0.0", "sha512-c"), ("1.1.0", "sha512-b")])
            (survivorKeys <$> mergePackuments [trusted, gated])
                `shouldBe` Just ["1.0.0", "1.1.0", "2.0.0"]

        it "identical integrity across sources yields no divergence" $ do
            let trusted = (TrustedSource, packument [("1.0.0", "sha512-same")])
                gated = (GatedSource, packument [("1.0.0", "sha512-same")])
            (mpDivergences <$> mergePackuments [trusted, gated]) `shouldBe` Just Set.empty

        it "a 3+-copy collision fans the winner out against each distinct loser" $ do
            -- THREE copies of one key with three distinct fingerprints: one trusted
            -- (wins), two gated. A non-associative pairwise divergence definition
            -- would miss or double-count one of the losing pairs; the set-of-distinct
            -- -fingerprints definition records the trusted winner against *each* of
            -- the two distinct losers, exactly once.
            let t = (TrustedSource, packument [("1.0.0", "sha512-T")]) -- index 0, wins
                g1 = (GatedSource, packument [("1.0.0", "sha512-G1")]) -- index 1
                g2 = (GatedSource, packument [("1.0.0", "sha512-G2")]) -- index 2
                plan = mergePackuments [t, g1, g2]
            (winnerOf "1.0.0" =<< plan) `shouldBe` Just 0
            let expected =
                    Set.fromList
                        [ ("1.0.0", [(SRI, "sha512-T")], [(SRI, "sha512-G1")])
                        , ("1.0.0", [(SRI, "sha512-T")], [(SRI, "sha512-G2")])
                        ]
                actual =
                    Set.map
                        (\d -> (divVersion d, integrityHashes (divWinning d), integrityHashes (divLosing d)))
                        . mpDivergences
                        <$> plan
            actual `shouldBe` Just expected

        it "a 3+-copy collision's divergences are associativity-stable (regroup the fold)" $ do
            -- The same three copies, folded in two different associativity groupings
            -- of 'contribute', must yield the same divergence fingerprint-pairs — the
            -- property a pairwise winner-vs-loser fold would violate.
            let t = contribute TrustedSource (packument [("1.0.0", "sha512-T")])
                g1 = contribute GatedSource (packument [("1.0.0", "sha512-G1")])
                g2 = contribute GatedSource (packument [("1.0.0", "sha512-G2")])
                left = planFrom ((t <> g1) <> g2)
                right = planFrom (t <> (g1 <> g2))
            (mpDivergences <$> left) `shouldBe` (mpDivergences <$> right)

        it "dist-tags: keep-unless-denied, absent-target dropped, by provenance" $ do
            -- 'latest' kept at the trusted source's surviving tag; a 'next' tag whose
            -- target is absent from the union is dropped; resolution is by provenance.
            let trusted =
                    ( TrustedSource
                    , (packument [("1.0.0", "sha512-a")])
                        { infoDistTags =
                            Map.fromList
                                [ ("latest", mkVersion Npm "1.0.0")
                                , ("next", mkVersion Npm "9.9.9")
                                ]
                        }
                    )
                gated = (GatedSource, packument [("2.0.0", "sha512-b")])
                plan = mergePackuments [gated, trusted]
            (latestKey =<< plan) `shouldBe` Just "1.0.0"
            (sort . Map.keys . mpDistTags <$> plan) `shouldBe` Just ["latest"]

        it "single source is the degenerate identity: all survive, won by source 0" $
            hedgehog $ do
                src@(_, info) <- forAll genSource
                plan <- H.evalMaybe (mergePackuments [src])
                Map.keys (mpSurvivors plan) === Map.keys (infoVersions info)
                nub (Map.elems (mpSurvivors plan)) === ([0 | not (Map.null (infoVersions info))])
                Map.keys (mpTime plan) === Map.keys (infoPublishedAt info)
                mpDivergences plan === Set.empty

        it "the always-invariant decisions survive any permutation of any inputs" $
            hedgehog $ do
                -- Over arbitrary mixed-provenance inputs ('genSources' freely collides
                -- keys, including *same-provenance* collisions), two decisions are
                -- order-independent without qualification: the surviving key *set* and
                -- the winning *provenance* per key. (A same-provenance collision's
                -- concrete winner is positional — provenance cannot break that tie —
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
                -- The architecture's defined two-source topology — exactly one trusted
                -- and one gated upstream — is where the merge actually runs today. Every
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
            -- position) wins — the same positional tiebreak that makes the Semigroup
            -- non-commutative. This case never arises in the npm topology (collisions
            -- there are always cross-provenance); it is the multi-same-provenance
            -- topology 'SourceId' exists for, where the winner legitimately tracks
            -- order. The *surviving set* and *winning provenance* stay invariant; only
            -- the winner/loser fingerprint labels flip.
            let a = (GatedSource, packument [("1.0.0", "sha512-A")]) -- earlier wins
                b = (GatedSource, packument [("1.0.0", "sha512-B")])
                forward = Set.toList . mpDivergences <$> mergePackuments [a, b]
                backward = Set.toList . mpDivergences <$> mergePackuments [b, a]
            (map (integrityHashes . divWinning) <$> forward) `shouldBe` Just [[(SRI, "sha512-A")]]
            (map (integrityHashes . divWinning) <$> backward) `shouldBe` Just [[(SRI, "sha512-B")]]

{- | Sources with pairwise-disjoint version keys, so the merge is a pure set union
with no collisions — the regime in which order cannot matter at all.
-}
genDisjointSources :: Gen [(Provenance, PackageInfo)]
genDisjointSources = do
    n <- Gen.int (Range.linear 1 4)
    pure [oneSource i | i <- [1 .. n]]
  where
    -- Source @i@ owns the single version @i.0.0@, so no two sources share a key.
    oneSource i =
        let ver = show (i :: Int) <> ".0.0"
         in (if even i then TrustedSource else GatedSource, packument [(ver, "sha512-" <> ver)])
