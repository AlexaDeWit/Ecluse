module Ecluse.Package.MergeSpec (spec) where

import Data.List (nub)
import Data.Map.Strict qualified as Map
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

{- | One tarball with the given integrity digest, so two versions of the same key
can be made to agree or diverge purely on integrity.
-}
artifactWith :: Text -> Artifact
artifactWith digest =
    Artifact
        { artFilename = "thing.tgz"
        , artUrl = "https://example.test/thing.tgz"
        , artKind = Tarball
        , artHashes = [Hash SRI digest]
        , artSize = Nothing
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }

{- | A per-version snapshot for a raw version string, carrying a chosen integrity
digest. Everything else is inert — the merge reads only the version key, the
parsed version (for @latest@), and artifact integrity (for divergence).
-}
detailsWith :: Text -> Text -> PackageDetails
detailsWith rawVer digest =
    PackageDetails
        { pkgName = name
        , pkgVersion = mkVersion Npm rawVer
        , pkgPublishedAt = Nothing
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = artifactWith digest :| []
        , pkgLicenses = ["MIT"]
        , pkgPublisher = Nothing
        , pkgMaintainers = []
        , pkgDependencies = []
        }

{- | Build a single-package packument from @(rawVersion, integrityDigest)@ pairs.
@latest@ is pointed at the lexically-highest version (a coherent packument always
tags its newest release), so a lone source is already a fixed point of the merge's
@latest@ reconciliation; @time@ gives each version a fixed instant.
-}
packument :: [(Text, Text)] -> PackageInfo
packument vs =
    PackageInfo
        { infoName = name
        , infoVersions = Map.fromList [(v, detailsWith v d) | (v, d) <- vs]
        , infoDistTags = case sortOn Down (map fst vs) of
            [] -> Map.empty
            (hi : _) -> Map.singleton "latest" (mkVersion Npm hi)
        , infoPublishedAt = Map.fromList [(v, t0) | (v, _) <- vs]
        }
  where
    t0 = UTCTime (fromGregorian 2026 1 1) 0

-- ── small accessors ──────────────────────────────────────────────────────────

versionKeys :: MergeResult -> [Text]
versionKeys = sort . Map.keys . infoVersions . mergedInfo

latestKey :: MergeResult -> Maybe Text
latestKey r = unVersion <$> Map.lookup "latest" (infoDistTags (mergedInfo r))

digestOf :: Text -> MergeResult -> Maybe [Hash]
digestOf key r =
    concatMap artHashes . toList . pkgArtifacts
        <$> Map.lookup key (infoVersions (mergedInfo r))

-- ── generators ───────────────────────────────────────────────────────────────

genDigest :: Gen Text
genDigest = ("sha512-" <>) <$> Gen.text (Range.singleton 6) Gen.alphaNum

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

-- ── spec ─────────────────────────────────────────────────────────────────────

spec :: Spec
spec = do
    describe "mergePackuments" $ do
        it "returns Nothing on an empty input (nothing to serve)" $
            mergePackuments [] `shouldBe` Nothing

        it "is the identity on a single input" $ do
            let info = packument [("1.0.0", "sha512-aaa"), ("2.0.0", "sha512-bbb")]
            (mergedInfo <$> mergePackuments [(GatedSource, info)]) `shouldBe` Just info

        it "reports no divergences for a single input" $ do
            let info = packument [("1.0.0", "sha512-aaa")]
            (mergeDivergences <$> mergePackuments [(TrustedSource, info)]) `shouldBe` Just []

        it "unions versions across sources" $ do
            let trusted = packument [("1.0.0", "sha512-aaa")]
                gated = packument [("2.0.0", "sha512-bbb")]
            (versionKeys <$> mergePackuments [(TrustedSource, trusted), (GatedSource, gated)])
                `shouldBe` Just ["1.0.0", "2.0.0"]

        it "private wins a collision, keeping the trusted artifact" $ do
            -- Same version key in both, with differing integrity: the trusted
            -- (private) copy is the one served.
            let trusted = packument [("1.0.0", "sha512-private")]
                gated = packument [("1.0.0", "sha512-public")]
            (digestOf "1.0.0" =<< mergePackuments [(GatedSource, gated), (TrustedSource, trusted)])
                `shouldBe` Just [Hash SRI "sha512-private"]

        it "detects a divergence when the same version's integrity differs" $ do
            let trusted = packument [("1.0.0", "sha512-private")]
                gated = packument [("1.0.0", "sha512-public")]
                result = mergePackuments [(TrustedSource, trusted), (GatedSource, gated)]
            (map divVersion . mergeDivergences <$> result) `shouldBe` Just ["1.0.0"]

        it "reports no divergence when a collision's integrity agrees" $ do
            let trusted = packument [("1.0.0", "sha512-same")]
                gated = packument [("1.0.0", "sha512-same")]
                result = mergePackuments [(TrustedSource, trusted), (GatedSource, gated)]
            (mergeDivergences <$> result) `shouldBe` Just []

        it "repoints latest to the highest surviving version across sources" $ do
            -- Each source tags its own latest; the merge picks the global max.
            let trusted = packument [("1.0.0", "sha512-aaa")]
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
            (Map.keys . infoDistTags . mergedInfo <$> mergePackuments [(GatedSource, info)])
                `shouldBe` Just ["latest"]

        it "restricts time to surviving versions" $ do
            let trusted = packument [("1.0.0", "sha512-aaa")]
                gated = packument [("2.0.0", "sha512-bbb")]
            (sort . Map.keys . infoPublishedAt . mergedInfo <$> mergePackuments [(TrustedSource, trusted), (GatedSource, gated)])
                `shouldBe` Just ["1.0.0", "2.0.0"]

    describe "properties" $ do
        it "the merged versions are exactly the union of every source's keys" $
            hedgehog $ do
                sources <- forAll genSources
                result <- H.evalMaybe (mergePackuments sources)
                let expected = sort (nub (concatMap (Map.keys . infoVersions . snd) sources))
                versionKeys result === expected

        it "every surviving dist-tag target is a surviving version key" $
            hedgehog $ do
                sources <- forAll genSources
                result <- H.evalMaybe (mergePackuments sources)
                let keys = Map.keys (infoVersions (mergedInfo result))
                    targets = map unVersion (Map.elems (infoDistTags (mergedInfo result)))
                H.assert (all (`elem` keys) targets)

        it "latest, when present, is a surviving key" $
            hedgehog $ do
                sources <- forAll genSources
                result <- H.evalMaybe (mergePackuments sources)
                case latestKey result of
                    Nothing -> H.success
                    Just k -> H.assert (k `elem` Map.keys (infoVersions (mergedInfo result)))

        it "a single input is the identity (versions, tags, and times)" $
            hedgehog $ do
                src@(_, info) <- forAll genSource
                result <- H.evalMaybe (mergePackuments [src])
                -- A lone source survives whole: it cannot collide, and every tag
                -- it carries targets one of its own versions.
                mergedInfo result === info
                mergeDivergences result === []

        it "the merge is order-independent on disjoint sources (no collisions)" $
            hedgehog $ do
                -- Give every source a disjoint key space so there is no collision:
                -- the union, tags, and times are then a set operation, and any
                -- permutation of the sources yields the same document.
                sources <- forAll genDisjointSources
                perm <- forAll (Gen.shuffle sources)
                a <- H.evalMaybe (mergePackuments sources)
                b <- H.evalMaybe (mergePackuments perm)
                mergedInfo a === mergedInfo b

        it "the private copy wins the tiebreak regardless of source order" $
            hedgehog $ do
                ver <- forAll genVersionStr
                privDigest <- forAll genDigest
                pubDigest <- forAll (Gen.filter (/= privDigest) genDigest)
                let trusted = (TrustedSource, packument [(ver, privDigest)])
                    gated = (GatedSource, packument [(ver, pubDigest)])
                forward <- H.evalMaybe (digestOf ver =<< mergePackuments [trusted, gated])
                backward <- H.evalMaybe (digestOf ver =<< mergePackuments [gated, trusted])
                forward === [Hash SRI privDigest]
                backward === [Hash SRI privDigest]

        it "a divergence is detected iff a shared version's integrity differs" $
            hedgehog $ do
                ver <- forAll genVersionStr
                d1 <- forAll genDigest
                d2 <- forAll genDigest
                let trusted = (TrustedSource, packument [(ver, d1)])
                    gated = (GatedSource, packument [(ver, d2)])
                result <- H.evalMaybe (mergePackuments [trusted, gated])
                let diverged = not (null (mergeDivergences result))
                diverged === (d1 /= d2)

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
