-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Grouped previews through the real cycle and shared rule evaluator.
module Ecluse.Core.Registry.Sweep.GroupSpec (spec) where

import Data.Conduit (yield)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageDetails (pkgPublishedAt), PackageInfo (infoVersions), PackageName, mkPackageName)
import Ecluse.Core.Registry.Maintenance (
    ConsentVerdict (ConsentWithheld),
    StoreFacts (factBackend, factNameAlphabet),
    StoreObservation (..),
    StoredVersion (StoredVersion),
    VersionPresence (VersionServed),
    mkNameAlphabet,
    protocolFault,
    renderNamePrefix,
 )
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Core.Registry.Sweep (sweepCycle)
import Ecluse.Core.Registry.Sweep.Group (boundedVersions)
import Ecluse.Core.Registry.Sweep.Types (
    CycleHalt (HaltStoreFault),
    CycleOutcome (..),
    EvidenceGaps (gapManifests),
    PrerequisiteStatus (PrerequisiteUnmet),
    SweepMount (..),
    SweepPacing (swpDeletionCap, swpShape),
    SweepShape (SweepEverything),
    SweepStore (..),
    SweepTally (..),
    TargetPrerequisites (tpConsent),
    outcomeComplete,
    renderStoreFault,
 )
import Ecluse.Core.Registry.Sweep.Walk (bucketNameBudget)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (PrecededRule (PrecededRule), Rule (AllowIfOlderThan, DenyByIdentity))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Maintenance (FakeStore (..), FakeStoreConfig (..), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Package (sampleManifest)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Sweep (RecordedSweep (..), previewMount, previewingReport, recordingPortsUnder, testPacing)

spec :: Spec
spec = describe "grouped preview" $ do
    it "finds mirror-only, cache-only and shared versions without origin data, counting three selections" $ do
        mirror <- seeded "mirrorTarget" [(packageName, ["1.0.0", "3.0.0"])]
        cache <- seeded "privateUpstream" [(packageName, ["2.0.0", "3.0.0"])]
        mount <- grouped mirror cache
        (recorded, outcome) <- runPreview mount
        tallyDeleted (outcomeTally outcome) `shouldBe` 3
        tallyExamined (outcomeTally outcome) `shouldBe` 4
        outcomeComplete outcome `shouldBe` True
        lines' <- recInfo recorded
        length (filter (T.isInfixOf "would delete") lines') `shouldBe` 4
        lines' `shouldSatisfy` any (T.isPrefixOf "privateUpstream:")
        readFakeContents mirror `shouldReturn` contents [(packageName, ["1.0.0", "3.0.0"])]
        readFakeContents cache `shouldReturn` contents [(packageName, ["2.0.0", "3.0.0"])]
        readFakeCursor mirror `shouldReturn` Nothing
        readFakeCursor cache `shouldReturn` Nothing

    it "does not enumerate a package at a location whose actual name listing omitted it" $ do
        mirror <- seeded "mirrorTarget" []
        cache <- seeded "privateUpstream" [(packageName, ["1.0.0"])]
        mount <- grouped mirror cache
        let original = smStore mount
            absent = (ssObserve original){obEnumerateVersions = \_ -> fail "an absent package must not be enumerated"}
        (_, outcome) <- runPreview mount{smStore = original{ssObserve = absent}}
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        outcomeComplete outcome `shouldBe` True

    it "deduplicates repeated listing and version identities within both locations" $ do
        mirror <- seeded "mirrorTarget" [(packageName, ["1.0.0", "1.0.0"])]
        cache <- seeded "privateUpstream" [(packageName, ["1.0.0", "1.0.0"])]
        mount <- grouped mirror cache
        let duplicated store = store{obListPackagesIn = \_ -> yield [packageName, packageName] >> yield [packageName] $> Nothing}
            original = smStore mount
        (_, outcome) <- runPreview mount{smStore = original{ssObserve = duplicated (ssObserve original), ssPrivate = duplicated <$> ssPrivate original}}
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        tallyExamined (outcomeTally outcome) `shouldBe` 2

    it "reports each location's withheld consent without withholding its preview" $ do
        mirror <- seeded "mirrorTarget" [(packageName, ["1.0.0"])]
        cache <- newFakeStore defaultFakeStoreConfig{fakeContents = contents [(packageName, ["1.0.0"])], fakeConsent = ConsentWithheld "cache consent absent"}
        mount <- grouped mirror cache
        (_, outcome) <- runPreview mount
        map tpConsent (outcomePrerequisites outcome) `shouldContain` [PrerequisiteUnmet "cache consent absent"]
        tallyDeleted (outcomeTally outcome) `shouldBe` 1

    it "protects first-party names before either metadata read or selection" $ do
        mirror <- seeded "mirrorTarget" [(packageName, ["1.0.0"])]
        cache <- seeded "privateUpstream" [(packageName, ["1.0.0"])]
        mount <- grouped mirror cache
        let original = smStore mount
        (_, outcome) <- runPreview mount{smFirstParty = const True, smStore = original{ssObserve = forbidMetadata (ssObserve original), ssPrivate = forbidMetadata <$> ssPrivate original}}
        tallyDeleted (outcomeTally outcome) `shouldBe` 0
        tallyGuardSkipped (outcomeTally outcome) `shouldBe` 2

    it "withholds selection when missing evidence could establish a higher-precedence allow" $ do
        mirror <- seeded "mirrorTarget" [(packageName, ["1.0.0"])]
        cache <- newFakeStore defaultFakeStoreConfig{fakeContents = contents [(packageName, ["1.0.0"])]}
        mount <- grouped mirror cache
        rules <- prepare inertRuleDeps [PrecededRule 100 (AllowIfOlderThan 0), PrecededRule 0 (DenyByIdentity "left-pad")]
        let original = smStore mount
            dated = (ssObserve original){obReadManifest = fmap (fmap dateManifest) . obReadManifest (ssObserve original)}
        (_, outcome) <- runPreview mount{smRules = rules, smStore = original{ssObserve = dated}}
        tallyDeleted (outcomeTally outcome) `shouldBe` 0
        tallyKept (outcomeTally outcome) `shouldBe` 2
        gapManifests (outcomeEvidence outcome) `shouldBe` 1
        outcomeComplete outcome `shouldBe` False

    it "keeps distinct decisions for differing metadata at the two locations" $ do
        mirror <- seeded "mirrorTarget" [(packageName, ["1.0.0"])]
        cache <- seeded "privateUpstream" [(packageName, ["1.0.0"])]
        mount <- grouped mirror cache
        rules <- prepare inertRuleDeps [PrecededRule 100 (AllowIfOlderThan 0), PrecededRule 0 (DenyByIdentity "left-pad")]
        let original = smStore mount
            dated = (ssObserve original){obReadManifest = fmap (fmap dateManifest) . obReadManifest (ssObserve original)}
        (recorded, outcome) <- runPreview mount{smRules = rules, smStore = original{ssObserve = dated}}
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        tallyKept (outcomeTally outcome) `shouldBe` 1
        outcomeComplete outcome `shouldBe` True
        lines' <- recInfo recorded
        lines' `shouldSatisfy` any (T.isPrefixOf "mirrorTarget: dry run, keeping")
        lines' `shouldSatisfy` any (T.isPrefixOf "privateUpstream: dry run, would delete")

    it "reports one logical cap crossing and continues past it" $ do
        mirror <- seeded "mirrorTarget" [(packageName, ["1.0.0", "2.0.0"])]
        cache <- seeded "privateUpstream" [(packageName, ["1.0.0", "2.0.0"])]
        mount <- grouped mirror cache
        recorded <- recordingPortsUnder previewingReport Nothing
        outcome <- sweepCycle testPacing{swpDeletionCap = 1} (recPorts recorded) [mount]
        tallyDeleted (outcomeTally outcome) `shouldBe` 2
        outcomeHalt outcome `shouldBe` Nothing
        lines' <- recInfo recorded
        length (filter (T.isInfixOf "reached the deletion cap") lines') `shouldBe` 1

    it "fails an oversized combined version union before reading metadata" $ do
        mirror <- seeded "mirrorTarget" [(packageName, ["1.0.0"])]
        cache <- seeded "privateUpstream" [(packageName, ["2.0.0"])]
        mount <- grouped mirror cache
        let original = smStore mount
        (_, outcome) <- runPreview mount{smStore = original{ssVersionLimit = 1}}
        outcomeComplete outcome `shouldBe` False
        tallyExamined (outcomeTally outcome) `shouldBe` 0

    it "reports a combined unsplittable name overflow instead of a complete truncated preview" $ do
        let leftNames = [(mkPackageName Npm Nothing ("a" <> show n), []) | n <- [1 .. bucketNameBudget `div` 2]]
            rightNames = [(mkPackageName Npm Nothing ("b" <> show n), []) | n <- [1 .. bucketNameBudget `div` 2 + 1]]
        mirror <- seeded "mirrorTarget" leftNames
        cache <- seeded "privateUpstream" rightNames
        mount <- grouped mirror cache
        (_, outcome) <- runPreview mount
        outcomeComplete outcome `shouldBe` False
        tallyExamined (outcomeTally outcome) `shouldBe` 0

    it "stops private pagination when individually bounded inventories exceed the shared name budget" $ do
        let mirrorNames = [mkPackageName Npm Nothing ("a" <> show n) | n <- [1 .. bucketNameBudget - 1]]
            lastAllowed = mkPackageName Npm Nothing "b1"
            overflowing = mkPackageName Npm Nothing "b2"
        mirror <- seeded "mirrorTarget" []
        cache <- seeded "privateUpstream" []
        mount <- grouped mirror cache
        pages <- newIORef (0 :: Int)
        let mirrorListing _ = yield mirrorNames $> Nothing
            cacheListing _ = do
                liftIO (modifyIORef' pages (+ 1))
                yield [lastAllowed]
                liftIO (modifyIORef' pages (+ 1))
                yield [overflowing]
                liftIO (expectationFailure "a page after the combined overflow must not be demanded")
                pure Nothing
            original = smStore mount
        (_, outcome) <-
            runPreview
                mount
                    { smStore =
                        original
                            { ssObserve = (ssObserve original){obListPackagesIn = mirrorListing}
                            , ssPrivate = Just ((fakeObservation cache){obListPackagesIn = cacheListing})
                            }
                    }
        readIORef pages `shouldReturn` 2
        outcomeComplete outcome `shouldBe` False
        tallyExamined (outcomeTally outcome) `shouldBe` 0

    it "does not request the private inventory after a mirror listing fault" $ do
        mirror <- seeded "mirrorTarget" []
        cache <- seeded "privateUpstream" []
        mount <- grouped mirror cache
        let original = smStore mount
            unread = (fakeObservation cache){obListPackagesIn = \_ -> fail "a prior listing fault must stop the next source"}
        (recorded, outcome) <- runPreview mount{smStore = original{ssObserve = (ssObserve original){obListPackagesIn = \_ -> pure (Just (protocolFault "mirror listing failed"))}, ssPrivate = Just unread}}
        outcomeComplete outcome `shouldBe` False
        errors <- recErrors recorded
        errors `shouldSatisfy` any (T.isInfixOf "mirrorTarget: mirror listing failed")

    it "accepts a full version union while deduplicating each target's observations" $ do
        mirror <- seeded "mirrorTarget" []
        cache <- seeded "privateUpstream" []
        let version = StoredVersion (mkVersion Npm "1.0.0") VersionServed
            locations = [(fakeObservation mirror, [version, version]), (fakeObservation cache, [version, version])]
        fmap (map (length . snd)) (boundedVersions 1 locations) `shouldBe` Right [1, 1]
        fmap (map (length . snd)) (boundedVersions 0 locations) `shouldSatisfy` isLeft

    it "splits a grouped parent bucket and preserves each copy through the completed narrower buckets" $ do
        let mirrorOnly = mkPackageName Npm Nothing "aa-mirror"
            cacheOnly = mkPackageName Npm Nothing "ab-cache"
            shared = mkPackageName Npm Nothing "aa-shared"
            fillers prefix = [(mkPackageName Npm Nothing (prefix <> show n), []) | n <- [1 .. bucketNameBudget `div` 2]]
            configured = map DenyByIdentity ["aa-mirror", "ab-cache", "aa-shared"]
        mirror <- seeded "mirrorTarget" ([(mirrorOnly, ["1.0.0"]), (shared, ["1.0.0"])] <> fillers "aa")
        cache <- seeded "privateUpstream" ([(cacheOnly, ["1.0.0"]), (shared, ["1.0.0"])] <> fillers "ab")
        mount <- grouped mirror cache
        rules <- prepare inertRuleDeps (map atDefaultPrecedence configured)
        listedBuckets <- newIORef ([] :: [(Text, Text)])
        let partitioned label store =
                store
                    { obFacts = (obFacts store){factNameAlphabet = mkNameAlphabet "ab"}
                    , obListPackagesIn = \prefix -> do
                        liftIO (modifyIORef' listedBuckets (<> [(label, renderNamePrefix prefix)]))
                        obListPackagesIn store prefix
                    }
            original = smStore mount
            splitMount =
                mount
                    { smRules = rules
                    , smConfigured = configured
                    , smStore =
                        original
                            { ssObserve = partitioned "mirrorTarget" (ssObserve original)
                            , ssPrivate = Just (partitioned "privateUpstream" (fakeObservation cache))
                            }
                    }
        recorded <- recordingPortsUnder previewingReport Nothing
        outcome <- sweepCycle testPacing{swpShape = SweepEverything} (recPorts recorded) [splitMount]
        outcomeComplete outcome `shouldBe` True
        tallyDeleted (outcomeTally outcome) `shouldBe` 3
        tallyExamined (outcomeTally outcome) `shouldBe` 4
        readIORef listedBuckets `shouldReturn` [(label, prefix) | prefix <- ["a", "aa", "ab", "b"], label <- ["mirrorTarget", "privateUpstream"]]
        selected <- filter (T.isInfixOf "would delete") <$> recInfo recorded
        length selected `shouldBe` 4
        for_ [("mirrorTarget", "aa-mirror"), ("mirrorTarget", "aa-shared"), ("privateUpstream", "ab-cache"), ("privateUpstream", "aa-shared")] $ \(label, name) ->
            length (filter (T.isPrefixOf (label <> ": dry run, would delete " <> name <> "@1.0.0:")) selected) `shouldBe` 1

    it "halts on a private version inventory fault before either location reads metadata or decides" $ do
        mirror <- seeded "mirrorTarget" [(packageName, ["1.0.0"])]
        cache <- seeded "privateUpstream" [(packageName, ["1.0.0"])]
        mount <- grouped mirror cache
        versionsRead <- newIORef ([] :: [Text])
        let fault = protocolFault "private version inventory unavailable"
            recordVersions label store =
                (forbidMetadata store)
                    { obEnumerateVersions = \name -> do
                        modifyIORef' versionsRead (<> [label])
                        obEnumerateVersions store name
                    }
            original = smStore mount
            private = (fakeObservation cache){obEnumerateVersions = \_ -> pure (Left fault)}
        (_, outcome) <-
            runPreview
                mount
                    { smStore =
                        original
                            { ssObserve = recordVersions "mirrorTarget" (ssObserve original)
                            , ssPrivate = Just (recordVersions "privateUpstream" private)
                            }
                    }
        readIORef versionsRead `shouldReturn` ["mirrorTarget", "privateUpstream"]
        outcomeComplete outcome `shouldBe` False
        outcomeTally outcome `shouldBe` mempty
        outcomeHalt outcome `shouldBe` Just (HaltStoreFault Npm "privateUpstream" (renderStoreFault fault))

    it "reports a private listing fault without treating that target as empty" $ do
        mirror <- seeded "mirrorTarget" [(packageName, ["1.0.0"])]
        cache <- newFakeStore defaultFakeStoreConfig{fakeFault = Just (protocolFault "inventory unavailable")}
        mount <- grouped mirror cache
        (_, outcome) <- runPreview mount
        outcomeComplete outcome `shouldBe` False
        tallyDeleted (outcomeTally outcome) `shouldBe` 0

    it "uses the same grouped inventory for a full walk" $ do
        mirror <- seeded "mirrorTarget" []
        cache <- seeded "privateUpstream" [(packageName, ["1.0.0"])]
        mount <- grouped mirror cache
        recorded <- recordingPortsUnder previewingReport Nothing
        outcome <- sweepCycle testPacing{swpShape = SweepEverything} (recPorts recorded) [mount]
        tallyDeleted (outcomeTally outcome) `shouldBe` 1
        outcomeComplete outcome `shouldBe` True

packageName :: PackageName
packageName = mkPackageName Npm Nothing "left-pad"

contents :: [(PackageName, [Text])] -> Map PackageName [StoredVersion]
contents = Map.fromList . map (second (map (\raw -> StoredVersion (mkVersion Npm raw) VersionServed)))

seeded :: Text -> [(PackageName, [Text])] -> IO FakeStore
seeded label packages =
    newFakeStore
        defaultFakeStoreConfig
            { fakeContents = contents packages
            , fakeFacts = (fakeFacts defaultFakeStoreConfig){factBackend = label}
            , fakeManifests = Map.fromList [(name, sampleManifest name (map (mkVersion Npm) versions)) | (name, versions) <- packages]
            }

grouped :: FakeStore -> FakeStore -> IO SweepMount
grouped mirror cache = do
    let configured = [DenyByIdentity "left-pad"]
    rules <- prepare inertRuleDeps (map atDefaultPrecedence configured)
    let mount = previewMount (fakeObservation mirror) rules configured
        store = smStore mount
    pure mount{smStore = store{ssPrivate = Just (fakeObservation cache)}}

runPreview :: SweepMount -> IO (RecordedSweep, CycleOutcome)
runPreview mount = do
    recorded <- recordingPortsUnder previewingReport Nothing
    outcome <- sweepCycle testPacing (recPorts recorded) [mount]
    pure (recorded, outcome)

dateManifest :: Manifest -> Manifest
dateManifest manifest = manifest{manifestInfo = info{infoVersions = Map.map dateDetails (infoVersions info)}}
  where
    info = manifestInfo manifest
    dateDetails details = details{pkgPublishedAt = Just (UTCTime (fromGregorian 2020 1 1) 0)}

forbidMetadata :: StoreObservation -> StoreObservation
forbidMetadata store = store{obReadManifest = \_ -> fail "this preview must stop before reading metadata"}
