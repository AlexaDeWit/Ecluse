-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.MaintenanceSpec (spec) where

import Data.Conduit (runConduit, (.|))
import Data.Conduit.List qualified as CL
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportTimeout), transportFault)
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName, mkPackageName, mkScope)
import Ecluse.Core.Registry.Maintenance (
    CompletionNotion (CompletesLater),
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    DeleteCeiling (AtMost),
    RefillPosture (RefillRefused),
    RetryAdvice (RetryFutile, RetryWorthwhile),
    StoreClass (StorePreserved),
    StoreCursor (..),
    StoreFacts (..),
    StoreFault (..),
    StoreMaintenance (..),
    StoredVersion (..),
    VersionOutcome (VersionRefused, VersionRemoving, VersionUnreached),
    VersionPresence (VersionServed, VersionWithdrawn),
    collectPages,
    noNameAlphabet,
    storeRefusal,
 )
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Test.Maintenance (
    FakeStore (..),
    FakeStoreConfig (..),
    defaultFakeStoreConfig,
    newFakeStore,
    withBucket,
 )
import Ecluse.Test.Package (sampleManifest)

{- The fake is the handle's third implementation, so these cases assert the contract carries a backend
nothing like CodeArtifact: a two-version ceiling, a late-finishing delete, and no re-publication. -}
spec :: Spec
spec = do
    describe "the fake store's enumeration" $ do
        it "lists the packages it was seeded with" $ do
            handle <- seeded
            listWholeStore handle `shouldReturn` Right [plainName, scopedName]

        it "cuts the listing into pages, so nothing downstream holds the store whole" $ do
            store <- newFakeStore seededConfig{fakePageSize = 1}
            pages <- pagesOf (fakeMaintenance store)
            pages `shouldBe` [[plainName], [scopedName]]

        it "buckets by the name's base component, so a namespace never decides the bucket" $ do
            handle <- seeded
            listBucketOf handle "l" `shouldReturn` Right [plainName]
            listBucketOf handle "c" `shouldReturn` Right [scopedName]
            listBucketOf handle "b" `shouldReturn` Right []

        it "lists a package's versions with what the store still serves" $ do
            handle <- seeded
            versions <- enumerateVersions handle plainName
            fmap (map (renderVersion . storedVersion)) versions `shouldBe` Right ["1.0.0", "1.1.0"]
            fmap (map storedPresence) versions `shouldBe` Right [VersionServed, VersionWithdrawn]

        it "lists nothing for a package it does not hold" $ do
            handle <- seeded
            enumerateVersions handle (mkPackageName Npm Nothing "absent") `shouldReturn` Right []

    describe "the fake store's manifest read" $ do
        it "answers the manifest it was seeded with, projected as the rules engine reads one" $ do
            handle <- seeded
            outcome <- readStoreManifest handle plainName
            fmap (Map.keys . infoVersions . manifestInfo) outcome `shouldBe` Right ["1.0.0", "1.1.0"]

        it "faults for a package it holds no manifest for, which keeps every version" $ do
            handle <- seeded
            (fmap faultRetry . leftToMaybe <$> readStoreManifest handle scopedName)
                `shouldReturn` Just RetryFutile

        it "answers the configured fault ahead of anything it holds" $ do
            store <- newFakeStore seededConfig{fakeFault = Just aFault}
            (leftToMaybe <$> readStoreManifest (fakeMaintenance store) plainName)
                `shouldReturn` Just aFault

    describe "the fake store's deletion" $ do
        it "removes a held version and reports the operation still running" $ do
            store <- newFakeStore seededConfig
            outcomes <- deleteVersions (fakeMaintenance store) plainName [version "1.0.0"]
            map snd outcomes `shouldBe` [VersionRemoving "fake-operation"]
            remaining <- readFakeContents store
            map (renderVersion . storedVersion) (Map.findWithDefault [] plainName remaining)
                `shouldBe` ["1.1.0"]

        it "refuses a version it does not hold rather than report it gone" $ do
            handle <- seeded
            outcomes <- deleteVersions handle plainName [version "9.9.9"]
            map (isRefusal . snd) outcomes `shouldBe` [True]

        it "reports one outcome per version whatever the batch size" $ do
            handle <- seeded
            outcomes <- deleteVersions handle plainName (map version ["1.0.0", "1.1.0", "9.9.9"])
            map (renderVersion . fst) outcomes `shouldBe` ["1.0.0", "1.1.0", "9.9.9"]

    describe "the fake store's verdicts" $ do
        it "answers the consent verdict it was configured with" $ do
            handle <- seeded
            verifyConsent handle `shouldReturn` Right ConsentGranted

        it "carries a how-to-attach descriptor when consent is withheld" $ do
            store <- newFakeStore seededConfig{fakeConsent = ConsentWithheld "attach the marker"}
            verifyConsent (fakeMaintenance store) `shouldReturn` Right (ConsentWithheld "attach the marker")

        it "answers the classification it was configured with" $ do
            store <- newFakeStore seededConfig{fakeClass = StorePreserved "it is a cache"}
            classifyStore (fakeMaintenance store) `shouldReturn` Right (StorePreserved "it is a cache")

    describe "the fake store's facts" $
        it "takes the arm of every backend-varying fact CodeArtifact does not" $ do
            handle <- seeded
            let facts = storeFacts handle
            factBackend facts `shouldBe` "fake"
            factDeleteCeiling facts `shouldBe` AtMost 2
            factRefill facts `shouldBe` RefillRefused
            factCompletion facts `shouldBe` CompletesLater
            factNameAlphabet facts `shouldBe` noNameAlphabet

    describe "the fake store's walk cursor" $ do
        it "reads back the bucket it was told the walk completed" $ do
            store <- newFakeStore seededConfig
            withBucket "l" $ \completed -> withCursor store $ \cursor -> do
                readCursor cursor `shouldReturn` Right Nothing
                writeCursor cursor completed `shouldReturn` Right ()
                readCursor cursor `shouldReturn` Right (Just completed)
                readFakeCursor store `shouldReturn` Just completed

        it "forgets the walk when it is cleared, so the next one starts over" $ do
            store <- newFakeStore seededConfig
            withBucket "l" $ \completed -> withCursor store $ \cursor -> do
                writeCursor cursor completed `shouldReturn` Right ()
                clearCursor cursor `shouldReturn` Right ()
                readCursor cursor `shouldReturn` Right Nothing

        it "offers none at all on the arm a store with nowhere to write one takes" $ do
            store <- newFakeStore seededConfig{fakeKeepsCursor = False}
            isNothing (storeCursor (fakeMaintenance store)) `shouldBe` True

    describe "the fake store's rehearsal" $ do
        it "reports the outcomes a delete would give, and deletes nothing" $ do
            store <- newFakeStore seededConfig
            case rehearseDelete (fakeMaintenance store) of
                Nothing -> expectationFailure "the fake store has a native rehearsal"
                Just rehearse -> do
                    outcomes <- rehearse plainName (map version ["1.0.0", "9.9.9"])
                    map snd outcomes
                        `shouldBe` [VersionRemoving "fake-operation", refused]
                    remaining <- readFakeContents store
                    map (renderVersion . storedVersion) (Map.findWithDefault [] plainName remaining)
                        `shouldBe` ["1.0.0", "1.1.0"]

    describe "the fake store under a fault" $ do
        it "faults every read" $ do
            store <- newFakeStore seededConfig{fakeFault = Just aFault}
            listWholeStore (fakeMaintenance store) `shouldReturn` Left aFault
            enumerateVersions (fakeMaintenance store) plainName `shouldReturn` Left aFault
            verifyConsent (fakeMaintenance store) `shouldReturn` Left aFault
            classifyStore (fakeMaintenance store) `shouldReturn` Left aFault

        it "marks every version of a faulted delete unreached, and deletes nothing" $ do
            store <- newFakeStore seededConfig{fakeFault = Just aFault}
            outcomes <- deleteVersions (fakeMaintenance store) plainName (map version ["1.0.0", "1.1.0"])
            map snd outcomes `shouldBe` replicate 2 (VersionUnreached aFault)
            remaining <- readFakeContents store
            map (renderVersion . storedVersion) (Map.findWithDefault [] plainName remaining)
                `shouldBe` ["1.0.0", "1.1.0"]
  where
    seeded = fakeMaintenance <$> newFakeStore seededConfig

    withCursor store act = case storeCursor (fakeMaintenance store) of
        Nothing -> expectationFailure "the fake store keeps a walk cursor by default"
        Just cursor -> act cursor

    isRefusal = \case
        VersionRefused _ -> True
        _ -> False

    refused = VersionRefused (storeRefusal "NOT_FOUND" "the store holds no such version")

seededConfig :: FakeStoreConfig
seededConfig =
    defaultFakeStoreConfig
        { fakeContents =
            Map.fromList
                [ (plainName, [served "1.0.0", withdrawn "1.1.0"])
                , (scopedName, [served "7.0.0"])
                ]
        , -- Only one of the two seeded packages carries a manifest, so both read arms are drivable.
          fakeManifests = Map.singleton plainName (sampleManifest plainName (map version ["1.0.0", "1.1.0"]))
        }
  where
    served raw = StoredVersion (version raw) VersionServed
    withdrawn raw = StoredVersion (version raw) VersionWithdrawn

-- The one bucket a store with no alphabet offers, which covers everything it holds.
listWholeStore :: StoreMaintenance -> IO (Either StoreFault [PackageName])
listWholeStore handle = listBucketOf handle ""

listBucketOf :: StoreMaintenance -> Text -> IO (Either StoreFault [PackageName])
listBucketOf handle raw = withBucket raw (collectPages . listPackagesIn handle)

pagesOf :: StoreMaintenance -> IO [[PackageName]]
pagesOf handle =
    withBucket "" $ \everything ->
        runConduit (void (listPackagesIn handle everything) .| CL.consume)

scopedName :: PackageName
scopedName = mkPackageName Npm (Just (mkScope "babel")) "core"

plainName :: PackageName
plainName = mkPackageName Npm Nothing "lodash"

version :: Text -> Version
version = mkVersion Npm

aFault :: StoreFault
aFault =
    StoreFault
        { faultTransport = transportFault TransportTimeout "the store did not answer"
        , faultRetry = RetryWorthwhile
        }
