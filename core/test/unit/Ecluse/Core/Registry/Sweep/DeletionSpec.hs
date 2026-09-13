-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Grouped destructive operations driven through the real sweep and mutable backend fixtures.
module Ecluse.Core.Registry.Sweep.DeletionSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Cve (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry.Maintenance
import Ecluse.Core.Registry.Sweep (sweepCycle)
import Ecluse.Core.Registry.Sweep.Types
import Ecluse.Core.Rules (PreparedRule (prepEval), prepare)
import Ecluse.Core.Rules.Types (Rule (DenyByIdentity), RuleVerdict (Allow, Deny))
import Ecluse.Core.Telemetry.Metrics (SweepResult (SweepDeleted), SweepTarget (..))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Maintenance
import Ecluse.Test.Package (sampleManifest)
import Ecluse.Test.Rules (atDefaultPrecedence, denyRule, inertRuleDeps)
import Ecluse.Test.Sweep

spec :: Spec
spec = describe "grouped deletion" $ do
    it "removes mirror-only, cache-only and both-present versions with three logical charges" $ do
        mirror <- seeded "mirror" ["1.0.0", "3.0.0"]
        cache <- seeded "cache" ["2.0.0", "3.0.0"]
        mount <- grouped mirror cache
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing{swpDeletionCap = 3} (recPorts recorded) [mount]
        outcomeHalt outcome `shouldBe` Just (HaltDeletionCap 3 3 Nothing)
        tallyDeleted (outcomeTally outcome) `shouldBe` 4
        operations <- recTargetResults recorded
        length (filter (== (SweepMirror, SweepDeleted)) operations) `shouldBe` 2
        length (filter (== (SweepPrivate, SweepDeleted)) operations) `shouldBe` 2
        held mirror `shouldReturn` []
        held cache `shouldReturn` []

    it "retains the denial's generation when the shared logical cap fills" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0"]
        mount <- grouped mirror cache
        let generation = Just (DbEtag "cap-generation")
            policy = denyRule{prepEval = \_ _ -> pure (Deny generation "current advisory")}
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing{swpDeletionCap = 1} (recPorts recorded) [mount{smRules = [policy]}]
        outcomeHalt outcome `shouldBe` Just (HaltDeletionCap 1 1 generation)
        held mirror `shouldReturn` []
        held cache `shouldReturn` []

    it "finishes both copies at the last charge before selecting another version" $ do
        mirror <- seeded "mirror" ["1.0.0", "2.0.0"]
        cache <- seeded "cache" ["1.0.0", "2.0.0"]
        mount <- grouped mirror cache
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing{swpDeletionCap = 1} (recPorts recorded) [mount]
        outcomeHalt outcome `shouldBe` Just (HaltDeletionCap 1 1 Nothing)
        held mirror `shouldReturn` [version "2.0.0"]
        held cache `shouldReturn` [version "2.0.0"]

    it "submits mirror work before cache work and rediscovers a failed cache after restart" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0"]
        mount <- grouped mirror cache
        calls <- newIORef ([] :: [Text])
        let failed = protocolFault "cache unavailable"
            cacheStore = (deletingStore (fakeMaintenance cache)){ssExecute = SweepRemoves (StoreDeletion (\checks _ versions -> deleteAll checks (\_ -> modifyIORef' calls (<> ["cache"]) $> Left failed) [versions]) Nothing)}
            first = mapDeletion (\send checks name versions -> modifyIORef' calls (<> ["mirror"]) >> send checks name versions) (smStore mount)
        recorded <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts recorded) [mount{smStore = first{ssPrivate = Just cacheStore}}]
        readIORef calls `shouldReturn` ["mirror", "cache"]
        held mirror `shouldReturn` []
        held cache `shouldReturn` [version "1.0.0"]
        restarted <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts restarted) [mount]
        held cache `shouldReturn` []

    it "rechecks lost responses without blindly replaying a version now absent" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0"]
        mount <- grouped mirror cache
        attempts <- newIORef (0 :: Int)
        let lost = (protocolFault "response lost"){faultRetry = RetryWorthwhile}
            original = fakeMaintenance mirror
            uncertain =
                mapDeletion
                    ( \_ checks name versions ->
                        deleteAll
                            checks
                            ( \batch -> do
                                modifyIORef' attempts (+ 1)
                                void (deleteVersions original testDeleteGuard name batch)
                                pure (Left lost)
                            )
                            [versions]
                    )
                    (smStore mount)
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing{swpDeletionCap = 1} (recPorts recorded) [mount{smStore = uncertain}]
        outcomeHalt outcome `shouldBe` Just (HaltDeletionCap 1 1 Nothing)
        readIORef attempts `shouldReturn` 1
        held cache `shouldReturn` []
        recErrors recorded >>= (`shouldSatisfy` any (T.isInfixOf "outcome is uncertain"))

    it "honours a consent withdrawal between the source and cache attempts" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0"]
        mount <- grouped mirror cache
        permitted <- newIORef True
        let withdrawn = (deletingStore (fakeMaintenance cache)){ssObserve = (fakeObservation cache){obVerifyConsent = readIORef permitted <&> \yes -> Right (if yes then ConsentGranted else ConsentWithheld "cache revoked")}}
            source = mapDeletion (\send checks name versions -> send checks name versions <* writeIORef permitted False) (smStore mount)
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing (recPorts recorded) [mount{smStore = source{ssPrivate = Just withdrawn}}]
        outcomeHalt outcome `shouldSatisfy` isJust
        held mirror `shouldReturn` []
        held cache `shouldReturn` [version "1.0.0"]

    it "does not retry when current policy permits the version after an uncertain result" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" []
        mount <- grouped mirror cache
        changed <- newIORef False
        attempts <- newIORef (0 :: Int)
        let policy = denyRule{prepEval = \ctx evidence -> readIORef changed >>= \allow -> if allow then pure (Allow "new policy") else prepEval denyRule ctx evidence}
            fault = (protocolFault "response lost"){faultRetry = RetryWorthwhile}
            source =
                mapDeletion
                    ( \_ checks _ versions ->
                        deleteAll
                            checks
                            ( \_ -> do
                                modifyIORef' attempts (+ 1)
                                writeIORef changed True
                                pure (Left fault)
                            )
                            [versions]
                    )
                    (smStore mount)
        recorded <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts recorded) [mount{smRules = [policy], smStore = source}]
        readIORef attempts `shouldReturn` 1
        held mirror `shouldReturn` [version "1.0.0"]

    it "reassesses a newly retained revision after an absence without charging it again" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" []
        mount <- grouped mirror cache
        attempts <- newIORef (0 :: Int)
        let lost = (protocolFault "response lost"){faultRetry = RetryWorthwhile}
            original = fakeMaintenance mirror
            source =
                mapDeletion
                    ( \_ checks name versions ->
                        deleteAll
                            checks
                            ( \batch -> do
                                count <- atomicModifyIORef' attempts (\n -> (n + 1, n + 1))
                                outcomes <- deleteVersions original testDeleteGuard name batch
                                pure (if count == 1 then Left lost else Right outcomes)
                            )
                            [versions]
                    )
                    (smStore mount)
        recorded <- recordingPorts Nothing
        let ports = (recPorts recorded){sweepDelay = \_ -> writeFakeContents mirror (Map.singleton packageName [StoredVersion (version "1.0.0") VersionServed (Just "retained-again")])}
        outcome <- sweepCycle testPacing{swpDeletionCap = 1} ports [mount{smStore = source}]
        outcomeHalt outcome `shouldBe` Just (HaltDeletionCap 1 1 Nothing)
        readIORef attempts `shouldReturn` 2
        held mirror `shouldReturn` []

    it "defers a revision that changes while its policy evidence is read" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" []
        mount <- grouped mirror cache
        reads <- newIORef (0 :: Int)
        let original = smStore mount
            observation =
                (ssObserve original)
                    { obEnumerateVersions = \name -> do
                        count <- atomicModifyIORef' reads (\n -> (n + 1, n + 1))
                        fmap (map (\item -> item{storedRevision = Just (if count < 3 then "old" else "new")})) <$> obEnumerateVersions (ssObserve original) name
                    }
        recorded <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts recorded) [mount{smStore = original{ssObserve = observation}}]
        held mirror `shouldReturn` [version "1.0.0"]

    it "protects first-party names without reading either manifest" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0"]
        mount <- grouped mirror cache
        let protect store = store{ssObserve = (ssObserve store){obReadManifest = \_ -> fail "first-party metadata must not be read"}}
            store = smStore mount
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing (recPorts recorded) [mount{smFirstParty = const True, smStore = (protect store){ssPrivate = protect <$> ssPrivate store}}]
        tallyGuardSkipped (outcomeTally outcome) `shouldBe` 2
        held mirror `shouldReturn` [version "1.0.0"]
        held cache `shouldReturn` [version "1.0.0"]

seeded :: Text -> [Text] -> IO FakeStore
seeded backend raw =
    newFakeStore
        defaultFakeStoreConfig
            { fakeContents = Map.singleton packageName [StoredVersion item VersionServed Nothing | item <- versions]
            , fakeManifests = Map.singleton packageName (sampleManifest packageName versions)
            , fakeFacts = (fakeFacts defaultFakeStoreConfig){factBackend = backend, factCompletion = CompletesOnCall}
            }
  where
    versions = map version raw

grouped :: FakeStore -> FakeStore -> IO SweepMount
grouped mirror cache = do
    let configured = [DenyByIdentity "left-pad"]
    rules <- prepare inertRuleDeps (map atDefaultPrecedence configured)
    let mount = testMount (fakeMaintenance mirror) rules configured
    pure mount{smStore = (smStore mount){ssPrivate = Just (deletingStore (fakeMaintenance cache))}}

held :: FakeStore -> IO [Version]
held store = map storedVersion . Map.findWithDefault [] packageName <$> readFakeContents store

mapDeletion :: ((DeleteGuard -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]) -> DeleteGuard -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]) -> SweepStore -> SweepStore
mapDeletion f store = case ssExecute store of
    SweepCounts -> store
    SweepRemoves deletion -> store{ssExecute = SweepRemoves deletion{dlDeleteVersions = f (dlDeleteVersions deletion)}}

packageName :: PackageName
packageName = mkPackageName Npm Nothing "left-pad"

version :: Text -> Version
version = mkVersion Npm
