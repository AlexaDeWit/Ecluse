-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Grouped destructive operations driven through the real sweep and mutable backend fixtures.
module Ecluse.Core.Registry.Sweep.DeletionSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Test.Hspec

import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance
import Ecluse.Core.Registry.Sweep (sweepCycle)
import Ecluse.Core.Registry.Sweep.Outcome
import Ecluse.Core.Registry.Sweep.Types
import Ecluse.Core.Rules (PreparedRule (prepEval), prepare)
import Ecluse.Core.Rules.Types (Rule (DenyByIdentity), RuleVerdict (Allow, Deny))
import Ecluse.Core.Telemetry.Metrics (SweepResult (SweepDeleted, SweepGuardSkipped), SweepTarget (..))
import Ecluse.Core.Version (Version)
import Ecluse.Test.Maintenance
import Ecluse.Test.Package (leftPadName, npmVersion)
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
        held mirror `shouldReturn` [npmVersion "2.0.0"]
        held cache `shouldReturn` [npmVersion "2.0.0"]

    it "counts a cache version the allowance held back under the private target" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0", "2.0.0"]
        mount <- grouped mirror cache
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing{swpDeletionCap = 1} (recPorts recorded) [mount]
        outcomeHalt outcome `shouldBe` Just (HaltDeletionCap 1 1 Nothing)
        operations <- recTargetResults recorded
        operations `shouldSatisfy` elem (SweepPrivate, SweepGuardSkipped)
        operations `shouldSatisfy` notElem (SweepMirror, SweepGuardSkipped)
        held cache `shouldReturn` [npmVersion "2.0.0"]

    it "counts a cache version the per-batch recheck refuses under the private target" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0", "2.0.0"]
        mount <- grouped mirror cache
        -- The backend hands the guard a version the allowance withheld, which is the batch the
        -- recheck exists to refuse, so the cap charge and not the allowance holds it back.
        let widened = mapCacheDeletion (\send checks name _ -> send checks name (map npmVersion ["1.0.0", "2.0.0"])) (deletingCache (fakeMaintenance cache))
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing{swpDeletionCap = 1} (recPorts recorded) [mount{smStore = (smStore mount){ssPrivate = widened}}]
        outcomeHalt outcome `shouldBe` Just (HaltDeletionCap 1 1 Nothing)
        operations <- recTargetResults recorded
        length (filter (== (SweepPrivate, SweepGuardSkipped)) operations) `shouldBe` 2
        operations `shouldSatisfy` notElem (SweepMirror, SweepGuardSkipped)
        held mirror `shouldReturn` []
        held cache `shouldReturn` [npmVersion "2.0.0"]

    it "submits mirror work before cache work and rediscovers a failed cache after restart" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0"]
        mount <- grouped mirror cache
        calls <- newIORef ([] :: [Text])
        let failed = protocolFault "cache unavailable"
            cacheStore = (deletingCache (fakeMaintenance cache)){scExecute = SweepRemoves (StoreDeletion (\checks _ versions -> deleteAll checks (\_ -> modifyIORef' calls (<> ["cache"]) $> Left failed) [versions]) Nothing)}
            sourceStore = mapDeletion (\send checks name versions -> modifyIORef' calls (<> ["mirror"]) >> send checks name versions) (smStore mount)
        recorded <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts recorded) [mount{smStore = sourceStore{ssPrivate = cacheStore}}]
        readIORef calls `shouldReturn` ["mirror", "cache"]
        held mirror `shouldReturn` []
        held cache `shouldReturn` [npmVersion "1.0.0"]
        restarted <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts restarted) [mount]
        held cache `shouldReturn` []

    it "keeps a refused source and its cache copy while continuing unrelated versions" $ do
        mirror <- seeded "mirror" ["1.0.0", "2.0.0"]
        cache <- seeded "cache" ["1.0.0", "2.0.0", "3.0.0"]
        mount <- grouped mirror cache
        let original = fakeMaintenance mirror
            withheld = npmVersion "1.0.0"
            refusal = storeRefusal "REFUSED" "this version is retained"
            source =
                mapDeletion
                    ( \_ checks name versions ->
                        deleteAll
                            checks
                            ( \batch -> do
                                removed <- deleteVersions original testDeleteGuard name (filter (/= withheld) batch)
                                pure (Right ([(item, VersionRefused refusal) | item <- batch, item == withheld] <> removed))
                            )
                            (chunksOfCeiling (factDeleteCeiling (storeFacts original)) versions)
                    )
                    (smStore mount)
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing (recPorts recorded) [mount{smStore = source}]
        outcomeHalt outcome `shouldBe` Nothing
        held mirror `shouldReturn` [withheld]
        held cache `shouldReturn` [withheld]
        errors <- recErrors recorded
        errors `shouldSatisfy` any (T.isInfixOf "REFUSED")
        errors `shouldSatisfy` (not . any (T.isInfixOf "cleanup remains incomplete"))

    it "halts when a reported successful deletion leaves the denied version present" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0"]
        mount <- grouped mirror cache
        let source = mapDeletion (\_ checks _ versions -> deleteAll checks (\batch -> pure (Right [(item, VersionRemoved) | item <- batch])) [versions]) (smStore mount)
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing (recPorts recorded) [mount{smStore = source}]
        outcomeHalt outcome
            `shouldBe` Just (HaltStoreFault Npm "mirror" (renderStoreFault (protocolFault "cleanup remains incomplete for versions [\"1.0.0\"]")))
        recErrors recorded >>= (`shouldSatisfy` any (T.isInfixOf "cleanup remains incomplete"))
        held mirror `shouldReturn` [npmVersion "1.0.0"]
        held cache `shouldReturn` [npmVersion "1.0.0"]

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
        recWarnings recorded >>= (`shouldSatisfy` any (T.isInfixOf "mirror: reassessing an uncertain deletion"))

    it "waits the delay the fault advises, then reassesses the delete exactly once" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" []
        mount <- grouped mirror cache
        attempts <- newIORef (0 :: Int)
        delays <- newIORef ([] :: [NominalDiffTime])
        let delayed = (protocolFault "response lost"){faultRetry = RetryDelayed (RetryAfter 7)}
            original = fakeMaintenance mirror
            source =
                mapDeletion
                    ( \_ checks name versions ->
                        deleteAll
                            checks
                            ( \batch -> do
                                count <- atomicModifyIORef' attempts (\n -> (n + 1, n + 1))
                                if count == 1
                                    then pure (Left delayed)
                                    else Right <$> deleteVersions original testDeleteGuard name batch
                            )
                            [versions]
                    )
                    (smStore mount)
        recorded <- recordingPorts Nothing
        let ports = (recPorts recorded){sweepDelay = \seconds -> modifyIORef' delays (<> [seconds])}
        outcome <- sweepCycle testPacing ports [mount{smStore = source}]
        outcomeHalt outcome `shouldBe` Nothing
        readIORef attempts `shouldReturn` 2
        readIORef delays `shouldReturn` [7]
        held mirror `shouldReturn` []

    it "honours a consent withdrawal between the source and cache attempts" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0"]
        mount <- grouped mirror cache
        permitted <- newIORef True
        let withdrawn = (deletingCache (fakeMaintenance cache)){scObserve = (fakeObservation cache){obVerifyConsent = readIORef permitted <&> \yes -> Right (if yes then ConsentGranted else ConsentWithheld "cache revoked")}}
            source = mapDeletion (\send checks name versions -> send checks name versions <* writeIORef permitted False) (smStore mount)
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing (recPorts recorded) [mount{smStore = source{ssPrivate = withdrawn}}]
        outcomeHalt outcome `shouldBe` Just (HaltStoreFault Npm "cache" (renderStoreFault (protocolFault "cache revoked")))
        held mirror `shouldReturn` []
        held cache `shouldReturn` [npmVersion "1.0.0"]

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
        held mirror `shouldReturn` [npmVersion "1.0.0"]

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
        let ports = (recPorts recorded){sweepDelay = \_ -> writeFakeContents mirror (Map.singleton leftPadName [StoredVersion (npmVersion "1.0.0") VersionServed (Just "retained-again")])}
        outcome <- sweepCycle testPacing{swpDeletionCap = 1} ports [mount{smStore = source}]
        outcomeHalt outcome `shouldBe` Just (HaltDeletionCap 1 1 Nothing)
        readIORef attempts `shouldReturn` 2
        held mirror `shouldReturn` []

    it "defers a revision that changes while its policy evidence is read" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" []
        mount <- grouped mirror cache
        inventoryReads <- newIORef (0 :: Int)
        let original = smStore mount
            observation =
                (ssObserve original)
                    { obEnumerateVersions = \name -> do
                        count <- atomicModifyIORef' inventoryReads (\n -> (n + 1, n + 1))
                        fmap (map (\item -> item{storedRevision = Just (if count < 3 then "old" else "new")})) <$> obEnumerateVersions (ssObserve original) name
                    }
        recorded <- recordingPorts Nothing
        _ <- sweepCycle testPacing (recPorts recorded) [mount{smStore = original{ssObserve = observation}}]
        held mirror `shouldReturn` [npmVersion "1.0.0"]

    it "protects first-party names without reading either manifest" $ do
        mirror <- seeded "mirror" ["1.0.0"]
        cache <- seeded "cache" ["1.0.0"]
        mount <- grouped mirror cache
        let unread _ = fail "first-party metadata must not be read"
            protect target = target{ssObserve = (ssObserve target){obReadManifest = unread}}
            protectCache target = target{ssPrivate = (ssPrivate target){scObserve = (scObserve (ssPrivate target)){obReadManifest = unread}}}
            store = smStore mount
        recorded <- recordingPorts Nothing
        outcome <- sweepCycle testPacing (recPorts recorded) [mount{smFirstParty = const True, smStore = protectCache (protect store)}]
        tallyGuardSkipped (outcomeTally outcome) `shouldBe` 2
        held mirror `shouldReturn` [npmVersion "1.0.0"]
        held cache `shouldReturn` [npmVersion "1.0.0"]

seeded :: Text -> [Text] -> IO FakeStore
seeded backend raw =
    newFakeStore
        (seededStoreConfig [(leftPadName, map npmVersion raw)])
            { fakeFacts = (fakeFacts defaultFakeStoreConfig){factBackend = backend, factCompletion = CompletesOnCall}
            }

grouped :: FakeStore -> FakeStore -> IO SweepMount
grouped mirror cache = do
    let configured = [DenyByIdentity "left-pad"]
    rules <- prepare inertRuleDeps (map atDefaultPrecedence configured)
    let mount = testMount (fakeMaintenance mirror) rules configured
    pure (withPrivateCache (deletingCache (fakeMaintenance cache)) mount)

held :: FakeStore -> IO [Version]
held = heldVersions leftPadName

mapDeletion :: ((DeleteGuard -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]) -> DeleteGuard -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]) -> SweepStore -> SweepStore
mapDeletion f store = case ssExecute store of
    SweepCounts -> store
    SweepRemoves deletion -> store{ssExecute = SweepRemoves deletion{dlDeleteVersions = f (dlDeleteVersions deletion)}}

mapCacheDeletion :: ((DeleteGuard -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]) -> DeleteGuard -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]) -> SweepCache -> SweepCache
mapCacheDeletion f cache = case scExecute cache of
    SweepCounts -> cache
    SweepRemoves deletion -> cache{scExecute = SweepRemoves deletion{dlDeleteVersions = f (dlDeleteVersions deletion)}}
