-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RecordWildCards #-}

-- | Advisory-driven revocation through compile, sync, sweep and install.
module Ecluse.Test.Contract.Sweep.Advisory (advisoryScenario, maliciousScenario, reportsRevocation) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Install (shouldFail, shouldSucceed)
import Ecluse.Test.Contract.Sweep (listed)
import Ecluse.Test.Contract.Sweep.Types
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

-- | Seed, publish, observe the generation swap, revoke, then test vulnerable and fixed installs.
advisoryScenario :: AdvisoryFixture -> StoreOps store -> ClientOps store project -> SweepOps plane -> AdvisoryOps plane store -> SpecWith (plane, store)
advisoryScenario fixture stores clients sweeps advisories =
    describe "advisory compilation, mirror revocation, and the next install" $
        it (caseLabel (advisoryCasePrefix fixture <> "advisory-revocation") "revokes the version a new advisory generation condemns and keeps its fix installable") $ \(plane, proxy) ->
            advisoryLifecycle fixture stores clients sweeps advisories plane proxy Nothing

-- | A trusted MAL copy serves until eligible deletion removes it from both retaining stores.
maliciousScenario :: AdvisoryFixture -> StoreOps store -> ClientOps store project -> SweepOps plane -> AdvisoryOps plane store -> SpecWith (plane, store, store)
maliciousScenario fixture stores clients sweeps advisories =
    describe "MAL compilation and two-store revocation" $
        it (caseLabel (advisoryCasePrefix fixture <> "mal-revocation") "serves the trusted MAL copy until deletion and refuses the next fresh install") $ \(plane, proxy, cache) ->
            advisoryLifecycle fixture stores clients sweeps advisories plane proxy (Just cache)

advisoryLifecycle :: AdvisoryFixture -> StoreOps store -> ClientOps store project -> SweepOps plane -> AdvisoryOps plane store -> plane -> store -> Maybe store -> Expectation
advisoryLifecycle fixture@AdvisoryFixture{..} stores@StoreOps{..} clients@ClientOps{..} sweeps@SweepOps{..} AdvisoryOps{..} plane proxy cache = do
    seeded <- seedAdvisoryVersions fixture stores clients proxy
    let name = packageName advisoryRevoked
        versions = sort (packageVersions advisoryRevoked)
    for_ cache $ \private -> do
        for_ versions $ \version -> void $ clientPublish private name version >>= shouldSucceed
        storeAwaitVersions private name versions `shouldReturn` versions
        listed stores private [name]
    generation <- publishNextGeneration plane
    (sweepExit generation, sweepOutput generation) `shouldSatisfy` ((== ExitSuccess) . fst)
    awaitGenerationSwap proxy `shouldReturn` True
    for_ cache $ \_ -> fst <$> clientArtifact proxy name advisoryVulnerable `shouldReturn` 200
    run <- runSweep plane Apply AdvisoryPolicy
    (sweepExit run, sweepOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
    let reason = case cache of
            Nothing -> "DenyIfCve (affected by " <> advisoryIdentifier <> " (CVSS >= 7.0))"
            Just _ -> "DenyIfKnownMalicious (affected by " <> advisoryIdentifier <> " (known malicious))"
        deletion = "deleting " <> name <> "@" <> advisoryVulnerable <> ": blocked by " <> reason
        tally = "mirror sweep cycle complete: examined 2, deleted 1, kept 1, guard-skipped 0"
        messages = sweepEvents run
    case cache of
        Just private -> do
            sweepOutput run `shouldSatisfy` T.isInfixOf ("blocked by " <> reason)
            storeVersions private name `shouldReturn` [advisoryFixed]
        Nothing ->
            unless (reportsRevocation deletion tally messages) $
                expectationFailure (toString ("the cycle reported these sweep lines:\n" <> T.unlines messages <> "\nexpected the deletion " <> deletion <> "\nand the tally " <> tally <> "\nwhole cycle output:\n" <> sweepOutput run))
    storeSnapshot proxy `shouldReturn` Map.adjust (filter (/= advisoryVulnerable)) name seeded
    assertAdvisoryInstalls fixture stores clients sweeps proxy

seedAdvisoryVersions :: AdvisoryFixture -> StoreOps store -> ClientOps store project -> store -> IO (Map Text [Text])
seedAdvisoryVersions AdvisoryFixture{..} stores@StoreOps{..} ClientOps{..} proxy = do
    let name = packageName advisoryRevoked
        versions = sort (packageVersions advisoryRevoked)
        unaffected = packageName advisoryUnaffected
        keptVersion = packageVersion advisoryUnaffected
    for_ versions $ \version -> void $ clientInstall proxy name (Just version) >>= shouldSucceed
    void $ clientInstall proxy unaffected Nothing >>= shouldSucceed
    storeAwaitVersions proxy name versions `shouldReturn` versions
    storeAwaitVersions proxy unaffected [keptVersion] `shouldReturn` [keptVersion]
    listed stores proxy [name, unaffected]
    storeSnapshot proxy

assertAdvisoryInstalls :: AdvisoryFixture -> StoreOps store -> ClientOps store project -> SweepOps plane -> store -> Expectation
assertAdvisoryInstalls AdvisoryFixture{..} StoreOps{..} ClientOps{..} SweepOps{..} proxy = do
    let name = packageName advisoryRevoked
    void $ clientInstall proxy name (Just advisoryVulnerable) >>= shouldFail
    (status, _) <- clientArtifact proxy name advisoryVulnerable
    status `shouldBe` 403
    awaitWrites
    storeVersions proxy name `shouldReturn` [advisoryFixed]
    void $ clientInstall proxy name (Just advisoryFixed) >>= shouldSucceed
    storeVersions proxy name `shouldReturn` [advisoryFixed]
    storeArtifact proxy name advisoryFixed >>= (`shouldSatisfy` servedBytes)
    served <- clientArtifact proxy name advisoryFixed
    second LBS.length served `shouldSatisfy` servedBytes

-- | Match the exact deletion and tally while retaining an unpinned advisory generation.
reportsRevocation :: Text -> Text -> [Text] -> Bool
reportsRevocation deletion tally messages =
    map (fst . T.breakOn "; advisory generation ") (filter (T.isPrefixOf "deleting ") messages) == [deletion]
        && filter (T.isPrefixOf "mirror sweep cycle ") messages == [tally]
