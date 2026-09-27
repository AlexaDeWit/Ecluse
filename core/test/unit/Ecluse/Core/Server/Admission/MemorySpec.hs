-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Admission.MemorySpec (spec) where

import Test.Hspec
import UnliftIO.Async (wait, withAsync)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Server.Admission.Memory
import Ecluse.Core.Server.Admission.Memory.Gate (defaultGateThresholds, reservedBytes)
import Ecluse.Core.Server.Cache.Store (MaterialReuse (KnownLocalReuse, NeedsMaterialisation))
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape), awaitMemoryWaiters, closedMemoryGate, idleMemoryReading)

-- | The gate's bracket: cheap work passes, heavy work waits then sheds, and reservations always release.
spec :: Spec
spec = describe "withMemoryAdmission" $ do
    it "classes a retained selected read as cheap and a miss as heavy" $ do
        selectedMemoryWork KnownLocalReuse `shouldBe` CheapWork
        selectedMemoryWork NeedsMaterialisation `shouldBe` ColdSelectedRead

    it "runs cheap work at once while the gate is closed" $ do
        gate <- closedMemoryGate 0 0
        withMemoryAdmission noopMetricsPort gate CheapWork (pure ()) `shouldReturn` Just ()

    it "sheds heavy work at once when the waiting room is full" $ do
        gate <- closedMemoryGate 0 1_000_000
        withMemoryAdmission noopMetricsPort gate ColdListing (pure ()) `shouldReturn` Nothing
        gsShedMemory <$> readGateStats gate `shouldReturn` 1

    it "sheds heavy work once the wait budget runs out" $ do
        gate <- closedMemoryGate 4 10_000
        withMemoryAdmission noopMetricsPort gate ColdSelectedRead (pure ()) `shouldReturn` Nothing
        stats <- readGateStats gate
        (gsWaited stats, gsShedMemory stats) `shouldBe` (1, 1)

    it "admits a waiting request when a new reading reopens the gate" $ do
        gate <- closedMemoryGate 4 5_000_000
        withAsync (withMemoryAdmission noopMetricsPort gate ColdListing (pure ())) $ \inFlight -> do
            awaitMemoryWaiters gate 1
            _ <- publishReading gate idleMemoryReading
            wait inFlight `shouldReturn` Just ()

    it "releases the reservation when the work throws" $ do
        gate <- newMemoryAdmissionTuned defaultGateThresholds 4 0
        _ <- publishReading gate idleMemoryReading
        outcome <- try (withMemoryAdmission noopMetricsPort gate ColdListing (throwIO (TestContractEscape "heavy")))
        outcome `shouldBe` (Left (TestContractEscape "heavy") :: Either TestContractEscape (Maybe ()))
        core <- publishReading gate idleMemoryReading
        reservedBytes core `shouldBe` 0
