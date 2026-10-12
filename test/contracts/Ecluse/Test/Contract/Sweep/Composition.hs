-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The full Dredger contract preserves five independent mutable-store lifetimes.
module Ecluse.Test.Contract.Sweep.Composition (SweepDriver (..), sweepContract) where

import Test.Hspec (Spec, aroundAll)

import Ecluse.Test.Contract.Sweep.Advisory qualified as Advisory
import Ecluse.Test.Contract.Sweep.Identity qualified as Identity
import Ecluse.Test.Contract.Sweep.Outage qualified as Outage
import Ecluse.Test.Contract.Sweep.Recovery qualified as Recovery
import Ecluse.Test.Contract.Sweep.Rollout qualified as Rollout
import Ecluse.Test.Contract.Sweep.Types

-- | Native resources and observations used by every required Dredger family.
data SweepDriver plane store project = SweepDriver
    { withIdentitySweep :: ((plane, store, store) -> IO ()) -> IO ()
    , withAdvisorySweep :: ((plane, store) -> IO ()) -> IO ()
    , withMaliciousSweep :: ((plane, store, store) -> IO ()) -> IO ()
    , withRecoverySweep :: ((plane, store, store) -> IO ()) -> IO ()
    , withRolloutSweep :: ((plane, store, store) -> IO ()) -> IO ()
    , sweepIdentityFixture :: IdentityFixture
    , sweepAdvisoryFixture :: AdvisoryFixture
    , sweepMaliciousFixture :: AdvisoryFixture
    , sweepRecoveryFixture :: RecoveryFixture
    , sweepRolloutFixture :: RolloutFixture
    , sweepStoreOps :: StoreOps store
    , sweepClientOps :: ClientOps store project
    , sweepGuardedOps :: SweepOps plane
    , sweepPlainOps :: SweepOps plane
    , sweepAdvisoryOps :: AdvisoryOps plane store
    , sweepMaliciousOps :: AdvisoryOps plane store
    , sweepRecoveryOps :: RecoveryOps plane store
    , sweepRolloutOps :: RolloutOps plane store
    , sweepAdmissionOutage :: Maybe (AdmissionOutageDriver plane store)
    }

-- | Compose identity, advisory revocation, recovery and stale-role obligations.
sweepContract :: SweepDriver plane store project -> Spec
sweepContract driver = do
    for_ (sweepAdmissionOutage driver) $ \outage ->
        aroundAll (withAdmissionOutage outage) (Outage.admissionOutageScenario outage stores clients)
    aroundAll (withIdentitySweep driver) $
        Identity.identityScenarios (sweepIdentityFixture driver) stores clients (sweepGuardedOps driver)
    aroundAll (withAdvisorySweep driver) $
        Advisory.advisoryScenario (sweepAdvisoryFixture driver) stores clients (sweepPlainOps driver) (sweepAdvisoryOps driver)
    aroundAll (withMaliciousSweep driver) $
        Advisory.maliciousScenario (sweepMaliciousFixture driver) stores clients (sweepPlainOps driver) (sweepMaliciousOps driver)
    aroundAll (withRecoverySweep driver) $
        Recovery.recoveryScenarios (sweepRecoveryFixture driver) stores clients (sweepPlainOps driver) (sweepRecoveryOps driver)
    aroundAll (withRolloutSweep driver) $
        Rollout.rolloutScenarios (sweepRolloutFixture driver) stores clients (sweepGuardedOps driver) (sweepRecoveryOps driver) (sweepRolloutOps driver)
  where
    stores = sweepStoreOps driver
    clients = sweepClientOps driver
