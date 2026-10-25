-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Origin outcomes preserve authority, while selected flights share their admission priority.
module Ecluse.Core.Server.Pipeline.OriginSpec (spec) where

import Test.Hspec
import UnliftIO (timeout, wait, withAsync)

import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataAbsent))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (PrepareRead))
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Npm.Route (npmRouter)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeSelectedRead))
import Ecluse.Core.Server.Admission.Meter (MeterSettings (..), charge, meterSnapshot, newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Context (MountBinding (..), PackumentDeps (..), RequestCtx (RequestCtx), ServeRuntime (srMemoryMeter), runHandler)
import Ecluse.Core.Server.Pipeline.Origin (
    OriginMiss (MissAbsent, MissUnresolved),
    OriginResult (
        OriginAbsent,
        OriginAuthorisationFailure,
        OriginNameMismatch,
        OriginNotFound,
        OriginResolved,
        OriginUnresolved
    ),
    originManifest,
    originMiss,
    withPublicMetadata,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpVersionCacheRequest))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Package (npmVersion, sampleManifest, thingName)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Handler (testServeRuntime)
import Ecluse.Test.Server.Mount (inertPackumentDeps)

spec :: Spec
spec = describe "originMiss -- the outcomes that leave nothing to serve" $ do
    sharedSelectedSpec
    it "reports no miss for a document, an access refusal, or an identity refusal" $
        map
            originMiss
            [ OriginResolved (sampleManifest thingName [])
            , OriginAuthorisationFailure 401
            , OriginAuthorisationFailure 403
            , OriginNameMismatch
            ]
            `shouldBe` [Nothing, Nothing, Nothing, Nothing]

    it "separates an origin that answered 404 from one that was never read" $
        map originMiss [OriginNotFound, OriginUnresolved]
            `shouldBe` [Just MissAbsent, Just MissUnresolved]

    it "groups an unconfigured origin with an absence, because no retry reaches it" $
        originMiss OriginAbsent `shouldBe` Just MissAbsent

    it "contributes no manifest from any miss" $
        map (isNothing . originManifest) [OriginNotFound, OriginUnresolved, OriginAbsent]
            `shouldBe` [True, True, True]

sharedSelectedSpec :: Spec
sharedSelectedSpec = it "lets a selected leader grow while its follower holds the overdraw token" $ withSweeper $ \deadlines -> do
    followerEntered <- newEmptyMVar
    allowToken <- newEmptyMVar
    leaderStarted <- newEmptyMVar
    chargeNow <- newEmptyMVar
    followerJoined <- newEmptyMVar
    let metrics = noopMetricsPort{mpVersionCacheRequest = \result -> when (result == Metric.Collapsed) (putMVar followerJoined ())}
    original <- testServeRuntime deadlines metrics
    meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = 30, msStepBytes = 10, msEntryRoom = 0, msEntryWaitMicros = 0}
    logEnv <- newTestLogEnv
    let runtime = original{srMemoryMeter = meter}
        factory = PrepareRead $ \_ origin -> do
            putMVar leaderStarted ()
            takeMVar chargeNow
            ocChargeSelectedRead origin 15
            pure (Left MetadataAbsent)
        deps = inertPackumentDeps{pdMetadata = (pdMetadata inertPackumentDeps){metadataRead = factory}}
        binding = MountBinding ("npm" :| []) npmRouter npmCredential deps Nothing
        selected ticket = runHandler logEnv mempty (RequestCtx runtime binding) (withPublicMetadata runtime deps ticket thingName (npmVersion "1.0.0") liftIO)
        follower = withMemoryEntry metrics meter $ \ticket -> do
            putMVar followerEntered ()
            takeMVar allowToken
            charge ticket 45
            selected ticket
        leader = withMemoryEntry metrics meter selected
    completed <- timeout 2_000_000 $
        withAsync follower $ \following -> do
            takeMVar followerEntered
            withAsync leader $ \leading -> do
                takeMVar leaderStarted
                putMVar allowToken ()
                takeMVar followerJoined
                putMVar chargeNow ()
                (,) <$> wait leading <*> wait following
    completed `shouldBe` Just (Right (Left MetadataAbsent), Right (Left MetadataAbsent))
    snChargedBytes <$> meterSnapshot meter `shouldReturn` 0
