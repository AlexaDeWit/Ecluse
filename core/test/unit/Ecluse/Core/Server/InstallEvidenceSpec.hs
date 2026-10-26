-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Public evidence uses anonymous bounded transport and never crosses first-party or authority guards.
module Ecluse.Core.Server.InstallEvidenceSpec (spec) where

import Control.Concurrent.MVar qualified as MVar
import UnliftIO.Async (wait, withAsync)
import UnliftIO.Timeout (timeout)

import Network.HTTP.Types (status200, status503)
import Test.Hspec

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Package (Artifact (artUrl), CodeExecSignal (NoCodeOnInstall), PackageDetails (pkgArtifacts))
import Ecluse.Core.Rules.Effectful (ReadFault (ReadFault))
import Ecluse.Core.Rules.Install
import Ecluse.Core.Rules.Types (Inability (InstallSignalUnread), Transience (WillResolve, WontResolve))
import Ecluse.Core.Security (Limits (maxMirrorArtifactBytes))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Meter (MeterSettings (..), charge, newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.Context (PackumentDeps (..), ServeRuntime (srMemoryMeter, srPublicClient, srPublicControl))
import Ecluse.Core.Server.InstallEvidence (newPublicInstallEvidenceReads, newTicketInstallEvidenceFlightBudget)
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Rules.Support (now)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Package (sampleArtifact, sampleDetails, thingName, v1_0_0)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Handler (testServeRuntime)
import Ecluse.Test.Server.Mount (npmServeDeps)
import Ecluse.Test.Stub (Stub, allCaptured, headerValue, stubLocalhostUrl, withStub, withStubHeaders)

spec :: Spec
spec = describe "public install evidence boundary" $ do
    it "lets an evidence flight grow with the priority of its waiting token holder" $
        withSweeper $ \deadlines -> do
            meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = 30, msStepBytes = 10, msEntryRoom = 2, msEntryWaitMicros = 1_000_000}
            entered <- MVar.newEmptyMVar
            go <- MVar.newEmptyMVar
            waiting <- MVar.newEmptyMVar
            release <- MVar.newEmptyMVar
            let runLeader = withMemoryEntry noopMetricsPort meter $ \ticket -> do
                    sharing <- newTicketInstallEvidenceFlightBudget ticket
                    MVar.putMVar entered ()
                    MVar.takeMVar go
                    withinInstallEvidenceFlight sharing "exact-subject" $ \budget ->
                        withInstallEvidenceBudget budget (\bytes _ -> bytes 15)
                runHolder = withMemoryEntry noopMetricsPort meter $ \ticket -> do
                    charge ticket 45
                    sharing <- newTicketInstallEvidenceFlightBudget ticket
                    withinInstallEvidenceFlight sharing "exact-subject" $ \_ ->
                        MVar.putMVar waiting () >> MVar.takeMVar release
            withAsync runLeader $ \leader -> do
                MVar.takeMVar entered
                withAsync runHolder $ \holder -> do
                    MVar.takeMVar waiting
                    MVar.putMVar go ()
                    timeout 5_000_000 (wait leader) `shouldReturn` Just (Right (Just ()))
                    MVar.putMVar release ()
                    wait holder `shouldReturn` Right ()
    it "fetches anonymously with the existing request identity" $
        withSweeper $ \deadlines -> withStub status200 "evidence bytes" $ \stub -> do
            runtime <- testServeRuntime deadlines noopMetricsPort
            let deps = (dependencies stub){pdInboundToken = Just (mkSecret "edge-secret")}
            withBoundReads deps runtime $ \evidenceReads ->
                runEvidence evidenceReads (subject stub) `shouldReturn` InstallDetermined NoCodeOnInstall
            requests <- allCaptured stub
            map (headerValue "authorization") requests `shouldBe` [Nothing]
            map (headerValue "cookie") requests `shouldBe` [Nothing]
            map (headerValue "user-agent") requests `shouldSatisfy` all isJust
            length requests `shouldBe` 1
    it "does not construct an evidence subject for a first-party name" $
        withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
            runtime <- testServeRuntime deadlines noopMetricsPort
            built <- newIORef (0 :: Int)
            let provider = InstallEvidenceProvider (\_ -> pure (\_ -> modifyIORef' built (+ 1) $> Nothing))
                deps = (dependencies stub){pdFirstParty = const True, pdInstallEvidence = Just provider}
            withBoundReads deps runtime $ \evidenceReads ->
                isNothing <$> evidenceReads (subject stub) `shouldReturn` True
            readIORef built `shouldReturn` 0
            allCaptured stub `shouldReturn` []
    it "refuses a foreign artifact authority before HTTP" $
        withSweeper $ \deadlines -> withStub status200 "never read" $ \stub -> do
            runtime <- testServeRuntime deadlines noopMetricsPort
            let foreignSubject = (subject stub){pkgArtifacts = sampleArtifact{artUrl = "https://foreign.invalid/archive"} :| []}
            withBoundReads (dependencies stub) runtime $ \evidenceReads ->
                runEvidence evidenceReads foreignSubject >>= (`shouldSatisfy` permanentFailure)
            allCaptured stub `shouldReturn` []
    it "honours authority cooldown without another evidence GET" $
        withSweeper $ \deadlines -> withStubHeaders status503 [("Retry-After", "30")] "busy" $ \stub -> do
            runtime <- testServeRuntime deadlines noopMetricsPort
            withBoundReads (dependencies stub) runtime $ \evidenceReads ->
                replicateM_ 2 (runEvidence evidenceReads (subject stub) >>= (`shouldSatisfy` transientFailure))
            length <$> allCaptured stub `shouldReturn` 1
    it "applies the reduced per-read byte ceiling" $
        withSweeper $ \deadlines -> withStub status200 "larger than four bytes" $ \stub -> do
            runtime <- testServeRuntime deadlines noopMetricsPort
            let deps = dependencies stub
                bounded = deps{pdLimits = (pdLimits deps){maxMirrorArtifactBytes = 4}}
            withBoundReads bounded runtime $ \evidenceReads ->
                runEvidence evidenceReads (subject stub) >>= (`shouldSatisfy` permanentFailure)

dependencies :: Stub -> PackumentDeps
dependencies stub =
    (npmServeDeps Nothing base NoMirrorWrite [] (pure now))
        { pdInstallEvidence = Just transportProvider
        , pdEgressUrl = Right . loopbackRegistryUrl
        }
  where
    base = loopbackRegistryUrl (stubLocalhostUrl stub)

subject :: Stub -> PackageDetails
subject stub = (sampleDetails thingName v1_0_0){pkgArtifacts = sampleArtifact{artUrl = stubLocalhostUrl stub <> "/archive"} :| []}

withBoundReads :: PackumentDeps -> ServeRuntime -> ((PackageDetails -> IO (Maybe InstallEvidenceRead)) -> IO ()) -> IO ()
withBoundReads deps runtime use = do
    completed <- withMemoryEntry noopMetricsPort (srMemoryMeter runtime) $ \ticket ->
        newPublicInstallEvidenceReads deps (srPublicClient runtime) (srPublicControl runtime) InstallPublicAdmission ticket (const pass) >>= use
    completed `shouldBe` Right ()

-- The native parser has separate tests. This provider observes the real role-bound transport alone.
transportProvider :: InstallEvidenceProvider
transportProvider = InstallEvidenceProvider $ \evidenceContext -> pure $ \details -> do
    let artifact :| _ = pkgArtifacts details
    pure . Just . InstallEvidenceRead $ do
        result <- withInstallEvidenceBudget (ieBudget evidenceContext) $ \bytes _ -> ieReadArtifact evidenceContext (ieLimits evidenceContext) artifact bytes
        pure $ case result of
            Nothing -> InstallUndetermined InstallSignalUnread
            Just (Left fault) -> InstallReadFailed fault
            Just (Right _) -> InstallDetermined NoCodeOnInstall

runEvidence :: (PackageDetails -> IO (Maybe InstallEvidenceRead)) -> PackageDetails -> IO InstallEvidenceResult
runEvidence evidenceReads details = evidenceReads details >>= maybe (fail "expected a public evidence subject") (\(InstallEvidenceRead action) -> action)

permanentFailure, transientFailure :: InstallEvidenceResult -> Bool
permanentFailure = \case
    InstallReadFailed (ReadFault WontResolve _ _) -> True
    _ -> False
transientFailure = \case
    InstallReadFailed (ReadFault (WillResolve _) _ _) -> True
    _ -> False
