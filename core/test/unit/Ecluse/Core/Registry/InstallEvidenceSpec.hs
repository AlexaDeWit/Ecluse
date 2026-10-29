-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Evidence budgets account for absolute regions and bound the actual artifact transport.
module Ecluse.Core.Registry.InstallEvidenceSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status404, status503)
import Test.Hspec

import Ecluse.Core.Fault (RetryAfter (RetryAfter), TransportCause (..), transportFault)
import Ecluse.Core.Registry (FetchFault (..), UrlFormationError (EmptyBaseUrl))
import Ecluse.Core.Registry.InstallEvidence
import Ecluse.Core.Registry.Public (ReadControl (Controlled))
import Ecluse.Core.Registry.Request (parseRequestEither)
import Ecluse.Core.Rules.Effectful (ReadFault (ReadFault))
import Ecluse.Core.Rules.Install (InstallEvidenceBudget (withInstallEvidenceBudget), InstallEvidenceBudgetExceeded (InstallEvidenceBudgetExceeded))
import Ecluse.Core.Rules.Types (Inability (EvaluationFailed, InstallEvidenceFailed), Transience (..))
import Ecluse.Core.Security (Limits (maxMetadataBytes, maxMirrorArtifactBytes), defaultLimits)
import Ecluse.Core.Telemetry.Upstream (ExchangeOutcome (ResponseStatus), PublicReadKind (Artifact))
import Ecluse.Test.Deadline (withExchangeClient, withSweeper)
import Ecluse.Test.Registry.Public (PublicFixture (..), newPublicFixture)
import Ecluse.Test.Stub (allCaptured, stubBaseUrl, withStub, withStubHeaders)

spec :: Spec
spec = do
    describe "maintenance evidence budget" $ do
        it "accepts the exact combined capacity and releases room when an absolute region decreases" $
            withSweeper $ \deadlines -> do
                budget <- newInstallEvidenceBudget deadlines defaultLimits{maxMetadataBytes = 10}
                withInstallEvidenceBudget budget (\bytes inspection -> bytes 6 >> inspection 4 >> bytes 3 >> inspection 7 >> bytes 0 >> inspection 10)
                    `shouldReturn` Just ()
                withInstallEvidenceBudget budget (\bytes inspection -> bytes 6 >> inspection 5)
                    `shouldThrow` (\InstallEvidenceBudgetExceeded -> True)
                withInstallEvidenceBudget budget (\bytes inspection -> bytes 10 >> inspection 0)
                    `shouldReturn` Just ()
        it "clamps negative regions to zero without creating room beyond capacity" $
            withSweeper $ \deadlines -> do
                budget <- newInstallEvidenceBudget deadlines defaultLimits{maxMetadataBytes = 10}
                withInstallEvidenceBudget budget (\bytes inspection -> bytes (-4) >> inspection 10 >> bytes 1)
                    `shouldThrow` (\InstallEvidenceBudgetExceeded -> True)
                withInstallEvidenceBudget budget (\bytes inspection -> inspection (-4) >> bytes 10 >> inspection 1)
                    `shouldThrow` (\InstallEvidenceBudgetExceeded -> True)
                withInstallEvidenceBudget budget (\bytes inspection -> bytes 5 >> inspection 5)
                    `shouldReturn` Just ()
        it "keeps a one-unit envelope when the configured metadata capacity is nonpositive" $
            withSweeper $ \deadlines ->
                for_ [0, -1] $ \capacity -> do
                    budget <- newInstallEvidenceBudget deadlines defaultLimits{maxMetadataBytes = capacity}
                    withInstallEvidenceBudget budget (\bytes inspection -> bytes 1 >> inspection 0) `shouldReturn` Just ()
                    withInstallEvidenceBudget budget (\bytes inspection -> bytes 1 >> inspection 1)
                        `shouldThrow` (\InstallEvidenceBudgetExceeded -> True)
    describe "bounded evidence transport" $ do
        it "reads exact-cap bytes and refuses one extra byte through controlled transport" $
            for_ [("four", Right "four"), ("five!", Left (evidenceFault WontResolve "BodyTooLarge (MirrorArtifactBodyLimit 4)"))] $ \(body, expected) ->
                withStub status200 body $ \stub -> withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                    fixture <- newPublicFixture
                    paid <- newIORef ([] :: [Int])
                    let limits = defaultLimits{maxMirrorArtifactBytes = 4}
                    readInstallArtifact limits client (Controlled (pfControl fixture) Artifact) (parseRequestEither (stubBaseUrl stub <> "/archive")) (\cost -> modifyIORef' paid (cost :))
                        `shouldReturn` expected
                    length <$> allCaptured stub `shouldReturn` 1
                    pfRequests fixture `shouldReturn` [("127.0.0.1", Artifact, ResponseStatus 200)]
                    when (isRight expected) (readIORef paid >>= (`shouldSatisfy` any (> 0)))
        it "classifies permanent and retryable HTTP statuses without accepting error bodies" $
            for_ [(status404, WontResolve, "archive source answered HTTP 404"), (status503, WillResolve Nothing, "archive source answered HTTP 503")] $ \(status, transience, detail) ->
                withStub status "untrusted error body" $ \stub -> withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                    fixture <- newPublicFixture
                    readInstallArtifact defaultLimits client (Controlled (pfControl fixture) Artifact) (parseRequestEither (stubBaseUrl stub)) (const pass)
                        `shouldReturn` Left (evidenceFault transience detail)
                    length <$> allCaptured stub `shouldReturn` 1
        it "refuses an unformable URL before making any request" $
            withStub status200 "never read" $ \stub -> withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                fixture <- newPublicFixture
                readInstallArtifact defaultLimits client (Controlled (pfControl fixture) Artifact) (Left EmptyBaseUrl) (const pass)
                    `shouldReturn` Left (evidenceFault WontResolve "EmptyBaseUrl")
                allCaptured stub `shouldReturn` []
        it "preserves the exact cooldown delay and suppresses the second GET" $
            withStubHeaders status503 [("Retry-After", "30")] "busy" $ \stub -> withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                fixture <- newPublicFixture
                let readArtifact = readInstallArtifact defaultLimits client (Controlled (pfControl fixture) Artifact) (parseRequestEither (stubBaseUrl stub)) (const pass)
                replicateM_ 2 (readArtifact `shouldReturn` Left (evidenceFault (WillResolve (Just (RetryAfter 30))) "archive source is cooling down"))
                length <$> allCaptured stub `shouldReturn` 1
    describe "install fetch faults" $ do
        it "retains typed retryability and bounds transport diagnostics" $
            for_ [(TransportTimeout, WillResolve Nothing), (TransportUnreachable, WillResolve Nothing), (TransportTls, WontResolve), (TransportProtocol, WontResolve)] $ \(cause, transience) ->
                installFetchFault (FetchTransport (transportFault cause (T.replicate 600 "x")))
                    `shouldBe` evidenceFault transience (T.replicate 512 "x")
        it "preserves supplied retry advice" $
            installFetchFault (FetchCooldown (RetryAfter 17))
                `shouldBe` evidenceFault (WillResolve (Just (RetryAfter 17))) "archive source is cooling down"

evidenceFault :: Transience -> Text -> ReadFault
evidenceFault transience detail = ReadFault transience EvaluationFailed (InstallEvidenceFailed detail)
