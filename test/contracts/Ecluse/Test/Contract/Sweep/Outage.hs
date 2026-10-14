-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Outage admission remains trusted until authorised grouped cleanup succeeds.
module Ecluse.Test.Contract.Sweep.Outage (admissionOutageScenario) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Install (shouldFail, shouldSucceed)
import Ecluse.Test.Contract.Sweep.Types
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

-- | Admit during a fault, recover the source, observe retained reads, then remove both copies.
admissionOutageScenario :: AdmissionOutageDriver plane store -> StoreOps store -> ClientOps store project -> SpecWith (plane, store, store)
admissionOutageScenario driver stores clients =
    it (caseLabel (outageCasePrefix driver <> "skipped-admission-recovery") "keeps outage-admitted copies until the recovered deny completes grouped deletion") $ \(plane, proxy, cache) -> do
        let name = packageName (outagePackage driver)
            version = packageVersion (outagePackage driver)
            trustedRead = do
                void $ clientInstall clients proxy name (Just version) >>= shouldSucceed
                (status, bytes) <- clientArtifact clients proxy name version
                status `shouldBe` 200
                LBS.length bytes `shouldSatisfy` (> 0)
        withLookupUnavailable driver proxy $ do
            void $ clientInstall clients proxy name (Just version) >>= shouldSucceed
            storeAwaitVersions stores proxy name [version] `shouldReturn` [version]
            void $ clientPublish clients cache name version >>= shouldSucceed
            logs <- admissionEvidence driver proxy
            T.count "admitted with a check skipped for unavailability" logs `shouldSatisfy` (>= 2)
            logs `shouldSatisfy` T.isInfixOf "DenyIfCve"
            logs `shouldSatisfy` T.isInfixOf "rule threw"
        awaitLookupRecovery driver proxy `shouldReturn` True
        trustedRead
        evidence <- admissionEvidence driver proxy
        refused <- runRecoveredSweep driver plane WithoutConsent
        sweepOutput refused `shouldSatisfy` T.isInfixOf "consent"
        storeHasVersion stores proxy name version `shouldReturn` True
        storeHasVersion stores cache name version `shouldReturn` True
        trustedRead
        afterRead <- admissionEvidence driver proxy
        T.count "admitted with a check skipped for unavailability" afterRead
            `shouldBe` T.count "admitted with a check skipped for unavailability" evidence
        swept <- runRecoveredSweep driver plane Apply
        (sweepExit swept, sweepOutput swept) `shouldSatisfy` ((== ExitSuccess) . fst)
        storeHasVersion stores proxy name version `shouldReturn` False
        storeHasVersion stores cache name version `shouldReturn` False
        void $ clientInstall clients proxy name (Just version) >>= shouldFail
        (status, _) <- clientArtifact clients proxy name version
        status `shouldBe` 403
