-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Missing production capabilities fail independently without erasing the required contract.
module Ecluse.Test.Contract.EcosystemSpec (spec) where

import Test.Hspec

import Ecluse.Test.Contract.Binding (allCaseIds)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Completion
import Ecluse.Test.Contract.Ecosystem
import Ecluse.Test.Contract.Fixtures qualified as Fixture
import Ecluse.Test.Contract.Phase (newPhaseRecorder)
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Runner
import Ecluse.Test.Contract.Types

spec :: Spec
spec = describe "canonical ecosystem composition" $ do
    it "discovers every missing law without running acquisition hooks" $ do
        let declarations = fullEcosystemContract unavailableDriver
        forM_ declarations $ \declaration -> do
            recorder <- newPhaseRecorder
            registered <- allCaseIds (declaredAction declaration recorder)
            registered `shouldMatchList` map caseId (groupCases $ declaredGroup declaration)
        validateDeclaredGroups (missingCatalogue declarations) "example" declarations
    it "constructs available canonical cases without acquiring their provider" $ do
        acquired <- newIORef False
        let metadata = nativeGroup{groupId = "example/installation", groupContractId = "example/installation"}
            fixture = FamilyFixture metadata $ \_ _ -> do
                writeIORef acquired True
                expectationFailure "discovery acquired the provider"
            driver = unavailableDriver{ecosystemInstallation = Available fixture}
        recorder <- newPhaseRecorder
        forM_ (fullEcosystemContract driver) $ \declaration -> do
            registered <- allCaseIds (declaredAction declaration recorder)
            registered `shouldMatchList` map caseId (groupCases $ declaredGroup declaration)
        readIORef acquired `shouldReturn` False
    it "registers missing capabilities as ordinary failed cases and continues through the remaining groups" $ do
        observed <- newIORef (0 :: Int)
        let native = DeclaredGroup nativeGroup $
                const $
                    it (caseLabel "example/native" "executes the final independent observation") $ do
                        modifyIORef' observed (+ 1)
                        readIORef observed `shouldReturn` 1
            declarations = fullEcosystemContract unavailableDriver{ecosystemNativeExtensions = [native]}
        report <- executeGroups Fixture.executionIdentity (RunSettings Nothing 1 Nothing) "example" declarations (const $ pure Nothing)
        readIORef observed `shouldReturn` 1
        let failures = filter ((/= "example/native") . crCaseId) (reportCases report)
        failures `shouldSatisfy` (not . null)
        map crStatus failures `shouldSatisfy` all (== Failed)
        map crStatus (filter ((== "example/native") . crCaseId) (reportCases report)) `shouldBe` [Passed]
    it "still requires runtime advisory evidence when production operations are unavailable" $ do
        let declarations = fullEcosystemContract unavailableDriver
            completion = ecosystemCompletion FullEcosystem unavailableDriver
        validateCompletion completion [missingCatalogue declarations] `shouldSatisfy` isLeft
    it "gives the read-only foundation a smaller explicit scope" $ do
        let full = fullEcosystemContract unavailableDriver
            foundation = readOnlyEcosystemFoundation unavailableDriver
        length foundation `shouldBe` 5
        length full `shouldBe` 8
        concatMap (map caseId . groupCases . declaredGroup) foundation `shouldSatisfy` notElem "example/mirror/dependency-graph"

unavailableDriver :: EcosystemDriver () () () () ()
unavailableDriver = EcosystemDriver "example" absent absent absent absent absent absent absent absent []
  where
    absent = Unavailable (MissingCapability "the production adapter has no operation")

missingCatalogue :: [DeclaredGroup] -> Catalogue
missingCatalogue declarations = Catalogue ["example"] (missingCapabilityProfiles "example" declarations) (missingCapabilityObligations declarations) (map declaredGroup declarations)

nativeGroup :: Group
nativeGroup = Group "example/native" "native" "example/native" E2E "example" [Case "example/native" ["example/native"]] [] [] [] []
