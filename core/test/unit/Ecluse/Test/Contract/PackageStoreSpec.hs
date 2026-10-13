-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Verdaccio's production adapter and failure controls for the full persistence composition.
module Ecluse.Test.Contract.PackageStoreSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Test.Contract.Completion (Completion (completionEvidence), LawEvidence (evidenceFamily), validateCompletion)
import Ecluse.Test.Contract.PackageStore
import Ecluse.Test.Contract.PackageStore.Protocol (verdaccioDriver)
import Ecluse.Test.Contract.Runner (DeclaredGroup (declaredGroup), validateDeclaredGroups)
import Ecluse.Test.Contract.Types

-- | Resources are acquired by each canonical example, never during declaration inspection.
spec :: Spec
spec = do
    packageStoreSpec verdaccioDriver
    describe "full package-store completeness" $ do
        it "keeps the npm catalogue wrapper and existing case IDs" $ do
            packageStoreCatalogueFor "npm" "verdaccio" "package-store-verdaccio" "verdaccio" `shouldBe` catalogue
            map caseId (concatMap groupCases (catalogueGroups catalogue))
                `shouldSatisfy` all ("package-store/verdaccio/" `T.isPrefixOf`)
        it "keeps ecosystem, backend and partition independent of the profile name" $ do
            let pypi = packageStoreCatalogueFor "pypi" "codeartifact" "package-store-codeartifact" "codeartifact-pypi"
            catalogueProductionEcosystems pypi `shouldBe` ["pypi"]
            map profileEcosystems (catalogueProfiles pypi) `shouldBe` [["pypi"]]
            map profileBackends (catalogueProfiles pypi) `shouldBe` [["codeartifact"]]
            ordNub (map groupPartition (catalogueGroups pypi)) `shouldBe` ["package-store-codeartifact"]
        it "discovers all actual laws without acquiring a provider" $ do
            let discoveryOnly = verdaccioDriver{psdWithSession = \_ -> fail "discovery acquired a provider"}
            validateDeclaredGroups catalogue "package-store-verdaccio" (fullPackageStoreContract discoveryOnly)
            validateCompletion completion [catalogue] `shouldBe` Right completion
        it "rejects a family omitted from both registration and evidence" $
            for_ (map (groupContractId . declaredGroup) (fullPackageStoreContract verdaccioDriver)) $ \family -> do
                let withoutFamily =
                        catalogue
                            { catalogueGroups = filter ((/= family) . groupContractId) (catalogueGroups catalogue)
                            , catalogueObligations = filter ((/= family) . obligationContract) (catalogueObligations catalogue)
                            }
                    withoutEvidence = completion{completionEvidence = filter ((/= family) . evidenceFamily) (completionEvidence completion)}
                validateCompletion withoutEvidence [withoutFamily] `shouldSatisfy` isLeft
  where
    catalogue = packageStoreCatalogue "verdaccio"
    completion = packageStoreCompletion "verdaccio"
