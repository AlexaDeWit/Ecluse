-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Both CodeArtifact formats execute the full laws and reject incomplete registration.
module Ecluse.Runtime.Test.PackageStoreSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import System.Exit (ExitCode (ExitFailure))
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec

import Ecluse.Runtime.Test.PackageStore
import Ecluse.Test.Contract.Completion (validateCompletion)
import Ecluse.Test.Contract.PackageStore
import Ecluse.Test.Contract.Runner (validateDeclaredGroups)
import Ecluse.Test.Contract.Types

-- | No AWS service, credentials, IAM, or provisioning participates in these unit laws.
spec :: Spec
spec = do
    packageStoreSpec codeArtifactDriver
    packageStoreSpec codeArtifactPyPIDriver
    describe "CodeArtifact format completeness" $ do
        it "discovers both formats without acquiring either provider" $ do
            let discoveryOnly driver = driver{psdWithSession = \_ -> fail "discovery acquired a provider"}
            validateDeclaredGroups
                codeArtifactCatalogue
                "package-store-codeartifact"
                (codeArtifactSuiteGroups (fullPackageStoreContract (discoveryOnly codeArtifactDriver) <> fullPackageStoreContract (discoveryOnly codeArtifactPyPIDriver)))
            validateCodeArtifact
        it "rejects the fixed suite when the PyPI registration is missing" $
            validateDeclaredGroups
                codeArtifactCatalogue
                "package-store-codeartifact"
                (codeArtifactSuiteGroups (fullPackageStoreContract codeArtifactDriver))
                `shouldThrow` (== ExitFailure 1)
        it "rejects completion when the PyPI profile and its groups disappear" $ do
            let withoutPyPI =
                    codeArtifactCatalogue
                        { catalogueProductionEcosystems = ["npm"]
                        , catalogueProfiles = filter ((/= "package-store/codeartifact-pypi") . profileId) (catalogueProfiles codeArtifactCatalogue)
                        , catalogueObligations = filter ((/= "package-store/codeartifact-pypi") . obligationProfile) (catalogueObligations codeArtifactCatalogue)
                        , catalogueGroups = filter ((/= "package-store/codeartifact-pypi") . groupProfileId) (catalogueGroups codeArtifactCatalogue)
                        }
            validateCompletion (packageStoreCompletion "codeartifact-pypi") [withoutPyPI] `shouldSatisfy` isLeft
        it "keeps both format profiles under one backend and one fixed job" $ do
            map profileBackends (catalogueProfiles codeArtifactCatalogue) `shouldBe` [["codeartifact"], ["codeartifact"]]
            ordNub (map groupPartition (catalogueGroups codeArtifactCatalogue)) `shouldBe` ["package-store-codeartifact"]
    for_ [("npm", codeArtifactDriver), ("PyPI", codeArtifactPyPIDriver)] $ \(ecosystem, driver) ->
        describe (ecosystem <> " integrity negative controls") $
            for_ corruptions $ \(label, corrupt) ->
                it label (integrityNegativeControl driver corrupt)
    describe "PyPI multi-file integrity" $
        it "rejects a read that loses two files" $
            integrityNegativeControl codeArtifactPyPIDriver (\release -> release{psrArtifacts = NE.head (psrArtifacts release) :| []})

integrityNegativeControl :: FullPackageStoreDriver -> (PackageStoreRelease -> PackageStoreRelease) -> Expectation
integrityNegativeControl driver corrupt = psdWithSession driver $ \session -> do
    reached <- newIORef False
    let original = pssOperations session
        altered =
            original
                { psoRead = \version -> do
                    result <- psoRead original version
                    case result of
                        Right (Just release) -> do
                            writeIORef reached True
                            pure (Right (Just (corrupt release)))
                        _ -> pure result
                }
    packageStoreIntegrityLaw driver session{pssOperations = altered}
        `shouldThrow` (const True :: HUnitFailure -> Bool)
    readIORef reached `shouldReturn` True

corruptions :: [(String, PackageStoreRelease -> PackageStoreRelease)]
corruptions =
    [ ("rejects altered stored bytes", changeFirst (\artifact -> artifact{psaBytes = "changed bytes"}))
    , ("rejects an altered stored digest", changeFirst (\artifact -> artifact{psaDigest = "changed digest"}))
    , ("rejects altered consumer metadata", \release -> release{psrMetadata = Map.map (const ">=99") (psrMetadata release)})
    ]

changeFirst :: (PackageStoreArtifact -> PackageStoreArtifact) -> PackageStoreRelease -> PackageStoreRelease
changeFirst change release = case psrArtifacts release of
    artifact :| rest -> release{psrArtifacts = change artifact :| rest}
