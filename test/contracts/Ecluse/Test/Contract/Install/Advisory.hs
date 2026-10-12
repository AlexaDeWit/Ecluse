-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory admission and remediation observed through a real client and the served mount.
module Ecluse.Test.Contract.Install.Advisory (
    AdvisoryDriver (..),
    AdvisoryPolicy (..),
    AdvisorySession (..),
    AdvisoryScenarioDriver (..),
    advisoryContract,
    Denial (..),
    advisoryRefuses,
    advisoryAdmits,
) where

import Data.Text qualified as T
import Test.Hspec (Expectation, SpecWith, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Test.Contract.Catalogue (caseLabel)

import Ecluse.Test.Contract.Install

-- | Policies exercised against the same compiled advisory generation.
data AdvisoryPolicy = DenyCve | DenyEpss | Remediation | RemediationDisabled
    deriving stock (Eq, Show)

-- | Native listing observations and fresh client operations for a release.
data AdvisorySession = AdvisorySession
    { advisoryListedVersions :: IO [Text]
    , advisoryRelease :: Text -> IO AdvisoryDriver
    }

-- | Independent advisory coordinates and native resource acquisition.
data AdvisoryScenarioDriver = AdvisoryScenarioDriver
    { advisoryWithPolicy :: AdvisoryPolicy -> (AdvisorySession -> IO ()) -> IO ()
    , advisoryPackage :: Text
    , advisoryAffectedVersion :: Text
    , advisoryFixedVersion :: Text
    , advisoryExpectedId :: Text
    , advisoryRefusalMessage :: Text -> Text
    }

-- | Require CVE and EPSS denial plus an exact-fix quarantine override and its negative control.
advisoryContract :: Text -> SpecWith AdvisoryScenarioDriver
advisoryContract prefix = do
    for_ [("deny-cve", DenyCve, "DenyIfCve"), ("deny-epss", DenyEpss, "DenyIfEpss")] $ \(suffix, policy, rule) ->
        it (caseLabel (prefix <> "/" <> suffix) ("denies an affected release by " <> toString rule)) $ \driver ->
            advisoryWithPolicy driver policy $ \session -> do
                advisoryListedVersions session `shouldReturn` [advisoryFixedVersion driver]
                refuse driver session (advisoryAffectedVersion driver) rule (Just (advisoryExpectedId driver))
    it (caseLabel (prefix <> "/exact-fix") "admits the exact fix while the disabled control quarantines it") $ \driver -> do
        advisoryWithPolicy driver Remediation $ \session ->
            advisoryRelease session (advisoryFixedVersion driver) >>= advisoryAdmits
        advisoryWithPolicy driver RemediationDisabled $ \session -> do
            advisoryListedVersions session `shouldReturn` [advisoryAffectedVersion driver]
            refuse driver session (advisoryFixedVersion driver) "BlockedByDefault" Nothing
  where
    refuse driver session version rule advisory = do
        release <- advisoryRelease session version
        advisoryRefuses (advisoryRefusalMessage driver version) (Denial (advisoryPackage driver) version rule advisory) release

-- | A parsed denial record, with coordinate and advisory ownership retained.
data Denial = Denial
    { denialPackage :: Text
    , denialVersion :: Text
    , denialRule :: Text
    , denialAdvisory :: Maybe Text
    }
    deriving stock (Eq, Show)

-- | Real client and mount observations for one requested release.
data AdvisoryDriver = AdvisoryDriver
    { advisoryInstall :: IO (ClientResult, Bool)
    , advisoryArtifactStatus :: IO Int
    , advisoryAwaitDenials :: Denial -> IO [Denial]
    }

-- | A blocked release leaves no installation, refuses direct reads and records the exact denial.
advisoryRefuses :: Text -> Denial -> AdvisoryDriver -> Expectation
advisoryRefuses nativeMessage expected driver = do
    (result, installed) <- advisoryInstall driver
    void (shouldFail result)
    (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf nativeMessage
    installed `shouldBe` False
    advisoryArtifactStatus driver `shouldReturn` 403
    observed <- advisoryAwaitDenials driver expected
    observed `shouldSatisfy` elem expected

-- | An admitted remediation installs the independently observed release.
advisoryAdmits :: AdvisoryDriver -> Expectation
advisoryAdmits driver =
    installsObserved
        InstallDriver
            { installRun = advisoryInstall driver
            , installExpected = True
            }
