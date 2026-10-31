-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory admission and remediation observed through a real client and the served mount.
module Ecluse.Test.Contract.Install.Advisory (
    AdvisoryDriver (..),
    AdvisoryPolicy (..),
    AdvisoryCase (..),
    AdvisorySession (..),
    AdvisoryScenarioDriver (..),
    advisoryContract,
    Denial (..),
    parseDenial,
    advisoryRefuses,
) where

import Data.Aeson (Value, decodeStrict')
import Data.List (lookup)
import Data.Text qualified as T
import Test.Hspec (Expectation, SpecWith, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Test.Contract.Catalogue (caseLabel)

import Ecluse.Test.Contract.Install
import Ecluse.Test.Json (asObject, objectAt, textAt)

-- | Each policy selects its independently compiled source and expected coordinates.
data AdvisoryPolicy
    = -- | Deny the independently scored affected release by CVSS.
      DenyCve
    | -- | Admit the old affected release under the same CVSS deny: its advisory scores below the threshold.
      CvssBelowThreshold
    | -- | Deny the affected release by EPSS, from an advisory with no CVSS score.
      DenyEpss
    | -- | Admit the old affected release under the same EPSS deny: its advisory is severe but unlikely to be exploited.
      EpssBelowThreshold
    | -- | Admit the exact fix despite its quarantine age.
      Remediation
    | -- | Keep the exact fix quarantined when remediation is disabled.
      RemediationDisabled
    | -- | Admit a partial fix whose remaining advisory sits inside the shipped limits.
      PartialFix
    | -- | Keep the same partial fix quarantined when remediation abstains on remaining advisories.
      PartialFixAbstaining
    | -- | Let the configured source activate the MAL default without an explicit rule.
      KnownMaliciousDefault
    deriving stock (Eq, Show)

-- | Expected releases and evidence belong to the selected source, not the scored corpus.
data AdvisoryCase = AdvisoryCase
    { advisoryPackage :: Text
    , advisoryAffectedVersion :: Text
    , advisoryFixedVersion :: Text
    , advisoryExpectedId :: Text
    -- ^ The advisory an advisory denial names, when the case expects one.
    , advisoryRefusalMessage :: Text -> Text
    , advisoryRequiresMirror :: Bool
    }

-- | Native listing observations and fresh client operations for a release.
data AdvisorySession = AdvisorySession
    { advisoryListedVersions :: IO [Text]
    , advisoryRelease :: Text -> IO AdvisoryDriver
    , advisoryMirrorContains :: Maybe (Text -> IO Bool)
    }

-- | Independent advisory coordinates and native resource acquisition.
data AdvisoryScenarioDriver = AdvisoryScenarioDriver
    { advisoryWithPolicy :: AdvisoryPolicy -> (AdvisorySession -> IO ()) -> IO ()
    , advisoryCaseForPolicy :: AdvisoryPolicy -> AdvisoryCase
    }

-- | Require scored and MAL denials, and the exact-fix and partial-fix overrides beside their controls.
advisoryContract :: Text -> SpecWith AdvisoryScenarioDriver
advisoryContract prefix = do
    for_ [("deny-cve", DenyCve, "DenyIfCve", Just CvssBelowThreshold), ("deny-epss", DenyEpss, "DenyIfEpss", Just EpssBelowThreshold), ("mal-denial", KnownMaliciousDefault, "DenyIfKnownMalicious", Nothing)] $ \(suffix, policy, rule, control) ->
        it (caseLabel (prefix <> "/" <> suffix) ("denies an affected release by " <> rule)) $ \driver -> do
            advisoryWithPolicy driver policy (advisoryDenies (advisoryCaseForPolicy driver policy) rule)
            -- The score decides, not the advisory alone: below the threshold the release stays admitted.
            for_ control $ \below ->
                advisoryWithPolicy driver below $ \session ->
                    advisoryRelease session (advisoryAffectedVersion (advisoryCaseForPolicy driver below)) >>= advisoryAdmits
    it (caseLabel (prefix <> "/exact-fix") "admits the exact fix while the disabled control quarantines it") $ \driver -> do
        let fixture = advisoryCaseForPolicy driver Remediation
        advisoryWithPolicy driver Remediation $ \session ->
            advisoryRelease session (advisoryFixedVersion fixture) >>= advisoryAdmits
        advisoryWithPolicy driver RemediationDisabled $ \session -> do
            advisoryListedVersions session `shouldReturn` [advisoryAffectedVersion fixture]
            refuse fixture session (advisoryFixedVersion fixture) "BlockedByDefault" Nothing
    it (caseLabel (prefix <> "/partial-fix") "admits a partial fix inside the shipped limits while the abstaining control quarantines it") $ \driver -> do
        let fixture = advisoryCaseForPolicy driver PartialFix
            control = advisoryCaseForPolicy driver PartialFixAbstaining
        advisoryWithPolicy driver PartialFix $ \session ->
            advisoryRelease session (advisoryFixedVersion fixture) >>= advisoryAdmits
        advisoryWithPolicy driver PartialFixAbstaining $ \session -> do
            advisoryListedVersions session `shouldReturn` [advisoryAffectedVersion control]
            refuse control session (advisoryFixedVersion control) "BlockedByDefault" (Just (advisoryExpectedId control))

-- A denial retains the unaffected listing and cannot publish into a required mirror.
advisoryDenies :: AdvisoryCase -> Text -> AdvisorySession -> Expectation
advisoryDenies fixture rule session = do
    advisoryListedVersions session `shouldReturn` [advisoryFixedVersion fixture]
    refuse fixture session (advisoryAffectedVersion fixture) rule (Just (advisoryExpectedId fixture))
    when (advisoryRequiresMirror fixture) $
        case advisoryMirrorContains session of
            Nothing -> expectationFailure "the advisory case requires a real mirror observation"
            Just contains -> contains (advisoryAffectedVersion fixture) `shouldReturn` False

refuse :: AdvisoryCase -> AdvisorySession -> Text -> Text -> Maybe Text -> Expectation
refuse fixture session version rule advisory = do
    release <- advisoryRelease session version
    advisoryRefuses (advisoryRefusalMessage fixture version) (Denial (advisoryPackage fixture) version rule advisory) release

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
    { advisoryExpectedArtifact :: Text
    , advisoryAdvertisedArtifacts :: [(Text, Text)]
    , advisoryInstall :: IO (ClientResult, Bool)
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

-- Require the exact native artifact before installing the admitted remediation.
advisoryAdmits :: AdvisoryDriver -> Expectation
advisoryAdmits driver = do
    lookup (advisoryExpectedArtifact driver) (advisoryAdvertisedArtifacts driver) `shouldSatisfy` isJust
    installsObserved
        InstallDriver
            { installRun = advisoryInstall driver
            , installExpected = True
            }

-- | Read one structured public denial without discarding its coordinate or advisory identifiers.
parseDenial :: Text -> Maybe Denial
parseDenial line = do
    value <- decodeStrict' (encodeUtf8 line) :: Maybe Value
    let top = asObject value
        fields = objectAt "data" top
    guard (textAt "message" top == Just "denied")
    Denial <$> textAt "package" fields <*> textAt "version" fields <*> textAt "rule" fields <*> pure (textAt "cve" fields)
