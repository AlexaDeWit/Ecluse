-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory admission and remediation observed through a real client and the served mount.
module Ecluse.Test.Contract.Install.Advisory (
    AdvisoryDriver (..),
    Denial (..),
    advisoryRefuses,
    advisoryAdmits,
) where

import Data.Text qualified as T
import Test.Hspec (Expectation, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Test.Contract.Install

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
