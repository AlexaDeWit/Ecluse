-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fresh-client refusal with independent proof that the public source answered nothing.
module Ecluse.Test.Contract.Install.Outage (
    OutageDriver (..),
    outageRefuses,
) where

import Test.Hspec (Expectation, shouldBe, shouldSatisfy)

import Ecluse.Test.Contract.Install (ClientResult, shouldFail)

-- | An isolated faulted invocation and the stub's observed closed and answered requests.
data OutageDriver = OutageDriver
    { outageRun :: IO (ClientResult, [Text], [Text])
    , outageExpectedClosed :: Text
    }

-- | Client failure must coincide with the required attempted read and no public response.
outageRefuses :: OutageDriver -> Expectation
outageRefuses driver = do
    (result, closed, answered) <- outageRun driver
    void (shouldFail result)
    closed `shouldSatisfy` elem (outageExpectedClosed driver)
    answered `shouldBe` []
