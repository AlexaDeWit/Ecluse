-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Token preservation at the provider boundary, independent of mint and refresh policy.
module Ecluse.Test.Contract.Credential (credentialContract) where

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Test.Hspec

import Ecluse.Core.Credential (AuthToken (..), CredentialProvider (..), unSecret)

-- | The fixture supplies an independently known token that remains valid for both reads.
credentialContract :: Text -> AuthToken -> SpecWith CredentialProvider
credentialContract group expected = describe "credential contract" $
    it (caseLabel (group <> "/token") "preserves the configured secret and expiry on repeated reads") $ \provider -> do
        tokens <- replicateM 2 (currentToken provider)
        map (unSecret . authSecret) tokens `shouldBe` replicate 2 (unSecret (authSecret expected))
        map authExpiresAt tokens `shouldBe` replicate 2 (authExpiresAt expected)
        for_ tokens $ \token -> show token `shouldNotContain` toString (unSecret (authSecret expected))
