-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | CodeArtifact's presentation table, row by row.
module Ecluse.Runtime.Credential.CodeArtifact.PresentationSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Registry.Request (AuthMechanism (AuthBasic, AuthBearer, AuthRawKey), StoreOperation (StoreRead, StoreWrite))
import Ecluse.Runtime.Credential.CodeArtifact.Presentation (codeArtifactAuthMechanism)

spec :: Spec
spec =
    describe "codeArtifactAuthMechanism" $
        -- The rows follow AWS's client configuration for each format: npm's _authToken, pip's
        -- aws:token index URL, Cargo's token-from-stdout provider, and the RubyGems guide's
        -- aws:token source beside a "Bearer" gem push key.
        it "declares each ecosystem's read and write mechanism" $
            [(eco, operation, codeArtifactAuthMechanism eco operation) | eco <- [Npm, PyPI, Cargo, RubyGems], operation <- [StoreRead, StoreWrite]]
                `shouldBe` [ (Npm, StoreRead, AuthBearer)
                           , (Npm, StoreWrite, AuthBearer)
                           , (PyPI, StoreRead, AuthBasic)
                           , (PyPI, StoreWrite, AuthBasic)
                           , (Cargo, StoreRead, AuthRawKey)
                           , (Cargo, StoreWrite, AuthRawKey)
                           , (RubyGems, StoreRead, AuthBasic)
                           , (RubyGems, StoreWrite, AuthBearer)
                           ]
