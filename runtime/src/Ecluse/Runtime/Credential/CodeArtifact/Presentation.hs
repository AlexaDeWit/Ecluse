-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | How CodeArtifact asks each ecosystem's client to present a token, per operation. The mirror
worker and a static publication token render through it, so neither composition nor an adapter
names the store.
-}
module Ecluse.Runtime.Credential.CodeArtifact.Presentation (
    codeArtifactAuthMechanism,
    codeArtifactUsername,
) where

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Registry.Request (AuthMechanism (AuthBasic, AuthBearer, AuthRawKey), StoreOperation (StoreRead, StoreWrite))

-- | The mechanism CodeArtifact's client configuration for each package format uses. Every Basic row carries the store's username.
codeArtifactAuthMechanism :: Ecosystem -> StoreOperation -> AuthMechanism
codeArtifactAuthMechanism = \case
    Npm -> const AuthBearer
    PyPI -> const (AuthBasic codeArtifactUsername)
    Cargo -> const AuthRawKey
    RubyGems -> \case
        StoreRead -> AuthBasic codeArtifactUsername
        StoreWrite -> AuthBearer

-- | The username CodeArtifact accepts beside a token on Basic. Configuration may name no other.
codeArtifactUsername :: Text
codeArtifactUsername = "aws"
