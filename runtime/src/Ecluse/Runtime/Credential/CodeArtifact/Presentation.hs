-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | How CodeArtifact asks each ecosystem's client to present a token, per operation. The mirror
worker and a static publication token render through it, so neither composition nor an adapter
names the store.
-}
module Ecluse.Runtime.Credential.CodeArtifact.Presentation (
    codeArtifactAuthMechanism,
) where

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Registry.Request (AuthMechanism (AuthBasic, AuthBearer, AuthRawKey), StoreOperation (StoreRead, StoreWrite))

{- | The mechanism CodeArtifact's client configuration for each package format uses. A Basic row
renders the credential's own username, which the store sets to @aws@.
-}
codeArtifactAuthMechanism :: Ecosystem -> StoreOperation -> AuthMechanism
codeArtifactAuthMechanism = \case
    Npm -> const AuthBearer
    PyPI -> const AuthBasic
    Cargo -> const AuthRawKey
    RubyGems -> \case
        StoreRead -> AuthBasic
        StoreWrite -> AuthBearer
