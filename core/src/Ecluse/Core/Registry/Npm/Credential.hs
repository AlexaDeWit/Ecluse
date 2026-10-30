-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's credential presentation: the @Bearer@ token the npm CLI sends on @Authorization@ for
an @.npmrc@ @\/\/host\/:_authToken=...@ entry, recovered at the edge and attached upstream.
-}
module Ecluse.Core.Registry.Npm.Credential (
    npmCredential,
) where

import Data.Text qualified as T
import Network.HTTP.Types.Header (RequestHeaders, hAuthorization)

import Ecluse.Core.Credential (ClientCredential, bareCredential, mkSecret)
import Ecluse.Core.Registry.Request (AuthMechanism (AuthBearer), CredentialMapping, authorizationUnder, credentialMapping, renderAuthMechanism)

{- | npm's credential mapping: @Bearer@ over @Authorization@ in both directions. The npm
adapter registers it on 'Ecluse.Core.Registry.Adapter.Types.serveCredential'.
-}
npmCredential :: CredentialMapping
npmCredential = credentialMapping recoverBearer hAuthorization (renderAuthMechanism AuthBearer)

-- npm's scheme carries no username, so the recovered pair has none. Another scheme, a bare or
-- empty token, or no header yields 'Nothing'.
recoverBearer :: RequestHeaders -> Maybe ClientCredential
recoverBearer headers = do
    token <- authorizationUnder "bearer" headers
    guard (not (T.null token))
    pure (bareCredential (mkSecret token))
