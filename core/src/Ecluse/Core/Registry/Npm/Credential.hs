-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's credential presentation: the @Bearer@ token an npm client sends on
@Authorization@, recovered at the edge and attached under the same scheme upstream.

The npm CLI turns an @.npmrc@ @\/\/host\/:_authToken=...@ entry into that one header, so a
mount serving npm accepts and presents that form alone. One 'CredentialMapping' declares
both directions, so the recovered token and the header it travels on cannot drift apart.
-}
module Ecluse.Core.Registry.Npm.Credential (
    npmCredential,
) where

import Data.Text qualified as T
import Network.HTTP.Types.Header (RequestHeaders, hAuthorization)

import Ecluse.Core.Credential (ClientCredential (credSecret), bareCredential, mkSecret, unSecret)
import Ecluse.Core.Registry.Request (CredentialMapping, authorizationUnder, credentialMapping)

{- | npm's credential mapping: @Bearer@ over @Authorization@ in both directions. The npm
adapter registers it on 'Ecluse.Core.Registry.Adapter.Types.serveCredential'.
-}
npmCredential :: CredentialMapping
npmCredential = credentialMapping recoverBearer hAuthorization renderBearer

-- npm's scheme carries no username, so the recovered pair has none. Another scheme, a bare or
-- empty token, or no header yields 'Nothing'.
recoverBearer :: RequestHeaders -> Maybe ClientCredential
recoverBearer headers = do
    token <- authorizationUnder "bearer" headers
    guard (not (T.null token))
    pure (bareCredential (mkSecret token))

-- The @Authorization@ value carrying a credential under npm's @Bearer@ scheme, whose
-- grammar has no username slot to render.
renderBearer :: ClientCredential -> ByteString
renderBearer credential = "Bearer " <> encodeUtf8 (unSecret (credSecret credential))
