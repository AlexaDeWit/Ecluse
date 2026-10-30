-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo tokens use the complete Authorization value without a scheme prefix.
module Ecluse.Core.Registry.Cargo.Credential (cargoCredential) where

import Network.HTTP.Types.Header (hAuthorization)

import Ecluse.Core.Credential (ClientCredential (credSecret), unSecret)
import Ecluse.Core.Registry.Request (CredentialMapping, bareAuthorization, credentialMapping)

-- | Preserve native token spelling when a caller retries through a credential provider.
cargoCredential :: CredentialMapping
cargoCredential = credentialMapping bareAuthorization hAuthorization (encodeUtf8 . unSecret . credSecret)
