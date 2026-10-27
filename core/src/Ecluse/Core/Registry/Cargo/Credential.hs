-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo tokens use the complete Authorization value without a scheme prefix.
module Ecluse.Core.Registry.Cargo.Credential (cargoCredential) where

import Data.ByteString qualified as BS
import Network.HTTP.Types.Header (hAuthorization)

import Ecluse.Core.Credential (ClientCredential (credSecret), bareCredential, mkSecret, unSecret)
import Ecluse.Core.Registry.Request (CredentialMapping, credentialMapping)

-- | Preserve native token spelling when a caller retries through a credential provider.
cargoCredential :: CredentialMapping
cargoCredential = credentialMapping recover hAuthorization (encodeUtf8 . unSecret . credSecret)
  where
    recover headers = do
        (_, raw) <- find ((== hAuthorization) . fst) headers
        guard (not (BS.null raw))
        pure (bareCredential (mkSecret (decodeUtf8 raw)))
