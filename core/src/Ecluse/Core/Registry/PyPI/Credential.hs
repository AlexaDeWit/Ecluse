-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Preserve the supplied HTTP Basic username and password when forwarding PyPI credentials.
module Ecluse.Core.Registry.PyPI.Credential (
    pypiCredential,
) where

import Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import Data.Text qualified as T
import Network.HTTP.Types.Header (RequestHeaders, hAuthorization)

import Ecluse.Core.Credential (ClientCredential (ClientCredential), mkSecret)
import Ecluse.Core.Registry.Request (AuthMechanism (AuthBasic), CredentialMapping, authorizationUnder, credentialMapping, renderAuthMechanism)

-- | PyPI's credential mapping: recover the client's Basic pair, re-present it upstream.
pypiCredential :: CredentialMapping
pypiCredential = credentialMapping recoverBasic hAuthorization (renderAuthMechanism AuthBasic)

-- The password half may itself carry a colon, so the split takes the first one. Another scheme,
-- undecodable base64, no colon, an empty password, or no header yields 'Nothing'.
recoverBasic :: RequestHeaders -> Maybe ClientCredential
recoverBasic headers = do
    encoded <- authorizationUnder "basic" headers
    decoded <- decodeBase64 (encodeUtf8 encoded)
    let (username, afterUser) = T.break (== ':') (decodeUtf8 decoded)
    password <- T.stripPrefix ":" afterUser
    guard (not (T.null password))
    pure (ClientCredential (Just username) (mkSecret password))

decodeBase64 :: ByteString -> Maybe ByteString
decodeBase64 = rightToMaybe . convertFromBase Base64
