-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | PyPI's credential presentation: the HTTP Basic pair a Python client sends on
@Authorization@, recovered here and attached under the same scheme going upstream.

The username is a client-side convention rather than an identity the index checks, so the
recovery admits any username and the edge gate compares the password half alone. A credential
Écluse holds rather than receives carries no username and travels under @__token__@, the name
PyPI's own tooling writes for a token. A pair a client sent travels verbatim, because rewriting
a username would authenticate as somebody else.
-}
module Ecluse.Core.Registry.PyPI.Credential (
    pypiCredential,
) where

import Data.ByteArray.Encoding (Base (Base64), convertFromBase, convertToBase)
import Data.Text qualified as T
import Network.HTTP.Types.Header (RequestHeaders, hAuthorization)

import Ecluse.Core.Credential (ClientCredential (ClientCredential, credSecret, credUsername), mkSecret, unSecret)
import Ecluse.Core.Registry.Request (CredentialMapping, authorizationUnder, credentialMapping)

-- | PyPI's credential mapping: recover the client's Basic pair, re-present it upstream.
pypiCredential :: CredentialMapping
pypiCredential = credentialMapping recoverBasic hAuthorization renderBasic

-- The password half may itself carry a colon, so the split takes the first one. Another scheme,
-- undecodable base64, no colon, an empty password, or no header yields 'Nothing'.
recoverBasic :: RequestHeaders -> Maybe ClientCredential
recoverBasic headers = do
    encoded <- authorizationUnder "basic" headers
    decoded <- decodeBase64 (encodeUtf8 encoded)
    let (username, afterUser) = T.break (== ':') (decodeUtf8 decoded)
    password <- T.stripPrefix ":" afterUser
    guard (not (T.null password))
    pure (ClientCredential (usernameGiven username) (mkSecret password))

renderBasic :: ClientCredential -> ByteString
renderBasic credential =
    "Basic " <> convertToBase Base64 (encodeUtf8 pair :: ByteString)
  where
    pair = fromMaybe tokenUsername (credUsername credential) <> ":" <> unSecret (credSecret credential)

tokenUsername :: Text
tokenUsername = "__token__"

-- An empty username is no username, which is how a client that names none presents itself.
usernameGiven :: Text -> Maybe Text
usernameGiven username = if T.null username then Nothing else Just username

decodeBase64 :: ByteString -> Maybe ByteString
decodeBase64 = rightToMaybe . convertFromBase Base64
