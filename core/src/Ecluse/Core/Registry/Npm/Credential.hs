-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's credential presentation: the @Bearer@ token an npm client presents on
@Authorization@, recovered from the headers of a request it makes and attached under the
same scheme to a request Écluse makes upstream.

The npm CLI turns an @.npmrc@ @\/\/host\/:_authToken=…@ entry into
@Authorization: Bearer …@, so that is the one form a mount serving npm accepts at its
edge and the one form it presents to an npm upstream. Both directions are declared here,
as one 'CredentialMapping', so the token text recovered from a client and the header it
is carried on upstream cannot drift apart.
-}
module Ecluse.Core.Registry.Npm.Credential (
    npmCredential,
) where

import Data.Text qualified as T
import Network.HTTP.Types.Header (RequestHeaders, hAuthorization)

import Ecluse.Core.Credential (Secret, mkSecret, unSecret)
import Ecluse.Core.Registry.Request (CredentialMapping, credentialMapping)

{- | npm's credential mapping: @Bearer@ over @Authorization@ in both directions. npm's
adapter registers it on its serve slice ('Ecluse.Core.Registry.Adapter.Types.serveCredential'),
and npm's request layer attaches every outbound credential through it.
-}
npmCredential :: CredentialMapping
npmCredential = credentialMapping recoverBearer hAuthorization renderBearer

{- The client's bearer credential, recovered from the @Authorization: Bearer …@ header as
the token text alone. The scheme name is matched case-insensitively (npm sends @Bearer@)
and the token is taken verbatim after it. Any other presentation is 'Nothing': another
scheme, a bare token, an empty token, or no header at all. -}
recoverBearer :: RequestHeaders -> Maybe Secret
recoverBearer headers = do
    (_, raw) <- find ((== hAuthorization) . fst) headers
    let value = decodeUtf8 raw
        (scheme, rest) = T.break (== ' ') value
    guard (T.toLower scheme == "bearer")
    let token = T.dropWhile (== ' ') rest
    guard (not (T.null token))
    pure (mkSecret token)

-- The @Authorization@ value carrying a token under npm's @Bearer@ scheme.
renderBearer :: Secret -> ByteString
renderBearer secret = "Bearer " <> encodeUtf8 (unSecret secret)
