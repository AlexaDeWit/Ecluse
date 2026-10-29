-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Ruby read credentials and publication keys use distinct native presentations.
module Ecluse.Core.Registry.RubyGems.Credential (
    rubyReadCredential,
    rubyPublishCredential,
    rubyCodeArtifactPublishCredential,
    rubyPublishExtras,
    rubyOtpHeaders,
) where

import Data.ByteString qualified as BS
import Network.HTTP.Types.Header (RequestHeaders, hAuthorization)

import Ecluse.Core.Credential (ClientCredential (..), mkSecret, unSecret)
import Ecluse.Core.Registry (ParseError (ParseError), PublishBodyFault (PublishMalformed))
import Ecluse.Core.Registry.Adapter.Capability (PublishExtras (..))
import Ecluse.Core.Registry.PyPI.Credential (pypiCredential)
import Ecluse.Core.Registry.Request (CredentialMapping, bareAuthorization, credentialMapping, credentialRecover)

-- | Preserve the caller's complete Basic pair for authorised private reads.
rubyReadCredential :: CredentialMapping
rubyReadCredential = pypiCredential

-- | A publication key is the complete native header value, including any configured scheme.
rubyPublishCredential :: CredentialMapping
rubyPublishCredential = credentialMapping bareAuthorization hAuthorization (encodeUtf8 . unSecret . credSecret)

-- | A declared CodeArtifact writer presents its minted service token as Bearer.
rubyCodeArtifactPublishCredential :: CredentialMapping
rubyCodeArtifactPublishCredential = credentialMapping (credentialRecover rubyPublishCredential) hAuthorization (("Bearer " <>) . encodeUtf8 . unSecret . credSecret)

-- | Reject duplicate or malformed OTP headers before publication body allocation.
rubyPublishExtras :: RequestHeaders -> Either PublishBodyFault PublishExtras
rubyPublishExtras headers = case [value | (name, value) <- headers, name == "OTP"] of
    [] -> Right NoPublishExtras
    [value]
        | not (BS.null value)
        , BS.length value <= 1024
        , BS.all (\byte -> byte >= 33 && byte <= 126) value ->
            Right (PublishOneTimePassword (mkSecret (decodeUtf8 value)))
    _ -> Left (PublishMalformed (ParseError "the publication OTP header is duplicate, empty, malformed or exceeds its byte bound"))

-- | Only the checked native OTP header can accompany a caller's publication.
rubyOtpHeaders :: PublishExtras -> RequestHeaders
rubyOtpHeaders NoPublishExtras = []
rubyOtpHeaders (PublishOneTimePassword otp) = [("OTP", encodeUtf8 (unSecret otp))]
