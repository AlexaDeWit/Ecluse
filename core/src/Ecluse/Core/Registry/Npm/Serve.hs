-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's client-facing error body, as a codec.

The agnostic serve layer decides a refusal's HTTP status. The body is npm's own
@{"error": ...}@ object, which its clients read the human-facing reason from. One
@autodocodec@ codec backs both the wire body and the OpenAPI schema, so the served denial
and its documentation cannot diverge. The manifest runs in its own tier, so @openapi3@
never reaches the proxy.
-}
module Ecluse.Core.Registry.Npm.Serve (
    NpmError (..),
    npmErrorCodec,
    npmError,
) where

import Autodocodec (HasCodec (codec), JSONCodec, object, requiredField, (.=))

import Ecluse.Core.Server.Response (HelpMessage, appendHelp)

-- | npm's error body: the human-facing reason under a single @error@ string ('npmErrorKey').
newtype NpmError = NpmError {npmErrorReason :: Text}
    deriving stock (Eq, Show)

-- The JSON key an npm denial body carries its reason under.
npmErrorKey :: Text
npmErrorKey = "error"

instance HasCodec NpmError where
    codec =
        object "NpmError" $
            NpmError <$> requiredField npmErrorKey "The human-facing reason the request was refused." .= npmErrorReason

-- | npm's error-body codec.
npmErrorCodec :: JSONCodec NpmError
npmErrorCodec = codec

-- | Build an npm error body, appending the operator help message when one is configured.
npmError :: Maybe HelpMessage -> Text -> NpmError
npmError help message = NpmError (appendHelp help message)
