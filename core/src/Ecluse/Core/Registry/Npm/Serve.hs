-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's client-facing error body, as a codec.

The agnostic serve layer decides the HTTP /status/ of a refusal; the /body/ shape is
npm's, and it lives here as one 'NpmError' type with an @autodocodec@ codec. That codec
is the single source of truth: the serve path encodes the wire denial from it, and the
capability manifest renders the /same/ codec to the documented schema (in its own tier,
so @openapi3@ never reaches the proxy). npm clients read the human-facing reason from a
JSON @{"error": …}@ object, matching npm's own denial bodies.

There is no separate renderer Handle any more. A route declares the
'Ecluse.Core.Server.Contract.Outcome's it can emit (a status paired with a body codec),
the handler answers through one of them, and the emitted body is the documented body by
construction.
-}
module Ecluse.Core.Registry.Npm.Serve (
    NpmError (..),
    npmErrorCodec,
    npmErrorKey,
    npmError,
) where

import Autodocodec (HasCodec (codec), JSONCodec, object, requiredField, (.=))

import Ecluse.Core.Server.Response (HelpMessage, appendHelp)

{- | npm's client-facing error body: a JSON object carrying the human-facing reason under
a single @error@ string ('npmErrorKey'). One codec backs both the wire encoding and the
documented schema, so the served body and its documentation cannot diverge.
-}
newtype NpmError = NpmError {npmErrorReason :: Text}
    deriving stock (Eq, Show)

-- | The JSON key an npm denial body carries its reason under.
npmErrorKey :: Text
npmErrorKey = "error"

instance HasCodec NpmError where
    codec =
        object "NpmError" $
            NpmError <$> requiredField npmErrorKey "The human-facing reason the request was refused." .= npmErrorReason

-- | npm's error-body codec: the source of truth for its wire form and documented schema.
npmErrorCodec :: JSONCodec NpmError
npmErrorCodec = codec

{- | Build an npm error body from the human-facing reason and the operator help message,
appending the help (if any) as the serve path has always done.
-}
npmError :: Maybe HelpMessage -> Text -> NpmError
npmError help message = NpmError (appendHelp help message)
