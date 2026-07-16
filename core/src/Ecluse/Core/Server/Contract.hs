-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ExistentialQuantification #-}

{- | The response contract algebra: the one description of what a route answers with,
folded three ways.

A route declares the closed set of 'Outcome's it can emit; each outcome pairs a status
and a documentation string with a 'BodySchema', the structural shape of its body. The
handler answers only by building an 'Answer' /through/ a declared outcome, so the set of
statuses and bodies a route can emit is, by construction, the set the capability manifest
documents. There is no second enumeration to keep in step.

The body vocabulary is __structural__, not a list of named documents: a body is nothing,
opaque relayed bytes (an artifact stream, documented as a media type), or a JSON document
whose @autodocodec@ 'JSONCodec' is the single source of truth. Core encodes the wire form
from that codec; the manifest tier renders the /same/ codec to an OpenAPI schema (via
@autodocodec-openapi3@), so the served body and its documented schema cannot diverge. The
codec never carries @openapi3@ into the proxy: @autodocodec@ alone has no such dependency.
-}
module Ecluse.Core.Server.Contract (
    -- * The documented body shape
    BodySchema (..),

    -- * A declared, emittable response
    Outcome (..),
    SomeOutcome (..),
    outcomeStatus,
    outcomeSchema,

    -- * The concrete answer a handler emits
    Answer (..),
    Body (..),
    answerWith,
    emptyAnswer,
    opaqueAnswer,

    -- * Rendering a JSON body to bytes
    encodeBody,
) where

import Autodocodec (JSONCodec, toJSONVia)
import Data.Aeson qualified as Aeson
import Network.HTTP.Types (Header, Status)

{- | The structural shape of a response body, for documentation. A closed, universal
vocabulary: every ecosystem's bodies are one of these three, so this never grows with
adapters.
-}
data BodySchema
    = -- | No body at all.
      SchemaEmpty
    | -- | Opaque relayed\/streamed bytes (an artifact), documented only by its media type.
      SchemaOpaque ByteString
    | -- | A JSON document whose codec is the source of truth for wire and schema alike.
      forall a. SchemaJson (JSONCodec a)

{- | A declared response outcome, typed by its body payload. Its 'ocStatus' and
'ocSchema' are what the manifest documents; the same value is how the handler answers
('answerWith'), so a route cannot emit a status or body it does not declare.
-}
data Outcome a = Outcome
    { ocStatus :: Status
    -- ^ The HTTP status this outcome answers with.
    , ocDoc :: Text
    -- ^ What this outcome means, in the route's own terms (the OpenAPI response description).
    , ocBody :: OutcomeBody a
    -- ^ The body this outcome carries.
    }

{- | How an outcome's body is produced: a typed codec (the payload is a value the handler
supplies), an opaque stream (the handler supplies the bytes\/source), or nothing.
-}
data OutcomeBody a
    = -- | A JSON body: the handler supplies a value of type @a@, encoded through the codec.
      JsonOutcome (JSONCodec a)
    | -- | An opaque body of the given media type; @a@ is unconstrained (the payload is bytes).
      OpaqueOutcome ByteString
    | -- | No body.
      EmptyOutcome

{- | A route's declared outcome with its body payload type erased, so a route can carry a
heterogeneous @['SomeOutcome']@ for the manifest to fold.
-}
data SomeOutcome = forall a. SomeOutcome (Outcome a)

-- | The status a declared outcome answers with.
outcomeStatus :: SomeOutcome -> Status
outcomeStatus (SomeOutcome o) = ocStatus o

-- | The documented body shape of a declared outcome.
outcomeSchema :: SomeOutcome -> BodySchema
outcomeSchema (SomeOutcome o) = case ocBody o of
    JsonOutcome c -> SchemaJson c
    OpaqueOutcome m -> SchemaOpaque m
    EmptyOutcome -> SchemaEmpty

{- | A concrete response a handler emits: a status, headers, and a body. Built only
through a declared 'Outcome' (see 'answerWith'), so what the server emits is what the
manifest documents.
-}
data Answer = Answer
    { answerStatus :: Status
    , answerHeaders :: [Header]
    , answerBody :: Body
    }

-- | The concrete body of an 'Answer': encoded JSON bytes, opaque bytes, or nothing.
data Body
    = -- | An already-encoded JSON body, tagged @application\/json@.
      JsonBody LByteString
    | -- | Opaque bytes of the given media type (a buffered artifact; streaming is a later leg).
      OpaqueBody ByteString LByteString
    | -- | No body.
      NoBody

{- | Answer through a declared JSON outcome: encode the payload with the outcome's codec.
The payload type is tied to the outcome's codec, so the emitted body is the documented
one.
-}
answerWith :: [Header] -> Outcome a -> a -> Answer
answerWith headers o value =
    Answer (ocStatus o) headers $ case ocBody o of
        JsonOutcome c -> JsonBody (encodeBody c value)
        OpaqueOutcome m -> OpaqueBody m mempty
        EmptyOutcome -> NoBody

-- | Answer through a declared empty outcome (no body).
emptyAnswer :: [Header] -> Outcome a -> Answer
emptyAnswer headers o = Answer (ocStatus o) headers NoBody

-- | Answer through a declared opaque outcome with the given already-buffered bytes.
opaqueAnswer :: [Header] -> Outcome a -> ByteString -> LByteString -> Answer
opaqueAnswer headers o media bytes = Answer (ocStatus o) headers (OpaqueBody media bytes)

-- | Encode a JSON value to bytes through its @autodocodec@ codec.
encodeBody :: JSONCodec a -> a -> LByteString
encodeBody c = Aeson.encode . toJSONVia c
