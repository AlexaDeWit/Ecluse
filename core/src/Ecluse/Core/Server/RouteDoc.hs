-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What a route __does__: its summary, what it accepts, and every status it answers
with, as plain data.

Declared beside the pattern that routes it ("Ecluse.Core.Server.RoutePattern"), so a
route cannot be added without documenting it: the record will not construct. The
capability manifest interprets these values into an OpenAPI document and holds no
per-route knowledge of its own, so it has nothing to drift with. That is a stronger
guarantee than a total case over a route sum, which stays exhaustive only for as long as
someone keeps it so.

The types are deliberately __OpenAPI-free__. Naming the shape of a body ('BodyDoc') is a
core concern: it is what the serve path emits. Knowing that a JSON Schema exists is not,
and the @openapi3@ dependency tree must never reach the running proxy (see
@docs\/architecture\/api-surface.md@). So the core says /which/ body a route carries and
the manifest's interpreter maps that to a schema: a closed correspondence on both sides,
since a route cannot omit its documentation and a new body shape cannot go unrendered.
-}
module Ecluse.Core.Server.RouteDoc (
    RouteDoc (..),
    RequestDoc (..),
    ResponseDoc (..),
    BodyDoc (..),
) where

{- | What a route does: how it is summarised, what it accepts, and every status it can
answer with.

Declared beside the pattern that routes it, so adding a route means writing its
documentation in the same record, not remembering to update a table somewhere else.
-}
data RouteDoc = RouteDoc
    { rdSummary :: Text
    -- ^ A one-line summary (the OpenAPI operation summary).
    , rdDescription :: Text
    -- ^ The fuller prose description of what the route does.
    , rdRequest :: Maybe RequestDoc
    -- ^ The request body a write route accepts; 'Nothing' for a read.
    , rdResponses :: [ResponseDoc]
    -- ^ Every status this route can answer with, and the body each carries.
    }
    deriving stock (Eq, Show)

-- | The request body a route accepts.
data RequestDoc = RequestDoc
    { reqDescription :: Text
    -- ^ What the body is.
    , reqRequired :: Bool
    -- ^ Whether the request is rejected without it.
    , reqBody :: BodyDoc
    -- ^ Its shape.
    }
    deriving stock (Eq, Show)

-- | One documented response: a status, what it means, and the body it carries.
data ResponseDoc = ResponseDoc
    { respStatus :: Int
    -- ^ The HTTP status code.
    , respDescription :: Text
    -- ^ What this status means for this route, in the route's own terms.
    , respBody :: BodyDoc
    -- ^ The body shape carried at this status.
    }
    deriving stock (Eq, Show)

{- | The shape of a body Écluse emits or accepts: a __closed__ vocabulary, so the
manifest's interpreter is total over it and a new shape cannot go unrendered.

Naming the shape is a core concern (it is what the serve path emits); knowing what a
JSON Schema is is not. The manifest maps each of these to its @openapi3@ schema, and the
schemas for the documents Écluse owns are @autodocodec@ codecs that also back their
@aeson@ instances, so the documented schema and the wire format cannot diverge.
-}
data BodyDoc
    = -- | No body at all, or one the proxy relays verbatim without owning its shape.
      NoBody
    | -- | An empty JSON object, the whole of what a liveness probe answers with.
      EmptyObjectBody
    | -- | The client-facing error\/denial envelope.
      ErrorEnvelopeBody
    | -- | The merged-and-filtered package metadata document Écluse synthesises.
      PackumentBody
    | -- | Artifact bytes, streamed verbatim.
      ArtifactBody
    | -- | A first-party publish document.
      PublishDocumentBody
    deriving stock (Eq, Show)
