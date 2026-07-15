-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's client-facing error surface: the denial-body renderer wired into an npm
mount.

The agnostic serve layer ("Ecluse.Core.Server.Response") decides the HTTP /status/ of a
refusal; the /body/ shape is npm's, and lives here. npm clients read the human-facing
reason from a JSON error object (preferring @message@, then @error@); \xc9cluse emits the
@error@ key, matching npm's own denial bodies. 'npmRenderer' is the 'MountRenderer' a
composition root binds to an npm mount, so the npm @{"error": \u2026}@ shape never leaks into
the ecosystem-neutral web layer.

The emitted body is the one named type 'NpmError', and its JSON key is 'npmErrorKey'.
The capability manifest ("Ecluse.Manifest") documents this same shape and consumes the
same 'npmErrorKey', so the documented schema and the wire body share the one key rather
than repeating the literal; a correspondence test ('Ecluse.ManifestSpec') holds an
emitted body against that documented schema, so the two cannot drift.

npm's /routes/ are its route table ("Ecluse.Core.Registry.Npm.Route"), which names this
renderer's responses through the agnostic 'Ecluse.Core.Server.Response.MountRenderer'
rather than importing it.
-}
module Ecluse.Core.Registry.Npm.Serve (
    npmRenderer,
    npmDenialBody,
    NpmError (..),
    npmErrorKey,
) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key

import Ecluse.Core.Server.Response (
    HelpMessage,
    MountRenderer (MountRenderer),
    RenderedBody (RenderedBody),
    appendHelp,
 )

{- | npm's client-facing error body: a JSON object carrying the human-facing reason
under a single @error@ string ('npmErrorKey'). The one definition of the shape npm
clients read a denial from; the capability manifest documents this exact shape, so
naming it here gives the wire body and its documented schema a shared point of truth.
-}
newtype NpmError = NpmError Text

instance ToJSON NpmError where
    toJSON (NpmError reason) = object [Key.fromText npmErrorKey .= reason]

{- | The JSON key an npm denial body carries its reason under. Exported so the
capability manifest documents the very key the wire emits, single-sourcing it across
the tier boundary rather than repeating the @"error"@ literal in the schema.
-}
npmErrorKey :: Text
npmErrorKey = "error"

{- | The npm mount renderer: every error body is npm's @{"error": \u2026}@ JSON object,
tagged @application\/json@.
-}
npmRenderer :: MountRenderer
npmRenderer =
    MountRenderer (\help message -> RenderedBody "application/json" (npmDenialBody help message))

{- | Render an npm denial body -- the 'NpmError' @{"error": \u2026}@ object whose @error@
string is the message with the operator help message, if any, appended. A blank or
absent help message is omitted rather than appended as empty text.

>>> npmDenialBody Nothing "denied because reasons"
"{\\"error\\":\\"denied because reasons\\"}"
-}
npmDenialBody :: Maybe HelpMessage -> Text -> LByteString
npmDenialBody help message =
    Aeson.encode (NpmError (appendHelp help message))
