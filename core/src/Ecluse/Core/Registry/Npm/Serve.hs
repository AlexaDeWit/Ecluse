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

npm's /routes/ are its route table ("Ecluse.Core.Registry.Npm.Route"), which names this
renderer's responses through the agnostic 'Ecluse.Core.Server.Response.MountRenderer'
rather than importing it.
-}
module Ecluse.Core.Registry.Npm.Serve (
    npmRenderer,
    npmDenialBody,
) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson

import Ecluse.Core.Server.Response (
    HelpMessage,
    MountRenderer (MountRenderer),
    RenderedBody (RenderedBody),
    appendHelp,
 )

{- | The npm mount renderer: every error body is npm's @{"error": \u2026}@ JSON object,
tagged @application\/json@.
-}
npmRenderer :: MountRenderer
npmRenderer =
    MountRenderer (\help message -> RenderedBody "application/json" (npmDenialBody help message))

{- | Render an npm denial body -- the @{"error": \u2026}@ object whose @error@ string is
the message with the operator help message, if any, appended. A blank or absent
help message is omitted rather than appended as empty text.

>>> npmDenialBody Nothing "denied because reasons"
"{\\"error\\":\\"denied because reasons\\"}"
-}
npmDenialBody :: Maybe HelpMessage -> Text -> LByteString
npmDenialBody help message =
    Aeson.encode (object ["error" .= appendHelp help message])
