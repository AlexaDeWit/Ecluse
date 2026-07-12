-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Denial rendering: how an error's /body/ is shaped, kept apart from what the
error's /status/ is.

The serve layer's status machinery is ecosystem-agnostic ("Ecluse.Core.Server.Response"
decides whether a request is a @403@, a @404@, a @503@), but the body a client reads an
error from is not: an npm client expects a JSON @{"error": …}@ object, a PyPI client a
different surface. This module holds the boundary between the two.

Each mount supplies a 'MountRenderer', chosen at the composition root alongside its path
grammar, so the agnostic web layer holds no body shape of its own and no ecosystem's
error format leaks into it.

The one part of denial rendering that /is/ ecosystem-neutral lives here too: an operator
help message, when configured, is appended to every denial the same way for every
ecosystem ('appendHelp'), so clients are always told where to ask.
-}
module Ecluse.Core.Server.Renderer (
    -- * The operator help message
    HelpMessage,
    mkHelpMessage,
    appendHelp,

    -- * The mount's error renderer
    RenderedBody (..),
    MountRenderer (..),
) where

import Data.Text qualified as T

{- | An operator-configured message appended to every denial -- typically where to
ask for help (e.g. a support channel). Stored trimmed of surrounding whitespace
so it joins the denial text with a single separating space and an all-blank value
contributes nothing.
-}
newtype HelpMessage = HelpMessage Text
    deriving stock (Eq, Show)

-- | Build a 'HelpMessage', trimming surrounding whitespace.
mkHelpMessage :: Text -> HelpMessage
mkHelpMessage = HelpMessage . T.strip

{- | Append a non-blank operator 'HelpMessage' to a denial message, separated by a
single space; a blank or absent help message contributes nothing.

This is the ecosystem-neutral part of denial rendering -- every ecosystem appends
the operator's help text the same way. How the joined text is then wrapped into
body bytes is the mount's 'MountRenderer'.
-}
appendHelp :: Maybe HelpMessage -> Text -> Text
appendHelp help message =
    case help of
        Just (HelpMessage h) | not (T.null h) -> T.strip message <> " " <> h
        _ -> message

{- | A rendered error body: its @Content-Type@ and the bytes.

The agnostic serve layer chooses the HTTP /status/; the body shape -- JSON, plain
text, HTML -- is the mount's, so a 'MountRenderer' returns this pair and the WAI
layer reads the content type off it rather than assuming one.
-}
data RenderedBody = RenderedBody
    { renderedContentType :: ByteString
    -- ^ The @Content-Type@ the body is tagged with (e.g. @application\/json@).
    , renderedBytes :: LByteString
    -- ^ The encoded error body.
    }
    deriving stock (Eq, Show)

{- | A mount's ecosystem-specific error renderer -- the Handle that keeps the npm
@{"error": …}@ shape (and any other ecosystem's) out of the agnostic web layer.

The status machinery in "Ecluse.Core.Server.Response" is ecosystem-agnostic, but the
body a client reads an error from is not: an npm client expects a JSON @{"error": …}@
object, a PyPI client a different surface. Each mount supplies a renderer, chosen at the
composition root alongside its path grammar, so the web layer holds no body shape
of its own. 'renderError' shapes a denial or meta-route error (a @403@\/@404@\/@501@
body) from the optional operator help message and the human-facing reason.
-}
newtype MountRenderer = MountRenderer
    { renderError :: Maybe HelpMessage -> Text -> RenderedBody
    }
