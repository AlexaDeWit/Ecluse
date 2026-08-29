-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared __URL-path vocabulary__ of the front door: an artifact's on-the-wire
name, and the component-safety gate every ecosystem's router applies.

This is what is genuinely common to every registry's paths, and it is deliberately
small. A /route/ is not here. The npm path @\/{pkg}\/-\/{file}.tgz@ and the RubyGems
whole-registry path @\/versions@ have nothing in common but the fact that something must
be done about them. Each ecosystem therefore declares its own route type and its own
table ("Ecluse.Core.Registry.Npm.Route" is npm's).

What every ecosystem /does/ share is the threat. A decoded path component interpolated
into an upstream URL can carry a traversal, a separator, or a control character. That
defence is ecosystem-independent, so it lives here. Every router applies it
('isSafeComponent'), and the URL build re-applies it on the way out
('encodeComponent').
-}
module Ecluse.Core.Server.Path (
    -- * The artifact name
    Filename,
    mkFilename,
    unFilename,

    -- * Component safety
    isSafeComponent,
    encodeComponent,
) where

import Data.Char (isControl)
import Data.Text qualified as T
import Network.HTTP.Types.URI (urlEncode)

{- | An artifact's on-the-wire file name, verbatim and safe to interpolate: it cleared
'isSafeComponent'. The upstream path uses this exact name, never one rebuilt from the version.
-}
newtype Filename = Filename Text
    deriving stock (Eq, Show)

{- | Read a filename from untrusted text, 'Nothing' when it is not a safe path component. Every
boundary that admits one (a route capture, a queue payload) parses through this single gate.
-}
mkFilename :: Text -> Maybe Filename
mkFilename raw
    | isSafeComponent raw = Just (Filename raw)
    | otherwise = Nothing

-- | The verbatim name, for interpolation into an upstream URL through 'encodeComponent'.
unFilename :: Filename -> Text
unFilename (Filename name) = name

{- | Whether one decoded path component is safe to interpolate into an upstream URL: the
deny-by-default gate a classifier applies to every scope, base name, and tarball filename. The
component arrives percent-decoded, so a segment can carry a separator, a control character, or a
dot-dot, which would enable path traversal or request smuggling upstream.

The gate is structural only. It still admits a percent sign, a question mark, a hash, or a space,
which 'encodeComponent' neutralises when the URL is built. Safety rests on encode-on-build, not on
this gate alone.
-}
isSafeComponent :: Text -> Bool
isSafeComponent c =
    not (T.null c)
        && c /= "."
        && c /= ".."
        && T.all safeChar c
  where
    safeChar ch = ch /= '/' && ch /= '\\' && not (isControl ch)

{- | Percent-encode one decoded path component for safe interpolation into an upstream URL: the
encode-on-build partner of 'isSafeComponent'. It keeps only the RFC 3986 unreserved set verbatim
and encodes every other UTF-8 byte as @%XX@. The caller writes the structural delimiters itself.

Encoding is not idempotent. A literal percent sign always becomes @%25@, because the component is
decoded content, so a once-decoded @%2e%2e%2f@ cannot survive as a live escape.
-}
encodeComponent :: Text -> Text
-- 'urlEncode' in query-string mode (True), not the path mode http-types recommends: path mode
-- passes ':@&=+$,' through unencoded, which a component must not carry.
encodeComponent = decodeUtf8 . urlEncode True . encodeUtf8
