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
    Filename (..),

    -- * Component safety
    isSafeComponent,
    encodeComponent,
) where

import Data.Char (isControl)
import Data.Text qualified as T
import Network.HTTP.Types.URI (urlEncode)

{- | An artifact's on-the-wire file name: the agnostic artifact-name type an
ecosystem's artifact route carries, and the data-plane handler
('Ecluse.Core.Server.Pipeline.serveTarball') takes.

It is held as a distinct type, not a bare 'Text', because it is __authoritative for
fetching the bytes__. The proxy fetches an artifact at the upstream path built from this
exact name, never one reconstructed from @(package, version)@. A registry whose artifact
naming differs from the proxy's own convention therefore still resolves. The name is
preserved verbatim as received. The router that produces it has already applied the
component-safety gate ('isSafeComponent'), so the value is safe to interpolate into a
downstream URL.
-}
newtype Filename = Filename Text
    deriving stock (Eq, Show)

{- | Whether a single decoded path component is __safe to interpolate__ into a
downstream upstream URL. This is the deny-by-default gate a classifier applies to every
component it accepts: a scope, base name, or tarball filename.

The path is percent-decoded before it reaches us, so a single segment can carry a
@\'\/\'@, a @\'\\\\\'@, a control character, or be @"."@\/@".."@. Any of these
enables path traversal or request smuggling once the name reaches the upstream
URL. A component is UNSAFE iff it is empty, is exactly @"."@ or @".."@, or
contains a @\'\/\'@, a @\'\\\\\'@, or any 'isControl' character. Everything else
is accepted. This is a security boundary, __not__ an ecosystem-policy validator,
so ordinary names with interior dots (@lodash.merge@, @is.odd@), hyphens,
underscores, digits, or uppercase all pass.

It lives in the agnostic layer because the threat is ecosystem-independent:
interpolating a hostile segment into an upstream URL. Every ecosystem's router and
the defence-in-depth check in "Ecluse.Core.Security" share this one rule.

This gate is __structural__. It stops a component that would change the upstream
URL's /shape/: a traversal, an embedded separator, or a control character. It does
__not__ stop a component that carries other URL-reserved bytes, such as a @\'%\'@,
@\'?\'@, @\'#\'@, @\'\;\'@, or a space. An accepted name can still hold those,
notably a once-decoded segment carrying a literal @%2e%2e%2f@.

They are neutralised by percent-encoding every accepted component with
'encodeComponent' when the upstream URL is built, not by widening this denylist. The
safety of an interpolated component therefore rests on encode-on-build, not on this
gate alone.
-}
isSafeComponent :: Text -> Bool
isSafeComponent c =
    not (T.null c)
        && c /= "."
        && c /= ".."
        && T.all safeChar c
  where
    safeChar ch = ch /= '/' && ch /= '\\' && not (isControl ch)

{- | Percent-encode a single decoded path component for __safe interpolation__
into an upstream URL: the encode-on-build partner of 'isSafeComponent'.

A component is the content between a URL's structural delimiters (a scope, base
name, or filename), never the delimiters themselves, so this encodes conservatively.
It keeps only the RFC 3986 __unreserved__ set
(@A-Z@, @a-z@, @0-9@, and @\'-\'@, @\'.\'@, @\'_\'@, @\'~\'@) verbatim. It
percent-encodes __every other byte__ of the component's UTF-8 encoding as
@%XX@ (upper-case hex).

A caller composing a path therefore writes the structural @\'\/\'@, scope @%2F@,
@\'\@\'@ sigil, and the like itself, around encoded components. A @\'%\'@,
@\'\/\'@, @\'?\'@, @\'#\'@, @\'\;\'@, space, or control byte inside a component then
cannot alter the URL's shape or inject a query or fragment. Nor can it survive as a
live escape a decode-and-normalise upstream could resolve to traversal, the
once-decoded @%2e%2e%2f@ case.

Encoding is per-byte over the UTF-8 form, so a multi-byte character is encoded one
@%XX@ per byte (@\'é\'@ → @%C3%A9@). It does __not__ encode an already-percent-encoded
escape idempotently: a literal @\'%\'@ is always re-encoded to @%25@. That is the
point. The component is decoded content, so any @\'%\'@ in it is a literal to be
escaped, not a structural escape to preserve.
-}
encodeComponent :: Text -> Text
-- 'urlEncode' in query-string mode (True), not path mode (False): its keep-verbatim
-- set is exactly the RFC 3986 unreserved set the contract above names. Path mode,
-- which http-types recommends for path elements, additionally passes ':@&=+$,'
-- through unencoded, which a component must not carry.
encodeComponent = decodeUtf8 . urlEncode True . encodeUtf8
