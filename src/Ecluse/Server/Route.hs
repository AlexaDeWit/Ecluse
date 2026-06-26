{- | The shared serve-action vocabulary of the front door, and the agnostic
default router.

A 'Route' is one classified request — everything the proxy is willing to serve,
named independently of any ecosystem's URL grammar. The /actions/ are common
across registries (fetch a packument, stream a tarball, answer a liveness probe,
deny a search); only the URL→action mapping is ecosystem-specific. That mapping
is a 'Classifier', injected at the composition root, so this module stays free of
any one ecosystem's path conventions while the dispatcher routes through whatever
classifier its mount carries.

The model is __deny by default__, mirroring the rules engine ("Ecluse.Core.Rules"):
the agnostic default 'denyAll' classifies every path as 'Unsupported' (a @404@ at
the edge), so a deployment that wires no ecosystem router serves nothing rather
than guessing. An ecosystem adapter supplies a 'Classifier' that recognises its
own paths and falls back to 'Unsupported' for the rest.

'Route' is a small sum so the whole routing table is unit-testable with __no
server__: feed a 'Classifier' some segments, assert the 'Route'.
-}
module Ecluse.Server.Route (
    -- * Routes
    Route (..),
    Filename (..),

    -- * Classification
    Classifier,
    denyAll,

    -- * Component safety
    isSafeComponent,
    encodeComponent,
) where

import Data.ByteString qualified as BS
import Data.Char (intToDigit, isControl, toUpper)
import Data.Text qualified as T

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Version (Version)

{- | A classified request. Everything the front door is willing to serve is one
of these; an unrecognised path is 'Unsupported' (deny by default).

The constructors are the proxy's /actions/, shared across ecosystems — the
artifact a 'Tarball' streams and the metadata a 'Packument' merges are the same
serve behaviour whether the upstream is npm, PyPI, or another registry. Only the
mapping from a request path to one of these (a 'Classifier') is
ecosystem-specific.
-}
data Route
    = -- | A package-metadata request — the /packument/.
      Packument PackageName
    | {- | An artifact request, as a __parsed coordinate__: the package, the
      'Version' the classifier read out of the artifact name, and the 'Filename'
      itself. The 'Version' is the coordinate the rules gate on; the 'Filename' is
      the artifact's on-the-wire name, __preserved verbatim__ — it, not a name
      rebuilt from @(package, version)@, is authoritative for fetching the bytes.
      -}
      Tarball PackageName Version Filename
    | -- | A registry liveness probe, answered locally.
      Ping
    | -- | Package search (unsupported).
      Search
    | {- | Anything unrecognised. Renders as a @404@ — deny by default at the
      routing layer.
      -}
      Unsupported
    deriving stock (Eq, Show)

{- | An artifact's on-the-wire file name, the agnostic artifact-name type a
'Tarball' route carries.

It is held as a distinct type, not a bare 'Text', because it is __authoritative
for fetching the bytes__: the proxy fetches an artifact at the upstream path built
from this exact name, never one reconstructed from @(package, version)@, so that a
registry whose artifact naming differs from the proxy's own convention still
resolves. The name is preserved verbatim as received; the classifier that produces
it has already applied the component-safety gate ('isSafeComponent'), so the value
is safe to interpolate into a downstream URL.
-}
newtype Filename = Filename Text
    deriving stock (Eq, Show)

{- | The mapping from an ecosystem-native request path to a 'Route'.

A classifier sees the already-mount-stripped, percent-decoded path segments and
returns the serve action. Each ecosystem adapter contributes its own —
recognising its path grammar and denying everything else — so the agnostic
dispatcher stays closed while every mount routes through its ecosystem's
template. Dispatch chooses the classifier per matched mount (see
"Ecluse.Server"), so the same shape carries either a single ecosystem or a
mount-keyed selection.
-}
type Classifier = [Text] -> Route

{- | The agnostic default classifier: every path is 'Unsupported'.

This is the deny-by-default base a deployment runs with until a composition root
wires an ecosystem's classifier in, so an unwired server serves nothing rather
than guessing a grammar. It deliberately knows no path conventions of its own.
-}
denyAll :: Classifier
denyAll _segments = Unsupported

{- | Whether a single decoded path component is __safe to interpolate__ into a
downstream upstream URL — the deny-by-default gate a classifier applies to every
component it accepts (a scope, base name, or tarball filename).

The path is percent-decoded before it reaches us, so a single segment can carry a
@\'\/\'@, a @\'\\\\\'@, a control character, or be @"."@\/@".."@; any of these
enables path traversal or request smuggling once the name reaches the upstream
URL. A component is UNSAFE iff it is empty, is exactly @"."@ or @".."@, or
contains a @\'\/\'@, a @\'\\\\\'@, or any 'isControl' character. Everything else
is accepted: this is a security boundary, __not__ an ecosystem-policy validator,
so ordinary names with interior dots (@lodash.merge@, @is.odd@), hyphens,
underscores, digits, or uppercase all pass.

It lives in the agnostic layer because the threat — interpolating a hostile
segment into an upstream URL — is ecosystem-independent; both an ecosystem's path
classifier and the defence-in-depth check in "Ecluse.Core.Security" share this one
rule.

This gate is __structural__: it stops a component that would change the upstream
URL's /shape/ (a traversal, an embedded separator, a control character). It does
__not__ stop a component that carries other URL-reserved bytes — a @\'%\'@,
@\'?\'@, @\'#\'@, @\'\;\'@, or a space — which an accepted name can still hold
(notably a once-decoded segment carrying a literal @%2e%2e%2f@). Those are
neutralised not by widening this denylist but by percent-encoding every accepted
component with 'encodeComponent' when the upstream URL is built, so the safety of
an interpolated component rests on encode-on-build, not on this gate alone.
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
into an upstream URL — the encode-on-build partner of 'isSafeComponent'.

A component is the content between a URL's structural delimiters (a scope, base
name, or filename), never the delimiters themselves, so this encodes
conservatively: it keeps only the RFC 3986 __unreserved__ set
(@A-Z@, @a-z@, @0-9@, and @\'-\'@, @\'.\'@, @\'_\'@, @\'~\'@) verbatim and
percent-encodes __every other byte__ of the component's UTF-8 encoding as
@%XX@ (upper-case hex). A caller composing a path therefore writes the structural
@\'\/\'@, scope @%2F@, @\'\@\'@ sigil, and the like itself, around encoded
components — so a @\'%\'@, @\'\/\'@, @\'?\'@, @\'#\'@, @\'\;\'@, space, or control
byte inside a component cannot alter the URL's shape, inject a query or fragment,
or — the once-decoded @%2e%2e%2f@ case — survive as a live escape a
decode-and-normalise upstream could resolve to traversal.

Encoding is per-byte over the UTF-8 form, so a multi-byte character is encoded one
@%XX@ per byte (@\'é\'@ → @%C3%A9@). It does __not__ encode an already-percent-encoded
escape idempotently — a literal @\'%\'@ is always re-encoded to @%25@ — which is the
point: the component is decoded content, so any @\'%\'@ in it is a literal to be
escaped, not a structural escape to preserve.
-}
encodeComponent :: Text -> Text
encodeComponent = T.concat . map encodeByte . BS.unpack . encodeUtf8
  where
    encodeByte :: Word8 -> Text
    encodeByte b
        | isUnreserved b = T.singleton (chr8 b)
        | otherwise = T.pack ['%', hexDigit (b `div` 16), hexDigit (b `mod` 16)]

    -- RFC 3986 §2.3 unreserved: ALPHA / DIGIT / "-" / "." / "_" / "~".
    isUnreserved :: Word8 -> Bool
    isUnreserved b =
        (b >= 0x41 && b <= 0x5A) -- A-Z
            || (b >= 0x61 && b <= 0x7A) -- a-z
            || (b >= 0x30 && b <= 0x39) -- 0-9
            || b == 0x2D -- '-'
            || b == 0x2E -- '.'
            || b == 0x5F -- '_'
            || b == 0x7E -- '~'

    -- An unreserved byte is ASCII, so its 'Char' is its code point.
    chr8 :: Word8 -> Char
    chr8 = toEnum . fromIntegral

    hexDigit :: Word8 -> Char
    hexDigit = toUpper . intToDigit . fromIntegral
