{- | The shared serve-action vocabulary of the front door, and the agnostic
default router.

A 'Route' is one classified request — everything the proxy is willing to serve,
named independently of any ecosystem's URL grammar. The /actions/ are common
across registries (fetch a packument, stream a tarball, answer a liveness probe,
deny a search); only the URL→action mapping is ecosystem-specific. That mapping
is a 'Classifier', injected at the composition root, so this module stays free of
any one ecosystem's path conventions while the dispatcher routes through whatever
classifier its mount carries.

The model is __deny by default__, mirroring the rules engine ("Ecluse.Rules"):
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

    -- * Classification
    Classifier,
    denyAll,

    -- * Component safety
    isSafeComponent,
) where

import Data.Char (isControl)
import Data.Text qualified as T

import Ecluse.Package (PackageName)

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
    | {- | An artifact request. The 'Text' is the tarball filename exactly as
      requested (with any ecosystem-specific basename handling already applied by
      the classifier).
      -}
      Tarball PackageName Text
    | -- | A registry liveness probe, answered locally.
      Ping
    | -- | Package search (unsupported).
      Search
    | {- | Anything unrecognised. Renders as a @404@ — deny by default at the
      routing layer.
      -}
      Unsupported
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
classifier and the defence-in-depth check in "Ecluse.Security" share this one
rule.
-}
isSafeComponent :: Text -> Bool
isSafeComponent c =
    not (T.null c)
        && c /= "."
        && c /= ".."
        && T.all safeChar c
  where
    safeChar ch = ch /= '/' && ch /= '\\' && not (isControl ch)
