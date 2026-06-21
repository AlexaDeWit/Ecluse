{- | The outbound-credential seam: minting the bearer token Écluse uses to
__write__ approved packages to the mirror target.

This is one of the two cloud seams (the other is "Ecluse.Queue"); it is separate
from the protocol seam "Ecluse.Registry" because protocol and authentication are
orthogonal axes — every managed npm registry (AWS CodeArtifact, GCP Artifact
Registry, a self-hosted Verdaccio) speaks the same npm protocol and differs only
in how its bearer token is obtained (see
@docs\/architecture\/cloud-backends.md@ → "Credential Provider").

A 'CredentialProvider' is used __only__ for the mirror-target write, never to
read on a user's behalf: private-upstream reads forward the /client's/ own
credential and public reads are anonymous (see
@docs\/architecture\/registry-model.md@ → "Credential flow and authority"). So a
deployment configures exactly one provider.

Like the other seams, the effectful field returns __'IO', not @App@__: an
adapter closes over its own backend state (an @amazonka@ env, an HTTP manager)
and never imports the proxy's @Env@\/@App@, so backends stay decoupled from the
core (see @docs\/architecture\/technology-stack.md@ → "Key Decisions").

This module provides the seam and its payload types. 'staticProvider' is the
in-memory implementation: a fixed token with no expiry.
-}
module Ecluse.Credential (
    -- * Provider seam
    CredentialProvider (..),

    -- * Tokens
    AuthToken (..),

    -- * Secrets
    Secret,
    mkSecret,
    unSecret,

    -- * In-memory double
    staticProvider,
) where

import Data.Time (UTCTime)
import Text.Show (showString, showsPrec)

{- | A short-lived bearer secret (an access token).

__Opaque, and its 'Show' is redacted__: the underlying token text is never
rendered, so a 'Secret' can be embedded in any value — an 'AuthToken', a log
record, an error — without risking disclosure (token material must never reach a
log, metric, or trace; see @docs\/architecture\/observability.md@). This
redaction is a load-bearing security property, pinned by a test.

Build one with 'mkSecret' and read the real value back __only__ at the point of
use with 'unSecret' (e.g. when setting the @Authorization@ header).
-}
newtype Secret = Secret Text
    deriving stock (Eq)

{- | Renders a fixed placeholder, __never__ the secret text. This is the whole
point of the type: it makes accidental disclosure through any @'show'@-based
signal (logs, errors, @deriving Show@ on an enclosing record) impossible.

Defined via 'showsPrec' (the 'Show' class method) rather than @show@, because
relude re-exports a polymorphic @show@ that is not the class method.
-}
instance Show Secret where
    showsPrec _ _ = showString "Secret <REDACTED>"

-- | Wrap raw token text as a 'Secret'.
mkSecret :: Text -> Secret
mkSecret = Secret

{- | Recover the raw token text from a 'Secret'. Call this __only__ at the point
of use (setting the auth header); never log or otherwise render the result.
-}
unSecret :: Secret -> Text
unSecret (Secret s) = s

{- | A bearer token for a registry endpoint, with its expiry when known.

The expiry is what a refresh wrapper schedules against (cloud token lifetimes
range from CodeArtifact's ~12h to ADC's ~1h, so refresh is driven off the
token's own 'authExpiresAt' rather than a fixed interval). A static token has no
expiry ('Nothing').
-}
data AuthToken = AuthToken
    { authSecret :: Secret
    -- ^ The bearer secret itself (redacted in 'Show').
    , authExpiresAt :: Maybe UTCTime
    -- ^ When the token expires, if it does; 'Nothing' for a non-expiring
    -- (e.g. static) token.
    }
    deriving stock (Eq, Show)

{- | The credential seam: yields the bearer token currently valid for the mirror
target, refreshing it before expiry __internally__ (a caller never sees a stale
token in the common case, and never blocks on a mint on the request hot path).

It is a __record of functions__ (the Handle pattern): the single field is the
operation, and a backend's smart constructor returns a 'CredentialProvider'
whose closure captures that backend's private state. 'currentToken' returns
__'IO', not @App@__ so adapters stay decoupled from the core (see the module
header).
-}
newtype CredentialProvider = CredentialProvider
    { currentToken :: IO AuthToken
    -- ^ The bearer token to use now. An adapter refreshes before expiry behind
    -- this field, so the caller just uses the returned token.
    }

{- | An in-memory 'CredentialProvider' that always returns a fixed token.

This is the @static@ leaf: it never expires and never refreshes, so it is the
right provider for a registry reached with a long-lived credential, and it is
the trivial double for tests of code that consumes a 'CredentialProvider'.
-}
staticProvider :: AuthToken -> CredentialProvider
staticProvider token = CredentialProvider{currentToken = pure token}
