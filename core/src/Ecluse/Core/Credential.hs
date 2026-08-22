-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The outbound-credential handle: it mints the bearer token Écluse uses to
__write__ approved packages to the mirror target.

This is one of the two cloud handles. The other is "Ecluse.Core.Queue". It stays
separate from the protocol handle "Ecluse.Core.Registry" because protocol and
authentication are orthogonal axes. Every managed npm registry speaks the same npm
protocol and differs only in how it hands out a bearer token. That holds for AWS
CodeArtifact, GCP Artifact Registry, and a self-hosted Verdaccio alike (see
@docs\/architecture\/cloud-backends.md@ → "Credential Provider").

A 'CredentialProvider' serves the mirror-target write __only__, never a read on a
user's behalf. A private-upstream read forwards the /client's/ own credential, and a
public read is anonymous (see @docs\/architecture\/registry-model.md@ → "Credential
flow and authority"). A deployment therefore configures exactly one provider.

Like the other handles, the effectful field returns __'IO', not @App@__. An adapter
closes over its own backend state (an @amazonka@ env, an HTTP manager) and never
imports the proxy's @Env@\/@App@. Backends therefore stay decoupled from the core (see
@docs\/architecture\/technology-stack.md@ → "Key Decisions").

This module holds the handle and its payload types. 'staticProvider' is the
in-memory leaf: a fixed token with no expiry. The refresh, cache, and expiry policy
that wraps a per-cloud token mint lives in "Ecluse.Core.Credential.Refresh".
-}
module Ecluse.Core.Credential (
    -- * Provider handle
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

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)
import Data.ByteArray qualified as BA
import Data.Time (UTCTime)
import Text.Show (showString, showsPrec)

{- | A short-lived bearer secret (an access token).

__Opaque, and its 'Show' is redacted__: nothing renders the underlying token text,
so any value can hold a 'Secret' without risking disclosure. An 'AuthToken', a log
record, or an error can each carry one. Token material must never reach a log, a
metric, or a trace (see @docs\/architecture\/observability.md@). This redaction is a
load-bearing security property.

Build one with 'mkSecret' and read the real value back __only__ at the point of use
with 'unSecret', for example when setting the @Authorization@ header.

__Equality is constant-time__: the 'Eq' instance compares two secrets over their
UTF-8 bytes with no content-dependent early out. The derived equality would be
'Data.Text'\'s short-circuiting compare. It returns as soon as two tokens first
differ, and so leaks the length of a shared prefix through timing. Folding that
property into the type itself means no comparison on a 'Secret' can accidentally
become non-constant-time, the inbound edge-auth gate above all. A constant-time
compare can still reveal the token /length/. Écluse accepts that residual leak, and
never short-circuits on the /content/.
-}
newtype Secret = Secret Text

{- | Constant-time equality over the UTF-8 encoding of the wrapped token.

'BA.constEq' compares every byte regardless of where the inputs first diverge, so the
compare takes the same time for a near-miss token as for a far-miss one. This is the
security property the whole type exists to make unmissable. The
@ECLUSE_SERVER__AUTH_TOKEN@ edge gate compares the client's bearer token against the
configured one through this instance. A short-circuiting compare there would leak
the secret's prefix length to a remote attacker.
-}
instance Eq Secret where
    Secret a == Secret b = BA.constEq (encodeUtf8 a :: ByteString) (encodeUtf8 b :: ByteString)

{- | Render a fixed placeholder, __never__ the secret text. This is the whole point
of the type: it makes accidental disclosure impossible through any @'show'@-based
signal. That covers a log, an error, and @deriving Show@ on an enclosing record.

Defined through 'showsPrec' (the 'Show' class method) rather than @show@, because
relude re-exports a polymorphic @show@ that is not the class method.
-}
instance Show Secret where
    showsPrec _ _ = showString "Secret <REDACTED>"

-- | Wrap raw token text as a 'Secret'.
mkSecret :: Text -> Secret
mkSecret = Secret

{- | Recover the raw token text from a 'Secret'. Call this __only__ at the point of
use, when setting the auth header. Never log or otherwise render the result.
-}
unSecret :: Secret -> Text
unSecret (Secret s) = s

-- | The JSON encoding redacts the secret, so it never leaks into a JSON log.
instance ToJSON Secret where
    toJSON _ = String "<REDACTED>"

-- | Decoding reads the secret from configuration, for example the environment AST.
instance FromJSON Secret where
    parseJSON = withText "Secret" (pure . mkSecret)

{- | A bearer token for a registry endpoint, with its expiry when known.

A refresh wrapper schedules against the expiry. Cloud token lifetimes range from
CodeArtifact's ~12h to ADC's ~1h, so a refresh runs off the token's own
'authExpiresAt' rather than a fixed interval. A static token has no expiry
('Nothing').
-}
data AuthToken = AuthToken
    { authSecret :: Secret
    -- ^ The bearer secret itself (redacted in 'Show').
    , authExpiresAt :: Maybe UTCTime
    {- ^ When the token expires, if it does. 'Nothing' for a token that does not
    expire, such as a static one.
    -}
    }
    deriving stock (Eq, Show)

{- | The credential handle: it yields the bearer token currently valid for the
mirror target and refreshes that token before expiry __internally__. A caller never
sees a stale token in the common case, and never blocks on a mint on the request
hot path.

It is a __record of functions__ (the Handle pattern). The single field is the
operation, and a backend's smart constructor returns a 'CredentialProvider' whose
closure captures that backend's private state. 'currentToken' returns __'IO', not
@App@__, so adapters stay decoupled from the core (see the module header).
-}
newtype CredentialProvider = CredentialProvider
    { currentToken :: IO AuthToken
    {- ^ The bearer token to use now. An adapter refreshes before expiry behind
    this field, so the caller just uses the token it gets back.
    -}
    }

{- | An in-memory 'CredentialProvider' that always returns a fixed token.

This is the @static@ leaf. It never expires and never refreshes, so it fits a
registry reached with a long-lived credential. It is also the trivial double for a
test of code that consumes a 'CredentialProvider'.
-}
staticProvider :: AuthToken -> CredentialProvider
staticProvider token = CredentialProvider{currentToken = pure token}
