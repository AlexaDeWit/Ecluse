-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The outbound-credential handle: the bearer token Écluse uses to __write__ approved packages
to the mirror target. It serves Écluse's own store access only, never a read on a user's behalf:
a private-upstream read forwards the client's own credential (see
@docs\/architecture\/registry-model.md@, "Credential flow and authority").

The handle stays apart from the protocol handle "Ecluse.Core.Registry" because every managed
registry speaks one protocol and differs only in how it hands out a token. Refresh, cache and
expiry policy over a per-cloud mint live in "Ecluse.Core.Credential.Refresh".
-}
module Ecluse.Core.Credential (
    -- * Secrets
    Secret,
    mkSecret,
    unSecret,

    -- * A client's presented credential
    ClientCredential (..),
    bareCredential,

    -- * Tokens
    AuthToken (..),

    -- * Provider handle
    CredentialProvider (..),
    mintSecret,

    -- * In-memory double
    staticProvider,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)
import Data.ByteArray qualified as BA
import Data.Time (UTCTime)
import Text.Show (showString, showsPrec)

{- | A short-lived secret (an access token). Build one with 'mkSecret' and recover the text
__only__ at the point of use with 'unSecret'.
-}
newtype Secret = Secret Text

{- | Constant-time equality over the UTF-8 encoding. The @ECLUSE_SERVER__AUTH_TOKEN@ edge gate
compares a client's token through it, and a short-circuiting compare would leak the prefix length.
-}
instance Eq Secret where
    Secret a == Secret b = BA.constEq (encodeUtf8 a :: ByteString) (encodeUtf8 b :: ByteString)

{- | Render a fixed placeholder, __never__ the secret text. It defines 'showsPrec' because relude
re-exports a polymorphic @show@ that is not the class method.
-}
instance Show Secret where
    showsPrec _ _ = showString "Secret <REDACTED>"

-- | The JSON encoding redacts the secret, so it never leaks into a JSON log.
instance ToJSON Secret where
    toJSON _ = String "<REDACTED>"

-- | Decoding reads the secret from configuration, for example the environment AST.
instance FromJSON Secret where
    parseJSON = withText "Secret" (pure . mkSecret)

-- | Wrap raw token text as a 'Secret'.
mkSecret :: Text -> Secret
mkSecret = Secret

{- | Recover the raw token text. Call this __only__ at the point of use, when setting the auth
header, and never log or otherwise render the result.
-}
unSecret :: Secret -> Text
unSecret (Secret s) = s

{- | A credential as a client presents it. The username is not part of the secret: a gate
compares 'credSecret' alone, and a passthrough leg renders the pair verbatim.
-}
data ClientCredential = ClientCredential
    { credUsername :: Maybe Text
    -- ^ The username the client presented, when its scheme carries one.
    , credSecret :: Secret
    -- ^ The secret half, the only half any gate compares.
    }
    deriving stock (Eq, Show)

-- | A credential carrying no username, the form a bearer scheme recovers and a configured token takes.
bareCredential :: Secret -> ClientCredential
bareCredential = ClientCredential Nothing

{- | A bearer token for a registry endpoint. Cloud lifetimes run from CodeArtifact's ~12h to
ADC's ~1h, so a refresh schedules off 'authExpiresAt' rather than a fixed interval.
-}
data AuthToken = AuthToken
    { authSecret :: Secret
    -- ^ The bearer secret itself (redacted in 'Show').
    , authExpiresAt :: Maybe UTCTime
    -- ^ When the token expires. 'Nothing' for a static token, which does not expire.
    }
    deriving stock (Eq, Show)

{- | The credential handle: it yields the token currently valid for the mirror target and
refreshes it before expiry internally, so no caller blocks on a mint on the hot path.
-}
newtype CredentialProvider = CredentialProvider
    { currentToken :: IO AuthToken
    -- ^ 'IO', not @App@, so an adapter closing over its own backend state stays off the core.
    }

{- | The secret a provider's current token carries, for a caller that presents it and reads no
expiry. It refreshes behind the provider, so a long-lived caller mints per use.
-}
mintSecret :: CredentialProvider -> IO Secret
mintSecret = fmap authSecret . currentToken

{- | A 'CredentialProvider' that always returns the same token, the @static@ leaf. It never
refreshes, so it fits a registry reached with a long-lived credential.
-}
staticProvider :: AuthToken -> CredentialProvider
staticProvider token = CredentialProvider{currentToken = pure token}
