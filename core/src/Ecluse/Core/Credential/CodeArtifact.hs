{- | The AWS CodeArtifact leaf of the outbound-credential handle: mint a
short-lived registry bearer token via CodeArtifact's @GetAuthorizationToken@.

This is the one genuinely cloud-specific part of outbound auth — everything else
(caching, proactive refresh, single-flight, the circuit breaker) is the
cloud-agnostic policy in "Ecluse.Core.Credential.Refresh", which this module wires its
mint into. The leaf itself is tiny: build an @amazonka@ 'Env' once (credentials
discovered the standard AWS way — environment, instance role, container role, SSO,
STS), then on each mint call @GetAuthorizationToken@ and return the token together
with its real expiry so the refresh policy schedules off the token's own
lifetime (CodeArtifact tokens last up to 12h).

This is __control plane__ only: @amazonka@ obtains the token, and the data plane
that then uses it to publish to the registry stays on @http-client@ (see
@docs\/architecture\/web-layer.md@ → "Control plane vs data plane"). The 'Env' is
constructed once at provider creation and captured in the mint closure, so the
backend's state never leaks into the proxy's @Env@\/@App@ (see
@docs\/architecture\/technology-stack.md@ → "Key Decisions").
-}
module Ecluse.Core.Credential.CodeArtifact (
    -- * Configuration
    CodeArtifactConfig (..),

    -- * The provider
    newCodeArtifactProvider,
    providerForEnv,
) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact.GetAuthorizationToken qualified as CA
import Control.Monad.Trans.Resource (runResourceT)
import Data.Time (getCurrentTime)
import Lens.Micro (Lens', (?~), (^.))
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, mkSecret)
import Ecluse.Core.Credential.Refresh (
    CredentialReporters (..),
    RefreshConfig (..),
    defaultRefreshConfig,
    refreshingProvider,
 )

{- The mint's one failure: @GetAuthorizationToken@ succeeded but carried no token.
Thrown (not a stringly exception) because the refresh breaker runs this leaf and
catches 'SomeException' to count failures and trip — the leaf must throw to be seen,
and a returned value would fight that contract (STYLE.md section 11.4). Not exported
for the same reason: the breaker sees it as a 'SomeException'. -}
data CodeArtifactMintError = AuthorizationTokenMissing
    deriving stock (Eq, Show)

instance Exception CodeArtifactMintError

{- | What the CodeArtifact leaf needs to mint a token. The AWS /credentials/ used
to make the call are __not__ here: they are discovered the standard AWS way
('AWS.discover') from the ambient environment (env vars, instance\/container role,
SSO, STS), so the proxy never holds long-lived AWS keys itself.
-}
data CodeArtifactConfig = CodeArtifactConfig
    { caRegion :: Text
    -- ^ The AWS region the CodeArtifact domain lives in (e.g. @"us-east-1"@).
    , caDomain :: Text
    -- ^ The CodeArtifact domain that scopes the token.
    , caDomainOwner :: Maybe Text
    {- ^ The 12-digit account number that owns the domain, when it differs from
    the calling account ('Nothing' to default to the caller's account).
    -}
    , caDurationSeconds :: Maybe Natural
    {- ^ Requested token lifetime in seconds (@900@–@43200@, i.e. 15 min–12 h);
    'Nothing' lets CodeArtifact default it (it ties the token to the caller's
    role-credential expiry). The refresh policy adapts to whatever expiry the
    minted token actually carries, so this is only a preference.
    -}
    }
    deriving stock (Eq, Show)

{- | Build a refreshing 'CredentialProvider' backed by CodeArtifact
@GetAuthorizationToken@. Discovers AWS credentials the standard way
('AWS.discover') and hands the resulting 'AWS.Env' to 'providerForEnv'.

Mints once eagerly to seed the cache, so a misconfiguration (bad region, missing
credentials, no permission) fails here at construction rather than on the first
mirror write.

The 'CredentialReporters' carry the telemetry observers the refresh policy records
through (the mint breaker's state and each refresh outcome); pass
'Ecluse.Core.Credential.Refresh.noCredentialReporters' for an unobserved provider.
-}
newCodeArtifactProvider :: CredentialReporters -> CodeArtifactConfig -> IO CredentialProvider
newCodeArtifactProvider reporters cfg =
    AWS.newEnv AWS.discover >>= \env -> providerForEnv reporters env cfg

{- | Build the provider over a caller-supplied @amazonka@ 'Env' — the boundary the
production 'newCodeArtifactProvider' wraps with credential discovery. The config's
region is applied to the 'Env', and each mint calls @GetAuthorizationToken@ through
it under the cache\/proactive-refresh\/single-flight\/breaker policy of
"Ecluse.Core.Credential.Refresh" (so the token API is not re-hit per request), reporting its
refresh and breaker signals through the given 'CredentialReporters'. Exposed so a test
can drive the real mint against an 'Env' aimed at a stub endpoint, with no live AWS.
-}
providerForEnv :: CredentialReporters -> AWS.Env -> CodeArtifactConfig -> IO CredentialProvider
providerForEnv reporters env cfg =
    refreshingProvider
        defaultRefreshConfig
            { rcMint = mint (regioned env)
            , rcClock = getCurrentTime
            , rcBreakerReporter = crBreakerReporter reporters
            , rcRefreshReporter = crRefreshReporter reporters
            }
  where
    regioned :: AWS.Env -> AWS.Env
    regioned e = e{AWS.region = AWS.Region' (caRegion cfg)}

    request :: CA.GetAuthorizationToken
    request =
        setOptional CA.getAuthorizationToken_domainOwner (caDomainOwner cfg)
            . setOptional CA.getAuthorizationToken_durationSeconds (caDurationSeconds cfg)
            $ CA.newGetAuthorizationToken (caDomain cfg)

    -- One mint: call GetAuthorizationToken and lift the response into an AuthToken.
    mint :: AWS.Env -> IO AuthToken
    mint e = do
        response <- runResourceT (AWS.send e request)
        secret <- case response ^. CA.getAuthorizationTokenResponse_authorizationToken of
            Just token -> pure (mkSecret token)
            Nothing -> throwIO AuthorizationTokenMissing
        pure
            AuthToken
                { authSecret = secret
                , authExpiresAt = response ^. CA.getAuthorizationTokenResponse_expiration
                }

{- | Set an optional request field only when present, leaving the @amazonka@
default ('Nothing') in place otherwise.
-}
setOptional :: Lens' s (Maybe a) -> Maybe a -> s -> s
setOptional l = maybe id (l ?~)
