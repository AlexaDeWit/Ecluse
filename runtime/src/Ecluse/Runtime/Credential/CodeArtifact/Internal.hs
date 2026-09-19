-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mint closure and its @amazonka@ environment behind
"Ecluse.Runtime.Credential.CodeArtifact", which documents the leaf and re-exports the curated
surface. Importing this module opts out of that stability promise, the convention @text@ and
@bytestring@ use, so production code imports the public one.
-}
module Ecluse.Runtime.Credential.CodeArtifact.Internal (
    -- * Configuration
    CodeArtifactConfig (..),

    -- * The provider
    newCodeArtifactProvider,
    providerForEnv,
) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact.GetAuthorizationToken qualified as CA
import Amazonka.CodeArtifact.Types qualified as CAT
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
import Ecluse.Runtime.Aws.Env (newAwsEnv)

{- The mint's one failure: @GetAuthorizationToken@ succeeded but carried no token. The refresh
breaker catches 'SomeException' to count failures, so this unexported leaf throws
(docs/style.md section 11.4).
-}
data CodeArtifactMintError = AuthorizationTokenMissing
    deriving stock (Eq, Show)

instance Exception CodeArtifactMintError

{- | What the CodeArtifact leaf needs to mint a token. The AWS /credentials/ are __not__ here:
'AWS.discover' finds them in the ambient environment, so the proxy never holds long-lived AWS keys.
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
    {- ^ Requested token lifetime in seconds (@900@-@43200@). 'Nothing' defaults it to the
    caller's role-credential expiry, and the refresh policy adapts to the minted expiry anyway.
    -}
    }
    deriving stock (Eq, Ord, Show)

{- | Build a refreshing 'CredentialProvider' backed by CodeArtifact @GetAuthorizationToken@. It
mints once eagerly, so a misconfiguration fails at construction, not on the first mirror write.
-}
newCodeArtifactProvider :: CredentialReporters -> CodeArtifactConfig -> IO CredentialProvider
newCodeArtifactProvider reporters cfg =
    -- No region here: 'providerForEnv' scopes the env it is handed, so a test can supply
    -- its own env and get the same scoping.
    newAwsEnv Nothing Nothing CAT.defaultService >>= \env -> providerForEnv reporters env cfg

{- | Build the provider over a caller-supplied @amazonka@ 'Env', minting through the policy of
"Ecluse.Core.Credential.Refresh". Exposed so a test can drive the mint against a stub endpoint.
-}
providerForEnv :: CredentialReporters -> AWS.Env -> CodeArtifactConfig -> IO CredentialProvider
providerForEnv reporters env cfg =
    refreshingProvider
        defaultRefreshConfig
            { rcMint = mintToken (regioned env) (tokenRequest cfg)
            , rcClock = getCurrentTime
            , rcReporters = reporters
            }
  where
    regioned :: AWS.Env -> AWS.Env
    regioned e = e{AWS.region = AWS.Region' (caRegion cfg)}

tokenRequest :: CodeArtifactConfig -> CA.GetAuthorizationToken
tokenRequest cfg =
    setOptional CA.getAuthorizationToken_domainOwner (caDomainOwner cfg)
        . setOptional CA.getAuthorizationToken_durationSeconds (caDurationSeconds cfg)
        $ CA.newGetAuthorizationToken (caDomain cfg)

mintToken :: AWS.Env -> CA.GetAuthorizationToken -> IO AuthToken
mintToken env request = do
    response <- runResourceT (AWS.send env request)
    secret <- case response ^. CA.getAuthorizationTokenResponse_authorizationToken of
        Just token -> pure (mkSecret token)
        Nothing -> throwIO AuthorizationTokenMissing
    pure
        AuthToken
            { authSecret = secret
            , authExpiresAt = response ^. CA.getAuthorizationTokenResponse_expiration
            }

-- | Set a request field only when the caller supplied a value.
setOptional :: Lens' s (Maybe a) -> Maybe a -> s -> s
setOptional l = maybe id (l ?~)
