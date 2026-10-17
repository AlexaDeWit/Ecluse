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
import Ecluse.Runtime.Aws.Fault (sendRaising)

-- @GetAuthorizationToken@ answered without a token.
data CodeArtifactMintError = AuthorizationTokenMissing
    deriving stock (Eq, Show)

instance Exception CodeArtifactMintError

-- | What the CodeArtifact leaf needs to mint a token. 'AWS.discover' finds the AWS credentials in the ambient environment, so the proxy holds no long-lived AWS key.
data CodeArtifactConfig = CodeArtifactConfig
    { caRegion :: Text
    -- ^ The AWS region the CodeArtifact domain lives in (e.g. @"us-east-1"@).
    , caDomain :: Text
    -- ^ The CodeArtifact domain that scopes the token.
    , caDomainOwner :: Maybe Text
    -- ^ The 12-digit account that owns the domain, or 'Nothing' for the calling account.
    , caDurationSeconds :: Maybe Natural
    -- ^ Requested lifetime in seconds (@900@-@43200@). 'Nothing' omits it, so AWS applies its 12-hour default.
    }
    deriving stock (Eq, Ord, Show)

-- | A refreshing provider over CodeArtifact @GetAuthorizationToken@. It mints once at construction, so a misconfiguration fails there, not on the first mirror write.
newCodeArtifactProvider :: CredentialReporters -> CodeArtifactConfig -> IO CredentialProvider
newCodeArtifactProvider reporters cfg =
    -- No region here: 'providerForEnv' scopes the env it is handed, so a test can supply
    -- its own env and get the same scoping.
    newAwsEnv Nothing Nothing CAT.defaultService >>= \env -> providerForEnv reporters env cfg

-- | 'newCodeArtifactProvider' over a caller's @amazonka@ env, so a test can drive the mint against a stub endpoint.
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

-- The refresh breaker counts failures by catching 'SomeException', so a failed mint throws, typed so that its text holds no request.
mintToken :: AWS.Env -> CA.GetAuthorizationToken -> IO AuthToken
mintToken env request = do
    response <- sendRaising env request
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
