-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | CodeArtifact requests that carry planted secrets to a closed port, so a test can check that no reader of their failure prints one.
module Ecluse.Runtime.Test.CodeArtifact (
    sessionSecrets,
    closedEndpoint,
    refusedMint,
    refusedRequest,
) where

import Amazonka qualified as AWS
import Amazonka.Auth (fromSession)
import Amazonka.CodeArtifact.GetAuthorizationToken qualified as CA

import Ecluse.Core.Credential (CredentialProvider)
import Ecluse.Core.Credential.Refresh (noCredentialReporters)
import Ecluse.Runtime.Credential.CodeArtifact.Internal (CodeArtifactConfig (..), providerForEnv)

-- | Every secret the requests below carry: a session-token header, as an IRSA, ECS or SSO role signs, and a query value.
sessionSecrets :: [Text]
sessionSecrets = ["token-secret", "query-secret"]

-- | The @host:port@ every request below dials. Nothing listens there.
closedEndpoint :: Text
closedEndpoint = "127.0.0.1:1"

-- | Build a CodeArtifact provider, whose mint at construction fails at 'closedEndpoint'.
refusedMint :: IO CredentialProvider
refusedMint = do
    env <- sessionEnv
    providerForEnv noCredentialReporters env CodeArtifactConfig{caRegion = "us-east-1", caDomain = "query-secret", caDomainOwner = Nothing, caDurationSeconds = Nothing}

-- | Send the mint's request with no classification, so its @amazonka@ error escapes as credential discovery's own would.
refusedRequest :: IO ()
refusedRequest = do
    env <- sessionEnv
    void (AWS.runResourceT (AWS.send env (CA.newGetAuthorizationToken "query-secret")))

-- One attempt, so a refused connection fails at once.
sessionEnv :: IO AWS.Env
sessionEnv = do
    base <- AWS.newEnv (pure . fromSession (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey") (AWS.SessionToken "token-secret"))
    let env = AWS.overrideService (AWS.setEndpoint False "127.0.0.1" 1) base
    pure (AWS.once env{AWS.retryCheck = \_ _ -> False})
