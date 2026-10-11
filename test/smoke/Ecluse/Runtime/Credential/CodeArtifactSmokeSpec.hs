-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Owner-run CodeArtifact checks. Configuration and evidence rules live in the test support module.
module Ecluse.Runtime.Credential.CodeArtifactSmokeSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact.GetRepositoryEndpoint qualified as CA
import Amazonka.CodeArtifact.Types qualified as CA
import Control.Monad.Trans.Resource (runResourceT)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Lens.Micro ((.~), (^.))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Credential (AuthToken (..), CredentialProvider (..), unSecret)
import Ecluse.Core.Registry.Exchange (boundedFetch, withinServeCap)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (..), defaultLimits)
import Ecluse.Runtime.Aws.Env (newAwsEnv)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..), newCodeArtifactProvider)
import Ecluse.Test.Credential (noCredentialReporters)
import Ecluse.Test.Registry.PyPI.Observation
import Ecluse.Test.Support (expectRightText)

-- | Partial observation configuration fails before either live case can discover AWS credentials.
spec :: Spec
spec = beforeAll configuredInputs $ describe "live CodeArtifact" $ do
    it "mints a non-empty bearer token with an expiry" $ \(inputs, _) ->
        case (Map.lookup RegionInput inputs, Map.lookup DomainInput inputs) of
            (Just region, Just domain) -> do
                token <- mintToken region domain (Map.lookup OwnerInput inputs)
                T.null (unSecret (authSecret token)) `shouldBe` False
                authExpiresAt token `shouldSatisfy` isJust
            _ -> pendingWith "CodeArtifact sandbox not configured (set ECLTEST_SMOKE_CODEARTIFACT_REGION + _DOMAIN)"
    it "records the PyPI file authorities without fetching files" $ \(_, config) ->
        case config of
            Nothing -> pendingWith "CodeArtifact observation not configured (set ECLTEST_SMOKE_CODEARTIFACT_REPOSITORY + _PACKAGE)"
            Just configured -> observeIndex configured

configuredInputs :: IO (Map SmokeInput Text, Maybe ObservationConfig)
configuredInputs = do
    pairs <- forM [minBound .. maxBound] $ \key -> do
        value <- fmap toText <$> lookupEnv (toString (smokeInputName key))
        pure ((key,) <$> value)
    let inputs = Map.fromList (catMaybes pairs)
    config <- expectObservation (observationConfig (`Map.lookup` inputs))
    pure (inputs, config)

mintToken :: Text -> Text -> Maybe Text -> IO AuthToken
mintToken region domain owner =
    observationAction TokenMintFailed $
        newCodeArtifactProvider noCredentialReporters (CodeArtifactConfig region domain owner Nothing) >>= currentToken

observeIndex :: ObservationConfig -> IO ()
observeIndex config = do
    env <- observationAction AwsDiscoveryFailed (newAwsEnv (Just (ocRegion config)) Nothing CA.defaultService)
    endpoint <- repositoryEndpoint env config
    target <- expectObservation (observationTarget endpoint (ocPackage config))
    token <- mintToken (ocRegion config) (ocDomain config) (ocOwner config)
    request <- expectObservation (observationRequest target (authSecret token))
    fetched <- observationAction IndexExchangeFailed $ do
        manager <- newManager tlsManagerSettings
        let progress = progressFloor defaultLimits
        withinServeCap progress id $
            boundedFetch manager progress (MetadataBodyLimit (maxMetadataBytes defaultLimits)) request
    evidence <- expectObservation (observeResponse target fetched)
    traverse_ putTextLn evidence

repositoryEndpoint :: AWS.Env -> ObservationConfig -> IO Text
repositoryEndpoint env config = do
    let request =
            CA.newGetRepositoryEndpoint (ocDomain config) (ocRepository config) CA.PackageFormat_Pypi
                & CA.getRepositoryEndpoint_domainOwner
                .~ ocOwner config
    response <- observationAction EndpointDiscoveryFailed (runResourceT (AWS.send env request))
    expectObservation (maybe (Left EndpointMissing) Right (response ^. CA.getRepositoryEndpointResponse_repositoryEndpoint))

observationAction :: ObservationError -> IO a -> IO a
observationAction failure action = tryAny action >>= expectObservation . first (const failure)

expectObservation :: Either ObservationError a -> IO a
expectObservation = expectRightText . first renderObservationError
