module Ecluse.Credential.CodeArtifactSpec (spec) where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)

import Ecluse.Credential (AuthToken (..), CredentialProvider (..), unSecret)
import Ecluse.Credential.CodeArtifact (CodeArtifactConfig (..), providerForEnv)

{- | Component test for the CodeArtifact credential leaf with __no live AWS__: an
in-process HTTP stub answers @GetAuthorizationToken@ with a canned response (the
shape from the API reference — @{"authorizationToken": string, "expiration":
number}@, a @200@), and an @amazonka@ 'AWS.Env' is pointed at it via an endpoint
override with static credentials. This drives the real mint path — request build,
SigV4 signing, response parse, token + expiry extraction — that the secret-gated
smoke test can only reach against the real service. (The token is a control-plane
AWS API call, not the npm protocol, so an npm-registry emulator cannot stand in;
the endpoint shim is the injection point.)
-}
spec :: Spec
spec = describe "CodeArtifact GetAuthorizationToken (stubbed endpoint)" $
    it "mints the token and expiry the endpoint returns, without live AWS" $
        testWithApplication (pure stubApp) $ \port -> do
            env <- stubEnv port
            token <- providerForEnv env config >>= currentToken
            unSecret (authSecret token) `shouldBe` "the-canned-token"
            authExpiresAt token `shouldBe` Just (posixSecondsToUTCTime 2000000000)
  where
    config :: CodeArtifactConfig
    config =
        CodeArtifactConfig
            { caRegion = "us-east-1"
            , caDomain = "my-domain"
            , caDomainOwner = Nothing
            , caDurationSeconds = Nothing
            }

    -- The canned GetAuthorizationToken response: status 200, JSON body carrying the
    -- token and a Unix-epoch expiration (the API reference's response shape).
    stubApp :: Application
    stubApp _req respond =
        respond
            ( responseLBS
                status200
                [(hContentType, "application/json")]
                "{\"authorizationToken\":\"the-canned-token\",\"expiration\":2000000000}"
            )

    -- An amazonka Env with static (dummy) credentials, its endpoint overridden to
    -- the in-process stub. The stub ignores the SigV4 signature, so any well-formed
    -- credentials suffice; the region is supplied by the provider from the config.
    stubEnv :: Int -> IO AWS.Env
    stubEnv port = do
        base <- AWS.newEnv (pure . fromKeys (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey"))
        pure (AWS.overrideService (AWS.setEndpoint False "127.0.0.1" port) base)
