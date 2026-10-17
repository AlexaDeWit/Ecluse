-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How an @amazonka@ failure leaves an adapter, as a value or raised: never with the request, a response header or a quoted signature.
module Ecluse.Runtime.Aws.FaultSpec (spec) where

import Amazonka.Auth.Exception (AuthError (..))
import Data.Text qualified as T
import Network.HTTP.Client (HttpException)
import Network.HTTP.Types.Status (status404)
import Test.Hspec
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Fault (HttpFault (HttpStatus), TransportFault (tfDetail))
import Ecluse.Runtime.Aws.Fault (
    AwsAnswer (AwsAnswer),
    AwsCredentialFault (AwsCredentialsExhausted),
    AwsFault (AwsHttp, AwsStatus),
    AwsParty (AwsServiceNamed),
    AwsRequestFailed (AwsRequestFailed, arfFault),
    classifyAwsTransport,
    displayAwsFaultT,
    retypeAmazonkaEscapes,
    retypeAwsFailure,
 )
import Ecluse.Runtime.Test.Aws (canonicalRequestMessage, deliverRefreshFailure, holdsNoPlantedSecret, plantedHeader, plantedRefusal, refreshFailures, refusedAtClosedPort)
import Ecluse.Test.Stub (redirectSecrets, redirectedStatusException, refusedFetch, signedRequest)

spec :: Spec
spec = do
    describe "classifyAwsTransport" $ do
        for_ ["InvalidSignatureException", "SignatureDoesNotMatch", "IncompleteSignature"] $ \code ->
            it ("details the " <> toString code <> " refusal by its code, status and request id, and none of the canonical request") $
                detailOf code canonicalRequestMessage `shouldBe` "SQS: HTTP 403 " <> signatureCode code <> " (request req-1)"

        it "keeps the first line of an access denial, which names the IAM action and resource" $
            detailOf "AccessDenied" accessDenial `shouldBe` "SQS: HTTP 403 AccessDenied (request req-1): " <> accessDenial

        it "keeps only the first line of another code's message" $
            detailOf "ThrottlingException" ("Rate exceeded\n" <> canonicalRequestMessage) `shouldBe` "SQS: HTTP 403 Throttling (request req-1): Rate exceeded"

        it "details no response header" $
            for_ [("SignatureDoesNotMatch", canonicalRequestMessage), ("AccessDenied", accessDenial)] $ \(code, message) ->
                detailOf code message `shouldNotSatisfy` T.isInfixOf (decodeUtf8 (snd plantedHeader))

    describe "retypeAwsFailure" $ do
        it "raises a refused connection by its transport cause at the dialled host:port, never the request" $
            retypeAwsFailure (throwIO =<< refusedFetch =<< signedRequest) `shouldThrow` refusedAtClosedPort

        it "raises a credential endpoint's status at the dialled host:port, never the request or the answer" $ do
            (authority, err) <- redirectedStatusException status404
            outcome <- try (retypeAwsFailure (throwIO err))
            case outcome of
                Left failed -> do
                    arfFault failed `shouldBe` AwsHttp (HttpStatus 404)
                    displayException failed `shouldBe` toString ("AWS request failed at " <> authority <> ": HTTP 404")
                    for_ redirectSecrets $ \secret -> toText (displayException failed) `shouldNotSatisfy` T.isInfixOf secret
                Right () -> expectationFailure "the credential endpoint's status was not raised"

        it "raises a service's refusal by the service's name, as discovery holds no request" $
            retypeAwsFailure (throwIO (plantedRefusal "STS" "AccessDenied" accessDenial))
                `shouldThrow` (== AwsRequestFailed (AwsServiceNamed "STS") (AwsStatus (AwsAnswer 403 "AccessDenied" (Just "req-1") (Just accessDenial))))

        it "raises an exhausted credential chain as a credential fault" $
            retypeAwsFailure (throwIO CredentialChainExhausted) `shouldThrow` (== AwsCredentialsExhausted)

    describe "displayAwsFaultT" $ do
        it "renders each failure a background refresh hands back as the request's typed cause, never the request" $ do
            failures <- refreshFailures
            for_ failures $ \failure -> do
                let rendered = displayAwsFaultT (toException failure)
                rendered `shouldSatisfy` T.isPrefixOf "AWS request failed at "
                rendered `shouldSatisfy` holdsNoPlantedSecret

        it "renders a credential failure without a request by amazonka's first line only" $
            map (displayAwsFaultT . toException) [MissingEnvError "Unable to read AWS_ROLE_ARN", MissingFileError "/root/.aws/credentials", InvalidFileError "/root/.aws/config: parse error\naws_session_token = token-secret", InvalidIAMError "Unable to get default IAM Profile from EC2 metadata", CredentialChainExhausted]
                `shouldBe` [ "AWS credentials missing: Unable to read AWS_ROLE_ARN"
                           , "AWS credentials missing: /root/.aws/credentials"
                           , "AWS credentials unusable: /root/.aws/config: parse error"
                           , "AWS credentials unusable: Unable to get default IAM Profile from EC2 metadata"
                           , "AWS credential discovery found no credentials"
                           ]

    describe "retypeAmazonkaEscapes" $ do
        it "raises a refresh failure thrown to the thread as its typed fault" $ do
            failures <- refreshFailures
            for_ failures $ \failure ->
                retypeAmazonkaEscapes (deliverRefreshFailure failure) `shouldThrow` (holdsNoPlantedSecret . toText . displayException @AwsRequestFailed)

        it "passes an http-client failure unchanged, so a fetch outside AWS is not named an AWS request" $
            retypeAmazonkaEscapes (throwIO =<< refusedFetch =<< signedRequest) `shouldThrow` (const True :: HttpException -> Bool)
  where
    detailOf code message = tfDetail (classifyAwsTransport (plantedRefusal "SQS" code message))
    signatureCode code = fromMaybe code (T.stripSuffix "Exception" code)
    accessDenial = "User: arn:aws:sts::123456789012:assumed-role/ecluse is not authorized to perform: sqs:ReceiveMessage on resource: arn:aws:sqs:us-east-1:123456789012:mirror"
