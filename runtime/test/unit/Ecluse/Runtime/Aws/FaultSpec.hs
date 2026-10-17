-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How an @amazonka@ failure leaves an adapter, as a value or raised: never with the request, a response header or a quoted signature.
module Ecluse.Runtime.Aws.FaultSpec (spec) where

import Amazonka qualified as AWS
import Data.Text qualified as T
import Network.HTTP.Types.Status (status200, status404)
import Test.Hspec
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Fault (HttpFault (HttpStatus), TransportFault (tfDetail))
import Ecluse.Runtime.Aws.Fault (
    AwsAnswer (AwsAnswer),
    AwsFault (AwsHttp, AwsStatus, AwsUndecodable),
    AwsParty (AwsServiceNamed),
    AwsRequestFailed (AwsRequestFailed, arfFault),
    classifyAwsTransport,
    retypeAwsFailure,
 )
import Ecluse.Runtime.Test.Aws (canonicalRequestMessage, plantedHeader, plantedRefusal, refusedAtClosedPort)
import Ecluse.Test.Stub (redirectSecrets, redirectedStatusException, refusedFetch, signedRequest)

spec :: Spec
spec = do
    describe "classifyAwsTransport" $ do
        for_ ["InvalidSignatureException", "SignatureDoesNotMatch", "IncompleteSignature", "signaturedoesnotmatch"] $ \code ->
            it ("details the " <> toString code <> " refusal by its code, status and request id, and none of the canonical request") $
                detailOf code canonicalRequestMessage `shouldBe` "SQS: HTTP 403 " <> signatureCode code <> " (request req-1)"

        it "keeps the first line of an access denial, which names the IAM action and resource" $
            detailOf "AccessDenied" accessDenial `shouldBe` "SQS: HTTP 403 AccessDenied (request req-1): " <> accessDenial

        it "keeps only the first line of another code's message" $
            detailOf "ThrottlingException" ("Rate exceeded\n" <> canonicalRequestMessage) `shouldBe` "SQS: HTTP 403 Throttling (request req-1): Rate exceeded"

        it "details no response header" $
            for_ [("SignatureDoesNotMatch", canonicalRequestMessage), ("AccessDenied", accessDenial)] $ \(code, message) ->
                detailOf code message `shouldNotSatisfy` T.isInfixOf (decodeUtf8 (snd plantedHeader))

        it "details a decoder failure by its status, never the body it read or the decoder's message" $
            tfDetail (classifyAwsTransport undecodableAnswer) `shouldBe` "STS: HTTP 200, an answer that could not be decoded"

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

        it "bounds a raised refusal's text to the detail budget the classified form keeps" $
            retypeAwsFailure (throwIO (plantedRefusal "STS" "AccessDenied" (T.replicate 2000 "x")))
                `shouldThrow` ((<= T.length "AWS request failed at " + 512) . T.length . toText . displayException @AwsRequestFailed)

        it "raises a decoder failure as its status, never the body it read or the decoder's message" $ do
            outcome <- try (retypeAwsFailure (throwIO undecodableAnswer))
            case outcome of
                Left failed -> do
                    failed `shouldBe` AwsRequestFailed (AwsServiceNamed "STS") (AwsUndecodable 200)
                    displayException failed `shouldBe` "AWS request failed at STS: HTTP 200, an answer that could not be decoded"
                Right () -> expectationFailure "the decoder failure was not raised"
  where
    -- An AssumeRole answer amazonka could not decode: its body holds the minted credentials.
    undecodableAnswer =
        AWS.SerializeError
            (AWS.SerializeError' "STS" status200 (Just "<AssumeRoleResponse><Credentials><SessionToken>token-secret</SessionToken></Credentials></AssumeRoleResponse>") "Failed reading: unexpected token-secret")
    detailOf code message = tfDetail (classifyAwsTransport (plantedRefusal "SQS" code message))
    signatureCode code = fromMaybe code (T.stripSuffix "Exception" code)
    accessDenial = "User: arn:aws:sts::123456789012:assumed-role/ecluse is not authorized to perform: sqs:ReceiveMessage on resource: arn:aws:sqs:us-east-1:123456789012:mirror"
