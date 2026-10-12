-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Wire tests use static credentials and an in-process S3 endpoint.
module Ecluse.Runtime.Mirror.State.S3.InternalSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)
import Data.ByteString qualified as BS
import Network.HTTP.Types
import Network.Wai (Application, queryString, requestHeaders, requestMethod, responseLBS, responseStream, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Security (Limits (..), defaultLimits, mkProgressFloor)
import Ecluse.Runtime.Mirror.State.S3.Internal
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "conditional mirror metadata storage" $ do
    it "signs create-if-absent and preserves the exact body" $ do
        observed <- newIORef Nothing
        withS3
            ( \request respond -> do
                bytes <- strictRequestBody request
                writeIORef observed (Just (requestHeaders request, bytes))
                respond (responseLBS status200 [(hETag, "\"one\"")] "")
            )
            $ \env -> do
                writeObject env limits "records" "prefix/project" Nothing "record" `shouldReturn` Right (ObjectStored "\"one\"")
        readIORef observed >>= \case
            Nothing -> expectationFailure "no request reached the endpoint"
            Just (headers, bytes) -> do
                lookup hIfNoneMatch headers `shouldBe` Just "*"
                lookup hIfMatch headers `shouldBe` Nothing
                lookup hAuthorization headers `shouldSatisfy` maybe False (BS.isInfixOf "if-none-match")
                bytes `shouldBe` "record"

    it "signs revision-matched replacement"
        $ withS3
            ( \request respond -> do
                lookup hIfMatch (requestHeaders request) `shouldBe` Just "\"old\""
                lookup hIfNoneMatch (requestHeaders request) `shouldBe` Nothing
                lookup hAuthorization (requestHeaders request) `shouldSatisfy` maybe False (BS.isInfixOf "if-match")
                respond (responseLBS status200 [(hETag, "\"new\"")] "")
            )
        $ \env ->
            writeObject env limits "records" "key" (Just "\"old\"") "new" `shouldReturn` Right (ObjectStored "\"new\"")

    for_ [status409, status412] $ \status ->
        it ("returns conditional collision " <> show (statusCode status)) $
            withS3 (reply status [] (xmlError "PreconditionFailed")) $ \env ->
                writeObject env limits "records" "key" Nothing "record" `shouldReturn` Right ObjectConflict

    it "bounds XML error bodies before the SDK decoder sees them" $
        withS3 (reply status403 [] (xmlError "AccessDenied")) $ \env ->
            readObject env limits{maxMetadataBytes = 8} "records" "key" Nothing `shouldReturn` Left (ObjectTooLarge 8)

    it "refuses a response body stalled below the configured progress floor" $ do
        floorSpec <- expectRight (mkProgressFloor 1 0.1 1)
        withS3
            ( \_ respond ->
                respond
                    ( responseStream
                        status200
                        [(hETag, "\"one\"")]
                        ( \write flush -> do
                            write "x"
                            flush
                            threadDelay 500_000
                            write "y"
                        )
                    )
            )
            $ \env ->
                readObject env limits{progressFloor = floorSpec} "records" "key" Nothing >>= (`shouldSatisfy` isLeft)

    it "refuses slow body drips rather than treating each chunk as enough progress" $ do
        floorSpec <- expectRight (mkProgressFloor 1 0.1 8)
        withS3
            ( \_ respond ->
                respond
                    ( responseStream
                        status200
                        [(hETag, "\"one\"")]
                        ( \write flush ->
                            replicateM_ 12 (write "x" >> flush >> threadDelay 50_000)
                        )
                    )
            )
            $ \env ->
                readObject env limits{progressFloor = floorSpec} "records" "key" Nothing >>= (`shouldSatisfy` isLeft)

    it "allows a progressing background transfer past the foreground serve cap" $ do
        floorSpec <- expectRight (mkProgressFloor 0.3 0.1 1)
        withS3
            ( \_ respond ->
                respond
                    ( responseStream
                        status200
                        [(hETag, "\"one\"")]
                        ( \write flush ->
                            replicateM_ 12 (write "x" >> flush >> threadDelay 50_000)
                        )
                    )
            )
            $ \env ->
                readObject env limits{progressFloor = floorSpec} "records" "key" Nothing `shouldReturn` Right (ObjectLoaded "\"one\"" "xxxxxxxxxxxx")

    it "requires an ETag after a successful write" $
        withS3 (reply status200 [] "") $ \env ->
            writeObject env limits "records" "key" Nothing "record" `shouldReturn` Left ObjectNoRevision

    it "returns a loaded object's own revision" $
        withS3 (reply status200 [(hETag, "\"one\"")] "record") $ \env ->
            readObject env limits "records" "key" Nothing `shouldReturn` Right (ObjectLoaded "\"one\"" "record")

    it "reuses only the revision sent with a conditional GET"
        $ withS3
            ( \request respond -> do
                lookup hIfNoneMatch (requestHeaders request) `shouldBe` Just "\"one\""
                respond (responseLBS status304 [] "")
            )
        $ \env ->
            readObject env limits "records" "key" (Just "\"one\"") `shouldReturn` Right (ObjectUnchanged "\"one\"")

    it "refuses an unsolicited 304" $
        withS3 (reply status304 [] "") $ \env ->
            readObject env limits "records" "key" Nothing `shouldReturn` Left ObjectNoRevision

    it "requires a loaded object's revision" $
        withS3 (reply status200 [] "record") $ \env ->
            readObject env limits "records" "key" Nothing `shouldReturn` Left ObjectNoRevision

    it "enforces the configured body cap" $
        withS3 (reply status200 [(hETag, "\"one\"")] "123456789") $ \env ->
            readObject env limits{maxMetadataBytes = 8} "records" "key" Nothing `shouldReturn` Left (ObjectTooLarge 8)

    it "accepts exactly the configured body cap" $
        withS3 (reply status200 [(hETag, "\"one\"")] "12345678") $ \env ->
            readObject env limits{maxMetadataBytes = 8} "records" "key" Nothing `shouldReturn` Right (ObjectLoaded "\"one\"" "12345678")

    it "recognises NoSuchKey as conclusive absence" $
        withS3 (reply status404 [] (xmlError "NoSuchKey")) $ \env ->
            readObject env limits "records" "key" Nothing `shouldReturn` Right ObjectAbsent

    it "does not treat a missing bucket as an absent record" $
        withS3 (reply status404 [] (xmlError "NoSuchBucket")) $ \env ->
            readObject env limits "records" "key" Nothing >>= (`shouldSatisfy` isLeft)

    it "uses one exact-prefix listing to disambiguate a refused GET"
        $ withS3
            ( \request respond ->
                if null (queryString request)
                    then respond (responseLBS status403 [] (xmlError "AccessDenied"))
                    else do
                        lookup "prefix" (queryString request) `shouldBe` Just (Just "key")
                        lookup "max-keys" (queryString request) `shouldBe` Just (Just "1")
                        respond (responseLBS status200 [] emptyList)
            )
        $ \env ->
            readObject env limits "records" "key" Nothing `shouldReturn` Right ObjectAbsent

    it "does not infer absence when listing is refused" $
        withS3 (reply status403 [] (xmlError "AccessDenied")) $ \env ->
            readObject env limits "records" "key" Nothing >>= (`shouldSatisfy` isLeft)

    it "does not infer absence from a truncated listing" $
        withS3 (listingReply "<ListBucketResult><IsTruncated>true</IsTruncated></ListBucketResult>") $ \env ->
            readObject env limits "records" "key" Nothing `shouldReturn` Left ObjectUnknownAbsence

    it "does not infer absence from a malformed listing" $
        withS3 (listingReply "broken") $ \env ->
            readObject env limits "records" "key" Nothing >>= (`shouldSatisfy` isLeft)

    it "rejects an oversized write before opening the endpoint" $
        withS3 (\_ _ -> expectationFailure "unexpected write" >> fail "unexpected write") $ \env ->
            writeObject env limits{maxMetadataBytes = 8} "records" "key" Nothing "123456789" `shouldReturn` Left (ObjectTooLarge 8)

limits :: Limits
limits = defaultLimits{maxMetadataBytes = 1024}

withS3 :: Application -> (AWS.Env -> IO ()) -> IO ()
withS3 application action = testWithApplication (pure application) $ \port -> do
    base <- AWS.newEnv (pure . fromKeys (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey"))
    let env = AWS.overrideService (\service -> AWS.setEndpoint False "127.0.0.1" port service{AWS.s3AddressingStyle = AWS.S3AddressingStylePath}) base
    action (AWS.once env{AWS.retryCheck = \_ _ -> False})

reply :: Status -> ResponseHeaders -> LByteString -> Application
reply status headers bytes _ respond = respond (responseLBS status headers bytes)

listingReply :: LByteString -> Application
listingReply bytes request respond =
    if requestMethod request == "GET" && null (queryString request)
        then respond (responseLBS status403 [] (xmlError "AccessDenied"))
        else respond (responseLBS status200 [] bytes)

xmlError :: LByteString -> LByteString
xmlError code = "<Error><Code>" <> code <> "</Code><Message>refused</Message></Error>"

emptyList :: LByteString
emptyList = "<ListBucketResult><IsTruncated>false</IsTruncated><KeyCount>0</KeyCount></ListBucketResult>"
