-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | An in-process S3 endpoint with static credentials, for wire tests of the S3 adapters.
module Ecluse.Runtime.Test.S3 (
    withS3,
    reply,
    listingReply,
    xmlError,
    emptyListing,
    populatedListing,
    brokenResponse,
) where

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)
import Network.HTTP.Types (ResponseHeaders, Status, status200, status403, status500)
import Network.Wai (Application, queryString, requestMethod, responseLBS, responseRaw)
import Network.Wai.Handler.Warp (testWithApplication)

-- | Serve the application and hand the test a single-attempt env that signs against it.
withS3 :: Application -> (AWS.Env -> IO ()) -> IO ()
withS3 application action = testWithApplication (pure application) $ \port -> do
    base <- AWS.newEnv (pure . fromKeys (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey"))
    let env = AWS.overrideService (\service -> AWS.setEndpoint False "127.0.0.1" port service{AWS.s3AddressingStyle = AWS.S3AddressingStylePath}) base
    action (AWS.once env{AWS.retryCheck = \_ _ -> False})

-- | Answer every request alike.
reply :: Status -> ResponseHeaders -> LByteString -> Application
reply status headers bytes _ respond = respond (responseLBS status headers bytes)

-- | Refuse a plain GET with 403, and answer the fallback listing with these bytes.
listingReply :: LByteString -> Application
listingReply bytes request respond =
    if requestMethod request == "GET" && null (queryString request)
        then respond (responseLBS status403 [] (xmlError "AccessDenied"))
        else respond (responseLBS status200 [] bytes)

-- | An S3 error document with this code.
xmlError :: LByteString -> LByteString
xmlError code = "<Error><Code>" <> code <> "</Code><Message>refused</Message></Error>"

-- | A complete listing with no object.
emptyListing :: LByteString
emptyListing = "<ListBucketResult><IsTruncated>false</IsTruncated><KeyCount>0</KeyCount></ListBucketResult>"

-- | A complete listing holding one object under @key@.
populatedListing :: LByteString
populatedListing =
    "<ListBucketResult><IsTruncated>false</IsTruncated><KeyCount>1</KeyCount><Contents>"
        <> "<ETag>&quot;one&quot;</ETag><Size>8</Size><Key>key</Key>"
        <> "<StorageClass>STANDARD</StorageClass><LastModified>2026-10-12T00:00:00.000Z</LastModified>"
        <> "</Contents></ListBucketResult>"

-- | Send a raw status line and these header and body bytes, then close, so a response can break early.
brokenResponse :: ByteString -> ByteString -> Application
brokenResponse status body _ respond =
    respond
        ( responseRaw
            (\_ send -> send ("HTTP/1.1 " <> status <> "\r\nETag: \"one\"\r\nConnection: close\r\n" <> body))
            (responseLBS status500 [] "raw response required")
        )
