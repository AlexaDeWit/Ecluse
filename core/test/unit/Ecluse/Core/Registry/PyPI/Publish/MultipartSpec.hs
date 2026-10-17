-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Multipart identity attacks and upload actions across the entire preserved relay body.
module Ecluse.Core.Registry.PyPI.Publish.MultipartSpec (spec) where

import Test.Hspec

import Ecluse.Core.Registry (PublishBodyFault (PublishUnsupported))
import Ecluse.Core.Registry.PyPI.Publish.Multipart (pypiFileUploadOnly, uploadClaims)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Registry.Publish (isMalformedBody)

spec :: Spec
spec = do
    namesSpec
    actionSpec

namesSpec :: Spec
namesSpec = describe "uploadClaims names" $ do
    it "accepts repeated canonically equal names, including after the file" $
        decode (part "name" "Example_Pkg" <> filePart <> part "name" "example-pkg") `shouldReturn` Right ["example-pkg"]
    it "rejects a conflicting claim after the file" $
        decode (part "name" "allowed" <> filePart <> part "name" "forbidden") >>= (`shouldSatisfy` isMalformedBody)
    for_ ["", "bad/name", "bad\nname"] $ \name ->
        it ("rejects an invalid name: " <> show name) $
            decode (part "name" name) >>= (`shouldSatisfy` isMalformedBody)
    it "rejects a missing name and a file named as the name claim" $ do
        decode filePart >>= (`shouldSatisfy` isMalformedBody)
        decode "--boundary\r\nContent-Disposition: form-data; name=\"name\"; filename=\"name.txt\"\r\n\r\nallowed\r\n" >>= (`shouldSatisfy` isMalformedBody)
    it "requires the supplied Content-Type boundary" $ do
        uploadClaims defaultLimits Nothing (fileUpload <> part "name" "allowed" <> end) >>= (`shouldSatisfy` isMalformedBody)
        uploadClaims defaultLimits (Just "multipart/form-data; boundary=other") (fileUpload <> part "name" "allowed" <> end) >>= (`shouldSatisfy` isMalformedBody)
    it "rejects conflicting boundary and disposition parameters" $ do
        uploadClaims defaultLimits (Just "multipart/form-data; boundary=boundary; boundary=other") (fileUpload <> part "name" "allowed" <> end) >>= (`shouldSatisfy` isMalformedBody)
        decode "--boundary\r\nContent-Disposition: form-data; name=\"name\"; name=\"ignored\"\r\n\r\nallowed\r\n" >>= (`shouldSatisfy` isMalformedBody)
    it "rejects a missing closing delimiter and trailing claims" $ do
        uploadClaims defaultLimits contentType (fileUpload <> part "name" "allowed") >>= (`shouldSatisfy` isMalformedBody)
        decode (part "name" "allowed" <> end <> part "name" "forbidden") >>= (`shouldSatisfy` isMalformedBody)
    it "checks late claims after an inline fake closing delimiter in file bytes" $ do
        let fakeFile = "--boundary\r\nContent-Disposition: form-data; name=\"content\"; filename=\"a.whl\"\r\n\r\nx--boundary--\r\n\r\nfile bytes\r\n"
        decode (part "name" "allowed" <> fakeFile <> part "name" "forbidden") >>= (`shouldSatisfy` isMalformedBody)
        decode (part "name" "allowed" <> fakeFile <> part "name" "") >>= (`shouldSatisfy` isMalformedBody)
        decode (part "name" "allowed" <> fakeFile <> part "name" "allowed") `shouldReturn` Right ["allowed"]
    it "rejects a late MIME quoted-pair spelling of the name attribute" $
        decode (part "name" "allowed" <> "--boundary\r\nContent-Disposition: form-data; name=\"na\\me\"\r\n\r\nforbidden\r\n") >>= (`shouldSatisfy` isMalformedBody)
    it "accepts a valid name containing the inbound delimiter text" $
        uploadClaims defaultLimits (Just "multipart/form-data; boundary=x") "--x\r\nContent-Disposition: form-data; name=\":action\"\r\n\r\nfile_upload\r\n--x\r\nContent-Disposition: form-data; name=\"name\"\r\n\r\nallowed--x\r\n--x--\r\n" `shouldReturn` Right ["allowed-x"]
    it "retains all claims when parsing crosses its chunk boundary" $ do
        let names = mconcat (replicate 600 (part "name" "allowed"))
        decode (names <> filePart <> part "name" "forbidden") >>= (`shouldSatisfy` isMalformedBody)
        decode (names <> filePart <> part "name" "allowed") `shouldReturn` Right ["allowed"]
    it "enforces byte and part limits" $ do
        uploadClaims defaultLimits{maxPublishRequestBytes = 10} contentType (fileUpload <> part "name" "allowed" <> end) >>= (`shouldSatisfy` isMalformedBody)
        uploadClaims defaultLimits{maxArtifactCount = 2} contentType (fileUpload <> part "name" "allowed" <> filePart <> end) >>= (`shouldSatisfy` isMalformedBody)
  where
    decode body = uploadClaims defaultLimits contentType (fileUpload <> body <> end)

actionSpec :: Spec
actionSpec = describe "uploadClaims :action" $ do
    it "relays one file_upload" $
        withAction [fileUpload] `shouldReturn` Right ["allowed"]
    it "refuses a missing or repeated action as malformed" $ do
        withAction [] >>= (`shouldSatisfy` isMalformedBody)
        withAction [fileUpload, fileUpload] >>= (`shouldSatisfy` isMalformedBody)
    for_ ["remove_pkg", "doc_upload", "submit"] $ \action ->
        it ("refuses " <> show action <> " as a write it does not relay") $
            withAction [part ":action" action] `shouldReturn` Left (PublishUnsupported pypiFileUploadOnly)
    it "refuses an action sent as a file part" $
        withAction ["--boundary\r\nContent-Disposition: form-data; name=\":action\"; filename=\"a\"\r\n\r\nfile_upload\r\n"] >>= (`shouldSatisfy` isMalformedBody)
  where
    withAction actions = uploadClaims defaultLimits contentType (mconcat actions <> part "name" "allowed" <> filePart <> end)

contentType :: Maybe ByteString
contentType = Just "multipart/form-data; boundary=\"boundary\""

end :: ByteString
end = "--boundary--\r\n"

part :: ByteString -> ByteString -> ByteString
part name value = "--boundary\r\nContent-Disposition: form-data; name=\"" <> name <> "\"\r\n\r\n" <> value <> "\r\n"

fileUpload :: ByteString
fileUpload = part ":action" "file_upload"

filePart :: ByteString
filePart = "--boundary\r\nContent-Disposition: form-data; name=\"content\"; filename=\"a.whl\"\r\nContent-Type: application/octet-stream\r\n\r\nPK archive bytes\r\n"
