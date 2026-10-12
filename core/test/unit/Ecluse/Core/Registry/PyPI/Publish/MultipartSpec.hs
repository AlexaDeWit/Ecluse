-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Multipart identity attacks across the entire preserved relay body.
module Ecluse.Core.Registry.PyPI.Publish.MultipartSpec (spec) where

import Test.Hspec

import Ecluse.Core.Registry.PyPI.Publish.Multipart (declaredNames)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)

spec :: Spec
spec = describe "declaredNames" $ do
    it "accepts repeated canonically equal names, including after the file" $
        decode (part "name" "Example_Pkg" <> filePart <> part "name" "example-pkg") `shouldReturn` Right ["example-pkg"]
    it "rejects a conflicting claim after the file" $
        decode (part "name" "allowed" <> filePart <> part "name" "forbidden") >>= (`shouldSatisfy` isLeft)
    for_ ["", "bad/name", "bad\nname"] $ \name ->
        it ("rejects an invalid name: " <> show name) $
            decode (part "name" name) >>= (`shouldSatisfy` isLeft)
    it "rejects a missing name and a file named as the name claim" $ do
        decode filePart >>= (`shouldSatisfy` isLeft)
        decode "--boundary\r\nContent-Disposition: form-data; name=\"name\"; filename=\"name.txt\"\r\n\r\nallowed\r\n" >>= (`shouldSatisfy` isLeft)
    it "requires the supplied Content-Type boundary" $ do
        declaredNames defaultLimits Nothing (part "name" "allowed" <> end) >>= (`shouldSatisfy` isLeft)
        declaredNames defaultLimits (Just "multipart/form-data; boundary=other") (part "name" "allowed" <> end) >>= (`shouldSatisfy` isLeft)
    it "rejects conflicting boundary and disposition parameters" $ do
        declaredNames defaultLimits (Just "multipart/form-data; boundary=boundary; boundary=other") (part "name" "allowed" <> end) >>= (`shouldSatisfy` isLeft)
        decode "--boundary\r\nContent-Disposition: form-data; name=\"name\"; name=\"ignored\"\r\n\r\nallowed\r\n" >>= (`shouldSatisfy` isLeft)
    it "rejects a missing closing delimiter and trailing claims" $ do
        declaredNames defaultLimits contentType (part "name" "allowed") >>= (`shouldSatisfy` isLeft)
        decode (part "name" "allowed" <> end <> part "name" "forbidden") >>= (`shouldSatisfy` isLeft)
    it "checks late claims after an inline fake closing delimiter in file bytes" $ do
        let fakeFile = "--boundary\r\nContent-Disposition: form-data; name=\"content\"; filename=\"a.whl\"\r\n\r\nx--boundary--\r\n\r\nfile bytes\r\n"
        decode (part "name" "allowed" <> fakeFile <> part "name" "forbidden") >>= (`shouldSatisfy` isLeft)
        decode (part "name" "allowed" <> fakeFile <> part "name" "") >>= (`shouldSatisfy` isLeft)
        decode (part "name" "allowed" <> fakeFile <> part "name" "allowed") `shouldReturn` Right ["allowed"]
    it "rejects a late MIME quoted-pair spelling of the name attribute" $
        decode (part "name" "allowed" <> "--boundary\r\nContent-Disposition: form-data; name=\"na\\me\"\r\n\r\nforbidden\r\n") >>= (`shouldSatisfy` isLeft)
    it "accepts a valid name containing the inbound delimiter text" $
        declaredNames defaultLimits (Just "multipart/form-data; boundary=x") "--x\r\nContent-Disposition: form-data; name=\"name\"\r\n\r\nallowed--x\r\n--x--\r\n" `shouldReturn` Right ["allowed-x"]
    it "retains all claims when parsing crosses its chunk boundary" $ do
        let names = mconcat (replicate 600 (part "name" "allowed"))
        decode (names <> filePart <> part "name" "forbidden") >>= (`shouldSatisfy` isLeft)
        decode (names <> filePart <> part "name" "allowed") `shouldReturn` Right ["allowed"]
    it "enforces byte and part limits" $ do
        declaredNames defaultLimits{maxPublishRequestBytes = 10} contentType (part "name" "allowed" <> end) >>= (`shouldSatisfy` isLeft)
        declaredNames defaultLimits{maxArtifactCount = 1} contentType (part "name" "allowed" <> filePart <> end) >>= (`shouldSatisfy` isLeft)
  where
    decode body = declaredNames defaultLimits contentType (body <> end)

contentType :: Maybe ByteString
contentType = Just "multipart/form-data; boundary=\"boundary\""

end :: ByteString
end = "--boundary--\r\n"

part :: ByteString -> ByteString -> ByteString
part name value = "--boundary\r\nContent-Disposition: form-data; name=\"" <> name <> "\"\r\n\r\n" <> value <> "\r\n"

filePart :: ByteString
filePart = "--boundary\r\nContent-Disposition: form-data; name=\"content\"; filename=\"a.whl\"\r\nContent-Type: application/octet-stream\r\n\r\nPK archive bytes\r\n"
