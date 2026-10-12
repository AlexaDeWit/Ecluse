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
