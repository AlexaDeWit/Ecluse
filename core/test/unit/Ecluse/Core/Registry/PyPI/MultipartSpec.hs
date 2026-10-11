-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Multipart identity extraction uses only the declared boundary and retains every name claim.
module Ecluse.Core.Registry.PyPI.MultipartSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.Adapter.Capability (PublishBody (..))
import Ecluse.Core.Registry.PyPI.Multipart (declaredNames)
import Ecluse.Test.Registry.PyPI.Multipart (multipartField, multipartFile, multipartUpload)

-- | Pin the public contract through deterministic inputs.
spec :: Spec
spec = describe "declaredNames" $ do
    it "reads repeated names and ignores a file containing name-like text" $
        declaredNames (multipartUpload [multipartField "name" "Acme.Widget", multipartField "name" "acme-widget", multipartFile "content" "name=public-package"]) `shouldReturn` ["Acme.Widget", "acme-widget"]
    it "accepts a quoted boundary" $
        declaredNames ((multipartUpload [multipartField "name" "acme-widget"]){publishContentType = Just "multipart/form-data; boundary=\"upload-boundary\""}) `shouldReturn` ["acme-widget"]
    for_ [Nothing, Just "application/json", Just "multipart/form-data", Just "multipart/form-data; boundary=", Just "multipart/form-data; boundary=other", Just "multipart/form-data; boundary=upload-boundary; boundary=other", Just "multipart/form-data; boundary=upload-boundary; Boundary=other", Just "multipart/form-data; invalid; boundary=upload-boundary"] $ \header ->
        it ("refuses a missing or ambiguous declared boundary: " <> show header) $
            declaredNames ((multipartUpload [multipartField "name" "acme-widget"]){publishContentType = header}) `shouldReturn` []
    it "refuses a missing closing delimiter" $
        declaredNames ((multipartUpload []){publishBytes = multipartField "name" "acme-widget"}) `shouldReturn` []
    it "refuses data after the closing delimiter" $
        declaredNames ((multipartUpload []){publishBytes = publishBytes (multipartUpload [multipartField "name" "acme-widget"]) <> multipartField "name" "public-package"}) `shouldReturn` []
    it "refuses a boundary token inside a field" $
        declaredNames (multipartUpload [multipartField "name" "acme--upload-boundarywidget"]) `shouldReturn` []
    it "refuses invalid UTF-8 in any name" $
        declaredNames (multipartUpload [multipartField "name" "acme-widget", multipartField "name" (BS.pack [255])]) `shouldReturn` []
    it "refuses a file part claiming to be the name" $
        declaredNames (multipartUpload [multipartField "name" "acme-widget", multipartFile "name" "public-package"]) `shouldReturn` []
    it "retains an empty name for the shared guard to refuse" $
        declaredNames (multipartUpload [multipartField "name" ""]) `shouldReturn` [""]
    it "refuses excessive part headers through the typed parser failure" $
        declaredNames (multipartUpload ["--upload-boundary\r\n" <> BS.concat (replicate 34 "X-Extra: x\r\n") <> "\r\nx\r\n"]) `shouldReturn` []
    it "discards large file bytes while retaining the name" $
        declaredNames (multipartUpload [multipartFile "content" (BS.replicate 1000000 120), multipartField "name" "acme-widget"]) `shouldReturn` ["acme-widget"]
