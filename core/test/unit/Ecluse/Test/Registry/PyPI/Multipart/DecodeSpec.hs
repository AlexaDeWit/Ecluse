-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Multipart identity extraction uses only the declared boundary and retains every name claim.
module Ecluse.Test.Registry.PyPI.Multipart.DecodeSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Registry.Adapter.Capability (PublishBody (..))
import Ecluse.Test.Registry.PyPI.Multipart (compatibleMultipartBodies, malformedMultipartEnvelopes, malformedMultipartParts, multipartBoundary, multipartField, multipartFile, multipartUpload)
import Ecluse.Test.Registry.PyPI.Multipart.Decode (declaredNames)

-- | Pin the public contract through deterministic inputs.
spec :: Spec
spec = describe "declaredNames" $ do
    it "reads repeated names and ignores a file containing name-like text" $
        declaredNames (multipartUpload [multipartField "name" "Acme.Widget", multipartField "name" "acme-widget", multipartFile "content" "name=public-package"]) `shouldReturn` ["Acme.Widget", "acme-widget"]
    it "decodes a quoted pair in a field name" $
        declaredNames (multipartUpload [multipartField "na\\me" "requests"]) `shouldReturn` ["requests"]
    it "keeps escaped quotes and semicolons inside parameter values" $
        declaredNames (multipartUpload [multipartField "ignored\\\";field" "opaque", multipartField "name" "requests"]) `shouldReturn` ["requests"]
    it "decodes a quoted pair in the declared boundary" $
        declaredNames ((multipartUpload [multipartField "name" "requests"]){publishContentType = Just "multipart/form-data; boundary=\"upload\\-boundary\""}) `shouldReturn` ["requests"]
    it "accepts a quoted boundary" $
        declaredNames ((multipartUpload [multipartField "name" "acme-widget"]){publishContentType = Just "multipart/form-data; boundary=\"upload-boundary\""}) `shouldReturn` ["acme-widget"]
    for_ [Nothing, Just "application/json", Just "multipart/form-data", Just "multipart/form-data; boundary=", Just "multipart/form-data; boundary=other", Just "multipart/form-data; boundary=upload-boundary; boundary=other", Just "multipart/form-data; boundary=upload-boundary; Boundary=other", Just "multipart/form-data; invalid; boundary=upload-boundary"] $ \header ->
        it ("refuses a missing or ambiguous declared boundary: " <> show header) $
            declaredNames ((multipartUpload [multipartField "name" "acme-widget"]){publishContentType = header}) `shouldReturn` []
    it "refuses a missing closing delimiter" $
        declaredNames ((multipartUpload []){publishBytes = multipartField "name" "acme-widget"}) `shouldReturn` []
    it "ignores epilogue bytes that resemble another part" $
        declaredNames ((multipartUpload []){publishBytes = publishBytes (multipartUpload [multipartField "name" "acme-widget"]) <> multipartField "name" "public-package"}) `shouldReturn` ["acme-widget"]
    for_ compatibleMultipartBodies $ \body ->
        it ("accepts the valid MIME envelope: " <> show body) $
            declaredNames body `shouldReturn` ["requests", "requests"]
    for_ malformedMultipartEnvelopes $ \body ->
        it ("refuses malformed boundary framing: " <> show body) $
            declaredNames body `shouldReturn` []
    it "does not recover names from an epilogue with no actual parts" $
        declaredNames ((multipartUpload []){publishBytes = publishBytes (multipartUpload []) <> multipartField "name" "requests"}) `shouldReturn` []
    it "refuses a truncated closing delimiter after padding" $
        declaredNames ((multipartUpload []){publishBytes = multipartField "name" "requests" <> "--upload-boundary-- \t\r"}) `shouldReturn` []

    it "preserves an inline marker inside a field" $
        declaredNames (multipartUpload [multipartField "name" "acme--upload-boundarywidget"]) `shouldReturn` ["acme--upload-boundarywidget"]
    it "preserves inline markers and longer delimiter-like lines in file bytes" $
        declaredNames (multipartUpload [multipartFile "content" ("inline--" <> multipartBoundary <> "\r\n--" <> multipartBoundary <> "-suffix"), multipartField "name" "requests"]) `shouldReturn` ["requests"]
    for_ malformedMultipartParts $ \malformed ->
        it ("refuses an ambiguous part before a later name: " <> show malformed) $
            declaredNames (multipartUpload [multipartField "name" "requests", malformed, multipartField "name" "public-project"]) `shouldReturn` []

    it "refuses invalid UTF-8 in any name" $
        declaredNames (multipartUpload [multipartField "name" "acme-widget", multipartField "name" (BS.pack [255])]) `shouldReturn` []
    it "refuses a file part claiming to be the name" $
        declaredNames (multipartUpload [multipartField "name" "acme-widget", multipartFile "name" "public-package"]) `shouldReturn` []
    it "retains an empty name for the shared guard to refuse" $
        declaredNames (multipartUpload [multipartField "name" ""]) `shouldReturn` [""]
    it "refuses excessive part headers" $
        declaredNames (multipartUpload ["--upload-boundary\r\n" <> BS.concat (replicate 34 "X-Extra: x\r\n") <> "\r\nx\r\n"]) `shouldReturn` []
    it "discards large file bytes while retaining the name" $
        declaredNames (multipartUpload [multipartFile "content" (BS.replicate 1000000 120), multipartField "name" "acme-widget"]) `shouldReturn` ["acme-widget"]
