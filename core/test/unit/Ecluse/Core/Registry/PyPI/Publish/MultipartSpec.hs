-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Multipart identity attacks, hidden delimiters, release file names and upload actions across the entire preserved relay body.
module Ecluse.Core.Registry.PyPI.Publish.MultipartSpec (spec) where

import Data.Text.Encoding qualified as TE
import Test.Hspec

import Ecluse.Core.Registry (ParseError (..), PublishBodyFault (PublishMalformed, PublishUnsupported))
import Ecluse.Core.Registry.PyPI.Publish.Multipart (pypiFileUploadOnly, pypiSdistNameForm, pypiVersionForm, uploadClaims)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Registry.Publish (isMalformedBody)

spec :: Spec
spec = do
    namesSpec
    delimiterSpec
    releaseSpec
    actionSpec

namesSpec :: Spec
namesSpec = describe "uploadClaims names" $ do
    it "accepts repeated canonically equal names, including after the file" $
        decode (part "name" "Example_Pkg" <> contentPart "example_pkg-1.0.tar.gz" <> part "name" "example-pkg") `shouldReturn` Right ["example-pkg"]
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
        let fakeFile = "--boundary\r\nContent-Disposition: form-data; name=\"content\"; filename=\"allowed-1.0.tar.gz\"\r\n\r\nx--boundary--\r\n\r\nfile bytes\r\n"
        decode (part "name" "allowed" <> fakeFile <> part "name" "forbidden") >>= (`shouldSatisfy` isMalformedBody)
        decode (part "name" "allowed" <> fakeFile <> part "name" "") >>= (`shouldSatisfy` isMalformedBody)
        decode (part "name" "allowed" <> fakeFile <> part "name" "allowed") `shouldReturn` Right ["allowed"]
    it "rejects a late MIME quoted-pair spelling of the name attribute" $
        decode (part "name" "allowed" <> "--boundary\r\nContent-Disposition: form-data; name=\"na\\me\"\r\n\r\nforbidden\r\n") >>= (`shouldSatisfy` isMalformedBody)
    it "accepts a valid name containing the inbound delimiter text" $
        uploadClaims defaultLimits (Just "multipart/form-data; boundary=x") "--x\r\nContent-Disposition: form-data; name=\":action\"\r\n\r\nfile_upload\r\n--x\r\nContent-Disposition: form-data; name=\"name\"\r\n\r\nallowed--x\r\n--x\r\nContent-Disposition: form-data; name=\"version\"\r\n\r\n1.0\r\n--x\r\nContent-Disposition: form-data; name=\"content\"; filename=\"allowed_x-1.0.tar.gz\"\r\n\r\narchive\r\n--x--\r\n" `shouldReturn` Right ["allowed-x"]
    it "retains all claims when parsing crosses its chunk boundary" $ do
        let names = mconcat (replicate 600 (part "name" "allowed"))
        decode (names <> filePart <> part "name" "forbidden") >>= (`shouldSatisfy` isMalformedBody)
        decode (names <> filePart <> part "name" "allowed") `shouldReturn` Right ["allowed"]
    it "enforces byte and part limits" $ do
        uploadClaims defaultLimits{maxPublishRequestBytes = 10} contentType (fileUpload <> part "name" "allowed" <> end) >>= (`shouldSatisfy` isMalformedBody)
        uploadClaims defaultLimits{multipartPartCeiling = 2} contentType (fileUpload <> part "name" "allowed" <> filePart <> end) >>= (`shouldSatisfy` isMalformedBody)
  where
    decode body = uploadClaims defaultLimits contentType (fileUpload <> part "version" "1.0" <> body <> end)

delimiterSpec :: Spec
delimiterSpec = describe "uploadClaims delimiters" $ do
    for_ [("a bare LF", "\n", ""), ("a bare CR", "\r", ""), ("a padded delimiter", "\r\n", "  ")] $ \(label, lineEnd, padding) ->
        it ("refuses a content part hidden after " <> label) $
            upload (contentData ("PK archive" <> lineEnd <> hiddenPart lineEnd padding)) >>= (`shouldSatisfy` isMalformedBody)
    it "refuses a delimiter at the start of a part's data" $
        upload (contentData (hiddenPart "\r\n" "")) >>= (`shouldSatisfy` isMalformedBody)
    it "refuses a padded closing delimiter" $
        uploadClaims defaultLimits contentType (fileUpload <> part "name" "allowed" <> part "version" "1.0" <> filePart <> "--boundary-- \r\n") >>= (`shouldSatisfy` isMalformedBody)
  where
    upload content = uploadClaims defaultLimits contentType (fileUpload <> part "name" "allowed" <> part "version" "1.0" <> content <> end)
    contentData = fileWith "allowed-1.0.tar.gz"
    hiddenPart lineEnd padding = "--boundary" <> padding <> lineEnd <> "Content-Disposition: form-data; name=\"content\"; filename=\"victim-9.0.tar.gz\"" <> lineEnd <> lineEnd <> "VICTIM"

releaseSpec :: Spec
releaseSpec = describe "uploadClaims file name" $ do
    for_ ["acme_widget-1.0.0-py3-none-any.whl", "Acme.Widget-1.0-py3-none-any.whl", "acme_widget-1.0.tar.gz"] $ \file ->
        it ("relays " <> show file <> " as acme-widget 1.0.0") $
            upload [part "version" "1.0.0", contentPart file] `shouldReturn` Right ["acme-widget"]
    it "relays the PEP 625 sdist of a dotted project" $
        release "acme.co-widget" "1" "acme_co_widget-1.tar.gz" `shouldReturn` Right ["acme-co-widget"]
    for_ [("acme-widget", "1.0", "acme-widget-1.0.tar.gz"), ("acme.co-widget", "1", "acme.co-widget-1.tar.gz"), ("acme", "1.0", "acme_1.0.tar.gz"), ("acme", "1.0", "acme.1.0.tar.gz"), ("acme", "1.0", "_acme-1.0.tar.gz"), ("acme-2x", "1.0", "acme-2x-1.0.tar.gz"), ("acme-widget", "1.0", "Acme_Widget-1.0.tar.gz"), ("acme-widget", "1.0", "acme_widget-1.0.zip"), ("acme-widget", "1.0", "acme_widget-1.0.tgz")] $ \(name, version, file) ->
        it ("refuses " <> show file <> " for " <> show name <> ", naming the PEP 625 sdist form") $
            release name version file `shouldReturn` Left (PublishMalformed (ParseError pypiSdistNameForm))
    for_ ["acme_widget-1.0.tar.gz", "acme_widget-1.0-py3-none-any.whl"] $ \file ->
        it ("names the version field when the declared version is not PEP 440, though " <> show file <> " is well named") $
            release "acme-widget" "nightly" file `shouldReturn` Left (PublishMalformed (ParseError pypiVersionForm))
    for_ [("1.0.post1", "acme_widget-1.0.post1.tar.gz"), ("1!1.0+local.1", "acme_widget-1!1.0+local.1-py3-none-any.whl")] $ \(version, file) ->
        it ("relays " <> show file <> " as acme-widget " <> show version) $
            upload [part "version" version, contentPart file] `shouldReturn` Right ["acme-widget"]
    for_ [("another project", "victim_target-1.0.0.tar.gz"), ("a longer project", "acme_widget_extra-1.0.0.tar.gz"), ("another version", "acme_widget-99.0-py3-none-any.whl"), ("no distribution", "acme-widget-1.0.0.txt"), ("no release", "acme_widget.tar.gz")] $ \(label, file) ->
        it ("refuses a file name with " <> label <> ": " <> show file) $
            upload [part "version" "1.0.0", contentPart file] >>= (`shouldSatisfy` isMalformedBody)
    for_ [("acme", "1.post1", "acme-1-1.tar.gz"), ("acme", "1", "acme-v1-2-py3-none-any.whl"), ("acme-widget", "1.0", "acme_widget-v1.0.tar.gz")] $ \(name, version, file) ->
        it ("refuses the legacy spelling " <> show file <> ", which a store files under another project or version") $
            release name version file >>= (`shouldSatisfy` isMalformedBody)
    for_ [("a path", "acme_widget-1.0-x-y/victim-9.0.whl"), ("a traversal", "acme_widget-1.0-a-b-c/../../../x.whl"), ("a space before the version", "acme_widget- 1.0.tar.gz"), ("a space after the version", "acme_widget-1.0 .tar.gz"), ("a tab after the version", "acme_widget-1.0\t.tar.gz"), ("a percent sign", "acme_widget-1.0-py3-none-x%2F..%2Fy.whl")] $ \(label, file) ->
        it ("refuses a file name with " <> label <> ": " <> show file) $
            upload [part "version" "1.0", contentPart file] >>= (`shouldSatisfy` isMalformedBody)
    it "refuses a non-ASCII project spelling that folds to the declared one" $
        uploadClaims defaultLimits contentType (fileUpload <> part "name" "kelvin" <> part "version" "1.0" <> contentPart (TE.encodeUtf8 "\8490elvin-1.0.tar.gz") <> end) >>= (`shouldSatisfy` isMalformedBody)
    it "refuses a missing, repeated, or non-PEP 440 version" $
        for_ [[], [part "version" "1.0.0", part "version" "1.0.0"], [part "version" "latest"]] $ \versions ->
            upload (versions <> [contentPart "acme_widget-1.0.0.tar.gz"]) >>= (`shouldSatisfy` isMalformedBody)
    it "refuses no content part, two content parts, and a content part with no file name" $
        for_ [[], [contentPart "acme_widget-1.0.0.tar.gz", contentPart "acme_widget-1.0.0-py3-none-any.whl"], [part "content" "archive"]] $ \files ->
            upload (part "version" "1.0.0" : files) >>= (`shouldSatisfy` isMalformedBody)
    it "refuses a version sent as a file part" $
        upload ["--boundary\r\nContent-Disposition: form-data; name=\"version\"; filename=\"1.0.0\"\r\n\r\n1.0.0\r\n", contentPart "acme_widget-1.0.0.tar.gz"] >>= (`shouldSatisfy` isMalformedBody)
  where
    upload parts = uploadClaims defaultLimits contentType (fileUpload <> part "name" "acme-widget" <> mconcat parts <> end)
    release name version file = uploadClaims defaultLimits contentType (fileUpload <> part "name" name <> part "version" version <> contentPart file <> end)

actionSpec :: Spec
actionSpec = describe "uploadClaims :action" $ do
    it "relays one file_upload" $
        withAction [fileUpload] `shouldReturn` Right ["allowed"]
    it "refuses a missing or repeated action as malformed" $ do
        withAction [] >>= (`shouldSatisfy` isMalformedBody)
        withAction [fileUpload, fileUpload] >>= (`shouldSatisfy` isMalformedBody)
    for_ ["remove_pkg", "doc_upload", "submit", "File_Upload", " file_upload"] $ \action ->
        it ("refuses " <> show action <> " as a write it does not relay") $
            withAction [part ":action" action] `shouldReturn` Left (PublishUnsupported pypiFileUploadOnly)
    it "reads a field named :Action as no action" $ do
        missing <- withAction []
        withAction [part ":Action" "file_upload"] `shouldReturn` missing
    it "refuses an action sent as a file part" $
        withAction ["--boundary\r\nContent-Disposition: form-data; name=\":action\"; filename=\"a\"\r\n\r\nfile_upload\r\n"] >>= (`shouldSatisfy` isMalformedBody)
    it "answers 501 for another action whatever its other parts hold" $
        uploadClaims defaultLimits contentType (part ":action" "remove_pkg" <> "--boundary\r\nContent-Disposition: form-data; name=\"name\"; filename=\"a\"\r\n\r\nallowed\r\n" <> part "content" "no file name" <> end)
            `shouldReturn` Left (PublishUnsupported pypiFileUploadOnly)
  where
    withAction actions = uploadClaims defaultLimits contentType (mconcat actions <> part "name" "allowed" <> part "version" "1.0" <> filePart <> end)

contentType :: Maybe ByteString
contentType = Just "multipart/form-data; boundary=\"boundary\""

end :: ByteString
end = "--boundary--\r\n"

part :: ByteString -> ByteString -> ByteString
part name value = "--boundary\r\nContent-Disposition: form-data; name=\"" <> name <> "\"\r\n\r\n" <> value <> "\r\n"

fileUpload :: ByteString
fileUpload = part ":action" "file_upload"

-- The release of the project "allowed" at version 1.0.
filePart :: ByteString
filePart = contentPart "allowed-1.0-py3-none-any.whl"

contentPart :: ByteString -> ByteString
contentPart file = fileWith file "PK archive bytes"

fileWith :: ByteString -> ByteString -> ByteString
fileWith file bytes = "--boundary\r\nContent-Disposition: form-data; name=\"content\"; filename=\"" <> file <> "\"\r\nContent-Type: application/octet-stream\r\n\r\n" <> bytes <> "\r\n"
