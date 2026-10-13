-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Core Metadata identity and author-field preservation regressions.
module Ecluse.Core.Registry.PyPI.UploadMetadataSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.PyPI.UploadMetadata (parseUploadMetadata, uploadFields, uploadMetadataMemoryBytes)
import Ecluse.Core.Version (mkVersion)

spec :: Spec
spec = describe "parseUploadMetadata" $ do
    it "preserves repeated dependencies, markers, extras, licences, and dynamic declarations" $ do
        let metadata = base <> "Requires-Dist: a>=1; python_version < '3.12'\nRequires-Dist: b; extra == 'test'\nProvides-Extra: test\nLicense-File: LICENSE\nLicense-Expression: MIT\nDynamic: Requires-Dist\n"
        fmap uploadFields (parse metadata)
            `shouldBe` Right
                [ ("metadata_version", "2.4")
                , ("name", "Example_Pkg")
                , ("version", "1.0")
                , ("requires_dist", "a>=1; python_version < '3.12'")
                , ("requires_dist", "b; extra == 'test'")
                , ("provides_extra", "test")
                , ("license_file", "LICENSE")
                , ("license_expression", "MIT")
                , ("dynamic", "Requires-Dist")
                ]
    it "preserves author spelling and a description body with CRLF" $
        fmap uploadFields (parse (base <> "Author: Jane Doe, Team\nClassifier: Development Status :: 5 - Production/Stable\n\nA description\r\nwith two lines.\r\n"))
            `shouldSatisfy` either (const False) (elem ("description", "A description\r\nwith two lines.\r\n"))
    it "keeps multiline licence text and repeated project URLs" $
        fmap uploadFields (parse (base <> "License: first line\n second line\nProject-URL: Source, https://example.test/source\nProject-URL: Docs, https://example.test/docs\n"))
            `shouldSatisfy` either (const False) (elem ("license", "first line\n second line"))
    for_ ["Name: other\n", "Version: 2\n", "Metadata-Version: 2.4\n", "Summary: a\nSummary: b\n", "Dynamic: Name\n", "No colon\n", "Unknown-Field: value\n"] $ \suffix ->
        it ("refuses malformed or ambiguous metadata: " <> show suffix) $
            parse (base <> suffix) `shouldSatisfy` isLeft
    it "rejects missing required metadata" $
        parse "Name: example-pkg\nVersion: 1\n" `shouldSatisfy` isLeft
    it "rejects invalid UTF-8 and oversized metadata" $ do
        parse (base <> BS.pack [255]) `shouldSatisfy` isLeft
        parseUploadMetadata 4 name version base `shouldSatisfy` isLeft
    it "decodes the specified legacy Description fold while preserving description-body pipes" $ do
        fmap uploadFields (parse (base <> "Description: first\r\n       |second\r\n       | third\r\n"))
            `shouldSatisfy` either (const False) (elem ("description", "first\r\nsecond\r\n third"))
        fmap uploadFields (parse (base <> "\nfirst\n       |literal body pipe\n"))
            `shouldSatisfy` either (const False) (elem ("description", "first\n       |literal body pipe\n"))
    it "projects mixed-case headers and preserves repeated native upload fields in order" $
        fmap uploadFields (parse (base <> "sUmMaRy:\tOriginal spelling\nClassifier: First\nClassifier: Second\nProject-URL: Source, https://example.test/source\nProject-URL: Docs, https://example.test/docs\nImport-Name: example; private\nImport-Namespace: example.plugins\n"))
            `shouldBe` Right
                [ ("metadata_version", "2.4")
                , ("name", "Example_Pkg")
                , ("version", "1.0")
                , ("summary", "Original spelling")
                , ("classifiers", "First")
                , ("classifiers", "Second")
                , ("project_urls", "Source, https://example.test/source")
                , ("project_urls", "Docs, https://example.test/docs")
                , ("import_name", "example; private")
                , ("import_namespace", "example.plugins")
                ]
    it "preserves tab-folded values and accepts the final header without a newline" $
        fmap uploadFields (parse (base <> "Summary: first\r\n\tsecond"))
            `shouldBe` Right [("metadata_version", "2.4"), ("name", "Example_Pkg"), ("version", "1.0"), ("summary", "first\r\n\tsecond")]
    it "compares canonical identity without rewriting the author values" $ do
        let declared = "Metadata-Version: 2.4\nName: EXAMPLE.pkg\nVersion: 1.0.0\n"
        fmap uploadFields (parse declared)
            `shouldBe` Right [("metadata_version", "2.4"), ("name", "EXAMPLE.pkg"), ("version", "1.0.0")]
        parse "Metadata-Version: 2.4\nName: other\nVersion: 1\n"
            `shouldBe` Left (ParseError "Core Metadata names another package")
        parse "Metadata-Version: 2.4\nName: example-pkg\nVersion: 2\n"
            `shouldBe` Left (ParseError "Core Metadata names another version")
    for_
        [ (" Summary: orphan\n" <> base, "Core Metadata starts with a continuation")
        , (base <> "Summary: before\rafter\n", "bare carriage return in Core Metadata")
        , (base <> "Summary: final\r", "bare carriage return in Core Metadata")
        , (base <> "Summary: \NUL\n", "Core Metadata contains a control character")
        , (base <> "Summary: \DEL\n", "Core Metadata contains a control character")
        , (base <> "Bad_Name: value\n", "malformed Core Metadata header")
        , (base <> ": value\n", "malformed Core Metadata header")
        , (base <> "Dynamic: Unknown-Field\n", "invalid Dynamic declaration")
        , ("Metadata-Version: 9.0\nName: example-pkg\nVersion: 1\n", "unsupported Core Metadata version")
        , ("Metadata-Version: 2.4\nName:\nVersion: 1\n", "missing or ambiguous Core Metadata field: name")
        ]
        $ \(bytes, reason) ->
            it ("classifies malformed metadata: " <> toString reason <> " " <> show bytes) $
                parse bytes `shouldBe` Left (ParseError reason)
    it "accepts the byte ceiling and refuses one byte beyond it or a disabled ceiling" $ do
        parseUploadMetadata (BS.length base) name version base `shouldBe` parse base
        for_ [0, -1, BS.length base - 1] $ \cap ->
            parseUploadMetadata cap name version base `shouldBe` Left (ParseError "Core Metadata exceeds its byte bound")
    it "bounds header lines separately from the total metadata bytes" $ do
        let header = "Summary: " <> BS.replicate (16384 - 9) 97
            decode = parseUploadMetadata 32768 name version
        fmap uploadFields (decode (base <> header <> "\n"))
            `shouldSatisfy` either (const False) (elem ("summary", decodeUtf8 (BS.drop 9 header)))
        decode (base <> header <> "a\n") `shouldBe` Left (ParseError "Core Metadata header line exceeds its bound")
    it "counts continuation lines toward the header ceiling" $ do
        let headers = base <> "Summary: first\n" <> BS.concat (replicate 65532 " continuation\n")
            decode = parseUploadMetadata (2 * 1024 * 1024) name version
        fmap (length . uploadFields) (decode headers) `shouldBe` Right 4
        decode (headers <> " continuation\n") `shouldBe` Left (ParseError "Core Metadata header count exceeds its bound")
    it "charges actual metadata bytes and header structures without expanding description lines" $ do
        let body = BS.replicate 65536 10
            opaque = BS.replicate 65536 65
        uploadMetadataMemoryBytes (base <> "\n" <> body) `shouldBe` uploadMetadataMemoryBytes (base <> "\n" <> opaque)
        uploadMetadataMemoryBytes (base <> "Classifier: a\nClassifier: b\n") `shouldSatisfy` either (const False) (> BS.length base)
    it "rejects conflicting description representations" $
        parse (base <> "Description: first\n\nsecond\n") `shouldSatisfy` isLeft
  where
    name = mkPackageName PyPI Nothing "example-pkg"
    version = mkVersion PyPI "1"
    parse = parseUploadMetadata 16384 name version

base :: ByteString
base = "Metadata-Version: 2.4\nName: Example_Pkg\nVersion: 1.0\n"
