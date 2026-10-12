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
import Ecluse.Core.Registry.PyPI.UploadMetadata (parseUploadMetadata, uploadFields)
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
    it "rejects conflicting description representations" $
        parse (base <> "Description: first\n\nsecond\n") `shouldSatisfy` isLeft
  where
    name = mkPackageName PyPI Nothing "example-pkg"
    version = mkVersion PyPI "1"
    parse = parseUploadMetadata 16384 name version

base :: ByteString
base = "Metadata-Version: 2.4\nName: Example_Pkg\nVersion: 1.0\n"
