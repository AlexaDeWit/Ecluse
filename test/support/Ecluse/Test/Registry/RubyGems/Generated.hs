-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Generated native declarations record source meaning before independent wire encoding and framing.
module Ecluse.Test.Registry.RubyGems.Generated (GeneratedInfo (..), genRubyInfo) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..))

-- | Expected rows retain original physical positions and bytes without production parsers or constructors.
data GeneratedInfo = GeneratedInfo
    { generatedName :: Text
    , generatedBase :: Text
    , generatedBody :: ByteString
    , generatedHeader :: ByteString
    , generatedRows :: [(RubyInfoRow, ByteString)]
    , generatedSelection :: Text
    , generatedFinalLf :: Bool
    }
    deriving stock (Show)

-- | Vary platform groups, native fields, opaque headers, blank positions and the final line terminator.
genRubyInfo :: Gen GeneratedInfo
genRubyInfo = do
    name <- Gen.element ["native-gem", "Case_Name", "dot.name", ".legacy", "under_score"]
    base <- Gen.element ["https://rubygems.org", "https://registry.test/native"]
    versions <- take <$> Gen.element [1, 2, 3, 4] <*> Gen.shuffle ["0.9.0", "1.0.0", "2.4.1.pre.2", "3.2.0.rc.1", "5.0.0", "6.0"]
    native <- concat <$> traverse versionRows versions
    ordered <- Gen.shuffle native
    physical <- concat <$> traverse withBlanks ordered
    headers <- Gen.list (Range.linear 0 3) (Gen.element (["opaque: metadata", "name: foreign", "created_at: 1999-12-31T23:59:59Z", "opaque \x1f600"] :: [Text]))
    finalLf <- Gen.bool
    selected <- Gen.frequency [(4, Gen.element versions), (1, pure "99.99.99")]
    let headerLines = map encodeUtf8 headers <> ["---"]
        dataLines = map (maybe "" encodeRow) physical
        numbered = [(row{rowPosition = position}, encodeRow row) | (position, Just row) <- zip [length headerLines ..] physical]
        body = BS.intercalate "\n" (headerLines <> dataLines) <> if finalLf then "\n" else ""
    pure (GeneratedInfo name base body (BS.intercalate "\n" headerLines <> "\n") numbered selected finalLf)
  where
    versionRows version = do
        platforms <- take <$> Gen.element [1, 2, 3] <*> Gen.shuffle ["ruby", "java", "x86_64-linux", "x86_64-linux-musl", "arm64-darwin-21", "x64-mingw-ucrt"]
        traverse (nativeRow version) platforms
    withBlanks row = do
        count <- Gen.element [0, 1, 2]
        pure (replicate count Nothing <> [Just row])

nativeRow :: Text -> Text -> Gen RubyInfoRow
nativeRow version platform = do
    names <- take <$> Gen.element [0, 1, 2, 3] <*> Gen.shuffle ["child-one", "child_two", "Child.Three"]
    dependencies <- traverse (\name -> (name,) <$> clauses) names
    checksum <- Gen.text (Range.singleton 64) (Gen.element (['0' .. '9'] <> ['a' .. 'f']))
    ruby <- optionalField "ruby" (T.intercalate "&" <$> clauses)
    gems <- optionalField "rubygems" (T.intercalate "&" <$> clauses)
    published <- optionalField "created_at" (Gen.element ["2020-01-01T00:00:00Z", "2026-01-02T03:04:05Z", "2026-10-27T12:34:56.125Z"])
    extra1 <- optionalField "extra1" (Gen.element ["opaque", "caf\xe9", "owned \x1f600"])
    extra2 <- optionalField "extra2" (Gen.element ["retained", "v2.0", ""])
    fields <- Gen.shuffle (("checksum", checksum) : ruby <> gems <> published <> extra1 <> extra2)
    pure (RubyInfoRow 0 version platform dependencies fields)
  where
    optionalField key values = do
        present <- Gen.bool
        value <- values
        pure [(key, value) | present]
    clauses = do
        count <- Gen.element [1, 2, 3]
        Gen.list (Range.singleton count) (Gen.element ["= 1.0.0", ">= 0", ">=1.0", "!= 1.2.3", "~> 2.0", "< 9.0", "> 0.1", "<= 3.4"])

encodeRow :: RubyInfoRow -> ByteString
encodeRow row =
    encodeUtf8 $
        rowVersion row
            <> (if rowPlatform row == "ruby" then "" else "-" <> rowPlatform row)
            <> " "
            <> T.intercalate "," [name <> ":" <> T.intercalate "&" requirement | (name, requirement) <- rowDependencies row]
            <> "|"
            <> T.intercalate "," [key <> ":" <> value | (key, value) <- rowFields row]
