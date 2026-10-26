-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native input rows carry declared expectations, independent of production validation.
module Ecluse.Test.Registry.Cargo.Generated (CargoInputs (..), GeneratedIndex (..), NativeRow (..), genCargoIndex) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Ecluse.Test.Package (validSha256)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestRecord)
import Ecluse.Test.Registry.JsonBytes qualified as JsonBytes

-- | Depth boundary cases contain sound JSON. Other cases include row-local faults.
data CargoInputs = SoundRows | MixedRows deriving stock (Eq, Show)

-- | Physical lines and their terminators define the complete generated source.
data GeneratedIndex = GeneratedIndex
    { generatedBody :: ByteString
    , generatedLines :: [NativeRow]
    , generatedSelection :: Text
    , generatedEnding :: ByteString
    , generatedFinalLf :: Bool
    }
    deriving stock (Show)

-- | A valid row declares its value, raw version, yank and timestamp presence. Claims exclude build metadata.
data NativeRow = NativeRow
    { nativeBytes :: ByteString
    , nativeExpected :: Maybe (Value, Text, Bool, Bool)
    , nativeClaim :: Maybe Text
    , nativeMalformed :: Bool
    }
    deriving stock (Show)

-- | Include late exact/build duplicates, optional fields, damaged records and native line endings.
genCargoIndex :: CargoInputs -> Gen GeneratedIndex
genCargoIndex inputs = do
    primaryRow <- validRow ("1.0.0+first", "1.0.0")
    middle <- concat <$> Gen.list (Range.linear 0 4) record
    duplicate <- Gen.bool
    late <- validRow =<< Gen.element [("1.0.0+first", "1.0.0"), ("1.0.0+second", "1.0.0")]
    linesWithBlanks <- concat <$> traverse withBlanks (primaryRow : middle <> if duplicate then [late] else [])
    emptySource <- Gen.frequency [(1, pure True), (15, pure False)]
    let rows = if emptySource then [] else linesWithBlanks
    ending <- Gen.element ["\n", "\r\n"]
    finalLf <- Gen.bool
    selected <- Gen.frequency [(4, pure "1.0.0+first"), (1, pure "2.0.0"), (1, pure "9.9.9")]
    pure (GeneratedIndex (BS.intercalate ending (map nativeBytes rows) <> if finalLf then ending else "") rows selected ending finalLf)
  where
    record = do
        nativeRow <- validRow =<< Gen.element [("1.0.0", "1.0.0"), ("2.0.0", "2.0.0"), ("3.1.0-rc.1", "3.1.0-rc.1")]
        if inputs == SoundRows
            then pure [nativeRow]
            else Gen.frequency [(5, pure [nativeRow]), (2, pure [unsupported nativeRow]), (2, pure <$> duplicateMember nativeRow), (2, malformed)]
    withBlanks nativeRow = do
        blanks <- Gen.list (Range.linear 0 2) (Gen.element ["", " ", "\t\r"])
        pure (map (\bytes -> NativeRow bytes Nothing Nothing False) blanks <> [nativeRow])
    malformed = do
        bytes <- Gen.element ["{broken}", "{", "null", "[{}]"] >>= JsonBytes.damaged
        -- Mutating these fragments cannot supply the four required native fields.
        pure [NativeRow part Nothing Nothing True | part <- BS.split 10 bytes]

validRow :: (Text, Text) -> Gen NativeRow
validRow (version, claim) = do
    yanked <- Gen.bool
    omitted <- Gen.subsequence ["features", "features2", "yanked", "links", "rust_version", "pubtime"]
    extra <- Gen.int (Range.linear 0 3)
    let base = case cargoTestRecord version of Object original -> original; _ -> mempty
        fields = foldr KeyMap.delete (KeyMap.insert "cksum" (String validSha256) (KeyMap.insert "yanked" (Bool yanked) base)) omitted
        value = Object (KeyMap.insert "native-extra" (nested extra) fields)
    pure (NativeRow (toStrict (encode value)) (Just (value, version, yanked && not (elem "yanked" omitted), not (elem "pubtime" omitted))) (Just claim) False)
  where
    nested level
        | level <= 0 = object ["nullable" .= Null, "text" .= ("owned \x1f600 \"value\"" :: Text)]
        | otherwise = object ["nested" .= [nested (level - 1), Null]]

unsupported :: NativeRow -> NativeRow
unsupported nativeRow = case nativeExpected nativeRow of
    Just (Object fields, _, _, _) -> nativeRow{nativeBytes = toStrict (encode (Object (KeyMap.insert "v" (Number 3) fields))), nativeExpected = Nothing}
    _ -> nativeRow

duplicateMember :: NativeRow -> Gen NativeRow
duplicateMember nativeRow = do
    prefix <- Gen.element ["{\"vers\":\"1.0.0\",", "{\"\\u0076ers\":\"1.0.0\",", "{\"duplicate-child\":{\"slot\":1,\"slot\":2},"]
    pure (NativeRow (prefix <> BS.drop 1 (nativeBytes nativeRow)) Nothing Nothing True)
