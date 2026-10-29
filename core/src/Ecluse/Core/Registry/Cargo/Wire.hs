-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Strict native index records retain additive fields and distinguish null from absence.
module Ecluse.Core.Registry.Cargo.Wire (
    CargoRecordError (..),
    decodeCargoRecord,
    validateCargoRecord,
    recordIdentity,
    cargoTreeCharge,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Parser (jsonNoDup')
import Data.Attoparsec.ByteString (endOfInput, parseOnly, skipWhile)
import Data.ByteString qualified as BS
import Data.Scientific (toBoundedInteger)

import Ecluse.Core.Registry.JsonStream (isJsonSpace)
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Security (LimitError (TooDeeplyNested))
import Ecluse.Core.Version.Cargo (cargoUniquenessKey, parseCargo)

-- | Malformed rows drop individually. Resource refusals stop the whole source read.
data CargoRecordError = CargoRecordMalformed Text | CargoRecordBound LimitError
    deriving stock (Eq, Show)

-- | Decode exactly one bounded object and reject duplicate member names at every depth.
decodeCargoRecord :: Int -> ByteString -> Either CargoRecordError Value
decodeCargoRecord depth bytes = do
    first CargoRecordBound (checkDepthBeforeDecode depth bytes)
    value <- first (const (CargoRecordMalformed "malformed Cargo index record")) (parseOnly (jsonNoDup' <* skipWhile isJsonSpace <* endOfInput) bytes)
    unless (withinDepth depth value) (Left (CargoRecordBound (TooDeeplyNested depth)))
    case value of
        Object _ -> Right value
        _ -> Left (CargoRecordMalformed "Cargo index record is not an object")

-- | Reserve bounded native parser trees and comparison structures before retaining them.
cargoTreeCharge :: Integer -> Int
cargoTreeCharge bytes = fromInteger (min (toInteger (maxBound :: Int)) (65536 + 64 * max 0 bytes))

-- | A syntactically native version claims uniqueness even when another record field is invalid.
recordIdentity :: Value -> Maybe Text
recordIdentity = \case
    Object fields -> stringField "vers" fields >>= parseCargo <&> cargoUniquenessKey
    _ -> Nothing

-- | Validate supported installation schema without interpreting Cargo's resolver expressions.
validateCargoRecord :: Value -> Either Text ()
validateCargoRecord = \case
    Object fields -> do
        traverse_ (required isString fields) ["name", "vers", "cksum"]
        required isArray fields "deps"
        case KeyMap.lookup "deps" fields of
            Just (Array dependencies) -> traverse_ dependency dependencies
            _ -> Left "invalid Cargo dependencies"
        optionalField featureMap fields "features"
        optionalField isBool fields "yanked"
        optionalField featureMap fields "features2"
        traverse_ (optionalField nullableText fields) ["links", "rust_version"]
        case KeyMap.lookup "v" fields of
            Nothing -> Right ()
            Just (Number number) | Just schema <- toBoundedInteger @Int number, schema == 1 || schema == 2 -> Right ()
            _ -> Left "unsupported or invalid Cargo index schema"
    _ -> Left "Cargo index record is not an object"
  where
    dependency = \case
        Object fields -> do
            traverse_ (required isString fields) ["name", "req"]
            optionalField stringArray fields "features"
            traverse_ (optionalField isBool fields) ["optional", "default_features", "lib"]
            traverse_ (optionalField nullableText fields) ["target", "kind", "registry", "package", "bindep_target"]
        _ -> Left "invalid Cargo dependency"

required :: (Value -> Bool) -> KeyMap.KeyMap Value -> Key.Key -> Either Text ()
required predicate fields key = case KeyMap.lookup key fields of
    Just value | predicate value -> Right ()
    _ -> Left ("invalid Cargo field: " <> Key.toText key)

optionalField :: (Value -> Bool) -> KeyMap.KeyMap Value -> Key.Key -> Either Text ()
optionalField predicate fields key = case KeyMap.lookup key fields of
    Nothing -> Right ()
    Just value -> unless (predicate value) (Left ("invalid Cargo field: " <> Key.toText key))

isString, nullableText, isBool, isArray, stringArray, featureMap :: Value -> Bool
isString = \case String _ -> True; _ -> False
nullableText = \case Null -> True; value -> isString value
isBool = \case Bool _ -> True; _ -> False
isArray = \case Array _ -> True; _ -> False
stringArray = \case Array entries -> all isString entries; _ -> False
featureMap = \case Object fields -> all stringArray fields; _ -> False

withinDepth :: Int -> Value -> Bool
withinDepth depth value
    | depth <= 0 = False
    | otherwise = case value of
        Object fields -> all (withinDepth (depth - 1)) fields
        Array items -> all (withinDepth (depth - 1)) items
        _ -> True

-- Reject deep container chains before aeson allocates their tree.
checkDepthBeforeDecode :: Int -> ByteString -> Either LimitError ()
checkDepthBeforeDecode limit bytes = go 0 0 False False
  where
    go !at !depth quoted escaped = case BS.indexMaybe bytes at of
        Nothing -> Right ()
        Just byte
            | quoted && escaped -> next depth True False
            | quoted -> case byte of
                92 -> next depth True True
                34 -> next depth False False
                _ -> next depth True False
            | byte == 34 -> next depth True False
            | byte == 91 || byte == 123 -> if depth >= limit then Left (TooDeeplyNested limit) else next (depth + 1) False False
            | byte == 93 || byte == 125 -> next (max 0 (depth - 1)) False False
            | otherwise -> next depth False False
      where
        next = go (at + 1)
