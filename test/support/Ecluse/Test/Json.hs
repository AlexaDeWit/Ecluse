-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Hedgehog generators for arbitrary JSON documents, and the readers a spec navigates a
served one with.

Every entry point that reads a registry document is total over the generators, so a suite
fuzzing one shares them with every other. The caller supplies the object-key pool, which is
the only part that differs between documents. The readers answer an absent or wrongly shaped
step with the empty value of their result, so a malformed body surfaces as an assertion
mismatch rather than a crash.
-}
module Ecluse.Test.Json (
    -- * Generating a document
    genValue,
    genKey,
    genJsonText,

    -- * Reading a document
    isObject,
    asObject,
    withKeys,
    fieldAt,
    objectAt,
    mapAt,
    keysAt,
    textAt,
    textAtPath,
    encodeStrict,
) where

import Data.Aeson (Object, Value (Array, Bool, Null, Number, Object, String), encode)
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific, scientific)
import Data.Vector qualified as V
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

{- | A depth- and breadth-bounded arbitrary 'Value' over the given object-key pool. The small
ranges keep it terminating, and 'Gen.recursive' shrinks toward the scalar cases.
-}
genValue :: [Text] -> Gen Value
genValue keyPool =
    Gen.recursive
        Gen.choice
        [ pure Null
        , Bool <$> Gen.bool
        , Number <$> genNumber
        , String <$> genJsonText
        ]
        [ Array . V.fromList <$> Gen.list (Range.linear 0 4) (genValue keyPool)
        , Object . KeyMap.fromList
            <$> Gen.list (Range.linear 0 4) ((,) <$> genKey keyPool <*> genValue keyPool)
        ]

{- | An object key biased toward the caller's pool. Without the bias almost every generated
object would miss the fields a decoder reads, leaving its success arm unsampled.
-}
genKey :: [Text] -> Gen Key.Key
genKey keyPool = Key.fromText <$> Gen.choice [Gen.element keyPool, genJsonText]

{- | A small arbitrary JSON number. A deliberate minority are hostile to a strict 'Int' decode,
fractional or far out of 'Int' range, so the magnitude is astronomical yet cheap to render.
-}
genNumber :: Gen Scientific
genNumber =
    Gen.frequency
        [ (3, fromInteger <$> genInteger)
        , (1, scientific <$> genInteger <*> Gen.int (Range.linearFrom 0 (-20) 400))
        ]

-- | A small arbitrary integer to seed a JSON number, kept in a modest range so 'Show' is cheap.
genInteger :: Gen Integer
genInteger = Gen.integral (Range.linearFrom 0 (-100000) 100000)

-- | A short arbitrary JSON string value (unicode, to probe text handling).
genJsonText :: Gen Text
genJsonText = Gen.text (Range.linear 0 8) Gen.unicode

-- | Whether a 'Value' is a JSON object.
isObject :: Value -> Bool
isObject = \case
    Object{} -> True
    _ -> False

-- | A 'Value' as its object, empty when it is not one.
asObject :: Value -> Object
asObject = \case
    Object fields -> fields
    _ -> KeyMap.empty

-- | An object with the given keys added or overridden, so an example names only its own axis.
withKeys :: [(Key, Value)] -> Value -> Value
withKeys overrides = \case
    Object base -> Object (foldr (uncurry KeyMap.insert) base overrides)
    other -> other

-- | One top-level field of a document, 'Nothing' when it is absent or the document is not an object.
fieldAt :: Key -> Value -> Maybe Value
fieldAt key = KeyMap.lookup key . asObject

-- | The object at one key, empty when the key is absent or does not hold an object.
objectAt :: Key -> Object -> Object
objectAt key = maybe KeyMap.empty asObject . KeyMap.lookup key

-- | 'objectAt' as a text-keyed 'Map', for an assertion that compares whole contents in key order.
mapAt :: Key -> Object -> Map Text Value
mapAt key fields = Map.fromList [(Key.toText k, v) | (k, v) <- KeyMap.toList (objectAt key fields)]

-- | The keys of the object at one key, in the order the object holds them.
keysAt :: Key -> Object -> [Text]
keysAt key = map Key.toText . KeyMap.keys . objectAt key

-- | A document as the strict bytes a decoder reads.
encodeStrict :: Value -> ByteString
encodeStrict = LBS.toStrict . encode

-- | The string at one key, 'Nothing' when it is absent or holds another shape.
textAt :: Key -> Object -> Maybe Text
textAt key fields = case KeyMap.lookup key fields of
    Just (String value) -> Just value
    _ -> Nothing

{- | The string at the end of a key path into a document. Any step that is absent or the wrong
shape yields 'Nothing', so a caller reads a nested leaf without matching each level.
-}
textAtPath :: [Key] -> Value -> Maybe Text
textAtPath [] (String value) = Just value
textAtPath (key : rest) (Object fields) = KeyMap.lookup key fields >>= textAtPath rest
textAtPath _ _ = Nothing
