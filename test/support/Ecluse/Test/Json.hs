-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Hedgehog generators for arbitrary JSON documents.

Every entry point that reads a registry document is total over these, so a suite fuzzing
one shares the generators with every other. The caller supplies the object-key pool,
which is the only part that differs between documents.
-}
module Ecluse.Test.Json (
    genValue,
    genKey,
    genJsonText,
) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
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
