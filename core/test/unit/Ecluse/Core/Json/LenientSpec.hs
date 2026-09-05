-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Json.LenientSpec (spec) where

import Data.Aeson (Object, Value (Array, Bool, Null, Number, Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, Result (Error, Success), parse, parseMaybe)
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Json.Lenient (lenientOptional, typeMismatchOneOf)

{- | Direct tests for the shared lenient-decode primitives, pinning them in isolation.
"Ecluse.Core.Registry.Npm.WireSpec" covers the npm consumers end to end.
-}
spec :: Spec
spec = do
    lenientOptionalSpec
    typeMismatchOneOfSpec

lenientOptionalSpec :: Spec
lenientOptionalSpec = describe "lenientOptional" $ do
    it "reads a well-formed value as Just" $
        readLenientInt (objOf (Number 4096)) `shouldBe` Just (Just 4096)

    it "degrades a wrong-typed value to Nothing rather than failing the decode" $
        readLenientInt (objOf (String "big")) `shouldBe` Just Nothing

    it "degrades a fractional number to Nothing (outside the Int target)" $
        readLenientInt (objOf (Number 1.5)) `shouldBe` Just Nothing

    it "degrades an out-of-range number to Nothing" $
        readLenientInt (objOf (Number 1e400)) `shouldBe` Just Nothing

    it "reads an absent field as Nothing" $
        readLenientInt KeyMap.empty `shouldBe` Just Nothing

    it "reads a null field as Nothing" $
        readLenientInt (objOf Null) `shouldBe` Just Nothing

typeMismatchOneOfSpec :: Spec
typeMismatchOneOfSpec = describe "typeMismatchOneOf" $ do
    it "names an object" $ messageFor (Object KeyMap.empty) `shouldBe` expected "an object"
    it "names a string" $ messageFor (String "x") `shouldBe` expected "a string"
    it "names an array" $ messageFor (Array mempty) `shouldBe` expected "an array"
    it "names a number" $ messageFor (Number 1) `shouldBe` expected "a number"
    it "names a boolean" $ messageFor (Bool True) `shouldBe` expected "a boolean"
    it "names null" $ messageFor Null `shouldBe` expected "null"

{- | Run 'lenientOptional' for an 'Int' field @n@, exposing the two-layer 'Maybe'. The outer
layer is the parser, which always succeeds, and the inner layer is the field's lenient presence.
-}
readLenientInt :: Object -> Maybe (Maybe Int)
readLenientInt = parseMaybe (`lenientOptional` "n")

-- | A single-field object @{"n": v}@ for the reads above.
objOf :: Value -> Object
objOf v = KeyMap.fromList [("n", v)]

{- | The failure message 'typeMismatchOneOf' renders for a value. Run the always-failing
parser and read back its decode error, discarding the path.
-}
messageFor :: Value -> String
messageFor v = case parse (const parser) () of
    Error msg -> msg
    Success () -> "unexpectedly succeeded"
  where
    parser :: Parser ()
    parser = typeMismatchOneOf "Widget (object or string)" v

-- | The message 'typeMismatchOneOf' renders for a given rendered JSON kind.
expected :: String -> String
expected kind = "expected Widget (object or string), but encountered " <> kind
