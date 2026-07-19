-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.CachedDocumentSpec (spec) where

import Data.Aeson (Value (Bool, Null, Number, String), encode, object, (.=))
import Data.ByteString.Lazy qualified as BSL
import Test.Hspec

import Ecluse.Core.Registry.CachedDocument (npmCached, weighCachedDoc)

{- | Pin the two properties the byte-identity claim rests on: npm's boundary pair
round-trips ('project . inject == Just'), and 'weighCachedDoc' is exactly the compact
encoding's byte length. If either drifts, the "npm behaviour and cache memory are
byte-identical" guarantee has been broken and this spec is the tripwire.
-}
spec :: Spec
spec = describe "CachedDocument (npm's opaque-carrier boundary)" $ do
    it "inject then project round-trips every sample to Just" $
        map (project . inject) samples `shouldBe` map Just samples

    it "weighCachedDoc is the sample's compact-encoded byte length" $
        map (weighCachedDoc . inject) samples `shouldBe` map (BSL.length . encode) samples
  where
    (inject, project) = npmCached

    -- Representative document shapes: scalars, an empty object, a packument-shaped
    -- nesting, and an array-bearing object, so the round-trip and the weigh identity
    -- are pinned across the wire shapes npm actually serves.
    samples :: [Value]
    samples =
        [ Null
        , Bool True
        , String "left-pad"
        , Number 42
        , object []
        , object
            [ "name" .= ("is-odd" :: Text)
            , "dist" .= object ["tarball" .= ("https://registry.example/is-odd-1.0.0.tgz" :: Text)]
            ]
        , object
            [ "versions" .= object ["1.0.0" .= object ["_extra" .= (1 :: Int)]]
            , "time" .= ([Null, String "2020-01-01T00:00:00.000Z"] :: [Value])
            ]
        ]
