-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Opaque document injection and projection.
module Ecluse.Core.Registry.CachedDocumentSpec (spec) where

import Data.Aeson (Value (Bool, Null, Number, String), encode, object, (.=))
import Data.ByteString.Lazy qualified as BSL
import Test.Hspec

import Ecluse.Core.Registry.CachedDocument (foldCachedDoc, npmCached, weighCachedDoc)

-- | Boundary pairs preserve their compact values and memoise an encoding-free accounting estimate.
spec :: Spec
spec = describe "CachedDocument (npm's opaque-carrier boundary)" $ do
    it "inject then project round-trips every sample to Just" $
        map (project . inject) samples `shouldBe` map Just samples

    it "foldCachedDoc preserves the same value for diagnostic accounting" $
        map (foldCachedDoc const . inject) samples `shouldBe` samples

    it "charges representative compact values without understating their encoded bytes" $
        forM_ samples $
            \sample -> weighCachedDoc (inject sample) `shouldSatisfy` (>= BSL.length (encode sample))
  where
    (inject, project) = npmCached

    -- Scalars, an empty object, a packument-shaped nesting, and an array-bearing object, so both
    -- identities hold across the wire shapes npm serves.
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
