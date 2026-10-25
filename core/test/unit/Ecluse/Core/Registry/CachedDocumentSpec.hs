-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Opaque document injection and projection.
module Ecluse.Core.Registry.CachedDocumentSpec (spec) where

import Data.Aeson (Value (Bool, Null, Number, String), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Test.Hspec

import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (cargoCached, cargoPacked, cargoRendered, npmCached, npmPacked, pypiSimpleCached, weighCachedDoc)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument, packCargoDocument, packedIndexPlan)
import Ecluse.Core.Registry.Json.Packed (docTable)
import Ecluse.Core.Registry.Json.Shape (Shape (Generic))
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Npm.Document (PackedPackument (..), tarballHole)
import Ecluse.Test.Registry.Packed (packValue)

{- | Boundary pairs preserve their compact values and memoise an encoding-free accounting estimate. A
damaged packed document projects to nothing.
-}
spec :: Spec
spec = describe "CachedDocument (npm's opaque-carrier boundary)" $ do
    it "retains native Cargo records across tree, packed and output representations" $ do
        let source = cargoDocument [(RecordEntry 0, object ["native" .= True])]
            packed = packCargoDocument source
            documents = [fst cargoCached source, fst cargoPacked packed, fst cargoRendered (packedIndexPlan packed)]
        for_ documents $ \document -> do
            snd cargoCached document `shouldBe` Just source
            snd npmCached document `shouldBe` Nothing
            snd pypiSimpleCached document `shouldBe` Nothing
            weighCachedDoc document `shouldSatisfy` (> 0)
    it "inject then project round-trips every sample to Just" $
        map (project . inject) samples `shouldBe` map Just samples

    it "charges representative compact values without understating their encoded bytes" $
        forM_ samples $
            \sample -> weighCachedDoc (inject sample) `shouldSatisfy` (>= BSL.length (encode sample))

    it "projects a packed document that names a string its table lacks to Nothing" $ do
        let release = object ["name" .= ("thing" :: Text), "dist" .= object ["tarball" .= ("https://registry.npmjs.org/thing/-/thing-1.0.0.tgz" :: Text)]]
        case packValue (Generic 64) tarballHole release of
            Right (StreamResult (Right (table, form)) _) -> do
                let packedWith strings = fst npmPacked (PackedPackument (KeyMap.singleton "name" "thing") strings (KeyMap.singleton "1.0.0" form))
                project (packedWith table) `shouldBe` Just (object ["name" .= ("thing" :: Text), "versions" .= object ["1.0.0" .= release]])
                project (packedWith (docTable mempty)) `shouldBe` Nothing
            _ -> expectationFailure "did not pack the release"
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
