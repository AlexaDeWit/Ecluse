-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.Json.BoundarySpec (spec) where

import Data.Aeson (Value (Object, String), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Package.Merge (
    MergePlan (MergePlan, mpDistTags, mpDivergences, mpName, mpSurvivors, mpTime),
    SourceId,
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Json.Boundary (
    assembleThroughBoundary,
    projectedDocument,
    serialiseThroughBoundary,
 )

{- | Pin the shared JSON served-document boundary. 'projectedDocument' must round-trip an
injected document and take the benign-miss default otherwise; 'assembleThroughBoundary'
must project every source and the base, hand them to the ecosystem's assembly, and inject
the result back; 'serialiseThroughBoundary' must be the projection's compact encoding.
npm's instantiation is exercised end-to-end by "Ecluse.Registry.Npm.FilterSpec".
-}
spec :: Spec
spec = do
    projectedDocumentSpec
    assembleThroughBoundarySpec
    serialiseThroughBoundarySpec

projectedDocumentSpec :: Spec
projectedDocumentSpec = describe "projectedDocument" $ do
    it "projects a document the ecosystem injected" $
        projectedDocument project (inject sample) `shouldBe` sample

    it "takes the empty-object benign-miss default when projection misses" $
        projectedDocument (const Nothing) (inject sample) `shouldBe` Object mempty

assembleThroughBoundarySpec :: Spec
assembleThroughBoundarySpec = describe "assembleThroughBoundary" $ do
    it "projects sources and base, assembles, and injects the result back" $
        project (assembleThroughBoundary npmCached spy "https://proxy.test/npm" sources emptyPlan (Just (inject sample)))
            `shouldBe` Just (assembledFrom sample)

    it "projects an absent base to the empty object" $
        project (assembleThroughBoundary npmCached spy "https://proxy.test/npm" sources emptyPlan Nothing)
            `shouldBe` Just (assembledFrom (Object mempty))

serialiseThroughBoundarySpec :: Spec
serialiseThroughBoundarySpec =
    describe "serialiseThroughBoundary" $
        it "is the projected document's compact encoding" $
            serialiseThroughBoundary project (inject sample) `shouldBe` encode sample

-- | npm's boundary pair, the only inject/project pair in the build.
inject :: Value -> CachedDoc
project :: CachedDoc -> Maybe Value
(inject, project) = npmCached

-- | A packument-shaped document, enough to witness the projection is lossless.
sample :: Value
sample = object ["name" .= ("thing" :: Text), "versions" .= object ["1.0.0" .= object []]]

{- | A stand-in assembly that records the base document it was handed and the number of
sources projected, so the boundary's plumbing is observable without npm's wire shape.
-}
spy :: Text -> Map SourceId Value -> MergePlan -> Value -> Value
spy _ bySource _ base =
    Object (KeyMap.fromList [("base", base), ("sources", String (show (Map.size bySource)))])

-- | What 'spy' returns for a given base, against the two-source fixture.
assembledFrom :: Value -> Value
assembledFrom base = Object (KeyMap.fromList [("base", base), ("sources", String "2")])

-- | Two injected source documents, to witness every source is projected.
sources :: Map SourceId CachedDoc
sources = Map.fromList [(0, inject sample), (1, inject (Object mempty))]

-- | The degenerate plan; this spec exercises the boundary, not the merge.
emptyPlan :: MergePlan
emptyPlan =
    MergePlan
        { mpName = mkPackageName Npm Nothing "thing"
        , mpSurvivors = mempty
        , mpDistTags = mempty
        , mpTime = mempty
        , mpDivergences = mempty
        }
