-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Assemble supported Simple-index fields from exact admitted source coordinates.
module Ecluse.Core.Registry.PyPI.Filter (
    assembleSimpleIndex,
    assembleSimpleDocument,
    serialiseSimpleDocument,
) where

import Data.Aeson (Value (Array, Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Package.Merge (MergePlan (mpName, mpSurvivors), SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, pypiPacked, pypiRendered, pypiSimpleCached, rendered)
import Ecluse.Core.Registry.Json.Packed (Piece (..), Pieces (ArrayPieces), RenderPlan (..), holeText, renderPlan, replacement)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple (..), SimpleDocument, simpleDocument, simpleEncoding, simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.PyPI.Route (distributionPath)
import Ecluse.Core.Registry.ServedDocument (overlaySurvivors, rebaseArtifactUrl, serialiseAcross, stringField)
import Ecluse.Core.Snapshot (Snapshot)
import Ecluse.Core.Text (joinUrlPath)

-- | Rebase admitted files under the requested project, preserving their winning source order.
assembleSimpleIndex :: Text -> Map SourceId (Snapshot SimpleDocument) -> MergePlan -> SimpleDocument -> SimpleDocument
assembleSimpleIndex mountBase bySource plan base =
    simpleDocument
        (servedEnvelope plan (simpleEnvelope base))
        (zipWith (\position value -> (ArrayEntry position, value)) [0 ..] survivingFiles)
  where
    survivingFiles =
        [ rebased
        | (_, entry) <- overlaySurvivors simpleFiles bySource plan
        , Just rebased <- [rebaseEntry (servedFileUrl mountBase (mpName plan)) entry]
        ]

-- The base envelope with the plan's surviving versions.
servedEnvelope :: MergePlan -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
servedEnvelope plan = KeyMap.insert "versions" (Array (V.fromList (map String (Map.keys (mpSurvivors plan)))))

{- | 'assembleSimpleIndex' over packed full reads: a file serves only when its @url@ hole rebases,
and the rebased URL replaces it.
-}
assemblePackedIndex :: Text -> Map SourceId (Snapshot PackedSimple) -> MergePlan -> Maybe PackedSimple -> RenderPlan
assemblePackedIndex mountBase bySource plan base =
    RenderPlan
        (servedEnvelope plan (KeyMap.delete "files" (maybe mempty packedEnvelope base)))
        "files"
        (ArrayPieces [piece | (_, (table, packed)) <- overlaySurvivors filesOf bySource plan, Just piece <- [rebased table packed]])
  where
    filesOf source = [(key, (packedTable source, packed)) | (key, packed) <- packedFiles source]
    rebased table packed = do
        url <- holeText table packed
        served <- rebaseArtifactUrl (servedFileUrl mountBase (mpName plan)) url
        pure (Piece table packed (Just (replacement served)))

servedFileUrl :: Text -> PackageName -> Text -> Maybe Text
servedFileUrl mountBase project filename = joinUrlPath mountBase <$> distributionPath project filename

rebaseEntry :: (Text -> Maybe Text) -> Value -> Maybe Value
rebaseEntry renderUrl = \case
    Object entry
        | Just url <- stringField "url" entry
        , Just rebased <- rebaseArtifactUrl renderUrl url ->
            Just (Object (KeyMap.insert "url" (String rebased) entry))
    _ -> Nothing

-- | Assemble a PyPI document. Sources from another ecosystem contribute nothing.
assembleSimpleDocument :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
assembleSimpleDocument mountBase bySource plan base =
    case (traverse (traverse (snd pypiPacked)) bySource, traverse (snd pypiPacked) base) of
        (Just packed, Just packedBase) -> pypiRendered (assemblePackedIndex mountBase packed plan packedBase)
        _ -> assembleValues mountBase bySource plan base

assembleValues :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
assembleValues mountBase bySource plan base =
    fst
        pypiSimpleCached
        ( assembleSimpleIndex
            mountBase
            (Map.mapMaybe (traverse (snd pypiSimpleCached)) bySource)
            plan
            (fromMaybe (simpleDocument mempty []) (snd pypiSimpleCached =<< base))
        )

-- | Serialise a PyPI document to compact JSON, or an empty object for another ecosystem.
serialiseSimpleDocument :: CachedDoc -> LByteString
serialiseSimpleDocument doc = maybe (serialiseAcross (fmap simpleEncoding . snd pypiSimpleCached) doc) (fromStrict . renderPlan) (rendered doc)
