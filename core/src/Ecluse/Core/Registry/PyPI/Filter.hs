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
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Vector qualified as V

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Package.Merge (MergePlan (mpName, mpSurvivors), SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, pypiPacked, pypiRendered, pypiSimpleCached)
import Ecluse.Core.Registry.Json.Packed (Piece (..), Pieces (ArrayPieces), RenderPlan (..), hasHole, renderPlan, urlPrefix)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple (..), SimpleDocument, packedSimplePlan, simpleDocument, simpleEncoding, simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.PyPI.Route (distributionPath)
import Ecluse.Core.Registry.ServedDocument (RenderRefused (RenderRefused), overlaySurvivors, rebaseArtifactUrl, serialiseAcross, stringField)
import Ecluse.Core.Snapshot (Snapshot, snapshotValue)
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

servedEnvelope :: MergePlan -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
servedEnvelope plan = KeyMap.insert "versions" (Array (V.fromList (map String (Map.keys (mpSurvivors plan)))))

assemblePackedIndex :: Text -> Map SourceId (Snapshot PackedSimple) -> MergePlan -> Maybe PackedSimple -> RenderPlan
assemblePackedIndex mountBase bySource plan base =
    RenderPlan
        { planMembers = servedEnvelope plan (maybe mempty packedEnvelope base)
        , planSlot = "files"
        , planTables = smallArrayFromList (map (packedTable . snapshotValue) (Map.elems bySource))
        , planPieces = ArrayPieces [piece | isJust prefix, (_, piece@(Piece _ file)) <- overlaySurvivors filesOf indexed plan, hasHole file]
        , planPrefix = urlPrefix <$> prefix
        }
  where
    prefix = servedFileUrl mountBase (mpName plan) ""
    indexed = Map.fromDistinctAscList (zipWith (\index (sid, source) -> (sid, (index,) <$> source)) [0 ..] (Map.toAscList bySource))
    filesOf (index, source) = [(key, Piece index file) | (key, file) <- packedFiles source]

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
        (Just packed, Just packedBase) -> fst pypiRendered (assemblePackedIndex mountBase packed plan packedBase)
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
serialiseSimpleDocument :: CachedDoc -> Either RenderRefused LByteString
serialiseSimpleDocument doc = case snd pypiRendered doc <|> (packedSimplePlan <$> snd pypiPacked doc) of
    Just plan -> maybe (Left RenderRefused) (Right . fromStrict) (renderPlan plan)
    Nothing -> Right (serialiseAcross (fmap simpleEncoding . snd pypiSimpleCached) doc)
