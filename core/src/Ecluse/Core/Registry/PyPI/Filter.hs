-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Assemble PEP 691 Simple indexes from a cross-upstream 'MergePlan' and raw documents.
Surviving entries retain unmodelled keys and must have a mount-local artifact URL.
-}
module Ecluse.Core.Registry.PyPI.Filter (
    -- * Assembling the served index
    assembleSimpleIndex,

    -- * The served-document boundary (PyPI's 'CachedDoc' capabilities)
    assembleSimpleDocument,
    serialiseSimpleDocument,
) where

import Data.Aeson (Value (Array, Object, String), encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Package.Merge (MergePlan (mpName, mpSurvivors), SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, pypiSimpleCached)
import Ecluse.Core.Registry.PyPI.Route (distributionPath)
import Ecluse.Core.Registry.ServedDocument (overlaySurvivors, rebaseArtifactUrl, stringField)
import Ecluse.Core.Snapshot (Snapshot)
import Ecluse.Core.Text (joinUrlPath)

{- | Assemble the served Simple index for @mountBase@, rebasing every location under the plan's own
project name so the index carries none this mount would not claim. Always an object.
-}
assembleSimpleIndex :: Text -> Map SourceId (Snapshot Value) -> MergePlan -> Value -> Value
assembleSimpleIndex mountBase bySource plan base =
    Object
        ( baseObject
            & KeyMap.insert "versions" (Array (V.fromList (map String (Map.keys (mpSurvivors plan)))))
            & KeyMap.insert "files" (Array (V.fromList survivingFiles))
        )
  where
    baseObject :: KeyMap Value
    baseObject = case base of
        Object o -> o
        _ -> mempty

    survivingFiles :: [Value]
    survivingFiles =
        [ dropSidecarKeys rebased
        | (_, entry) <- overlaySurvivors (zipWith (\position entry -> (ArrayEntry position, entry)) [0 ..] . entriesOf) bySource plan
        , Just rebased <- [rebaseEntry (servedFileUrl mountBase (mpName plan)) entry]
        ]

servedFileUrl :: Text -> PackageName -> Text -> Maybe Text
servedFileUrl mountBase project filename = joinUrlPath mountBase <$> distributionPath project filename

rebaseEntry :: (Text -> Maybe Text) -> Value -> Maybe Value
rebaseEntry renderUrl = \case
    Object entry
        | Just url <- stringField "url" entry
        , Just rebased <- rebaseArtifactUrl renderUrl url ->
            Just (Object (KeyMap.insert "url" (String rebased) entry))
    _ -> Nothing

-- Écluse serves no @.metadata@ companion, and the wheel carries the same metadata.
dropSidecarKeys :: Value -> Value
dropSidecarKeys = \case
    Object entry -> Object (foldr KeyMap.delete entry sidecarKeys)
    other -> other

-- The two spellings PEP 714 has an index emit for the same sidecar.
sidecarKeys :: [Key.Key]
sidecarKeys = ["core-metadata", "data-dist-info-metadata"]

entriesOf :: Value -> [Value]
entriesOf = \case
    Object o | Just (Array files) <- KeyMap.lookup "files" o -> toList files
    _ -> []

{- | PyPI's served-document __assemble__ capability. A source another ecosystem injected projects as
'Nothing' and contributes nothing.
-}
assembleSimpleDocument :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
assembleSimpleDocument mountBase bySource plan base =
    fst pypiSimpleCached (assembleSimpleIndex mountBase sources plan baseValue)
  where
    sources = Map.mapMaybe (traverse (snd pypiSimpleCached)) bySource
    baseValue = fromMaybe (Object mempty) (snd pypiSimpleCached =<< base)

{- | PyPI's served-document __serialise__ capability
('Ecluse.Core.Registry.Adapter.Types.metadataSerialise'), to the compact wire bytes.
-}
serialiseSimpleDocument :: CachedDoc -> LByteString
serialiseSimpleDocument = encode . fromMaybe (Object mempty) . snd pypiSimpleCached
