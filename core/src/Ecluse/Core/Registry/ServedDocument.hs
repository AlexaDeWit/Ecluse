-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared outbound document assembly, paired with inbound "Ecluse.Core.Registry.WireSupport".
Ecosystem adapters own their wire shapes and reuse these name and location gates.
-}
module Ecluse.Core.Registry.ServedDocument (
    -- * The cached-document boundary
    assembleAcross,
    serialiseAcross,

    -- * Replaying a merge plan
    overlaySurvivors,
    overlayObjectSurvivors,

    -- * The interpolated-name gate
    safeDocumentName,

    -- * Rebasing an artifact location
    rebaseArtifactUrl,

    -- * Reading and editing a raw document
    documentObject,
    stringField,
    adjustField,
) where

import Data.Aeson (Value (Object, String), encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Package.Entry (AdmittedEntry (..), EntryKey (..))
import Ecluse.Core.Package.Merge (MergePlan (mpArtifacts, mpSurvivors), SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Snapshot (ContentDigest, Snapshot (..))
import Ecluse.Core.Text (urlFilename)

-- | Foreign ecosystem documents contribute nothing to assembly.
assembleAcross ::
    (Value -> CachedDoc, CachedDoc -> Maybe Value) ->
    (Text -> Map SourceId (Snapshot Value) -> MergePlan -> Value -> Value) ->
    Text ->
    Map SourceId (Snapshot CachedDoc) ->
    MergePlan ->
    Maybe CachedDoc ->
    CachedDoc
assembleAcross (inject, project) assemble mountBase bySource plan base =
    inject
        ( assemble
            mountBase
            (Map.mapMaybe (traverse project) bySource)
            plan
            (fromMaybe (Object mempty) (project =<< base))
        )

-- | Encode a served document to its compact wire bytes, an empty object for a foreign one.
serialiseAcross :: (CachedDoc -> Maybe Value) -> CachedDoc -> LByteString
serialiseAcross project = encode . fromMaybe (Object mempty) . project

{- | Select exact admitted entries from the winning source snapshot, preserving each source's order.
Missing keys, ambiguous keys, and mismatched snapshots contribute nothing.
-}
overlaySurvivors :: (src -> [(EntryKey, entry)]) -> Map SourceId (Snapshot src) -> MergePlan -> [(Text, entry)]
overlaySurvivors entriesOf bySource plan =
    [ (version, entry)
    | (sid, source) <- Map.toAscList bySource
    , let entries = entriesOf (snapshotValue source)
    , let unambiguous = uniqueEntries entries
    , (key, entry) <- entries
    , Map.member key unambiguous
    , Just (version, kept) <- [Map.lookup (sid, snapshotDigest source, key) admitted]
    , usableEntry kept
    ]
  where
    admitted = admittedIndex plan

-- | Look up admitted object entries in existing unique-key maps, in source and key order.
overlayObjectSurvivors :: (src -> KeyMap entry) -> Map SourceId (Snapshot src) -> MergePlan -> [(Text, entry)]
overlayObjectSurvivors entriesOf bySource plan =
    [ (version, entry)
    | ((sid, digest, ObjectEntry key), (version, kept)) <- Map.toAscList (admittedIndex plan)
    , usableEntry kept
    , Just source <- [Map.lookup sid bySource]
    , snapshotDigest source == digest
    , Just entry <- [KeyMap.lookup (Key.fromText key) (entriesOf (snapshotValue source))]
    ]

admittedIndex :: MergePlan -> Map (SourceId, ContentDigest, EntryKey) (Text, AdmittedEntry)
admittedIndex plan =
    uniqueEntries
        [ ((sid, admittedSnapshot entry, admittedKey entry), (version, entry))
        | (version, entries) <- Map.toList (mpArtifacts plan)
        , Just sid <- [Map.lookup version (mpSurvivors plan)]
        , entry <- toList entries
        ]

usableEntry :: AdmittedEntry -> Bool
usableEntry entry = validKey (admittedKey entry) && not (T.null (admittedFilename entry))

uniqueEntries :: (Ord key) => [(key, value)] -> Map key value
uniqueEntries = Map.mapMaybe id . Map.fromListWith (\_ _ -> Nothing) . map (second Just)

validKey :: EntryKey -> Bool
validKey = \case
    ArrayEntry position -> position >= 0
    ObjectEntry _ -> True
    SingletonEntry -> True

-- | Gate the document's claimed name before it enters a rewritten artifact URL.
safeDocumentName :: (Text -> Maybe a) -> KeyMap Value -> Maybe a
safeDocumentName parse document = case KeyMap.lookup "name" document of
    Just (String name) -> parse name
    _ -> Nothing

{- | Rebase an artifact under this mount, checking filenames before and after URL whitespace trimming.
Idempotent while the renderer keeps the filename in the terminal path segment.
-}
rebaseArtifactUrl :: (Text -> Maybe Text) -> Text -> Maybe Text
rebaseArtifactUrl renderMountUrl url = do
    filename <- urlFilename url
    _ <- urlFilename (T.strip url)
    renderMountUrl filename

-- | A raw document's own object, empty for a document that is not one.
documentObject :: Value -> KeyMap Value
documentObject = \case
    Object o -> o
    _ -> mempty

-- | The 'Text' at @key@ in a raw document object, if present and a JSON string.
stringField :: Key.Key -> KeyMap Value -> Maybe Text
stringField key o = case KeyMap.lookup key o of
    Just (String s) -> Just s
    _ -> Nothing

-- | Missing fields stay absent.
adjustField :: Key.Key -> (Value -> Value) -> KeyMap Value -> KeyMap Value
adjustField key edit o = case KeyMap.lookup key o of
    Just v -> KeyMap.insert key (edit v) o
    Nothing -> o
