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
    RenderRefused (..),

    -- * Replaying a merge plan
    overlaySurvivors,
    uniqueEntries,
    validEntryKey,
    overlayObjectSurvivors,
    overlayObjectVersions,

    -- * The interpolated-name gate
    safeDocumentName,

    -- * Rebasing an artifact location
    rebaseArtifactUrl,

    -- * Reading and editing a raw document
    documentObject,
    stringField,
    objectField,
    adjustField,
) where

import Data.Aeson (Encoding, Value (Object, String))
import Data.Aeson.Encoding (emptyObject_, encodingToLazyByteString)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, newSmallArray, readSmallArray, runSmallArray, sizeofSmallArray, writeSmallArray)
import Data.Text qualified as T

import Ecluse.Core.Package.Entry (AdmittedEntry (..), EntryKey (..))
import Ecluse.Core.Package.Merge (MergePlan (mpArtifacts, mpSurvivors), SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Snapshot (ContentDigest, Snapshot (..))
import Ecluse.Core.Text (urlFilename)

{- | Run ecosystem assembly across its cached-document boundary.
A source or base that another ecosystem injected contributes nothing.
-}
assembleAcross ::
    (source -> CachedDoc, CachedDoc -> Maybe source) ->
    source ->
    (Text -> Map SourceId (Snapshot source) -> MergePlan -> source -> source) ->
    Text ->
    Map SourceId (Snapshot CachedDoc) ->
    MergePlan ->
    Maybe CachedDoc ->
    CachedDoc
assembleAcross (inject, project) emptySource assemble mountBase bySource plan base =
    inject
        ( assemble
            mountBase
            (Map.mapMaybe (traverse project) bySource)
            plan
            (fromMaybe emptySource (project =<< base))
        )

-- | Encode a served document to its compact wire bytes, an empty object for a foreign one.
serialiseAcross :: (CachedDoc -> Maybe Encoding) -> CachedDoc -> LByteString
serialiseAcross project = encodingToLazyByteString . fromMaybe emptyObject_ . project

{- | A served document whose render refused its plan, because the plan names a table or string it
lacks or the render does not fill its buffer exactly. The pipeline answers it as a render fault.
-}
data RenderRefused = RenderRefused
    deriving stock (Eq, Show)

instance Exception RenderRefused

{- | Select exact admitted entries from the winning source snapshot, preserving each source's order.
Missing keys, ambiguous keys, and mismatched snapshots contribute nothing.
-}
overlaySurvivors :: (src -> [(EntryKey, entry)]) -> Map SourceId (Snapshot src) -> MergePlan -> [(Text, entry)]
overlaySurvivors entriesOf bySource plan =
    [ selected
    | (sid, source) <- Map.toAscList bySource
    , selected <- selectEntries (sourceAdmissions plan sid source) (entriesOf (snapshotValue source))
    ]

-- Positions that ascend from zero, the last under twice the count, index a slot table. Other sources key a map.
selectEntries :: [(EntryKey, (Text, AdmittedEntry))] -> [(EntryKey, entry)] -> [(Text, entry)]
selectEntries admissions entries = case ascendingSpan entries of
    Just (count, end) | end < 2 * count -> slotted (admissionSlots end admissions) entries
    Just _ -> keyed (uniqueEntries admissions) entries
    Nothing -> keyed (uniqueEntries admissions) (filter (\(key, _) -> Map.member key unrepeated) entries)
  where
    unrepeated = uniqueEntries entries

-- The count and last position of entries at strictly ascending array positions from zero.
ascendingSpan :: [(EntryKey, entry)] -> Maybe (Int, Int)
ascendingSpan = go 0 (-1)
  where
    go !count !previous = \case
        (ArrayEntry position, _) : rest | position > previous -> go (count + 1) position rest
        [] -> Just (count, previous)
        _ -> Nothing

data Slot = Vacant | Admitted Text AdmittedEntry | Ambiguous

-- One slot per position up to @end@. A position admitted twice selects nothing.
admissionSlots :: Int -> [(EntryKey, (Text, AdmittedEntry))] -> SmallArray Slot
admissionSlots end admissions = runSmallArray $ do
    slots <- newSmallArray (end + 1) Vacant
    for_ admissions $ \case
        (ArrayEntry position, (version, kept)) | position >= 0 && position <= end -> do
            slot <- readSmallArray slots position
            writeSmallArray slots position $! case slot of
                Vacant -> Admitted version kept
                _ -> Ambiguous
        _ -> pass
    pure slots

slotted :: SmallArray Slot -> [(EntryKey, entry)] -> [(Text, entry)]
slotted slots entries =
    [ (version, entry)
    | (ArrayEntry position, entry) <- entries
    , position >= 0 && position < sizeofSmallArray slots
    , Admitted version kept <- [indexSmallArray slots position]
    , usableEntry kept
    ]

keyed :: Map EntryKey (Text, AdmittedEntry) -> [(EntryKey, entry)] -> [(Text, entry)]
keyed admitted entries =
    [ (version, entry)
    | (key, entry) <- entries
    , Just (version, kept) <- [Map.lookup key admitted]
    , usableEntry kept
    ]

{- | Select exact admitted entries by lookup in each winning source's key map, in source and key order.
Only object coordinates are served. Missing keys, ambiguous keys, and mismatched snapshots contribute nothing.
-}
overlayObjectSurvivors :: (src -> KeyMap entry) -> Map SourceId (Snapshot src) -> MergePlan -> [(Text, entry)]
overlayObjectSurvivors entriesOf bySource plan =
    [ (version, entry)
    | (sid, source) <- Map.toAscList bySource
    , let entries = entriesOf (snapshotValue source)
    , (key, (version, kept)) <- Map.toAscList (objectAdmissions plan sid source)
    , usableEntry kept
    , Just entry <- [KeyMap.lookup (Key.fromText key) entries]
    ]

{- | 'overlayObjectSurvivors' as one entry per served version, in version order, with its source.
A version with several selected keys keeps the greatest, as a key map built in key order does.
-}
overlayObjectVersions :: (src -> KeyMap entry) -> Map SourceId (Snapshot src) -> MergePlan -> [(Text, SourceId, entry)]
overlayObjectVersions entriesOf bySource plan =
    [ (version, sid, entry)
    | (version, admitted) <- Map.toAscList (mpArtifacts plan)
    , Just sid <- [Map.lookup version (mpSurvivors plan)]
    , Just source <- [Map.lookup sid indexed]
    , Just entry <- [versionEntry source admitted]
    ]
  where
    indexed = Map.mapWithKey (\sid source -> (snapshotDigest source, entriesOf (snapshotValue source), objectAdmissions plan sid source)) bySource

versionEntry :: (ContentDigest, KeyMap entry, Map Text admission) -> NonEmpty AdmittedEntry -> Maybe entry
versionEntry (digest, entries, unique) admitted =
    snd <$> foldl' greater Nothing selected
  where
    selected =
        [ (key, entry)
        | kept <- toList admitted
        , admittedSnapshot kept == digest
        , ObjectEntry key <- [admittedKey kept]
        , Map.member key unique
        , usableEntry kept
        , Just entry <- [KeyMap.lookup (Key.fromText key) entries]
        ]
    greater best candidate = if all ((< fst candidate) . fst) best then Just candidate else best

-- The plan's admissions of this source's snapshot under versions the source won, in version order.
sourceAdmissions :: MergePlan -> SourceId -> Snapshot src -> [(EntryKey, (Text, AdmittedEntry))]
sourceAdmissions plan sid source =
    [ (admittedKey entry, (version, entry))
    | (version, entries) <- Map.toList (mpArtifacts plan)
    , Map.lookup version (mpSurvivors plan) == Just sid
    , entry <- toList entries
    , admittedSnapshot entry == snapshotDigest source
    ]

-- The object keys this source's snapshot admits exactly once.
objectAdmissions :: MergePlan -> SourceId -> Snapshot src -> Map Text (Text, AdmittedEntry)
objectAdmissions plan sid source = uniqueEntries [(key, admission) | (ObjectEntry key, admission) <- sourceAdmissions plan sid source]

usableEntry :: AdmittedEntry -> Bool
usableEntry entry = validEntryKey (admittedKey entry) && not (T.null (admittedFilename entry))

-- | Keep only keys that occur exactly once, so ambiguous source coordinates cannot select an entry.
uniqueEntries :: (Ord key) => [(key, value)] -> Map key value
uniqueEntries = Map.mapMaybe id . Map.fromListWith (\_ _ -> Nothing) . map (second Just)

-- | Array coordinates cannot precede the first source entry.
validEntryKey :: EntryKey -> Bool
validEntryKey = \case
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
    guard (trimmed == url || isJust (urlFilename trimmed))
    renderMountUrl filename
  where
    trimmed = T.strip url

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

-- | The object at @key@ in a raw document object, if present and a JSON object.
objectField :: Key.Key -> KeyMap Value -> Maybe (KeyMap Value)
objectField key o = case KeyMap.lookup key o of
    Just (Object inner) -> Just inner
    _ -> Nothing

-- | Edit the value an object carries at @key@. A missing field stays absent.
adjustField :: Key.Key -> (Value -> Value) -> KeyMap Value -> KeyMap Value
adjustField key edit o = case KeyMap.lookup key o of
    Just v -> KeyMap.insert key (edit v) o
    Nothing -> o
