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
    overlayObjectSurvivors,
    overlayObjectSources,

    -- * The interpolated-name gate
    safeDocumentName,

    -- * Rebasing an artifact location
    rebaseArtifactUrl,
    artifactUrlPacked,

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
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

import Ecluse.Core.Package.Entry (AdmittedEntry (..), EntryKey (..), validEntryKey)
import Ecluse.Core.Package.Merge (MergePlan (mpArtifacts, mpSurvivors), SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Json.Packed (Packed, withoutHole)
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

-- The plan's admissions of this source's snapshot under versions the source won, in version order.
sourceAdmissions :: MergePlan -> SourceId -> Snapshot src -> [(EntryKey, (Text, AdmittedEntry))]
sourceAdmissions plan sid source =
    [ (admittedKey entry, (version, entry))
    | (version, entries) <- Map.toList (mpArtifacts plan)
    , Map.lookup version (mpSurvivors plan) == Just sid
    , entry <- toList entries
    , admittedSnapshot entry == snapshotDigest source
    ]

-- A slot costs one word per position and a map node six per admission, so the table serves the denser case.
selectEntries :: [(EntryKey, (Text, AdmittedEntry))] -> [(EntryKey, entry)] -> [(Text, entry)]
selectEntries admissions entries = case lastAscending entries of
    Just end | end < 6 * length admissions -> slotted (admissionSlots end admissions) entries
    Just _ -> keyed (uniqueEntries admissions) entries
    Nothing -> keyed (uniqueEntries admissions) (filter (\(key, _) -> Map.member key unrepeated) entries)
  where
    unrepeated = uniqueEntries entries

-- The last of strictly ascending non-negative array positions, which are already unique.
lastAscending :: [(EntryKey, entry)] -> Maybe Int
lastAscending = go (-1)
  where
    go !previous = \case
        (ArrayEntry position, _) : rest | position > previous -> go position rest
        [] -> Just previous
        _ -> Nothing

data Slot = Vacant | Admitted Text AdmittedEntry | Ambiguous

-- One slot per position up to @end@. A position admitted twice selects nothing.
admissionSlots :: Int -> [(EntryKey, (Text, AdmittedEntry))] -> Vector Slot
admissionSlots end admissions = V.create $ do
    slots <- MV.replicate (end + 1) Vacant
    for_ admissions $ \case
        (ArrayEntry position, (version, kept)) ->
            MV.readMaybe slots position >>= traverse_ (\slot -> MV.write slots position $! occupy version kept slot)
        _ -> pass
    pure slots

occupy :: Text -> AdmittedEntry -> Slot -> Slot
occupy version kept = \case
    Vacant -> Admitted version kept
    _ -> Ambiguous

slotted :: Vector Slot -> [(EntryKey, entry)] -> [(Text, entry)]
slotted slots entries =
    [ (version, entry)
    | (ArrayEntry position, entry) <- entries
    , Just (Admitted version kept) <- [slots V.!? position]
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
overlayObjectSurvivors entriesOf bySource plan = [(version, entry) | (version, _, entry) <- overlayObjectSources entriesOf bySource plan]

-- | 'overlayObjectSurvivors' with the source each entry came from.
overlayObjectSources :: (src -> KeyMap entry) -> Map SourceId (Snapshot src) -> MergePlan -> [(Text, SourceId, entry)]
overlayObjectSources entriesOf bySource plan =
    [ (version, sid, entry)
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
usableEntry entry = validEntryKey (admittedKey entry) && not (T.null (admittedFilename entry))

-- | Keep only keys that occur exactly once, so ambiguous source coordinates cannot select an entry.
uniqueEntries :: (Ord key) => [(key, value)] -> Map key value
uniqueEntries = Map.mapMaybe id . Map.fromListWith (\_ _ -> Nothing) . map (second Just)

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

-- | Remove an unusable URL hole without changing the source payload or encoded length.
artifactUrlPacked :: Maybe Text -> Packed -> Packed
artifactUrlPacked url packed
    | isJust (url >>= rebaseArtifactUrl Just) = packed
    | otherwise = withoutHole packed

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
