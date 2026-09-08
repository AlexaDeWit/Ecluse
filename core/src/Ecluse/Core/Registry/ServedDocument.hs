-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared outbound document assembly, paired with inbound "Ecluse.Core.Registry.WireSupport".
Ecosystem adapters own their wire shapes and reuse these name and location gates.
-}
module Ecluse.Core.Registry.ServedDocument (
    -- * Replaying a merge plan
    overlaySurvivors,

    -- * The interpolated-name gate
    safeDocumentName,

    -- * Rebasing an artifact location
    rebaseArtifactUrl,

    -- * Reading a raw document
    stringField,
) where

import Data.Aeson (Value (String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Package.Entry (AdmittedEntry (..), EntryKey (..))
import Ecluse.Core.Package.Merge (MergePlan (mpArtifacts, mpSurvivors), SourceId)
import Ecluse.Core.Snapshot (Snapshot (..))
import Ecluse.Core.Text (urlFilename)

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
    , validKey key
    , Map.member key unambiguous
    , Just (version, kept) <- [Map.lookup (sid, snapshotDigest source, key) admitted]
    , not (T.null (admittedFilename kept))
    ]
  where
    admitted =
        uniqueEntries
            [ ((sid, admittedSnapshot entry, admittedKey entry), (version, entry))
            | (version, entries) <- Map.toList (mpArtifacts plan)
            , Just sid <- [Map.lookup version (mpSurvivors plan)]
            , entry <- toList entries
            ]

uniqueEntries :: (Ord key) => [(key, value)] -> Map key value
uniqueEntries = Map.mapMaybe id . Map.fromListWith (\_ _ -> Nothing) . map (second Just)

validKey :: EntryKey -> Bool
validKey = \case
    ArrayEntry position -> position >= 0
    ObjectEntry _ -> True
    SingletonEntry -> True

{- | What the ecosystem's own name parser makes of the name a document claims for itself. The
projection already refuses such a name, so this is defence in depth on the interpolated URL.
-}
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

-- | The 'Text' at @key@ in a raw document object, if present and a JSON string.
stringField :: Key.Key -> KeyMap Value -> Maybe Text
stringField key o = case KeyMap.lookup key o of
    Just (String s) -> Just s
    _ -> Nothing
