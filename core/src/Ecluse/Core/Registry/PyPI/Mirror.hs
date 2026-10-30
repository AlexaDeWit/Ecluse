-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed simple pages restore retained file entries under the current target's URLs.
module Ecluse.Core.Registry.PyPI.Mirror (
    pypiMirrorDocuments,
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray (smallArrayFromList)

import Ecluse.Core.Mirror.Documents (MirrorDocuments (..), restoreFragments)
import Ecluse.Core.Mirror.State (MirrorVersion, recordEnvelope)
import Ecluse.Core.Package (Artifact (..), PackageDetails (pkgArtifacts))
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.CachedDocument (pypiPacked, pypiSimpleCached)
import Ecluse.Core.Registry.Json.Packed (docTable)
import Ecluse.Core.Registry.Json.Writer (packTree)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (PyPIMirrorRead))
import Ecluse.Core.Registry.PyPI.Document (packedSimple, simpleDocument, simpleEnvelope, simpleFiles, simpleSourceCompanions, urlHole)

-- | Files keep array coordinates and versions keep version-text order. The listing envelope is the retained project's.
pypiMirrorDocuments :: MirrorDocuments
pypiMirrorDocuments =
    MirrorDocuments
        { mdRestore = restoreSimpleVersion
        , mdOrder = id
        , mdEntryKey = ArrayEntry
        , mdRekey = \rekey raw -> do
            simple <- snd pypiSimpleCached raw
            pure (fst pypiSimpleCached (simpleDocument (simpleEnvelope simple) [(rekey key, value) | (key, value) <- simpleFiles simple]))
        , mdAssemble = \record docs ->
            let files = concat [simpleFiles document | doc <- docs, Just raw <- [vdRaw doc], Just document <- [snd pypiSimpleCached raw]]
             in fst pypiPacked (packedSimple (recordEnvelope record) (docTable (smallArrayFromList [])) [(key, packTree urlHole value) | (key, value) <- files])
        , mdCompanions = simpleSourceCompanions
        }

restoreSimpleVersion :: MirrorVersion -> [Artifact] -> Maybe VersionDoc
restoreSimpleVersion mirror current = do
    fragments@((firstDetails, (), _) :| _) <- restoreFragments ArrayEntry simpleEntries mirror current
    let pairs = concatMap (\(_, _, restored) -> toList restored) fragments
    files <- traverse currentUrl pairs
    artifacts <- nonEmpty (map fst pairs)
    pure (VersionDoc firstDetails{pkgArtifacts = artifacts} (Just (fst pypiSimpleCached (simpleDocument mempty files))))
  where
    simpleEntries = \case
        PyPIMirrorRead document -> Just ((), Map.fromList (simpleFiles document))
        _ -> Nothing
    currentUrl (artifact, entry) = case entry of
        Object fields -> Just (artEntryKey artifact, Object (KeyMap.insert "url" (String (artUrl artifact)) fields))
        _ -> Nothing
