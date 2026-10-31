-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed simple pages restore retained file entries under the current target's URLs.
module Ecluse.Core.Registry.PyPI.Mirror (
    pypiMirrorDocuments,
    pypiStateFormat,
) where

import Data.Aeson (Value (Object, String), object, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray (smallArrayFromList)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.Documents (MirrorDocuments (..), restoreFragments)
import Ecluse.Core.Mirror.State (MirrorVersion, recordEnvelope)
import Ecluse.Core.Mirror.State.Codec (forceSourceEntries, parseSourceEntries, sourceEntries, sourceEntriesCharge)
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat (..), ProjectEnvelope (..))
import Ecluse.Core.Mirror.State.Weight (fragmentShellAllowance, sourceEntriesWeight, valueBackingExcess)
import Ecluse.Core.Package (Artifact (..), PackageDetails (pkgArtifacts, pkgName), pkgEcosystem)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.CachedDocument (pypiPacked, pypiSimpleCached)
import Ecluse.Core.Registry.Json.Packed (docTable)
import Ecluse.Core.Registry.Json.Writer (packTree)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (PyPIMirrorRead))
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorFragment)
import Ecluse.Core.Registry.PyPI.Document (packedSimple, simpleDocument, simpleEnvelope, simpleFiles, simpleSourceCompanions, urlHole)
import Ecluse.Core.Server.Cache.VersionWeight.Fields (retainedRawWeight)

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
        }

-- | A record keeps the project's envelope once, and each fragment keeps its files under their array coordinates.
pypiStateFormat :: MirrorStateFormat
pypiStateFormat =
    MirrorStateFormat
        { msfKind = "pypi"
        , msfEncodeSource = \case
            PyPIMirrorRead document -> Just (object ["kind" .= ("pypi" :: Text), "envelope" .= simpleEnvelope document, "entries" .= sourceEntries (simpleFiles document)])
            _ -> Nothing
        , msfDecodeSource = \value -> do
            envelope <- value .: "envelope"
            PyPIMirrorRead . simpleDocument envelope <$> parseSourceEntries "file entry" value
        , msfDecodeCharge = sourceEntriesCharge
        , msfForceSource = \case
            PyPIMirrorRead document -> force (simpleEnvelope document) `seq` forceSourceEntries (simpleFiles document)
            _ -> ()
        , msfEnvelope =
            ProjectEnvelope
                { envelopeSplit = \case
                    PyPIMirrorRead document -> (simpleEnvelope document, PyPIMirrorRead (simpleDocument mempty (simpleFiles document)))
                    other -> (mempty, other)
                , envelopeCheck = \envelope details -> \case
                    PyPIMirrorRead document -> first (const "invalid project envelope identity") (void (restoreMirrorFragment details (PyPIMirrorRead (simpleDocument envelope (simpleFiles document)))))
                    _ -> Left "invalid project envelope identity"
                }
        , -- Three 256-byte blocks cover identity, parsed, time and install shells, and fragment and source roots.
          msfFragmentShell = \details -> if pkgEcosystem (pkgName details) == PyPI then 768 else fragmentShellAllowance
        , msfSourceWeight = \case
            PyPIMirrorRead document -> retainedRawWeight (fst pypiSimpleCached document) + valueBackingExcess (Object (simpleEnvelope document)) + sourceEntriesWeight (simpleFiles document)
            _ -> 0
        , msfCompanions = simpleSourceCompanions
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
