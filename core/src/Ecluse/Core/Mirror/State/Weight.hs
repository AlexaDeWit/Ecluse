-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Durable graph accounting counts aliases per occurrence and complete text backing allocations.
module Ecluse.Core.Mirror.State.Weight (retainedRecordWeight, retainedFragmentWeight, valueBackingExcess) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text.Unsafe (lengthWord8)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Package
import Ecluse.Core.Registry (MirrorArtifact (..))
import Ecluse.Core.Registry.CachedDocument (cargoCached, estimateValueBytes, npmCached, pypiSimpleCached, rubyGemsCached)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.MirrorUpload (ExpectedMirrorFile, MirrorReadData (..), expectedArtifact, expectedDescriptor)
import Ecluse.Core.Registry.PyPI.Document (simpleEnvelope, simpleFiles)
import Ecluse.Core.Server.Cache.VersionWeight.Fields
import Ecluse.Core.Server.MemoryModel (expandWireBytesInteger)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Text (textStorageBytes)

-- | The project shell covers map roots and fixed identities. Companion bodies belong to their read reservation.
retainedRecordWeight :: ProjectMirrorRecord -> Integer
retainedRecordWeight record =
    4096
        + textWeight (targetKey (recordTarget record))
        + nameWeight (recordPackage record)
        + expandWireBytesInteger (toInteger (estimateValueBytes envelope))
        + valueBackingExcess envelope
        + sum [256 + textWeight key + versionWeight version | (key, version) <- Map.toList (recordVersions record)]
  where
    envelope = Object (recordEnvelope record)

versionWeight :: MirrorVersion -> Integer
versionWeight version =
    sum (map retainedFragmentWeight (toList (versionFragments version)))
        + sum [256 + textWeight name + hashWeight digest | (name, digest) <- Map.toList (versionVerifiedIdentities version)]
        + sum [256 + textWeight name + companionWeight companion | (name, companion) <- Map.toList (versionCompanions version)]

-- | PyPI's three 256-byte blocks cover identity, parsed/time/install shells, and fragment/source roots.
retainedFragmentWeight :: StoredMirrorFragment -> Integer
retainedFragmentWeight (StoredMirrorFragment details source expected) =
    shell + detailsFieldsWeight details + sourceWeight source + sum (map expectedWeight (toList expected))
  where
    shell = case source of
        PyPIMirrorRead _ | pkgEcosystem (pkgName details) == PyPI -> 768
        _ -> 256 + 16 * 1024

sourceWeight :: MirrorReadData -> Integer
sourceWeight = \case
    NpmMirrorRead fields -> retainedRawWeight (fst npmCached (Object fields)) + valueBackingExcess (Object fields)
    PyPIMirrorRead document ->
        retainedRawWeight (fst pypiSimpleCached document)
            + valueBackingExcess (Object (simpleEnvelope document))
            + sum [weighEntryKey key + valueBackingExcess value | (key, value) <- simpleFiles document]
    RubyGemsMirrorRead document -> retainedRawWeight (fst rubyGemsCached document)
    CargoMirrorRead document -> retainedRawWeight (fst cargoCached document) + sum [weighEntryKey key + valueBackingExcess value | (key, value) <- cargoRecords document]

expectedWeight :: ExpectedMirrorFile -> Integer
expectedWeight expected =
    256
        + 256
        + artifactWeight (expectedArtifact expected)
        + textWeight (unFilename (maFilename descriptor))
        + sum (map hashWeight (toList (maHashes descriptor)))
  where
    descriptor = expectedDescriptor expected

hashWeight :: Hash -> Integer
hashWeight digest = 256 + textWeight (hashValue digest)

companionWeight :: CompanionState -> Integer
companionWeight CompanionPending = 0
companionWeight (CompanionRetained (CompanionRef digest _)) = 256 + hashWeight digest

-- | Compact JSON estimates count visible bytes. Add each string or key's uncounted backing allocation.
valueBackingExcess :: Value -> Integer
valueBackingExcess = \case
    String text -> textExcess text
    Array values -> sum (map valueBackingExcess (toList values))
    Object fields -> sum [textExcess (Key.toText key) + valueBackingExcess value | (key, value) <- KeyMap.toList fields]
    _ -> 0
  where
    textExcess text = max 0 (toInteger (textStorageBytes text) - toInteger (lengthWord8 text))
