-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Durable graph accounting counts aliases per occurrence and complete text backing allocations.
module Ecluse.Core.Mirror.State.Weight (retainedRecordWeight, retainedFragmentWeight, fragmentShellAllowance, sourceEntriesWeight, valueBackingExcess) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text.Unsafe (lengthWord8)

import Ecluse.Core.Mirror.State.Format (MirrorStateFormat (msfFragmentShell, msfSourceWeight))
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry (MirrorArtifact (..))
import Ecluse.Core.Registry.CachedDocument (estimateValueBytes)
import Ecluse.Core.Registry.MirrorUpload (ExpectedMirrorFile, expectedArtifact, expectedDescriptor)
import Ecluse.Core.Server.Cache.VersionWeight.Fields
import Ecluse.Core.Server.MemoryModel (expandWireBytesInteger)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Text (textStorageBytes)

-- | The project shell covers map roots and fixed identities. Companion bodies belong to their read reservation.
retainedRecordWeight :: MirrorStateFormat -> ProjectMirrorRecord -> Integer
retainedRecordWeight format record =
    4096
        + textWeight (targetKey (recordTarget record))
        + nameWeight (recordPackage record)
        + expandWireBytesInteger (toInteger (estimateValueBytes envelope))
        + valueBackingExcess envelope
        + sum [256 + textWeight key + versionWeight format version | (key, version) <- Map.toList (recordVersions record)]
  where
    envelope = Object (recordEnvelope record)

versionWeight :: MirrorStateFormat -> MirrorVersion -> Integer
versionWeight format version =
    sum (map (retainedFragmentWeight format) (toList (versionFragments version)))
        + sum [256 + textWeight name + hashWeight digest | (name, digest) <- Map.toList (versionVerifiedIdentities version)]
        + sum [256 + textWeight name + companionWeight companion | (name, companion) <- Map.toList (versionCompanions version)]

-- | A fragment's format declares its shell allowance and its stored source's weight.
retainedFragmentWeight :: MirrorStateFormat -> StoredMirrorFragment -> Integer
retainedFragmentWeight format (StoredMirrorFragment details source expected) =
    msfFragmentShell format details + detailsFieldsWeight details + msfSourceWeight format source + sum (map expectedWeight (toList expected))

-- | A fragment's shell allowance for its identity, parsed version, time and install facts, and source roots.
fragmentShellAllowance :: Integer
fragmentShellAllowance = 256 + 16 * 1024

-- | Stored entries' coordinates and the uncounted backing of their values.
sourceEntriesWeight :: [(EntryKey, Value)] -> Integer
sourceEntriesWeight entries = sum [weighEntryKey key + valueBackingExcess value | (key, value) <- entries]

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
