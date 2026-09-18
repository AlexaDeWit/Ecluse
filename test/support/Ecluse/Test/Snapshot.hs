-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Snapshot fixtures share one byte digest between projection and assembly, and build the
version pair a read carries when a case decides on the typed view alone.
-}
module Ecluse.Test.Snapshot (
    jsonSnapshot,
    projectJsonSnapshot,
    syntheticSnapshot,
    versionDocOf,
    versionReadOf,
    readDetails,
) where

import Data.Aeson (Value, encode)

import Ecluse.Core.Package (PackageDetails)
import Ecluse.Core.Registry.Metadata (VersionDoc (VersionDoc, vdDetails, vdRaw), VersionRead (VersionRead, vrUpstreamLatest, vrVersion))
import Ecluse.Core.Snapshot (Snapshot (..), digestOf)
import Ecluse.Core.Version (Version)
import Ecluse.Test.Support (expectRight)

-- | Treat a fixture's compact encoding as its upstream bytes.
jsonSnapshot :: Value -> Snapshot Value
jsonSnapshot value = Snapshot (digestOf (toStrict (encode value))) value

-- | Project and fingerprint the same fixture bytes.
projectJsonSnapshot :: (Show err) => (ByteString -> Either err a) -> Value -> IO (Snapshot a)
projectJsonSnapshot project value = do
    let body = toStrict (encode value)
    Snapshot (digestOf body) <$> expectRight (project body)

-- | Scope a synthetic domain fixture to its textual representation, without a wire adapter.
syntheticSnapshot :: (Show a) => a -> Snapshot a
syntheticSnapshot value = Snapshot (digestOf (encodeUtf8 (show value :: Text))) value

-- | A synthetic pair with no raw object, for a case that decides on the typed view alone.
versionDocOf :: PackageDetails -> Snapshot VersionDoc
versionDocOf details = syntheticSnapshot VersionDoc{vdDetails = details, vdRaw = Nothing}

-- | A version read carrying the given release and the document's own latest.
versionReadOf :: Maybe PackageDetails -> Maybe Version -> VersionRead
versionReadOf details upstreamLatest = VersionRead{vrVersion = versionDocOf <$> details, vrUpstreamLatest = upstreamLatest}

-- | The typed side of a read, for parity against the whole-document projection.
readDetails :: VersionRead -> Maybe PackageDetails
readDetails = fmap (vdDetails . snapshotValue) . vrVersion
