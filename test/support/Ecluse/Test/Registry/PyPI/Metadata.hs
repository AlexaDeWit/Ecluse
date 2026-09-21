-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Caller-owned byte fixtures use the production PyPI extraction and projection.
module Ecluse.Test.Registry.PyPI.Metadata (
    projectPyPIIndex,
    projectPyPIVersion,
    projectPyPIChunks,
    documentFromValue,
) where

import Data.Aeson (Value (Array, Object))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageDetails, PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.JsonStream (StreamResult)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded))
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, simpleDocument)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIStream)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..), pypiFields)
import Ecluse.Core.Registry.PyPI.StreamingProjection (PyPIProjection, collectField, emptyProjection)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits, maxMetadataBytes, maxNestingDepth)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)

-- | Project a complete fixture through the same compact extraction as an HTTP response.
projectPyPIIndex :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, SimpleDocument)
projectPyPIIndex limits name body = projectPyPIChunks limits name FullRead [body] >>= projectPyPIStream limits name

-- | Select one release without retaining its siblings, using original file positions.
projectPyPIVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError (Maybe PackageDetails)
projectPyPIVersion limits name version body = do
    streamed <- projectPyPIChunks limits name (SelectedRead name (renderVersion version)) [body]
    (info, _) <- projectPyPIStream limits name streamed
    pure (Map.lookup (renderVersion version) (infoVersions info))

-- | Exercise explicit chunk boundaries with the production byte counter and source digest.
projectPyPIChunks :: Limits -> PackageName -> PyPIRead -> [ByteString] -> Either MetadataError (StreamResult PyPIProjection)
projectPyPIChunks limits name mode =
    first MetadataBoundExceeded
        . parseJsonChunks
            (MetadataBodyLimit (maxMetadataBytes limits))
            (pypiFields (maxNestingDepth limits) mode)
            (collectField limits name mode)
            emptyProjection

-- | Build assembly fixtures without projection, including intentionally malformed entries.
documentFromValue :: Value -> SimpleDocument
documentFromValue = \case
    Object fields -> simpleDocument fields $ case KeyMap.lookup "files" fields of
        Just (Array files) -> zipWith (\position value -> (ArrayEntry position, value)) [0 ..] (toList files)
        _ -> []
    _ -> simpleDocument mempty []
