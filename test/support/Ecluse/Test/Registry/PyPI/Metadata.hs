-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI fixtures pass through the production extraction and projection, and documents convert to and from JSON.
module Ecluse.Test.Registry.PyPI.Metadata (
    projectPyPIIndex,
    projectPyPIVersion,
    projectPyPIChunks,
    documentFromValue,
    simpleValue,
) where

import Data.Aeson (Value (Array, Object))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageDetails, PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.JsonStream (StreamResult)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded))
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, simpleDocument, simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIStream, pypiIndexWalk)
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..))
import Ecluse.Core.Registry.PyPI.StreamingProjection (PyPIProjection)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits, maxMetadataBytes)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Registry.JsonStream (testTable, walkJsonChunks)

-- | Project a complete fixture through the same compact extraction as an HTTP response.
projectPyPIIndex :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, SimpleDocument)
projectPyPIIndex limits name body = projectPyPIChunks limits name FullRead [body] >>= projectPyPIStream limits name

-- | Select one release without retaining its siblings, using original file positions.
projectPyPIVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError (Maybe PackageDetails)
projectPyPIVersion limits name version body = do
    streamed <- projectPyPIChunks limits name (SelectedRead name (renderVersion version)) [body]
    (info, _) <- projectPyPIStream limits name streamed
    pure (Map.lookup (renderVersion version) (infoVersions info))

-- | Exercise explicit chunk boundaries with the production byte counter.
projectPyPIChunks :: Limits -> PackageName -> PyPIRead -> [ByteString] -> Either MetadataError (StreamResult PyPIProjection)
projectPyPIChunks limits name mode =
    first MetadataBoundExceeded
        . walkJsonChunks
            (MetadataBodyLimit (maxMetadataBytes limits))
            (pypiIndexWalk limits name mode (testTable fileUniqueFields))

-- | Build assembly fixtures without projection, including intentionally malformed entries.
documentFromValue :: Value -> SimpleDocument
documentFromValue = \case
    Object fields -> simpleDocument fields $ case KeyMap.lookup "files" fields of
        Just (Array files) -> zipWith (\position value -> (ArrayEntry position, value)) [0 ..] (toList files)
        _ -> []
    _ -> simpleDocument mempty []

-- | Render a document as the JSON object its encoder writes, for field-level assertions.
simpleValue :: SimpleDocument -> Value
simpleValue document =
    Object (KeyMap.insert "files" (Array (fromList (map snd (simpleFiles document)))) (simpleEnvelope document))
