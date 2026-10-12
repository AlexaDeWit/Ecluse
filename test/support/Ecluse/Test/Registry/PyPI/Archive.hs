-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Native Python archive fixtures shared by extraction and tar-framing specs.
module Ecluse.Test.Registry.PyPI.Archive (metadata, tarBytes, zipBytes, zip64Bytes, paxRecord) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Archive.Zip.Conduit.Zip qualified as Zip
import Conduit
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Time (LocalTime (..), fromGregorian, midnight)
import Ecluse.Test.Osv (osvZipOf)
import Ecluse.Test.Support (expectRight)

-- | The same Core Metadata payload for every native archive format.
metadata :: ByteString
metadata = "Metadata-Version: 2.4\nName: example\nVersion: 1\n\nDescription.\n"

-- | Use the existing native ZIP fixture generator.
zipBytes :: [(Text, ByteString)] -> IO ByteString
zipBytes entries = BSL.toStrict <$> osvZipOf (map (second BSL.fromStrict) entries)

-- | Build tar headers and payload framing with the native tar writer.
tarBytes :: [(FilePath, ByteString)] -> IO BSL.ByteString
tarBytes entries = Tar.write <$> traverse entry entries
  where
    entry (path, bytes) = do
        tarPath <- expectRight (Tar.toTarPath False path)
        pure (Tar.fileEntry tarPath (BSL.fromStrict bytes))

-- | Frame one POSIX pax attribute with its decimal byte count.
paxRecord :: ByteString -> BSL.ByteString
paxRecord payload = BSL.fromStrict (encodeUtf8 (show size :: Text) <> " " <> payload)
  where
    size = settle (BS.length payload + 2)
    settle candidate =
        let next = BS.length payload + 1 + length (show candidate :: String)
         in if next == candidate then candidate else settle next

-- | The shared ZIP helper fixes default options, so this variant enables native ZIP64 records.
zip64Bytes :: [(Text, ByteString)] -> IO ByteString
zip64Bytes entries = BSL.toStrict <$> runConduit (yieldMany (map entry entries) .| void (Zip.zipStream Zip.defaultZipOptions{Zip.zipOpt64 = True}) .| sinkLazy)
  where
    entry (name, bytes) = (Zip.ZipEntry (Left name) (LocalTime (fromGregorian 2026 1 1) midnight) Nothing Nothing, Zip.ZipDataSource (yield bytes))
