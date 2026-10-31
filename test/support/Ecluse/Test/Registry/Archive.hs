-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native tar fixtures shared across registry ecosystems.
module Ecluse.Test.Registry.Archive (tarBytes, tarEntries, readTarEntries, regular, pax, paxAt, paxRecord, checksumHeader, patchFirstHeader) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Numeric (showOct)

import Ecluse.Test.Support (expectRight)

-- | Build tar headers and payload framing with the native tar writer.
tarBytes :: [(FilePath, ByteString)] -> IO BSL.ByteString
tarBytes entries = Tar.write <$> traverse (uncurry regular) entries

-- | Read every entry of a tar stream in order, failing the test on a malformed stream.
tarEntries :: BSL.ByteString -> IO [Tar.Entry]
tarEntries = either (fail . toString) pure . readTarEntries

-- | 'tarEntries' as a value, reporting a malformed stream as 'Left'.
readTarEntries :: BSL.ByteString -> Either Text [Tar.Entry]
readTarEntries = Tar.foldEntries (\entry rest -> (entry :) <$> rest) (Right []) (Left . show) . Tar.read

-- | One regular file. A path over 100 bytes splits into a ustar prefix and name.
regular :: FilePath -> ByteString -> IO Tar.Entry
regular path bytes = do
    name <- expectRight (Tar.toTarPath False path)
    pure (Tar.fileEntry name (BSL.fromStrict bytes))

-- | One extended header of the given type flag, such as PAX @x@ or @g@, over raw records.
pax :: Char -> BSL.ByteString -> IO Tar.Entry
pax = paxAt "PaxHeaders/entry"

-- | 'pax' under its own header name, for a reader that lists each extended header as a member.
paxAt :: FilePath -> Char -> BSL.ByteString -> IO Tar.Entry
paxAt path code bytes = do
    name <- expectRight (Tar.toTarPath False path)
    pure (Tar.simpleEntry name (Tar.OtherEntryType code bytes (BSL.length bytes)))

-- | Frame one POSIX pax attribute with its decimal byte count.
paxRecord :: ByteString -> BSL.ByteString
paxRecord payload = BSL.fromStrict (encodeUtf8 (show size :: Text) <> " " <> payload)
  where
    size = settle (BS.length payload + 2)
    settle candidate =
        let next = BS.length payload + 1 + length (show candidate :: String)
         in if next == candidate then candidate else settle next

-- | Recompute a 512-byte tar header's checksum field, for a header a case edited byte by byte.
checksumHeader :: ByteString -> ByteString
checksumHeader header = BS.take 148 changed <> checksum <> BS.drop 156 changed
  where
    changed = BS.take 148 header <> BS.replicate 8 32 <> BS.drop 156 header
    digits = toText (showOct (BS.foldl' (\total byte -> total + fromIntegral byte) (0 :: Integer) changed) "")
    checksum = encodeUtf8 (T.replicate (6 - T.length digits) "0" <> digits) <> "\NUL "

-- | Overwrite a tar stream's first header from the given byte offset, then repair its checksum.
patchFirstHeader :: Int -> ByteString -> ByteString -> ByteString
patchFirstHeader offset bytes archive = checksumHeader (BS.take offset header <> bytes <> BS.drop (offset + BS.length bytes) header) <> BS.drop 512 archive
  where
    header = BS.take 512 archive
