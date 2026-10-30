-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native tar fixtures shared across registry ecosystems.
module Ecluse.Test.Registry.Archive (tarBytes, tarEntries, regular, pax, paxAt, paxRecord) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Ecluse.Test.Support (expectRight)

-- | Build tar headers and payload framing with the native tar writer.
tarBytes :: [(FilePath, ByteString)] -> IO BSL.ByteString
tarBytes entries = Tar.write <$> traverse (uncurry regular) entries

-- | Read every entry of a tar stream in order, failing the test on a malformed stream.
tarEntries :: BSL.ByteString -> IO [Tar.Entry]
tarEntries = Tar.foldEntries (\entry rest -> (entry :) <$> rest) (pure []) (fail . show) . Tar.read

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
