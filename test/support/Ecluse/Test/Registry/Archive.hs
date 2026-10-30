-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native tar fixtures shared across registry ecosystems.
module Ecluse.Test.Registry.Archive (tarBytes, paxRecord) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Ecluse.Test.Support (expectRight)

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
