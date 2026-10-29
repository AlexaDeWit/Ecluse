-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Scoped XZ and legacy LZMA decoding under the same native dictionary limit.
module Ecluse.Core.Registry.PyPI.Archive.Lzma (LzmaFormat (..), withLzmaChunks) where

import Codec.Compression.Lzma qualified as Lzma
import Conduit
import Control.Monad.Except (throwError)
import Data.ByteString qualified as BS
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Archive.Tar (TarDecode)

-- | ZIP's LZMA properties are converted to an alone-format header before decoding.
data LzmaFormat = XzArchive | ZipLzmaMember

data NativeLzmaFault = LzmaAllocationFailed | LzmaInvalidState
    deriving stock (Show)

instance Exception NativeLzmaFault

-- | Pay the enforced native memory limit before creation. The callback cannot retain the source beyond its scope.
withLzmaChunks :: LzmaFormat -> (Int -> IO ()) -> [ByteString] -> (ConduitT () ByteString TarDecode () -> IO a) -> IO a
withLzmaChunks format charge input consume = do
    charge (64 * 1024 * 1024 + 32752)
    Lzma.withDecompressIO params (consume . go input)
  where
    params =
        Lzma.defaultDecompressParams
            { Lzma.decompressMemLimit = 64 * 1024 * 1024
            , Lzma.decompressAutoDecoder = case format of XzArchive -> False; ZipLzmaMember -> True
            , Lzma.decompressConcatenated = case format of XzArchive -> True; ZipLzmaMember -> False
            }
    go remaining = \case
        Lzma.DecompressInputRequired supply -> do
            let (chunk, rest) = inputChunk remaining
            liftIO (supply chunk) >>= go rest
        Lzma.DecompressOutputAvailable chunk next -> yield chunk >> liftIO next >>= go remaining
        Lzma.DecompressStreamEnd rest ->
            unless (BS.null rest && all BS.null remaining) (lift (throwError (ParseError "LZMA stream has trailing compressed bytes")))
        Lzma.DecompressStreamError failure -> case failure of
            -- Native allocation and programming faults must reach the worker fault boundary.
            Lzma.LzmaRetMemError -> liftIO (throwIO LzmaAllocationFailed)
            Lzma.LzmaRetProgError -> liftIO (throwIO LzmaInvalidState)
            _ -> lift (throwError (ParseError "malformed, truncated, or excessive-memory LZMA stream"))

inputChunk :: [ByteString] -> (ByteString, [ByteString])
inputChunk [] = (BS.empty, [])
inputChunk (bytes : remaining)
    | BS.null bytes = inputChunk remaining
    | otherwise =
        let (chunk, rest) = BS.splitAt 32768 bytes
         in (chunk, if BS.null rest then remaining else rest : remaining)
