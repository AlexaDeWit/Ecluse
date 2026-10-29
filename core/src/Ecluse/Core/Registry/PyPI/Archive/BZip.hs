-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | BZIP2 decoding with one resource scope per compressed member.
module Ecluse.Core.Registry.PyPI.Archive.BZip (withBzipChunks) where

import Conduit
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Resource (InternalState, allocate, closeInternalState, createInternalState, release, runInternalState)
import Data.ByteString qualified as BS
import Data.Conduit.BZlib qualified as BZip
import System.IO.Error (ioeGetErrorString, isUserError)
import UnliftIO.Exception (bracket, tryJust)

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Archive.Tar (TarDecode)

-- | Charge each validated block-size declaration before native allocation. Refusal closes every registered resource.
withBzipChunks :: Int -> (Int -> IO ()) -> ByteString -> (ConduitT () ByteString TarDecode () -> IO a) -> IO a
withBzipChunks limit charge bytes consume =
    bracket createInternalState closeInternalState $ \scope ->
        consume (sourceChunks bytes .| members scope 0)
  where
    members scope count = do
        header <- takeCE (4 :: Int) .| foldC
        when (BS.null header && count == 0) (lift (throwError (ParseError "missing bzip2 stream")))
        unless (BS.null header) $ do
            when (count >= limit) (lift (throwError (ParseError "bzip2 member count exceeds its bound")))
            case BS.unpack header of
                [66, 90, 104, level] | level >= 49 && level <= 57 -> do
                    -- libbzip2 documents 100 kB + 4 * block size. The conduit buffers 32 KiB input and 4 KiB output.
                    liftIO (charge (100000 + 4 * 100000 * fromIntegral (level - 48) + 32768 + 4096))
                    leftover header
                    (key, memberScope) <- liftIO (runInternalState (allocate createInternalState closeInternalState) scope)
                    transPipe (runNative memberScope) (BZip.decompress1 BZip.def)
                    liftIO (runInternalState (release key) scope)
                    members scope (count + 1)
                _ -> lift (throwError (ParseError "malformed bzip2 block-size header"))

sourceChunks :: ByteString -> ConduitT i ByteString TarDecode ()
sourceChunks bytes = unless (BS.null bytes) $ do
    let (chunk, remaining) = BS.splitAt 32768 bytes
    yield chunk
    sourceChunks remaining

runNative :: InternalState -> ResourceT IO a -> TarDecode a
runNative scope action = ExceptT $ do
    result <- tryJust malformed (runInternalState action scope)
    pure (first (const (ParseError "malformed or truncated bzip2 archive")) result)
  where
    malformed fault
        | isUserError fault && ioeGetErrorString fault `elem` ["BZ2_bzDecompress: -4", "BZ2_bzDecompress: -5", "unexpected EOF on decompress"] = Just ()
        | otherwise = Nothing
