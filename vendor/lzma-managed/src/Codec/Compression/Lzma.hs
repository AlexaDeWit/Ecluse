-- SPDX-FileCopyrightText: 2015 Herbert Valerio Riedel
--
-- SPDX-License-Identifier: BSD-3-Clause

-- | Scoped incremental LZMA and XZ decoding for the archive reader.
module Codec.Compression.Lzma
    ( DecompressStream(..)
    , withDecompressIO
    , LzmaRet(..)
    , defaultDecompressParams
    , DecompressParams
    , decompressTellNoCheck
    , decompressTellUnsupportedCheck
    , decompressTellAnyCheck
    , decompressConcatenated
    , decompressAutoDecoder
    , decompressMemLimit
    ) where

import           Control.Exception (bracket)
import           Control.Monad ((>=>), unless)
import           Control.Monad.ST (stToIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           LibLzma

-- | A decoder state valid only inside its 'withDecompressIO' callback.
data DecompressStream m =
     DecompressInputRequired !(ByteString -> m (DecompressStream m)) -- ^ Decoding process requires input to proceed. An empty 'ByteString' chunk signals end of input.
   | DecompressOutputAvailable !ByteString !(m (DecompressStream m)) -- ^ Decompressed output chunk available.
   | DecompressStreamEnd !ByteString -- ^ Decoding finished with these unconsumed bytes.
   | DecompressStreamError !LzmaRet

-- | Scope decoder state to the callback, including early return and cancellation. Do not retain the stream outside it.
withDecompressIO :: DecompressParams -> (DecompressStream IO -> IO a) -> IO a
withDecompressIO parms consume =
    bracket (stToIO (newDecodeLzmaStream parms))
            (either (const (return ())) (stToIO . endLzmaStream))
            (either (consume . DecompressStreamError) (startDecompressIO >=> consume))

startDecompressIO :: LzmaStream -> IO (DecompressStream IO)
startDecompressIO = go
  where
    bUFSIZ = 32752

    go :: LzmaStream -> IO (DecompressStream IO)
    go ls = return inputRequired
      where
        inputRequired = DecompressInputRequired goInput

        goInput :: ByteString -> IO (DecompressStream IO)
        goInput chunk
          | BS.null chunk = goFinish
          | otherwise = do
            (rc, used, obuf) <- stToIO $ runLzmaStream ls chunk LzmaRun bUFSIZ

            let chunk' = BS.drop used chunk

            case rc of
                LzmaRetOK
                  | BS.null obuf -> do
                      unless (used > 0) $
                          fail "decompressIO: input chunk not consumed"
                      withChunk (return inputRequired) goInput chunk'
                  | otherwise    -> return (DecompressOutputAvailable obuf
                                            (withChunk goDrain goInput chunk'))

                LzmaRetStreamEnd
                  | BS.null obuf -> retStreamEnd chunk'
                  | otherwise    -> return (DecompressOutputAvailable obuf
                                            (retStreamEnd chunk'))

                _ -> return (DecompressStreamError rc)

        goDrain, goFinish :: IO (DecompressStream IO)
        goDrain  = goSync LzmaRun (return inputRequired)
        goFinish = goSync LzmaFinish (return $ DecompressStreamError LzmaRetOK)

        goSync :: LzmaAction -> IO (DecompressStream IO) -> IO (DecompressStream IO)
        goSync action next = goSync'
          where
            goSync' = do
                (rc, 0, obuf) <- stToIO $ runLzmaStream ls BS.empty action bUFSIZ
                case rc of
                  LzmaRetOK
                    | BS.null obuf -> next
                    | otherwise    -> return (DecompressOutputAvailable obuf goSync')

                  LzmaRetStreamEnd
                    | BS.null obuf -> eof0
                    | otherwise    -> return (DecompressOutputAvailable obuf eof0)

                  _ -> return (DecompressStreamError rc)

            eof0 = retStreamEnd BS.empty

        retStreamEnd chunk' = do
            () <- stToIO (endLzmaStream ls)
            return (DecompressStreamEnd chunk')

withChunk :: t -> (ByteString -> t) -> ByteString -> t
withChunk emptyChunk nemptyChunk chunk
  | BS.null chunk = emptyChunk
  | otherwise     = nemptyChunk chunk
