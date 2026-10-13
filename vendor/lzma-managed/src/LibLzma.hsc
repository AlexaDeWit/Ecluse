{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

-- Copyright (c) 2014, Herbert Valerio Riedel <hvr@gnu.org>
--
-- This code is BSD3 licensed, see ../LICENSE file for details
--

-- | Internal low-level bindings to liblzma
--
-- See @<lzma.h>@ header file for documentation about primitives not
-- documented here
module LibLzma
    ( LzmaStream
    , LzmaRet(..)

    , newDecodeLzmaStream
    , DecompressParams(..)
    , defaultDecompressParams

    , runLzmaStream
    , endLzmaStream
    , LzmaAction(..)
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.ST.Strict (ST)
import           Control.Monad.ST.Unsafe (unsafeIOToST)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Typeable
import           Foreign
import           Prelude

#include <lzma.h>

newtype LzmaStream = LS (ForeignPtr LzmaStream)

data LzmaRet = LzmaRetOK
             | LzmaRetStreamEnd
             | LzmaRetUnsupportedCheck
             | LzmaRetGetCheck
             | LzmaRetMemError
             | LzmaRetMemlimitError
             | LzmaRetFormatError
             | LzmaRetOptionsError
             | LzmaRetDataError
             | LzmaRetBufError
             | LzmaRetProgError
             deriving (Eq,Ord,Read,Show,Typeable)

instance Exception LzmaRet

toLzmaRet :: Int -> Maybe LzmaRet
toLzmaRet i = case i of
    (#const LZMA_OK               ) -> Just LzmaRetOK
    (#const LZMA_STREAM_END       ) -> Just LzmaRetStreamEnd
    (#const LZMA_UNSUPPORTED_CHECK) -> Just LzmaRetUnsupportedCheck
    (#const LZMA_GET_CHECK        ) -> Just LzmaRetGetCheck
    (#const LZMA_MEM_ERROR        ) -> Just LzmaRetMemError
    (#const LZMA_MEMLIMIT_ERROR   ) -> Just LzmaRetMemlimitError
    (#const LZMA_FORMAT_ERROR     ) -> Just LzmaRetFormatError
    (#const LZMA_OPTIONS_ERROR    ) -> Just LzmaRetOptionsError
    (#const LZMA_DATA_ERROR       ) -> Just LzmaRetDataError
    (#const LZMA_BUF_ERROR        ) -> Just LzmaRetBufError
    (#const LZMA_PROG_ERROR       ) -> Just LzmaRetProgError
    _                               -> Nothing

-- | Set of parameters for decompression. The defaults are
-- 'defaultDecompressParams'.
data DecompressParams = DecompressParams
    { decompressTellNoCheck          :: !Bool -- ^ 'DecompressParams' field: If set, abort if decoded stream has no integrity check.
    , decompressTellUnsupportedCheck :: !Bool -- ^ 'DecompressParams' field: If set, abort (via 'LzmaRetGetCheck') if decoded stream integrity check is unsupported.
    , decompressTellAnyCheck         :: !Bool -- ^ 'DecompressParams' field: If set, abort (via 'LzmaRetGetCheck') as soon as the type of the integrity check has been detected.
    , decompressConcatenated         :: !Bool -- ^ 'DecompressParams' field: If set, concatenated files as decoded consecutively.
    , decompressAutoDecoder          :: !Bool -- ^ 'DecompressParams' field: If set, legacy @.lzma@-encoded streams are allowed too.
    , decompressMemLimit             :: !Word64 -- ^ 'DecompressParams' field: decompressor memory limit. Set to 'maxBound' to disable memory limit.
    } deriving (Eq,Show)

-- | Default decoder flags and an unlimited native memory allowance.
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams {..}
  where
    decompressTellNoCheck          = False
    decompressTellUnsupportedCheck = False
    decompressTellAnyCheck         = False
    decompressConcatenated         = True
    decompressAutoDecoder          = False
    decompressMemLimit             = maxBound -- disables limit-check

newDecodeLzmaStream :: DecompressParams -> ST s (Either LzmaRet LzmaStream)
newDecodeLzmaStream (DecompressParams {..}) = unsafeIOToST $ mask_ $ do
    fp <- mallocForeignPtrBytes (#size lzma_stream)
    addForeignPtrFinalizer c_hs_lzma_done_funptr fp
    (do rc <- withForeignPtr fp (\ptr -> c_hs_lzma_init_decoder ptr decompressAutoDecoder decompressMemLimit flags')
        rc' <- maybe (fail "newDecodeLzmaStream: invalid return code") pure $ toLzmaRet rc
        case rc' of
            LzmaRetOK -> return (Right (LS fp))
            _ -> finalizeForeignPtr fp >> return (Left rc'))
        `onException` finalizeForeignPtr fp

  where
    flags' =
        (if decompressTellNoCheck          then (#const LZMA_TELL_NO_CHECK)          else 0) .|.
        (if decompressTellUnsupportedCheck then (#const LZMA_TELL_UNSUPPORTED_CHECK) else 0) .|.
        (if decompressTellAnyCheck         then (#const LZMA_TELL_ANY_CHECK)         else 0) .|.
        (if decompressConcatenated         then (#const LZMA_CONCATENATED)           else 0)

data LzmaAction = LzmaRun
                | LzmaFinish
                deriving (Eq,Show)

runLzmaStream :: LzmaStream -> ByteString -> LzmaAction -> Int -> ST s (LzmaRet,Int,ByteString)
runLzmaStream (LS ls) ibs action0 buflen
  | buflen <= 0 = return (LzmaRetOptionsError,0,BS.empty)
  | otherwise = unsafeIOToST $ withForeignPtr ls $ \lsptr ->
      BS.unsafeUseAsCStringLen ibs $ \(ibsptr, ibslen) -> do
          (obuf,rc) <- BS.createAndTrim' buflen $ \bufptr -> do
              rc' <- c_hs_lzma_run lsptr action (castPtr ibsptr) ibslen bufptr buflen
              rc'' <- maybe (fail "runLzmaStream: invalid return code") pure $ toLzmaRet rc'

              availOut <- (#peek lzma_stream, avail_out) lsptr
              unless (buflen >= availOut && availOut >= 0) $
                  fail "runLzmaStream: invalid avail_out"
              let produced = buflen - availOut

              return (0, produced, rc'')

          availIn <- (#peek lzma_stream, avail_in) lsptr
          unless (ibslen >= availIn && availIn >= 0) $
              fail "runLzmaStream: invalid avail_in"
          let consumed = ibslen - availIn

          return (rc, fromIntegral consumed, obuf)
  where
    action = case action0 of
        LzmaRun       -> #const LZMA_RUN
        LzmaFinish    -> #const LZMA_FINISH


-- | Release native decoder state immediately. Do not run the stream afterwards.
endLzmaStream :: LzmaStream -> ST s ()
endLzmaStream (LS ls) = unsafeIOToST $ finalizeForeignPtr ls

foreign import ccall "hs_lzma_init_decoder"
    c_hs_lzma_init_decoder :: Ptr LzmaStream -> Bool -> Word64 -> Word32 -> IO Int

foreign import ccall "hs_lzma_run"
    c_hs_lzma_run :: Ptr LzmaStream -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO Int

foreign import ccall "&hs_lzma_done"
    c_hs_lzma_done_funptr :: FunPtr (Ptr LzmaStream -> IO ())
