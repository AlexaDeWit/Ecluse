-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A growable byte buffer that one writer fills front to back, rewinds in place, and copies each
finished value out of. It writes the bytes aeson writes for strings and integers without building
them first.
-}
module Ecluse.Core.Registry.Json.Scratch (
    Scratch,
    newScratch,
    scratchCursor,
    rewindTo,
    scratchBuffer,
    reserve,
    putByte,
    putVarint,
    putEncodedText,
    putPlainBytes,
    putRawBytes,
    putRawText,
    putDecimal,
    putAt,
    copyOut,
    decimalLength,
    scratchBytes,
) where

import Control.Monad.ST (ST, runST)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, copyByteArray, copyByteArrayToAddr, copyMutableByteArray, getSizeofMutableByteArray, newByteArray, unsafeFreezeByteArray, writeByteArray)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Text.Internal qualified as TI

import Ecluse.Core.Registry.Json.Packed (varintSize, writeVarint)
import Ecluse.Core.Registry.Json.StringEncoding (encodedLength, quote, writeEncodedSized)
import Ecluse.Core.Text (writeDigits)

-- | The buffer and the offset of its next byte.
data Scratch st = Scratch !(MutVar st (MutableByteArray st)) !(PrimVar st Int)

-- | An empty buffer with room for the given bytes.
newScratch :: Int -> ST st (Scratch st)
newScratch size = Scratch <$> (newByteArray (max 64 size) >>= newMutVar) <*> newPrimVar 0

-- | Where the next byte goes.
scratchCursor :: Scratch st -> ST st Int
scratchCursor (Scratch _ cursor) = readPrimVar cursor
{-# INLINE scratchCursor #-}

-- | Forget every byte from the offset on.
rewindTo :: Scratch st -> Int -> ST st ()
rewindTo (Scratch _ cursor) = writePrimVar cursor
{-# INLINE rewindTo #-}

-- | The buffer as it stands. Any later write that needs room may replace it.
scratchBuffer :: Scratch st -> ST st (MutableByteArray st)
scratchBuffer (Scratch buffer _) = readMutVar buffer
{-# INLINE scratchBuffer #-}

-- | The buffer with room for the given bytes past the cursor, and the cursor.
reserve :: Scratch st -> Int -> (MutableByteArray st -> Int -> ST st a) -> ST st a
reserve (Scratch ref cursor) count use = do
    buffer <- readMutVar ref
    at <- readPrimVar cursor
    size <- getSizeofMutableByteArray buffer
    if at + count <= size
        then use buffer at
        else do
            grown <- newByteArray (max (at + count) (2 * size))
            copyMutableByteArray grown 0 buffer 0 at
            writeMutVar ref grown
            use grown at
{-# INLINE reserve #-}

-- | Write bytes at the cursor with the given writer, which returns the offset after them.
putAt :: Scratch st -> Int -> (MutableByteArray st -> Int -> ST st Int) -> ST st ()
putAt scratch@(Scratch _ cursor) count write = reserve scratch count (\buffer at -> write buffer at >>= writePrimVar cursor)
{-# INLINE putAt #-}

-- | Write one byte.
putByte :: Scratch st -> Word8 -> ST st ()
putByte scratch byte = putAt scratch 1 (\buffer at -> writeByteArray buffer at byte >> pure (at + 1))
{-# INLINE putByte #-}

-- | Write a non-negative integer seven bits at a time, low bits first.
putVarint :: Scratch st -> Int -> ST st ()
putVarint scratch n = putAt scratch (varintSize n) (writeVarint n)
{-# INLINE putVarint #-}

-- | Write the header's varint and aeson's string bytes, quotes included.
putEncodedText :: Scratch st -> (Int -> Int) -> Text -> ST st ()
putEncodedText scratch header text = putVarint scratch (header len) >> putAt scratch len (writeEncodedSized text len)
  where
    len = encodedLength text

-- | Write a string whose bytes need no escape, between quotes.
putPlainBytes :: Scratch st -> ByteString -> ST st ()
putPlainBytes scratch bytes = putAt scratch (BS.length bytes + 2) $ \buffer at -> do
    writeByteArray buffer at quote
    end <- copyBytes bytes buffer (at + 1)
    writeByteArray buffer end quote
    pure (end + 1)

-- | Write bytes as they are.
putRawBytes :: Scratch st -> ByteString -> ST st ()
putRawBytes scratch bytes = putAt scratch (BS.length bytes) (copyBytes bytes)

-- | Write a text's UTF-8 bytes as they are.
putRawText :: Scratch st -> Text -> ST st ()
putRawText scratch (TI.Text array offset len) = putAt scratch len (\buffer at -> copyByteArray buffer at array offset len >> pure (at + len))

copyBytes :: ByteString -> MutableByteArray st -> Int -> ST st Int
copyBytes bytes buffer = go 0
  where
    len = BS.length bytes
    go !index !at
        | index >= len = pure at
        | otherwise = writeByteArray buffer at (BSU.unsafeIndex bytes index) >> go (index + 1) (at + 1)

-- | Write an integer in decimal, as aeson writes one.
putDecimal :: Scratch st -> Int -> ST st ()
putDecimal scratch n = putAt scratch len $ \buffer at -> do
    when (n < 0) (writeByteArray buffer at (0x2d :: Word8))
    writeDigits buffer (at + len - 1) (magnitude n)
    pure (at + len)
  where
    len = decimalLength n

magnitude :: Int -> Word
magnitude n = if n < 0 then fromIntegral (negate n) else fromIntegral n

-- | The bytes 'putDecimal' writes.
decimalLength :: Int -> Int
decimalLength n = (if n < 0 then 1 else 0) + count (magnitude n) 1
  where
    count :: Word -> Int -> Int
    count !remaining !total
        | remaining < 10 = total
        | otherwise = count (remaining `quot` 10) (total + 1)

-- | A copy of the bytes between two offsets, in an array of their exact size.
copyOut :: Scratch st -> Int -> Int -> ST st ByteArray
copyOut scratch from to = do
    buffer <- scratchBuffer scratch
    target <- newByteArray (to - from)
    copyMutableByteArray target 0 buffer from (to - from)
    unsafeFreezeByteArray target

-- | The bytes a writer leaves in a new buffer of the given starting size, copied once into a 'ByteString'.
scratchBytes :: Int -> (forall st. Scratch st -> ST st ()) -> ByteString
scratchBytes size write = BSI.unsafeCreate len (\target -> copyByteArrayToAddr target written 0 len)
  where
    (written, len) = runST $ do
        scratch <- newScratch size
        write scratch
        (,) <$> (scratchBuffer scratch >>= unsafeFreezeByteArray) <*> scratchCursor scratch
