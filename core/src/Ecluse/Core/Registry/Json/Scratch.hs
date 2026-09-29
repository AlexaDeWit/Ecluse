-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A growable byte buffer that one read writes front to back, rewinds in place, and copies each
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
    putBytes,
    putEncodedText,
    putPlainBytes,
    putRawBytes,
    putDecimal,
    putAt,
    copyOut,
    decimalLength,
) where

import Control.Monad.ST (ST)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, copyByteArray, copyMutableByteArray, getSizeofMutableByteArray, newByteArray, unsafeFreezeByteArray, writeByteArray)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI

import Ecluse.Core.Registry.Json.Packed (encodedLength, plain, varintSize)

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

putByte :: Scratch st -> Word8 -> ST st ()
putByte scratch byte = putAt scratch 1 (\buffer at -> writeByteArray buffer at byte >> pure (at + 1))
{-# INLINE putByte #-}

-- | Write a non-negative integer seven bits at a time, low bits first.
putVarint :: Scratch st -> Int -> ST st ()
putVarint scratch n = putAt scratch (varintSize n) (writeVarint n)
{-# INLINE putVarint #-}

writeVarint :: Int -> MutableByteArray st -> Int -> ST st Int
writeVarint !n buffer !at
    | n < 0x80 = writeByteArray buffer at (fromIntegral n :: Word8) >> pure (at + 1)
    | otherwise = writeByteArray buffer at (fromIntegral (n .&. 0x7f) .|. 0x80 :: Word8) >> writeVarint (n `shiftR` 7) buffer (at + 1)

-- | Copy bytes of an immutable array.
putBytes :: Scratch st -> ByteArray -> Int -> Int -> ST st ()
putBytes scratch source from count = putAt scratch count (\buffer at -> copyByteArray buffer at source from count >> pure (at + count))
{-# INLINE putBytes #-}

-- | Write the bytes aeson writes for a string, quotes included.
putEncodedText :: Scratch st -> Text -> ST st ()
putEncodedText scratch text@(TI.Text array offset len)
    | plain text = putAt scratch (len + 2) $ \buffer at -> do
        writeByteArray buffer at quote
        copyByteArray buffer (at + 1) array offset len
        writeByteArray buffer (at + len + 1) quote
        pure (at + len + 2)
    | otherwise = putAt scratch (encodedLength text) $ \buffer at -> do
        writeByteArray buffer at quote
        end <- escapeFrom array offset (offset + len) buffer (at + 1)
        writeByteArray buffer end quote
        pure (end + 1)

-- aeson's escapes: a backslash, a quote, a newline, a return and a tab by name, any other byte below a
-- space as a lower-case hexadecimal code, and every other byte as it is.
escapeFrom :: TA.Array -> Int -> Int -> MutableByteArray st -> Int -> ST st Int
escapeFrom array !from !to buffer !at
    | from >= to = pure at
    | otherwise = case TA.unsafeIndex array from of
        0x5c -> pair 0x5c
        0x22 -> pair 0x22
        byte
            | byte >= 0x20 -> writeByteArray buffer at byte >> escapeFrom array (from + 1) to buffer (at + 1)
            | byte == 0x0a -> pair 0x6e
            | byte == 0x0d -> pair 0x72
            | byte == 0x09 -> pair 0x74
            | otherwise -> do
                writeByteArray buffer at (0x5c :: Word8)
                writeByteArray buffer (at + 1) (0x75 :: Word8)
                writeByteArray buffer (at + 2) (0x30 :: Word8)
                writeByteArray buffer (at + 3) (0x30 :: Word8)
                writeByteArray buffer (at + 4) (hexDigit (byte `shiftR` 4))
                writeByteArray buffer (at + 5) (hexDigit (byte .&. 0x0f))
                escapeFrom array (from + 1) to buffer (at + 6)
  where
    pair escaped = do
        writeByteArray buffer at (0x5c :: Word8)
        writeByteArray buffer (at + 1) (escaped :: Word8)
        escapeFrom array (from + 1) to buffer (at + 2)

hexDigit :: Word8 -> Word8
hexDigit digit
    | digit < 10 = 0x30 + digit
    | otherwise = 0x57 + digit

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

-- Write the digits of a magnitude backwards from the offset.
writeDigits :: MutableByteArray st -> Int -> Word -> ST st ()
writeDigits buffer !at !value = do
    writeByteArray buffer at (0x30 + fromIntegral (value `rem` 10) :: Word8)
    if value >= 10 then writeDigits buffer (at - 1) (value `quot` 10) else pass

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

quote :: Word8
quote = 0x22
