-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | JSON string bytes and exact lengths shared by packed values and their writer.
module Ecluse.Core.Registry.Json.StringEncoding (
    encodeString,
    encodedLength,
    writeEncoded,
    writeEncodedSized,
    plain,
    quote,
) where

import Control.Monad.ST (ST)
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Short qualified as SBS
import Data.Primitive.ByteArray (MutableByteArray, copyByteArray, copyByteArrayToAddr, createByteArray, writeByteArray)
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI
import Foreign.Ptr (plusPtr)
import Foreign.Storable (pokeByteOff)

-- | The bytes aeson writes for a string, quotes included: 'encodedLength' of them.
encodeString :: Text -> ByteString
encodeString text@(TI.Text array offset len)
    | plain text = BSI.unsafeCreate (len + 2) $ \target -> do
        pokeByteOff target 0 quote
        copyByteArrayToAddr (target `plusPtr` 1) array offset len
        pokeByteOff target (len + 1) quote
    | otherwise = SBS.fromShort (SBS.ShortByteString (createByteArray (encodedLength text) (\target -> void (writeEncoded text target 0))))

-- | The length of 'encodeString', from the escape 'writeEncoded' writes for each byte.
encodedLength :: Text -> Int
encodedLength (TI.Text array offset len) = go offset 2
  where
    end = offset + len
    go !i !total
        | i >= end = total
        | otherwise = go (i + 1) (total + escapeWidth (escapeOf (TA.unsafeIndex array i)))

-- | Whether aeson writes the string's bytes as they are: no backslash, quote or byte below a space.
plain :: Text -> Bool
plain (TI.Text array offset len) = go offset
  where
    end = offset + len
    go !i
        | i >= end = True
        | otherwise = case escapeOf (TA.unsafeIndex array i) of
            Verbatim -> go (i + 1)
            _ -> False

-- | Write 'encodeString' at an offset, and return the offset after it.
writeEncoded :: Text -> MutableByteArray st -> Int -> ST st Int
writeEncoded text = writeEncodedWith (plain text) text

-- | Like 'writeEncoded': the length must equal 'encodedLength' for this same text without 'Int' overflow, and fit from a non-negative target offset.
writeEncodedSized :: Text -> Int -> MutableByteArray st -> Int -> ST st Int
writeEncodedSized text@(TI.Text _ _ len) encoded = writeEncodedWith (encoded == len + 2) text

writeEncodedWith :: Bool -> Text -> MutableByteArray st -> Int -> ST st Int
writeEncodedWith isPlain (TI.Text array offset len) target at
    | isPlain = do
        writeByteArray target at quote
        copyByteArray target (at + 1) array offset len
        writeByteArray target (at + len + 1) quote
        pure (at + len + 2)
    | otherwise = do
        writeByteArray target at quote
        end <- escapeFrom array offset (offset + len) target (at + 1)
        writeByteArray target end quote
        pure (end + 1)
{-# INLINE writeEncodedWith #-}

data Escape = Verbatim | Named !Word8 | Coded

escapeOf :: Word8 -> Escape
escapeOf byte
    | byte == 0x5c = Named 0x5c
    | byte == 0x22 = Named 0x22
    | byte >= 0x20 = Verbatim
    | byte == 0x0a = Named 0x6e
    | byte == 0x0d = Named 0x72
    | byte == 0x09 = Named 0x74
    | otherwise = Coded
{-# INLINE escapeOf #-}

escapeWidth :: Escape -> Int
escapeWidth = \case
    Verbatim -> 1
    Named _ -> 2
    Coded -> 6
{-# INLINE escapeWidth #-}

escapeFrom :: TA.Array -> Int -> Int -> MutableByteArray st -> Int -> ST st Int
escapeFrom array !from !to target !at
    | from >= to = pure at
    | otherwise = case escapeOf byte of
        Verbatim -> writeByteArray target at byte >> next 1
        Named letter -> do
            writeByteArray target at (0x5c :: Word8)
            writeByteArray target (at + 1) letter
            next 2
        Coded -> do
            writeByteArray target at (0x5c :: Word8)
            writeByteArray target (at + 1) (0x75 :: Word8)
            writeByteArray target (at + 2) (0x30 :: Word8)
            writeByteArray target (at + 3) (0x30 :: Word8)
            writeByteArray target (at + 4) (hexDigit (byte `shiftR` 4))
            writeByteArray target (at + 5) (hexDigit (byte .&. 0x0f))
            next 6
  where
    byte = TA.unsafeIndex array from
    next width = escapeFrom array (from + 1) to target (at + width)

hexDigit :: Word8 -> Word8
hexDigit digit
    | digit < 10 = 0x30 + digit
    | otherwise = 0x57 + digit

-- | The quote byte that opens and closes a string's encoding.
quote :: Word8
quote = 0x22
