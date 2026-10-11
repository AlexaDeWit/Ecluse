-- SPDX-FileCopyrightText: 2015 Ondrej Palkovsky
-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | JSON string decoding for the private lexer and its compatibility parser.
module Data.JsonStream.Unescape (unescapeText, unsafeDecodeASCII) where

import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short.Internal as SBS
import Data.Char (ord)
import qualified Data.Primitive as P
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import Data.Text.Encoding.Error (UnicodeException (..))
import Data.Text.Internal (Text (..))
import Data.Word (Word32, Word8)

-- | Copy bytes already classified as ASCII by the lexer. Callers must establish that precondition.
unsafeDecodeASCII :: ByteString -> Text
unsafeDecodeASCII bs
    | BS.null bs = T.empty
    | otherwise = case SBS.toShort bs of
        SBS.SBS arr -> Text (TA.ByteArray arr) 0 (BS.length bs)

-- | Decode UTF-8 and JSON escapes, retaining the inherited acceptance of raw control bytes.
unescapeText :: ByteString -> Either UnicodeException Text
unescapeText bs = runST (decodeText bs)

decodeError :: UnicodeException
decodeError = DecodeError "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" Nothing

-- The state machine derives from aeson-2.0's staged-streams Unicode.JSON decoder.
-- Output never exceeds consumed input. Only the finished buffer escapes this ST region.
decodeText :: forall s. ByteString -> ST s (Either UnicodeException Text)
decodeText bs = do
    let end = BS.length bs
        readByte offset next = case BS.indexMaybe bs offset of
            Nothing -> pure (Left decodeError)
            Just byte -> next byte
    arr <- P.newPrimArray end

    let write3bytes :: Int -> Word8 -> Word8 -> Word8 -> Int -> ST s (Either UnicodeException Text)
        write3bytes !out !b1 !b2 !b3 !inp = do
            P.writePrimArray arr out b1
            write2bytes (out + 1) b2 b3 inp

        write2bytes :: Int -> Word8 -> Word8 -> Int -> ST s (Either UnicodeException Text)
        write2bytes !out !b1 !b2 !inp = do
            P.writePrimArray arr out b1
            write1byte (out + 1) b2 inp

        write1byte :: Int -> Word8 -> Int -> ST s (Either UnicodeException Text)
        write1byte !out !b1 !inp = do
            P.writePrimArray arr out b1
            state_start (out + 1) inp

        writeCodePoint :: Int -> Int -> Word32 -> ST s (Either UnicodeException Text)
        writeCodePoint !out !inp !acc
            | acc <= asciiMaximum = do
                P.writePrimArray arr out (fromIntegral acc :: Word8)
                state_start (out + 1) (inp + 1)
            | acc <= twoByteMaximum = do
                let b1 = fromIntegral (shiftR acc continuationBits .|. twoBytePrefix) :: Word8
                let b2 = fromIntegral ((acc .&. continuationPayloadMask) .|. continuationPrefix) :: Word8
                P.writePrimArray arr out b1
                write1byte (out + 1) b2 (inp + 1)
            | acc <= threeByteMaximum = do
                let b1 = fromIntegral (shiftR acc twoContinuationBits .|. threeBytePrefix) :: Word8
                let b2 = fromIntegral ((shiftR acc continuationBits .&. continuationPayloadMask) .|. continuationPrefix) :: Word8
                let b3 = fromIntegral ((acc .&. continuationPayloadMask) .|. continuationPrefix) :: Word8
                P.writePrimArray arr out b1
                write2bytes (out + 1) b2 b3 (inp + 1)
            | otherwise = do
                let b1 = fromIntegral (shiftR acc threeContinuationBits .|. fourBytePrefix) :: Word8
                let b2 = fromIntegral ((shiftR acc twoContinuationBits .&. continuationPayloadMask) .|. continuationPrefix) :: Word8
                let b3 = fromIntegral ((shiftR acc continuationBits .&. continuationPayloadMask) .|. continuationPrefix) :: Word8
                let b4 = fromIntegral ((acc .&. continuationPayloadMask) .|. continuationPrefix) :: Word8
                P.writePrimArray arr out b1
                write3bytes (out + 1) b2 b3 b4 (inp + 1)

        state_sudone :: Int -> Int -> Word32 -> Word32 -> ST s (Either UnicodeException Text)
        state_sudone !out !inp !hi !lo
            | lowSurrogateStart <= lo
            , lo <= surrogateEnd =
                writeCodePoint out inp (supplementaryStart + (shiftL (hi - highSurrogateStart) surrogateBits .|. (lo - lowSurrogateStart)))
            | otherwise =
                pure (Left decodeError)

        state_su4 :: Int -> Int -> Word32 -> Word32 -> ST s (Either UnicodeException Text)
        state_su4 !out !inp !hi !acc = readByte inp $ \w8 ->
            if
                | hexDigitZero <= w8
                , w8 <= hexDigitNine ->
                    state_sudone out inp hi (shiftL acc hexBits .|. fromIntegral (w8 - hexDigitZero))
                | hexUpperA <= w8
                , w8 <= hexUpperF ->
                    state_sudone out inp hi (shiftL acc hexBits .|. fromIntegral (w8 - hexUpperOffset))
                | hexLowerA <= w8
                , w8 <= hexLowerF ->
                    state_sudone out inp hi (shiftL acc hexBits .|. fromIntegral (w8 - hexLowerOffset))
                | otherwise ->
                    pure (Left decodeError)

        state_su3 :: Int -> Int -> Word32 -> Word32 -> ST s (Either UnicodeException Text)
        state_su3 !out !inp !hi !acc = readByte inp $ \w8 ->
            if
                | hexDigitZero <= w8
                , w8 <= hexDigitNine ->
                    state_su4 out (inp + 1) hi (shiftL acc hexBits .|. fromIntegral (w8 - hexDigitZero))
                | hexUpperA <= w8
                , w8 <= hexUpperF ->
                    state_su4 out (inp + 1) hi (shiftL acc hexBits .|. fromIntegral (w8 - hexUpperOffset))
                | hexLowerA <= w8
                , w8 <= hexLowerF ->
                    state_su4 out (inp + 1) hi (shiftL acc hexBits .|. fromIntegral (w8 - hexLowerOffset))
                | otherwise ->
                    pure (Left decodeError)

        state_su2 :: Int -> Int -> Word32 -> Word32 -> ST s (Either UnicodeException Text)
        state_su2 !out !inp !hi !acc = readByte inp $ \w8 ->
            if
                | hexDigitZero <= w8
                , w8 <= hexDigitNine ->
                    state_su3 out (inp + 1) hi (shiftL acc hexBits .|. fromIntegral (w8 - hexDigitZero))
                | hexUpperA <= w8
                , w8 <= hexUpperF ->
                    state_su3 out (inp + 1) hi (shiftL acc hexBits .|. fromIntegral (w8 - hexUpperOffset))
                | hexLowerA <= w8
                , w8 <= hexLowerF ->
                    state_su3 out (inp + 1) hi (shiftL acc hexBits .|. fromIntegral (w8 - hexLowerOffset))
                | otherwise ->
                    pure (Left decodeError)

        state_su1 :: Int -> Int -> Word32 -> ST s (Either UnicodeException Text)
        state_su1 !out !inp !hi = readByte inp $ \w8 ->
            if
                | hexDigitZero <= w8
                , w8 <= hexDigitNine ->
                    state_su2 out (inp + 1) hi (fromIntegral (w8 - hexDigitZero))
                | hexUpperA <= w8
                , w8 <= hexUpperF ->
                    state_su2 out (inp + 1) hi (fromIntegral (w8 - hexUpperOffset))
                | hexLowerA <= w8
                , w8 <= hexLowerF ->
                    state_su2 out (inp + 1) hi (fromIntegral (w8 - hexLowerOffset))
                | otherwise ->
                    pure (Left decodeError)

        state_su :: Int -> Int -> Word32 -> ST s (Either UnicodeException Text)
        state_su !out !inp !hi = readByte inp $ \w8 ->
            case w8 of
                _ | w8 == escapeUnicodeByte -> state_su1 out (inp + 1) hi
                _ -> pure (Left decodeError)

        state_ss :: Int -> Int -> Word32 -> ST s (Either UnicodeException Text)
        state_ss !out !inp !hi = readByte inp $ \w8 ->
            case w8 of
                _ | w8 == backslashByte -> state_su out (inp + 1) hi
                _ -> pure (Left decodeError)

        state_udone :: Int -> Int -> Word32 -> ST s (Either UnicodeException Text)
        state_udone !out !inp !acc
            | acc < highSurrogateStart || acc > surrogateEnd =
                writeCodePoint out inp acc
            | acc < lowSurrogateStart =
                state_ss out (inp + 1) acc
            | otherwise =
                pure (Left decodeError)

        state_u4 :: Int -> Int -> Word32 -> ST s (Either UnicodeException Text)
        state_u4 !out !inp !acc = readByte inp $ \w8 ->
            if
                | hexDigitZero <= w8
                , w8 <= hexDigitNine ->
                    state_udone out inp (shiftL acc hexBits .|. fromIntegral (w8 - hexDigitZero))
                | hexUpperA <= w8
                , w8 <= hexUpperF ->
                    state_udone out inp (shiftL acc hexBits .|. fromIntegral (w8 - hexUpperOffset))
                | hexLowerA <= w8
                , w8 <= hexLowerF ->
                    state_udone out inp (shiftL acc hexBits .|. fromIntegral (w8 - hexLowerOffset))
                | otherwise ->
                    pure (Left decodeError)

        state_u3 :: Int -> Int -> Word32 -> ST s (Either UnicodeException Text)
        state_u3 !out !inp !acc = readByte inp $ \w8 ->
            if
                | hexDigitZero <= w8
                , w8 <= hexDigitNine ->
                    state_u4 out (inp + 1) (shiftL acc hexBits .|. fromIntegral (w8 - hexDigitZero))
                | hexUpperA <= w8
                , w8 <= hexUpperF ->
                    state_u4 out (inp + 1) (shiftL acc hexBits .|. fromIntegral (w8 - hexUpperOffset))
                | hexLowerA <= w8
                , w8 <= hexLowerF ->
                    state_u4 out (inp + 1) (shiftL acc hexBits .|. fromIntegral (w8 - hexLowerOffset))
                | otherwise ->
                    pure (Left decodeError)

        state_u2 :: Int -> Int -> Word32 -> ST s (Either UnicodeException Text)
        state_u2 !out !inp !acc = readByte inp $ \w8 ->
            if
                | hexDigitZero <= w8
                , w8 <= hexDigitNine ->
                    state_u3 out (inp + 1) (shiftL acc hexBits .|. fromIntegral (w8 - hexDigitZero))
                | hexUpperA <= w8
                , w8 <= hexUpperF ->
                    state_u3 out (inp + 1) (shiftL acc hexBits .|. fromIntegral (w8 - hexUpperOffset))
                | hexLowerA <= w8
                , w8 <= hexLowerF ->
                    state_u3 out (inp + 1) (shiftL acc hexBits .|. fromIntegral (w8 - hexLowerOffset))
                | otherwise ->
                    pure (Left decodeError)

        state_u1 :: Int -> Int -> ST s (Either UnicodeException Text)
        state_u1 !out !inp = readByte inp $ \w8 ->
            if
                | hexDigitZero <= w8
                , w8 <= hexDigitNine ->
                    state_u2 out (inp + 1) (fromIntegral (w8 - hexDigitZero))
                | hexUpperA <= w8
                , w8 <= hexUpperF ->
                    state_u2 out (inp + 1) (fromIntegral (w8 - hexUpperOffset))
                | hexLowerA <= w8
                , w8 <= hexLowerF ->
                    state_u2 out (inp + 1) (fromIntegral (w8 - hexLowerOffset))
                | otherwise ->
                    pure (Left decodeError)

        state_escape :: Int -> Int -> ST s (Either UnicodeException Text)
        state_escape !out !inp = readByte inp $ \w8 ->
            case w8 of
                _ | w8 == quoteByte -> do
                    P.writePrimArray arr out quoteByte
                    state_start (out + 1) (inp + 1)
                _ | w8 == backslashByte -> do
                    P.writePrimArray arr out backslashByte
                    state_start (out + 1) (inp + 1)
                _ | w8 == slashByte -> do
                    P.writePrimArray arr out slashByte
                    state_start (out + 1) (inp + 1)
                _ | w8 == escapeBackspaceByte -> do
                    P.writePrimArray arr out backspaceByte
                    state_start (out + 1) (inp + 1)
                _ | w8 == escapeFormFeedByte -> do
                    P.writePrimArray arr out formFeedByte
                    state_start (out + 1) (inp + 1)
                _ | w8 == escapeNewlineByte -> do
                    P.writePrimArray arr out newlineByte
                    state_start (out + 1) (inp + 1)
                _ | w8 == escapeReturnByte -> do
                    P.writePrimArray arr out returnByte
                    state_start (out + 1) (inp + 1)
                _ | w8 == escapeTabByte -> do
                    P.writePrimArray arr out tabByte
                    state_start (out + 1) (inp + 1)
                _
                    | w8 == escapeUnicodeByte ->
                        state_u1 out (inp + 1)
                _ -> pure (Left decodeError)

        state_input4c :: Int -> Int -> Word8 -> Word8 -> Word8 -> ST s (Either UnicodeException Text)
        state_input4c !out !inp !b1 !b2 !b3 = readByte inp $ \w8 ->
            if
                | (w8 .&. twoBytePrefix) == continuationPrefix
                , let acc = shiftL (fromIntegral (b1 .&. fourBytePayloadMask)) threeContinuationBits
                , let acc' = acc .|. shiftL (fromIntegral (b2 .&. continuationPayloadMask)) twoContinuationBits
                , let acc'' = acc' .|. shiftL (fromIntegral (b3 .&. continuationPayloadMask)) continuationBits
                , let acc''' = acc'' .|. fromIntegral (w8 .&. continuationPayloadMask) :: Word32
                , acc''' >= supplementaryStart && acc''' < unicodeLimit -> do
                    P.writePrimArray arr out b1
                    write3bytes (out + 1) b2 b3 w8 (inp + 1)
                | otherwise ->
                    pure (Left decodeError)

        state_input4b :: Int -> Int -> Word8 -> Word8 -> ST s (Either UnicodeException Text)
        state_input4b !out !inp !b1 !b2 = readByte inp $ \w8 ->
            if (w8 .&. twoBytePrefix) == continuationPrefix
                then state_input4c out (inp + 1) b1 b2 w8
                else pure (Left decodeError)

        state_input4 :: Int -> Int -> Word8 -> ST s (Either UnicodeException Text)
        state_input4 !out !inp !b1 = readByte inp $ \w8 ->
            if (w8 .&. twoBytePrefix) == continuationPrefix
                then state_input4b out (inp + 1) b1 w8
                else pure (Left decodeError)

        state_input3b :: Int -> Int -> Word8 -> Word8 -> ST s (Either UnicodeException Text)
        state_input3b !out !inp !b1 !b2 = readByte inp $ \w8 ->
            if
                | (w8 .&. twoBytePrefix) == continuationPrefix
                , let acc = shiftL (fromIntegral (b1 .&. threeBytePayloadMask)) twoContinuationBits
                , let acc' = acc .|. shiftL (fromIntegral (b2 .&. continuationPayloadMask)) continuationBits
                , let acc'' = acc' .|. fromIntegral (w8 .&. continuationPayloadMask) :: Word32
                , (acc'' >= threeByteMinimum && acc'' < highSurrogateStart) || acc'' > surrogateEnd -> do
                    P.writePrimArray arr out b1
                    write2bytes (out + 1) b2 w8 (inp + 1)
                | otherwise ->
                    pure (Left decodeError)

        state_input3 :: Int -> Int -> Word8 -> ST s (Either UnicodeException Text)
        state_input3 !out !inp !b1 = readByte inp $ \w8 ->
            if (w8 .&. twoBytePrefix) == continuationPrefix
                then state_input3b out (inp + 1) b1 w8
                else pure (Left decodeError)

        state_input2 :: Int -> Int -> Word8 -> ST s (Either UnicodeException Text)
        state_input2 !out !inp !b1 = readByte inp $ \w8 ->
            if
                | (w8 .&. twoBytePrefix) == continuationPrefix
                , let acc = shiftL (fromIntegral (b1 .&. continuationPayloadMask)) continuationBits :: Word32
                      acc' = acc .|. fromIntegral (w8 .&. continuationPayloadMask) :: Word32
                , acc' >= continuationPrefix -> do
                    P.writePrimArray arr out b1
                    write1byte (out + 1) w8 (inp + 1)
                | otherwise ->
                    pure (Left decodeError)

        state_start :: Int -> Int -> ST s (Either UnicodeException Text)
        state_start !out !inp
            | inp == end = do
                P.shrinkMutablePrimArray arr out
                frozenArr <- P.unsafeFreezePrimArray arr
                pure $ Right $ case frozenArr of
                    P.PrimArray ba -> Text (TA.ByteArray ba) 0 out
            | otherwise = readByte inp $ \w8 ->
                if
                    | w8 == backslashByte -> state_escape out (inp + 1)
                    | w8 < continuationPrefix -> do
                        P.writePrimArray arr out w8
                        state_start (out + 1) (inp + 1)
                    | w8 < twoBytePrefix -> pure (Left decodeError)
                    | w8 < threeBytePrefix -> state_input2 out (inp + 1) w8
                    | w8 < fourBytePrefix -> state_input3 out (inp + 1) w8
                    | w8 < invalidPrefix -> state_input4 out (inp + 1) w8
                    | otherwise -> pure (Left decodeError)

    state_start 0 0

ascii :: Char -> Word8
ascii = fromIntegral . ord

fourBytePayloadMask :: (Num a) => a
fourBytePayloadMask = 7

threeBytePayloadMask :: (Num a) => a
threeBytePayloadMask = 15

hexDigitZero :: (Num a) => a
hexDigitZero = 48

hexDigitNine :: (Num a) => a
hexDigitNine = 57

hexUpperA :: (Num a) => a
hexUpperA = 65

hexUpperF :: (Num a) => a
hexUpperF = 70

hexLowerA :: (Num a) => a
hexLowerA = 97

hexLowerF :: (Num a) => a
hexLowerF = 102

hexUpperOffset :: (Num a) => a
hexUpperOffset = 55

hexLowerOffset :: (Num a) => a
hexLowerOffset = 87

continuationPayloadMask :: (Num a) => a
continuationPayloadMask = 63

asciiMaximum :: (Num a) => a
asciiMaximum = 127

continuationPrefix :: (Num a) => a
continuationPrefix = 128

twoBytePrefix :: (Num a) => a
twoBytePrefix = 192

threeBytePrefix :: (Num a) => a
threeBytePrefix = 224

fourBytePrefix :: (Num a) => a
fourBytePrefix = 240

invalidPrefix :: (Num a) => a
invalidPrefix = 248

twoByteMaximum :: Word32
twoByteMaximum = 2047

threeByteMaximum :: Word32
threeByteMaximum = 65535

highSurrogateStart :: Word32
highSurrogateStart = 55296

lowSurrogateStart :: Word32
lowSurrogateStart = 56320

surrogateEnd :: Word32
surrogateEnd = 57343

supplementaryStart :: Word32
supplementaryStart = 65536

unicodeLimit :: Word32
unicodeLimit = 1114112

threeByteMinimum :: Word32
threeByteMinimum = 2048

hexBits :: Int
hexBits = 4

continuationBits :: Int
continuationBits = 6

surrogateBits :: Int
surrogateBits = 10

twoContinuationBits :: Int
twoContinuationBits = 12

threeContinuationBits :: Int
threeContinuationBits = 18

quoteByte :: Word8
quoteByte = ascii '\"'

backslashByte :: Word8
backslashByte = ascii '\\'

slashByte :: Word8
slashByte = ascii '/'

escapeBackspaceByte :: Word8
escapeBackspaceByte = ascii 'b'

escapeFormFeedByte :: Word8
escapeFormFeedByte = ascii 'f'

escapeNewlineByte :: Word8
escapeNewlineByte = ascii 'n'

escapeReturnByte :: Word8
escapeReturnByte = ascii 'r'

escapeTabByte :: Word8
escapeTabByte = ascii 't'

escapeUnicodeByte :: Word8
escapeUnicodeByte = ascii 'u'

backspaceByte :: Word8
backspaceByte = ascii '\b'

formFeedByte :: Word8
formFeedByte = ascii '\f'

newlineByte :: Word8
newlineByte = ascii '\n'

returnByte :: Word8
returnByte = ascii '\r'

tabByte :: Word8
tabByte = ascii '\t'
