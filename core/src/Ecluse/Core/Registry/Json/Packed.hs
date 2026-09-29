-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE UnboxedTuples #-}

{- | The packed form: one table per document holding the bytes aeson writes for each shared string,
and one opcode blob per retained value. A render copies those bytes into a buffer of the output's
exact length, so no string is escaped again. A value holds at most one hole: a URL string that a
render may rebase onto a per-request prefix, keeping the URL's file name.
-}
module Ecluse.Core.Registry.Json.Packed (
    -- * Encoded strings
    encodeString,
    encodedLength,
    writeEncoded,
    plain,
    quote,

    -- * The format
    opNull,
    opFalse,
    opTrue,
    opShared,
    opObject,
    opArray,
    opInline,
    varintSize,
    readVarint,
    writeVarint,
    valueEnd,

    -- * The document table
    DocTable,
    docTable,
    tableResident,

    -- * Packed values
    Packed,
    packed,
    packedBlob,
    packedBytes,
    packedResident,
    withoutHole,

    -- * Rendering
    UrlPrefix,
    urlPrefix,
    Piece (..),
    Pieces (..),
    RenderPlan (..),
    renderPlan,
    planValue,
    planResident,

    -- * Reading back
    TableStrings (..),
    decodeWith,
    decodeKeyWith,
    decodeScalar,
    packedValue,
) where

import Control.Monad.ST (ST, runST)
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Unsafe qualified as BSU
import Data.Map.Internal (Map (Bin, Tip))
import Data.Map.Strict qualified as Map
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, copyByteArray, copyByteArrayToAddr, createByteArray, indexByteArray, sizeofByteArray, writeByteArray)
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, newPrimArray, runPrimArray, sizeofPrimArray, writePrimArray)
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Scientific (Scientific, normalize, scientific)
import Data.Scientific qualified as Scientific
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI
import Data.Vector qualified as V
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)

import Ecluse.Core.Text (textStorageBytes)

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
writeEncoded text@(TI.Text array offset len) target at
    | plain text = do
        writeByteArray target at quote
        copyByteArray target (at + 1) array offset len
        writeByteArray target (at + len + 1) quote
        pure (at + len + 2)
    | otherwise = do
        writeByteArray target at quote
        end <- escapeFrom array offset (offset + len) target (at + 1)
        writeByteArray target end quote
        pure (end + 1)

{- aeson's escape for one byte of a string: the byte as it is, a backslash and a letter for a
backslash, a quote, a newline, a return or a tab, and a lower-case hexadecimal code below a space. -}
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

{- | Each value's first byte. A table index, a count, or an inline length and aeson's bytes follow. A
member key is twice its table index, or twice its inline length plus one followed by its bytes.
-}
opNull, opFalse, opTrue, opShared, opObject, opArray, opInline :: Word8
opNull = 0
opFalse = 1
opTrue = 2
opShared = 3
opObject = 4
opArray = 5
opInline = 6

-- | The bytes a varint of the integer takes.
varintSize :: Int -> Int
varintSize n
    | n < 0x80 = 1
    | otherwise = 1 + varintSize (n `div` 0x80)

-- | The varint at a position, and the position after it.
readVarint :: ByteArray -> Int -> (# Int, Int #)
readVarint blob = go 0 0
  where
    go !acc !shift !i =
        let byte = indexByteArray blob i :: Word8
            acc' = acc .|. (fromIntegral (byte .&. 0x7f) `shiftL` shift)
         in if byte < 0x80 then (# acc', i + 1 #) else go acc' (shift + 7) (i + 1)
{-# INLINE readVarint #-}

-- | Write the integer's varint at an offset, and return the offset after it.
writeVarint :: Int -> MutableByteArray st -> Int -> ST st Int
writeVarint !n buffer !at
    | n < 0x80 = writeByteArray buffer at (fromIntegral n :: Word8) >> pure (at + 1)
    | otherwise = writeByteArray buffer at (fromIntegral (n .&. 0x7f) .|. 0x80 :: Word8) >> writeVarint (n `shiftR` 7) buffer (at + 1)

-- | The position after the value that starts at a position.
valueEnd :: ByteArray -> Int -> Int
valueEnd blob position = case indexByteArray blob position :: Word8 of
    byte
        | byte <= opTrue -> position + 1
        | byte == opShared -> case readVarint blob (position + 1) of (# _, next #) -> next
        | byte == opObject -> case readVarint blob (position + 1) of (# count, next #) -> members count next
        | byte == opArray -> case readVarint blob (position + 1) of (# count, next #) -> items count next
        | otherwise -> case readVarint blob (position + 1) of (# len, next #) -> next + len
  where
    members !count !at
        | count <= 0 = at
        | otherwise = case readVarint blob at of
            (# tagged, next #) -> members (count - 1) (valueEnd blob (if even tagged then next else next + tagged `div` 2))
    items !count !at
        | count <= 0 = at
        | otherwise = items (count - 1) (valueEnd blob at)

-- | One document's shared strings, back to back as aeson writes them, and where each begins.
data DocTable = DocTable !ByteArray !(PrimArray Int)
    deriving stock (Eq, Show)

-- | Lay out the table from its strings in index order.
docTable :: SmallArray Text -> DocTable
docTable strings = DocTable arena offsets
  where
    count = sizeofSmallArray strings
    offsets = runPrimArray $ do
        target <- newPrimArray (count + 1)
        let fill !index !offset = do
                writePrimArray target index offset
                when (index < count) (fill (index + 1) (offset + encodedLength (indexSmallArray strings index)))
        fill 0 0
        pure target
    arena = createByteArray (indexPrimArray offsets count) (`placeFrom` 0)
    placeFrom target !index = when (index < count) $ do
        void (writeEncoded (indexSmallArray strings index) target (indexPrimArray offsets index))
        placeFrom target (index + 1)

-- | The heap bytes the table holds: its record, and its two arrays with their headers.
tableResident :: DocTable -> Int
tableResident (DocTable arena offsets) = 24 + arrayResident (sizeofByteArray arena) + arrayResident (8 * sizeofPrimArray offsets)

-- The heap bytes of a byte array with the given payload: a two-word header and the payload in whole words.
arrayResident :: Int -> Int
arrayResident size = 16 + 8 * ((size + 7) `div` 8)

-- Where a table string's encoding starts in the arena, and its length. An index past the table has
-- length -1, which a render refuses, so a damaged blob never reads outside the arena.
tableEntry :: DocTable -> Int -> (# Int, Int #)
tableEntry (DocTable _ offsets) index
    | index < 0 || index + 1 >= sizeofPrimArray offsets = (# 0, -1 #)
    | otherwise =
        let start = indexPrimArray offsets index
         in (# start, indexPrimArray offsets (index + 1) - start #)
{-# INLINE tableEntry #-}

tableArena :: DocTable -> ByteArray
tableArena (DocTable arena _) = arena

-- | One retained value: its opcodes, and where its hole's string starts, or -1.
data Packed = Packed !ByteArray !Int
    deriving stock (Eq, Show)

-- | A value from its opcodes and the position of its hole, or -1 for none.
packed :: ByteArray -> Int -> Packed
packed = Packed

-- | The value's opcodes.
packedBlob :: Packed -> ByteArray
packedBlob (Packed blob _) = blob

-- | The bytes the value holds itself, outside its table.
packedBytes :: Packed -> Int
packedBytes (Packed blob _) = sizeofByteArray blob

-- | The heap bytes the value holds itself: its record, and its blob with the array's header.
packedResident :: Packed -> Int
packedResident value = 24 + arrayResident (packedBytes value)

-- | The value with no hole, so every render writes it as read.
withoutHole :: Packed -> Packed
withoutHole (Packed blob _) = Packed blob (-1)

-- The encoded length of the value at a position, or -1 when it names a string the table lacks, and
-- the position after it.
encodedAt :: DocTable -> ByteArray -> Int -> (# Int, Int #)
encodedAt table blob position = case indexByteArray blob position :: Word8 of
    byte
        | byte == opNull -> (# 4, position + 1 #)
        | byte == opFalse -> (# 5, position + 1 #)
        | byte == opTrue -> (# 4, position + 1 #)
        | byte == opShared -> case readVarint blob (position + 1) of
            (# index, next #) -> case tableEntry table index of (# _, len #) -> (# len, next #)
        | byte == opObject -> case readVarint blob (position + 1) of
            (# count, next #) -> members count next (separators count)
        | byte == opArray -> case readVarint blob (position + 1) of
            (# count, next #) -> items count next (separators count)
        | otherwise -> case readVarint blob (position + 1) of (# len, next #) -> (# len, next + len #)
  where
    members !count !at !total
        | count <= 0 = (# total, at #)
        | otherwise = case readVarint blob at of
            (# tagged, next #)
                | even tagged -> case tableEntry table (tagged `div` 2) of
                    (# _, keyLen #)
                        | keyLen < 0 -> (# -1, next #)
                        | otherwise -> member count next (keyLen + 1) total
                | otherwise -> member count (next + tagged `div` 2) (tagged `div` 2 + 1) total
    member !count !at !keyed !total = case encodedAt table blob at of
        (# len, after #)
            | len < 0 -> (# -1, after #)
            | otherwise -> members (count - 1) after (total + keyed + len)
    items !count !at !total
        | count <= 0 = (# total, at #)
        | otherwise = case encodedAt table blob at of
            (# len, after #)
                | len < 0 -> (# -1, after #)
                | otherwise -> items (count - 1) after (total + len)

-- Two brackets and a comma between each pair of members or items.
separators :: Int -> Int
separators count = 2 + max 0 (count - 1)

-- The array holding the scalar at a position, where its encoding starts and how long it is, and the
-- position after it in the blob.
scalarSource :: DocTable -> ByteArray -> Int -> (# ByteArray, Int, Int, Int #)
scalarSource table blob position
    | (indexByteArray blob position :: Word8) == opShared = case readVarint blob (position + 1) of
        (# index, next #) -> case tableEntry table index of (# start, len #) -> (# tableArena table, start, len, next #)
    | otherwise = case readVarint blob (position + 1) of
        (# len, next #) -> (# blob, next, len, next + len #)

{- | A URL prefix a render writes in place of a hole's URL, before the URL's file name: the bytes
aeson writes for its characters, quotes excluded.
-}
newtype UrlPrefix = UrlPrefix ByteArray
    deriving stock (Eq, Show)

-- | Encode a prefix once for every hole a render rebases.
urlPrefix :: Text -> UrlPrefix
urlPrefix text = UrlPrefix (SBS.unShortByteString (SBS.toShort (BS.take (BS.length encoded - 2) (BS.drop 1 encoded))))
  where
    encoded = encodeString text

{- Where the file name lies in an encoded URL: after the last slash before any query or fragment. No
escape writes a slash, a question mark or a hash, so the span is the file name's own encoding. -}
fileSpan :: ByteArray -> Int -> Int -> (# Int, Int #)
fileSpan array start len
    | len < 2 = (# start, 0 #)
    | otherwise = (# fileStart, pathEnd - fileStart #)
  where
    end = start + len - 1
    pathEnd = scanPath (start + 1)
    scanPath !at
        | at >= end = end
        | otherwise = case indexByteArray array at :: Word8 of
            0x3f -> at
            0x23 -> at
            _ -> scanPath (at + 1)
    fileStart = scanSlash pathEnd
    scanSlash !at
        | at <= start + 1 = start + 1
        | (indexByteArray array (at - 1) :: Word8) == 0x2f = at
        | otherwise = scanSlash (at - 1)

-- The length of a value's encoding, with its hole rebased onto the prefix when one is given, or -1
-- when it names a string the table lacks.
renderedLength :: DocTable -> Maybe UrlPrefix -> Packed -> Int
renderedLength table prefix (Packed blob hole) = case encodedAt table blob 0 of
    (# len, _ #)
        | len < 0 -> -1
        | otherwise -> case prefix of
            Just (UrlPrefix bytes) | hole >= 0 -> case scalarSource table blob hole of
                (# array, start, old, _ #) -> case fileSpan array start old of
                    (# _, file #) -> len - old + 2 + sizeofByteArray bytes + file
            _ -> len

{- Write the value's encoding at an offset, before the limit, and return the offset after it. A write
that would pass the limit, or a string the table lacks, returns an offset past the limit instead. -}
pokePacked :: Ptr Word8 -> Int -> Int -> DocTable -> Maybe UrlPrefix -> Packed -> IO Int
pokePacked target limit start table prefix (Packed blob hole) = do
    at <- newPrimVar 0
    out <- newPrimVar start
    let bounded len write = do
            o <- readPrimVar out
            if len >= 0 && o + len <= limit then write o >> writePrimVar out (o + len) else writePrimVar out (limit + 1)
        byte w = bounded 1 (\o -> pokeByteOff target o (w :: Word8))
        copyFrom array from len = bounded len (\o -> copyByteArrayToAddr (target `plusPtr` o) array from len)
        literal bytes = bounded (BS.length bytes) (\o -> BSU.unsafeUseAsCStringLen bytes (\(source, len) -> copyBytes (target `plusPtr` o) (castPtr source) len))
        value = do
            position <- readPrimVar at
            case prefix of
                Just (UrlPrefix bytes) | position == hole -> case scalarSource table blob position of
                    (# array, from, len, next #) -> case fileSpan array from len of
                        (# file, fileLen #) -> do
                            writePrimVar at next
                            byte quote >> copyFrom bytes 0 (sizeofByteArray bytes) >> copyFrom array file fileLen >> byte quote
                _ -> stored position
        stored position = case indexByteArray blob position :: Word8 of
            code
                | code == opNull -> writePrimVar at (position + 1) >> literal "null"
                | code == opFalse -> writePrimVar at (position + 1) >> literal "false"
                | code == opTrue -> writePrimVar at (position + 1) >> literal "true"
                | code == opShared -> case readVarint blob (position + 1) of
                    (# index, next #) -> writePrimVar at next >> sharedString index
                | code == opObject -> case readVarint blob (position + 1) of
                    (# count, next #) -> writePrimVar at next >> byte 0x7b >> members count True >> byte 0x7d
                | code == opArray -> case readVarint blob (position + 1) of
                    (# count, next #) -> writePrimVar at next >> byte 0x5b >> items count True >> byte 0x5d
                | otherwise -> case readVarint blob (position + 1) of
                    (# len, next #) -> writePrimVar at (next + len) >> copyFrom blob next len
        sharedString index = case tableEntry table index of (# from, len #) -> copyFrom (tableArena table) from len
        members !count leading = when (count > 0) $ do
            unless leading (byte 0x2c)
            position <- readPrimVar at
            case readVarint blob position of
                (# tagged, next #)
                    | even tagged -> writePrimVar at next >> sharedString (tagged `div` 2)
                    | otherwise -> writePrimVar at (next + tagged `div` 2) >> copyFrom blob next (tagged `div` 2)
            byte 0x3a
            value
            members (count - 1) False
        items !count leading = when (count > 0) $ do
            unless leading (byte 0x2c)
            value
            items (count - 1) False
    value
    readPrimVar out

{- | A string or number as aeson reads the bytes aeson wrote for it. A number written in exponent or
fraction form reads back with an exponent aeson writes in that form again.
-}
decodeScalar :: ByteArray -> Int -> Int -> Value
decodeScalar array start len
    | len >= 2 && opening == quote = if noEscape (start + 1) then String (copyText (start + 1) (len - 2)) else escaped
    | otherwise = maybe Null Number (decodeNumber array start len)
  where
    opening = indexByteArray array start :: Word8
    end = start + len - 1
    noEscape !at = at >= end || ((indexByteArray array at :: Word8) /= 0x5c && noEscape (at + 1))
    copyText from count = TI.text (TA.run (TA.new count >>= \target -> TA.copyI count target 0 array from >> pure target)) 0 count
    escaped = fromRight Null (eitherDecodeStrict (BSI.unsafeCreate len (\target -> copyByteArrayToAddr target array start len)))

-- A number in aeson's encoding: an integer, or a fraction or exponent form for an exponent below zero
-- or above 1024.
decodeNumber :: ByteArray -> Int -> Int -> Maybe Scientific
decodeNumber array start len
    | len <= 18 + signWidth, digitsOnly afterSign = Just (fromIntegral (smallInteger afterSign 0))
    | otherwise = do
        let (whole, afterWhole) = digitsFrom afterSign 0
            (fraction, fractionDigits, afterFraction) =
                if afterWhole < end && byteAt afterWhole == 0x2e
                    then let (value, next) = digitsFrom (afterWhole + 1) whole in (value, next - afterWhole - 1, next)
                    else (whole, 0, afterWhole)
        exponent <-
            if afterFraction < end && (byteAt afterFraction == 0x65 || byteAt afterFraction == 0x45)
                then exponentFrom (afterFraction + 1)
                else if afterFraction == end then Just 0 else Nothing
        let written = scientific (signed fraction) (exponent - fractionDigits)
        pure $! if afterWhole == end then written else aesonForm written
  where
    end = start + len
    byteAt at = indexByteArray array at :: Word8
    negative = len > 0 && byteAt start == 0x2d
    signWidth = if negative then 1 else 0
    afterSign = start + signWidth
    isDigit byte = byte >= 0x30 && byte <= 0x39
    digitsOnly !at = at < end && all' at
    all' !at = at >= end || (isDigit (byteAt at) && all' (at + 1))
    smallInteger :: Int -> Int -> Int
    smallInteger !at !acc
        | at >= end = if negative then negate acc else acc
        | otherwise = smallInteger (at + 1) (acc * 10 + fromIntegral (byteAt at - 0x30))
    signed value = if negative then negate value else value
    digitsFrom :: Int -> Integer -> (Integer, Int)
    digitsFrom !at !acc
        | at < end, isDigit (byteAt at) = digitsFrom (at + 1) (acc * 10 + fromIntegral (byteAt at - 0x30))
        | otherwise = (acc, at)
    exponentFrom at =
        let (minus, digitsAt) = case byteAt at of
                0x2d -> (True, at + 1)
                0x2b -> (False, at + 1)
                _ -> (False, at)
            (value, next) = digitsFrom digitsAt 0
         in if next == end && next > digitsAt then Just (fromInteger (if minus then negate value else value)) else Nothing
    -- aeson writes a number whose exponent lies in [0, 1024] as an integer, so a fraction or exponent
    -- form came from an exponent outside that range, which a read keeps.
    aesonForm written
        | Scientific.coefficient written == 0 = scientific 0 (-1)
        | otherwise =
            let normal = normalize written
                e = Scientific.base10Exponent normal
             in if e < 0 || e > 1024 then normal else scientific (Scientific.coefficient normal * 10 ^ (e + 1)) (-1)

-- | Where a decode finds the string and the key a table index names.
class TableStrings t where
    tableValue :: t st -> Int -> ST st Value
    tableKey :: t st -> Int -> ST st Key.Key

-- A document table as a decode reads it.
newtype OfTable st = OfTable DocTable

instance TableStrings OfTable where
    tableValue (OfTable table) index = pure $! tableString table index
    tableKey (OfTable table) index =
        pure $! case tableString table index of
            String text -> Key.fromText text
            _ -> ""

-- A table string as aeson reads it, or null for an index past the table.
tableString :: DocTable -> Int -> Value
tableString table index = case tableEntry table index of
    (# start, len #)
        | len < 0 -> Null
        | otherwise -> decodeScalar (tableArena table) start len

{- | Read the value at the position the variable holds back as aeson's tree, and move the variable
past it. The strings resolve a table index to its string and to its key.
-}
decodeWith :: (TableStrings t) => t st -> ByteArray -> PrimVar st Int -> ST st Value
decodeWith strings blob at = do
    position <- readPrimVar at
    case indexByteArray blob position :: Word8 of
        byte
            | byte == opNull -> writePrimVar at (position + 1) >> pure Null
            | byte == opFalse -> writePrimVar at (position + 1) >> pure (Bool False)
            | byte == opTrue -> writePrimVar at (position + 1) >> pure (Bool True)
            | byte == opShared -> case readVarint blob (position + 1) of
                (# index, next #) -> writePrimVar at next >> tableValue strings index
            | byte == opObject -> case readVarint blob (position + 1) of
                (# count, next #) -> do
                    writePrimVar at next
                    members <- decodeMapWith strings blob at count
                    pure $! Object (KeyMap.fromMap members)
            | byte == opArray -> case readVarint blob (position + 1) of
                (# count, next #) -> do
                    writePrimVar at next
                    list <- decodeItemsWith strings blob at count []
                    pure $! Array (V.fromListN count (reverse list))
            | otherwise -> case readVarint blob (position + 1) of
                (# len, next #) -> do
                    writePrimVar at (next + len)
                    pure $! decodeScalar blob next len
{-# INLINEABLE decodeWith #-}

-- The next members of an object, the given number of them, as a balanced map built in key order.
decodeMapWith :: (TableStrings t) => t st -> ByteArray -> PrimVar st Int -> Int -> ST st (Map.Map Key.Key Value)
decodeMapWith strings blob at !count
    | count <= 0 = pure Tip
    | otherwise = do
        let before = (count - 1) `div` 2
        left <- decodeMapWith strings blob at before
        name <- decodeKeyWith strings blob at
        member <- decodeWith strings blob at
        right <- decodeMapWith strings blob at (count - 1 - before)
        pure $! Bin count name member left right
{-# INLINEABLE decodeMapWith #-}

-- | The member key at the position the variable holds, moving the variable to its value.
decodeKeyWith :: (TableStrings t) => t st -> ByteArray -> PrimVar st Int -> ST st Key.Key
decodeKeyWith strings blob at = do
    position <- readPrimVar at
    case readVarint blob position of
        (# tagged, next #)
            | even tagged -> writePrimVar at next >> tableKey strings (tagged `div` 2)
            | otherwise -> do
                writePrimVar at (next + tagged `div` 2)
                pure $! case decodeScalar blob next (tagged `div` 2) of
                    String text -> Key.fromText text
                    _ -> ""
{-# INLINEABLE decodeKeyWith #-}

-- An array's items, last first.
decodeItemsWith :: (TableStrings t) => t st -> ByteArray -> PrimVar st Int -> Int -> [Value] -> ST st [Value]
decodeItemsWith strings blob at !count acc
    | count <= 0 = pure acc
    | otherwise = decodeWith strings blob at >>= \item -> decodeItemsWith strings blob at (count - 1) (item : acc)
{-# INLINEABLE decodeItemsWith #-}

-- | The value back as aeson's tree.
packedValue :: DocTable -> Packed -> Value
packedValue table (Packed blob _) = runST (newPrimVar 0 >>= decodeWith (OfTable table) blob)

-- The value as aeson's tree, with its hole's URL rebased onto the prefix as a render writes it.
rebasedValue :: DocTable -> Maybe UrlPrefix -> Packed -> Value
rebasedValue table prefix value@(Packed blob hole) = case prefix of
    Just (UrlPrefix bytes) | hole >= 0 -> case scalarSource table blob hole of
        (# array, from, len, next #) -> case fileSpan array from len of
            (# file, fileLen #) ->
                let size = 2 + sizeofByteArray bytes + fileLen
                    rest = sizeofByteArray blob - next
                    rebased = createByteArray (hole + 1 + varintSize size + size + rest) $ \target -> do
                        copyByteArray target 0 blob 0 hole
                        writeByteArray target hole opInline
                        at <- writeVarint size target (hole + 1)
                        writeByteArray target at quote
                        copyByteArray target (at + 1) bytes 0 (sizeofByteArray bytes)
                        copyByteArray target (at + 1 + sizeofByteArray bytes) array file fileLen
                        writeByteArray target (at + size - 1) quote
                        copyByteArray target (at + size) blob next rest
                 in packedValue table (Packed rebased (-1))
    _ -> packedValue table value

-- | One packed value in a render, and the index of the plan's table its own document's read sealed.
data Piece = Piece !Int !Packed
    deriving stock (Eq, Show)

{- | The packed member of a rendered document: an object's members, sorted by key with no key twice,
or an array's items. A render writes them in list order.
-}
data Pieces = ObjectPieces ![(Text, Piece)] | ArrayPieces ![Piece]
    deriving stock (Eq, Show)

{- | An assembled document: small members aeson encodes, and one member that renders from pieces over
its sources' tables, each hole rebased onto the prefix when the document rebases.
-}
data RenderPlan = RenderPlan
    { planMembers :: !(KeyMap.KeyMap Value)
    , planSlot :: !Key.Key
    , planTables :: !(SmallArray DocTable)
    , planPieces :: !Pieces
    , planPrefix :: !(Maybe UrlPrefix)
    }
    deriving stock (Eq, Show)

-- The plan's table at an index, or nothing past the plan.
tableAt :: SmallArray DocTable -> Int -> Maybe DocTable
tableAt tables index
    | index >= 0 && index < sizeofSmallArray tables = Just (indexSmallArray tables index)
    | otherwise = Nothing

data Part = Encoded !ByteString | Packs

-- The document's members in key order, each key's encoding with its value encoded or packed.
planParts :: RenderPlan -> [(ByteString, Part)]
planParts plan =
    [ (encodeString (Key.toText key), maybe Packs (Encoded . toStrict . encodingToLazyByteString . Encoding.value) part)
    | (key, part) <- KeyMap.toAscList (KeyMap.insert (planSlot plan) Nothing (Just <$> planMembers plan))
    ]

-- The render's length, or nothing when a piece names a table or string the plan lacks.
partsLength :: RenderPlan -> [(ByteString, Part)] -> Maybe Int
partsLength plan parts = (separators (length parts) +) . sum <$> traverse partLength parts
  where
    partLength (key, part) =
        (BS.length key + 1 +) <$> case part of
            Encoded bytes -> Just (BS.length bytes)
            Packs -> case planPieces plan of
                ObjectPieces members -> (separators (length members) +) . sum <$> traverse (\(version, piece) -> (encodedLength version + 1 +) <$> pieceLength piece) members
                ArrayPieces items -> (separators (length items) +) . sum <$> traverse pieceLength items
    pieceLength (Piece index value) = do
        table <- tableAt (planTables plan) index
        let len = renderedLength table (planPrefix plan) value
        guard (len >= 0)
        pure len

{- | Render an assembled document into one buffer of its exact length, or nothing when a piece names a
table or string the plan lacks, or the render does not fill the buffer exactly.
-}
renderPlan :: RenderPlan -> Maybe ByteString
renderPlan plan = do
    size <- partsLength plan parts
    let rendered = BSI.unsafeCreateUptoN size $ \target -> do
            let within offset len write
                    | len >= 0 && offset + len <= size = write >> pure (offset + len)
                    | otherwise = pure (size + 1)
                byte offset w = within offset 1 (pokeByteOff target offset (w :: Word8))
                bytes offset b = within offset (BS.length b) (BSU.unsafeUseAsCStringLen b (\(source, len) -> copyBytes (target `plusPtr` offset) (castPtr source) len))
                separated offset list write = foldlM (\o (index, item) -> (if index > (0 :: Int) then byte o 0x2c else pure o) >>= \afterComma -> write afterComma item) offset (zip [0 ..] list)
                member offset key write = bytes offset key >>= \afterKey -> byte afterKey 0x3a >>= write
                piece offset (Piece index value) = case tableAt (planTables plan) index of
                    Just table | offset <= size -> pokePacked target size offset table (planPrefix plan) value
                    _ -> pure (size + 1)
                part offset = \case
                    Encoded b -> bytes offset b
                    Packs -> case planPieces plan of
                        ObjectPieces members -> do
                            open <- byte offset 0x7b
                            end <- separated open members $ \at (version, item) -> member at (encodeString version) (`piece` item)
                            byte end 0x7d
                        ArrayPieces items -> do
                            open <- byte offset 0x5b
                            end <- separated open items piece
                            byte end 0x5d
            open <- byte 0 0x7b
            end <- separated open parts $ \at (key, p) -> member at key (`part` p)
            final <- byte end 0x7d
            pure (if final == size then size else 0)
    guard (BS.length rendered == size)
    pure rendered
  where
    parts = planParts plan

{- | The assembled document as aeson's tree, as its render writes it, or nothing when a piece names a
table the plan lacks.
-}
planValue :: RenderPlan -> Maybe Value
planValue plan = (\slotValue -> Object (KeyMap.insert (planSlot plan) slotValue (planMembers plan))) <$> slot
  where
    slot = case planPieces plan of
        ObjectPieces list -> Object . KeyMap.fromList <$> traverse (\(version, item) -> (Key.fromText version,) <$> pieceValue item) list
        ArrayPieces list -> Array . V.fromList <$> traverse pieceValue list
    pieceValue (Piece index value) = (\table -> rebasedValue table (planPrefix plan) value) <$> tableAt (planTables plan) index

-- | The heap bytes the plan's tables and pieces hold, each piece with its list cell, record and key.
planResident :: RenderPlan -> Int
planResident plan =
    sum (map tableResident (toList (planTables plan))) + case planPieces plan of
        ObjectPieces list -> sum [72 + textStorageBytes version + pieceResident item | (version, item) <- list]
        ArrayPieces list -> sum [48 + pieceResident item | item <- list]
  where
    pieceResident (Piece _ value) = packedResident value
