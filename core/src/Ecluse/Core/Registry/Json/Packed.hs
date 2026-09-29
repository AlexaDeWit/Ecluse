-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE UnboxedTuples #-}

{- | The packed served form: one table per document holding the bytes aeson writes for each shared
string, and one opcode blob per retained release or file. A render copies those bytes into one
buffer of the output's exact length, so no string is escaped again. A value holds at most one hole:
a string the ecosystem's assembly may replace per request.
-}
module Ecluse.Core.Registry.Json.Packed (
    -- * Encoded strings
    encodeString,
    encodedLength,
    plain,

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
    valueEnd,

    -- * The document table
    DocTable,
    docTable,
    tableBytes,

    -- * Packed values
    Packed,
    packed,
    packedBlob,
    packedHole,
    packedBytes,
    packedLength,
    holeText,

    -- * Reading back
    TableStrings (..),
    decodeWith,
    decodeKeyWith,
    decodeScalar,
    packedValue,

    -- * Rendering
    Replacement,
    replacement,
    Piece (..),
    Pieces (..),
    RenderPlan (..),
    renderPlan,
    planLength,
    planValue,
) where

import Control.Monad.ST (ST, runST)
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Extra qualified as Builder
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Unsafe qualified as BSU
import Data.Map.Internal (Map (Bin, Tip))
import Data.Map.Strict qualified as Map
import Data.Primitive.ByteArray (ByteArray, copyByteArray, copyByteArrayToAddr, createByteArray, indexByteArray, sizeofByteArray, writeByteArray)
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

-- | The bytes aeson writes for a string, quotes included.
encodeString :: Text -> ByteString
encodeString text@(TI.Text array offset len)
    | plain text = BSI.unsafeCreate (len + 2) $ \target -> do
        pokeByteOff target 0 quote
        copyByteArrayToAddr (target `plusPtr` 1) array offset len
        pokeByteOff target (len + 1) quote
    | otherwise = toStrict (Builder.toLazyByteStringWith (Builder.untrimmedStrategy size size) mempty (Encoding.fromEncoding (Encoding.text text)))
  where
    size = encodedLength text

{- | The length of 'encodeString', counted from aeson's escapes: two bytes for a backslash, a quote, a
newline, a return or a tab, six for any other byte below a space, and one for every other byte.
-}
encodedLength :: Text -> Int
encodedLength (TI.Text array offset len) = go offset 2
  where
    end = offset + len
    go !i !total
        | i >= end = total
        | otherwise = go (i + 1) (total + width (TA.unsafeIndex array i))
    width byte
        | byte == 0x5c || byte == 0x22 = 2
        | byte >= 0x20 = 1
        | byte == 0x0a || byte == 0x0d || byte == 0x09 = 2
        | otherwise = 6

-- | Whether aeson writes the string's bytes as they are: no backslash, quote or byte below a space.
plain :: Text -> Bool
plain (TI.Text array offset len) = go offset
  where
    end = offset + len
    go !i
        | i >= end = True
        | otherwise =
            let byte = TA.unsafeIndex array i
             in byte >= 0x20 && byte /= 0x22 && byte /= 0x5c && go (i + 1)

quote :: Word8
quote = 0x22

{- | The opcodes that start each value. A shared string is followed by its table index, an object or
array by its member or item count, and an inline scalar by its length and aeson's bytes for it. A
member's key is its table index times two, or its inline length times two plus one and its bytes.
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
data DocTable = DocTable !ByteArray !(PrimArray Word32)
    deriving stock (Eq, Show)

-- | Lay out the table from its strings in index order.
docTable :: SmallArray Text -> DocTable
docTable strings = DocTable arena offsets
  where
    count = sizeofSmallArray strings
    offsets = runPrimArray $ do
        target <- newPrimArray (count + 1)
        let fill !index !offset = do
                writePrimArray target index (fromIntegral offset)
                when (index < count) (fill (index + 1) (offset + encodedLength (indexSmallArray strings index)))
        fill 0 0
        pure target
    arena = createByteArray (fromIntegral (indexPrimArray offsets count)) $ \target ->
        forM_ [0 .. count - 1] $ \index -> place target (fromIntegral (indexPrimArray offsets index)) (indexSmallArray strings index)
    place target offset text@(TI.Text array start len)
        | plain text = do
            writeByteArray target offset quote
            copyByteArray target (offset + 1) array start len
            writeByteArray target (offset + len + 1) quote
        | otherwise = let bytes = SBS.toShort (encodeString text) in copyByteArray target offset (SBS.unShortByteString bytes) 0 (SBS.length bytes)

-- | The table's bytes, its strings and their offsets.
tableBytes :: DocTable -> Int
tableBytes (DocTable arena offsets) = sizeofByteArray arena + 4 * sizeofPrimArray offsets

-- Where a table string's encoding starts in the arena, and its length. An index past the table
-- reads as the empty span, so a damaged blob never reads outside the arena.
tableEntry :: DocTable -> Int -> (# Int, Int #)
tableEntry (DocTable _ offsets) index
    | index < 0 || index + 1 >= sizeofPrimArray offsets = (# 0, 0 #)
    | otherwise =
        let start = fromIntegral (indexPrimArray offsets index)
         in (# start, fromIntegral (indexPrimArray offsets (index + 1)) - start #)
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

-- | Where the hole's string starts, or -1.
packedHole :: Packed -> Int
packedHole (Packed _ hole) = hole

-- | The bytes the value holds itself, outside its table.
packedBytes :: Packed -> Int
packedBytes (Packed blob _) = sizeofByteArray blob

-- | The length of the value's encoding.
packedLength :: DocTable -> Packed -> Int
packedLength table (Packed blob _) = case encodedAt table blob 0 of (# len, _ #) -> len

-- The encoded length of the value at a position, and the position after it.
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
                    (# _, keyLen #) -> case encodedAt table blob next of
                        (# len, after #) -> members (count - 1) after (total + keyLen + 1 + len)
                | otherwise -> case encodedAt table blob (next + tagged `div` 2) of
                    (# len, after #) -> members (count - 1) after (total + tagged `div` 2 + 1 + len)
    items !count !at !total
        | count <= 0 = (# total, at #)
        | otherwise = case encodedAt table blob at of (# len, after #) -> items (count - 1) after (total + len)

-- Two brackets and a comma between each pair of members or items.
separators :: Int -> Int
separators count = 2 + max 0 (count - 1)

-- | The text of the value's hole, when it has one.
holeText :: DocTable -> Packed -> Maybe Text
holeText table (Packed blob hole)
    | hole < 0 = Nothing
    | otherwise = case scalarSource table blob hole of
        (# array, start, len, _ #) -> case decodeScalar array start len of
            String text -> Just text
            _ -> Nothing

-- The array holding the scalar at a position, where its encoding starts and how long it is, and the
-- position after it in the blob.
scalarSource :: DocTable -> ByteArray -> Int -> (# ByteArray, Int, Int, Int #)
scalarSource table blob position
    | (indexByteArray blob position :: Word8) == opShared = case readVarint blob (position + 1) of
        (# index, next #) -> case tableEntry table index of (# start, len #) -> (# tableArena table, start, len, next #)
    | otherwise = case readVarint blob (position + 1) of
        (# len, next #) -> (# blob, next, len, next + len #)

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
    tableValue (OfTable table) index = pure $! case tableEntry table index of (# start, len #) -> decodeScalar (tableArena table) start len
    tableKey (OfTable table) index =
        pure $! case tableEntry table index of
            (# start, len #) -> case decodeScalar (tableArena table) start len of
                String text -> Key.fromText text
                _ -> ""

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

-- | The value back as aeson's tree, with the hole's string replaced when one is given.
packedValue :: DocTable -> Packed -> Maybe Text -> Value
packedValue table (Packed blob hole) substitute = runST $ do
    at <- newPrimVar 0
    decodeWith (OfTable table) (patched blob) at
  where
    -- A replaced hole is decoded from a copy of the blob with the new string inline.
    patched original = case substitute of
        Just text | hole >= 0 -> splice original hole text
        _ -> original
    splice original at text =
        let after = valueEnd original at
            encoded = encodeString text
            size = BS.length encoded
            header = 1 + varintSize size
         in createByteArray (at + header + size + sizeofByteArray original - after) $ \target -> do
                copyByteArray target 0 original 0 at
                writeByteArray target at opInline
                void (writeVarintAt target (at + 1) size)
                forM_ [0 .. size - 1] $ \i -> writeByteArray target (at + header + i) (BSU.unsafeIndex encoded i)
                copyByteArray target (at + header + size) original after (sizeofByteArray original - after)
    writeVarintAt target !position !n
        | n < 0x80 = writeByteArray target position (fromIntegral n :: Word8) >> pure (position + 1)
        | otherwise = writeByteArray target position (fromIntegral (n .&. 0x7f) .|. 0x80 :: Word8) >> writeVarintAt target (position + 1) (n `div` 0x80)

-- | A string a render writes in place of a hole, with the length of its encoding.
data Replacement = Replacement !Text !Int
    deriving stock (Eq, Show)

-- | The replacement for a string.
replacement :: Text -> Replacement
replacement text = Replacement text (encodedLength text)

-- Write a string's encoding at an offset and return the next offset, copying a plain string's bytes.
pokeString :: Ptr Word8 -> Int -> Text -> IO Int
pokeString target offset text@(TI.Text array start len)
    | plain text = do
        pokeByteOff target offset quote
        copyByteArrayToAddr (target `plusPtr` (offset + 1)) array start len
        pokeByteOff target (offset + len + 1) quote
        pure (offset + len + 2)
    | otherwise = BSU.unsafeUseAsCStringLen (encodeString text) $ \(source, size) -> copyBytes (target `plusPtr` offset) (castPtr source) size >> pure (offset + size)

-- | One packed value in a render, from its own document's table, with its hole's replacement.
data Piece = Piece !DocTable !Packed !(Maybe Replacement)
    deriving stock (Eq, Show)

-- | The packed member of a rendered document: an object's members in order, or an array's items.
data Pieces = ObjectPieces ![(Text, Piece)] | ArrayPieces ![Piece]
    deriving stock (Eq, Show)

-- | An assembled document: small members aeson encodes, and one member that renders from pieces.
data RenderPlan = RenderPlan !(KeyMap.KeyMap Value) !Key.Key !Pieces
    deriving stock (Eq, Show)

data Part = Encoded !ByteString | Packs !Pieces

pieceLength :: Piece -> Int
pieceLength (Piece table value@(Packed blob hole) substitute) = case substitute of
    Just (Replacement _ size) | hole >= 0 -> case scalarSource table blob hole of
        (# _, _, old, _ #) -> packedLength table value - old + size
    _ -> packedLength table value

planParts :: RenderPlan -> [(ByteString, Part)]
planParts (RenderPlan members slot pieces) =
    [ (encodeString (Key.toText key), maybe (Packs pieces) (Encoded . toStrict . encodingToLazyByteString . Encoding.value) part)
    | (key, part) <- KeyMap.toAscList (KeyMap.insert slot Nothing (Just <$> members))
    ]

-- | The length of 'renderPlan'.
planLength :: RenderPlan -> Int
planLength = partsLength . planParts

partsLength :: [(ByteString, Part)] -> Int
partsLength parts = listSeparators parts + sum (map partLength parts)
  where
    partLength (key, part) =
        BS.length key + 1 + case part of
            Encoded bytes -> BS.length bytes
            Packs (ObjectPieces pieces) -> listSeparators pieces + sum [encodedLength version + 1 + pieceLength piece | (version, piece) <- pieces]
            Packs (ArrayPieces pieces) -> listSeparators pieces + sum (map pieceLength pieces)

listSeparators :: [a] -> Int
listSeparators list = 2 + max 0 (length list - 1)

-- | Render an assembled document into one buffer of its exact length.
renderPlan :: RenderPlan -> ByteString
renderPlan plan = BSI.unsafeCreate (partsLength parts) $ \target -> do
    let byte offset w = pokeByteOff target offset (w :: Word8) >> pure (offset + 1)
        bytes offset b = BSU.unsafeUseAsCStringLen b $ \(source, len) -> copyBytes (target `plusPtr` offset) (castPtr source) len >> pure (offset + len)
        separated offset list write = foldlM (\o (index, item) -> (if index > (0 :: Int) then byte o 0x2c else pure o) >>= \o' -> write o' item) offset (zip [0 ..] list)
        part offset = \case
            Encoded b -> bytes offset b
            Packs (ObjectPieces pieces) -> do
                o <- byte offset 0x7b
                end <- separated o pieces $ \o' (version, piece) -> pokeString target o' version >>= \o'' -> byte o'' 0x3a >>= \o''' -> writePiece target piece o'''
                byte end 0x7d
            Packs (ArrayPieces pieces) -> do
                o <- byte offset 0x5b
                end <- separated o pieces (flip (writePiece target))
                byte end 0x5d
    o <- byte 0 0x7b
    end <- separated o parts $ \o' (key, p) -> bytes o' key >>= \o'' -> byte o'' 0x3a >>= \o''' -> part o''' p
    void (byte end 0x7d)
  where
    parts = planParts plan

-- Write one piece, copying its table strings and inline bytes, and return the next offset.
writePiece :: Ptr Word8 -> Piece -> Int -> IO Int
writePiece target (Piece table (Packed blob hole) substitute) start = snd <$> value 0 start
  where
    arena = tableArena table
    byte offset w = pokeByteOff target offset (w :: Word8) >> pure (offset + 1)
    copyFrom array from offset len = copyByteArrayToAddr (target `plusPtr` offset) array from len >> pure (offset + len)
    word offset (w :: ByteString) = BSU.unsafeUseAsCStringLen w $ \(source, len) -> copyBytes (target `plusPtr` offset) (castPtr source) len >> pure (offset + len)
    value :: Int -> Int -> IO (Int, Int)
    value position offset
        | position == hole
        , Just (Replacement text _) <- substitute = case scalarSource table blob position of
            (# _, _, _, next #) -> (,) next <$> pokeString target offset text
        | otherwise = case indexByteArray blob position :: Word8 of
            code
                | code == opNull -> (,) (position + 1) <$> word offset "null"
                | code == opFalse -> (,) (position + 1) <$> word offset "false"
                | code == opTrue -> (,) (position + 1) <$> word offset "true"
                | code == opShared -> case readVarint blob (position + 1) of
                    (# index, next #) -> case tableEntry table index of (# from, len #) -> (,) next <$> copyFrom arena from offset len
                | code == opObject -> case readVarint blob (position + 1) of
                    (# count, next #) -> do
                        o <- byte offset 0x7b
                        (after, end) <- members count next o True
                        (,) after <$> byte end 0x7d
                | code == opArray -> case readVarint blob (position + 1) of
                    (# count, next #) -> do
                        o <- byte offset 0x5b
                        (after, end) <- items count next o True
                        (,) after <$> byte end 0x5d
                | otherwise -> case readVarint blob (position + 1) of
                    (# len, next #) -> (,) (next + len) <$> copyFrom blob next offset len
    members :: Int -> Int -> Int -> Bool -> IO (Int, Int)
    members count position offset leading
        | count <= 0 = pure (position, offset)
        | otherwise = do
            o <- if leading then pure offset else byte offset 0x2c
            (afterKey, o') <- case readVarint blob position of
                (# tagged, next #)
                    | even tagged -> case tableEntry table (tagged `div` 2) of (# from, len #) -> (,) next <$> copyFrom arena from o len
                    | otherwise -> let len = tagged `div` 2 in (,) (next + len) <$> copyFrom blob next o len
            colon <- byte o' 0x3a
            (next, end) <- value afterKey colon
            members (count - 1) next end False
    items :: Int -> Int -> Int -> Bool -> IO (Int, Int)
    items count position offset leading
        | count <= 0 = pure (position, offset)
        | otherwise = do
            o <- if leading then pure offset else byte offset 0x2c
            (next, end) <- value position o
            items (count - 1) next end False

-- | The assembled document as aeson's tree.
planValue :: RenderPlan -> Value
planValue (RenderPlan members slot pieces) = Object (KeyMap.insert slot slotValue members)
  where
    slotValue = case pieces of
        ObjectPieces list -> Object (KeyMap.fromList [(Key.fromText version, pieceValue piece) | (version, piece) <- list])
        ArrayPieces list -> Array (V.fromList (map pieceValue list))
    pieceValue (Piece table value substitute) = packedValue table value ((\(Replacement text _) -> text) <$> substitute)
