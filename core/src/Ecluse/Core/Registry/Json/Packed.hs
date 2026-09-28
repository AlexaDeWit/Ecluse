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

    -- * The document table
    DocTable,
    TableString (..),
    docTable,
    tableBytes,

    -- * Packed values
    Packed,
    packedLength,
    packedBytes,
    holeText,

    -- * Writing a blob
    Opcode (..),
    varintSize,
    Blob,
    newBlob,
    blobOffset,
    putOpcode,
    putVarint,
    putRaw,
    putString,
    finishBlob,

    -- * Reading back
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

import Control.Monad.ST (ST)
import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Extra qualified as Builder
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Unsafe qualified as BSU
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, copyByteArray, copyByteArrayToAddr, createByteArray, indexByteArray, newByteArray, sizeofByteArray, unsafeFreezeByteArray, writeByteArray)
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, primArrayFromListN, sizeofPrimArray)
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as TE
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

-- | One table string, as its text when no byte needs escaping, and as aeson's bytes otherwise.
data TableString = PlainString !Text | EscapedString !SBS.ShortByteString

-- | One document's shared strings, back to back, and where each begins.
data DocTable = DocTable !ByteArray !(PrimArray Word32)
    deriving stock (Eq, Show)

-- | Lay out the table from its strings in index order.
docTable :: [TableString] -> DocTable
docTable strings = DocTable arena (primArrayFromListN (length strings + 1) (scanl (+) 0 (map (fromIntegral . stringLength) strings)))
  where
    total = sum (map stringLength strings)
    arena = createByteArray total $ \target -> void (foldlM (place target) 0 strings)
    place target offset string = case string of
        PlainString (TI.Text array start len) -> do
            writeByteArray target offset quote
            copyByteArray target (offset + 1) array start len
            writeByteArray target (offset + len + 1) quote
            pure (offset + len + 2)
        EscapedString bytes -> do
            copyByteArray target offset (SBS.unShortByteString bytes) 0 (SBS.length bytes)
            pure (offset + SBS.length bytes)
    stringLength = \case
        PlainString (TI.Text _ _ len) -> len + 2
        EscapedString bytes -> SBS.length bytes

-- | The table's bytes, its strings and their offsets.
tableBytes :: DocTable -> Int
tableBytes (DocTable arena offsets) = sizeofByteArray arena + 4 * sizeofPrimArray offsets

tableEntry :: DocTable -> Int -> (Int, Int)
tableEntry (DocTable _ offsets) index =
    let start = fromIntegral (indexPrimArray offsets index)
     in (start, fromIntegral (indexPrimArray offsets (index + 1)) - start)

tableArena :: DocTable -> ByteArray
tableArena (DocTable arena _) = arena

-- | One retained value: its opcodes, its encoded length, and where its hole's string starts, or -1.
data Packed = Packed !ByteArray !Int !Int
    deriving stock (Eq, Show)

-- | The length of the value's encoding.
packedLength :: Packed -> Int
packedLength (Packed _ len _) = len

-- | The bytes the value holds itself, outside its table.
packedBytes :: Packed -> Int
packedBytes (Packed blob _ _) = sizeofByteArray blob

-- | The opcodes. A member's key is a table index times two, or an inline length times two plus one.
data Opcode = OpNull | OpFalse | OpTrue | OpShared | OpObject | OpArray | OpInline

opByte :: Opcode -> Word8
opByte = \case
    OpNull -> 0
    OpFalse -> 1
    OpTrue -> 2
    OpShared -> 3
    OpObject -> 4
    OpArray -> 5
    OpInline -> 6

-- | The bytes 'putVarint' writes.
varintSize :: Int -> Int
varintSize n
    | n < 0x80 = 1
    | otherwise = 1 + varintSize (n `shiftR` 7)

-- | A blob being written, with the offset of its next byte.
data Blob s = Blob !(MutableByteArray s) !(PrimVar s Int)

-- | A blob of the given size, to be written front to back.
newBlob :: Int -> ST s (Blob s)
newBlob size = Blob <$> newByteArray size <*> newPrimVar 0

-- | Where the next byte goes.
blobOffset :: Blob s -> ST s Int
blobOffset (Blob _ cursor) = readPrimVar cursor

putByte :: Blob s -> Word8 -> ST s ()
putByte (Blob target cursor) byte = do
    offset <- readPrimVar cursor
    writeByteArray target offset byte
    writePrimVar cursor (offset + 1)

-- | Write an opcode.
putOpcode :: Blob s -> Opcode -> ST s ()
putOpcode blob = putByte blob . opByte

-- | Write a non-negative integer seven bits at a time.
putVarint :: Blob s -> Int -> ST s ()
putVarint blob !n
    | n < 0x80 = putByte blob (fromIntegral n)
    | otherwise = putByte blob (fromIntegral (n .&. 0x7f) .|. 0x80) >> putVarint blob (n `shiftR` 7)

-- | Copy bytes in.
putRaw :: Blob s -> SBS.ShortByteString -> ST s ()
putRaw (Blob target cursor) bytes = do
    offset <- readPrimVar cursor
    copyByteArray target offset (SBS.unShortByteString bytes) 0 (SBS.length bytes)
    writePrimVar cursor (offset + SBS.length bytes)

-- | Write a string's encoding, as 'encodeString' writes it.
putString :: Blob s -> Text -> ST s ()
putString blob@(Blob target cursor) text@(TI.Text array start len)
    | plain text = do
        offset <- readPrimVar cursor
        writeByteArray target offset quote
        copyByteArray target (offset + 1) array start len
        writeByteArray target (offset + len + 1) quote
        writePrimVar cursor (offset + len + 2)
    | otherwise = putRaw blob (SBS.toShort (encodeString text))

-- | Finish a blob with its encoded length and the offset of its hole, or -1.
finishBlob :: Blob s -> Int -> Int -> ST s Packed
finishBlob (Blob target _) encoded hole = do
    blob <- unsafeFreezeByteArray target
    pure (Packed blob encoded hole)

readVarint :: ByteArray -> Int -> (# Int, Int #)
readVarint blob = go 0 0
  where
    go !acc !shift !i =
        let byte = indexByteArray blob i :: Word8
            acc' = acc .|. (fromIntegral (byte .&. 0x7f) `shiftL` shift)
         in if byte < 0x80 then (# acc', i + 1 #) else go acc' (shift + 7) (i + 1)

-- | The text of the value's hole, when it has one. A string with no escape copies its bytes once.
holeText :: DocTable -> Packed -> Maybe Text
holeText table (Packed blob _ hole)
    | hole < 0 = Nothing
    | otherwise = case scalarSource table blob hole of
        (# array, start, len, _ #)
            | len >= 2 && noEscape array (start + 1) (start + len - 1) -> Just (copyText array (start + 1) (len - 2))
            | otherwise -> case decodeScalar (sliceOf array start len) of
                String text -> Just text
                _ -> Nothing
  where
    noEscape array from to = from > to || (indexByteArray array from /= (0x5c :: Word8) && noEscape array (from + 1) to)
    copyText array from len = TI.text (TA.run (TA.new len >>= \target -> TA.copyI len target 0 array from >> pure target)) 0 len

-- The array holding the scalar at a position, where its encoding starts and how long it is, and the
-- position after it in the blob.
scalarSource :: DocTable -> ByteArray -> Int -> (# ByteArray, Int, Int, Int #)
scalarSource table blob position = case indexByteArray blob position :: Word8 of
    3 -> case readVarint blob (position + 1) of
        (# index, next #) -> let (start, len) = tableEntry table index in (# tableArena table, start, len, next #)
    _ -> case readVarint blob (position + 1) of
        (# len, next #) -> (# blob, next, len, next + len #)

-- The encoded bytes of the scalar at a position, and the position after it.
scalarAt :: DocTable -> ByteArray -> Int -> (# ByteString, Int #)
scalarAt table blob position = case indexByteArray blob position :: Word8 of
    3 -> case readVarint blob (position + 1) of
        (# index, next #) -> let (start, len) = tableEntry table index in (# sliceOf (tableArena table) start len, next #)
    _ -> case readVarint blob (position + 1) of
        (# len, next #) -> (# sliceOf blob next len, next + len #)

sliceOf :: ByteArray -> Int -> Int -> ByteString
sliceOf array start len = BSI.unsafeCreate len $ \target -> copyByteArrayToAddr target array start len

-- A string or number as aeson reads the bytes aeson wrote for it.
decodeScalar :: ByteString -> Value
decodeScalar bytes
    | BS.length bytes >= 2 && BSU.unsafeHead bytes == quote && BS.notElem 0x5c bytes =
        either (const aeson) String (TE.decodeUtf8' (BS.take (BS.length bytes - 2) (BS.drop 1 bytes)))
    | otherwise = aeson
  where
    aeson = fromRight Null (eitherDecodeStrict bytes)

-- | The value back as aeson's tree, with the hole's string replaced when one is given.
packedValue :: DocTable -> Packed -> Maybe Text -> Value
packedValue table (Packed blob _ hole) substitute = case value 0 of (# decoded, _ #) -> decoded
  where
    value position
        | position == hole, Just text <- substitute = case scalarAt table blob position of (# _, next #) -> (# String text, next #)
        | otherwise = case indexByteArray blob position :: Word8 of
            0 -> (# Null, position + 1 #)
            1 -> (# Bool False, position + 1 #)
            2 -> (# Bool True, position + 1 #)
            4 -> case readVarint blob (position + 1) of
                (# count, next #) -> case members count next [] of
                    (# pairs, end #) -> (# Object (KeyMap.fromList pairs), end #)
            5 -> case readVarint blob (position + 1) of
                (# count, next #) -> case items count next [] of
                    (# list, end #) -> (# Array (V.fromListN count (reverse list)), end #)
            _ -> case scalarAt table blob position of (# bytes, next #) -> (# decodeScalar bytes, next #)
    members :: Int -> Int -> [(Key.Key, Value)] -> (# [(Key.Key, Value)], Int #)
    members count position acc
        | count <= 0 = (# acc, position #)
        | otherwise = case keyAt position of
            (# key, afterKey #) -> case value afterKey of
                (# member, next #) -> members (count - 1) next ((key, member) : acc)
    items :: Int -> Int -> [Value] -> (# [Value], Int #)
    items count position acc
        | count <= 0 = (# acc, position #)
        | otherwise = case value position of (# item, next #) -> items (count - 1) next (item : acc)
    keyAt position = case readVarint blob position of
        (# tagged, next #)
            | even tagged -> let (start, len) = tableEntry table (tagged `shiftR` 1) in (# keyOf (sliceOf (tableArena table) start len), next #)
            | otherwise -> let len = tagged `shiftR` 1 in (# keyOf (sliceOf blob next len), next + len #)
    keyOf bytes = case decodeScalar bytes of
        String text -> Key.fromText text
        _ -> ""

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
pieceLength (Piece table packed@(Packed blob _ hole) substitute) = case substitute of
    Just (Replacement _ size) | hole >= 0 -> case scalarSource table blob hole of
        (# _, _, old, _ #) -> packedLength packed - old + size
    _ -> packedLength packed

planParts :: RenderPlan -> [(ByteString, Part)]
planParts (RenderPlan members slot pieces) =
    [ (encodeString (Key.toText key), maybe (Packs pieces) (Encoded . toStrict . encodingToLazyByteString . Encoding.value) part)
    | (key, part) <- KeyMap.toAscList (KeyMap.insert slot Nothing (Just <$> members))
    ]

-- | The length of 'renderPlan'.
planLength :: RenderPlan -> Int
planLength = partsLength . planParts

partsLength :: [(ByteString, Part)] -> Int
partsLength parts = separators parts + sum (map partLength parts)
  where
    partLength (key, part) =
        BS.length key + 1 + case part of
            Encoded bytes -> BS.length bytes
            Packs (ObjectPieces pieces) -> separators pieces + sum [encodedLength version + 1 + pieceLength piece | (version, piece) <- pieces]
            Packs (ArrayPieces pieces) -> separators pieces + sum (map pieceLength pieces)

-- Two brackets and a comma between each pair of items.
separators :: [a] -> Int
separators list = 2 + max 0 (length list - 1)

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
writePiece target (Piece table (Packed blob _ hole) substitute) start = snd <$> value 0 start
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
            0 -> (,) (position + 1) <$> word offset "null"
            1 -> (,) (position + 1) <$> word offset "false"
            2 -> (,) (position + 1) <$> word offset "true"
            3 -> case readVarint blob (position + 1) of
                (# index, next #) -> let (from, len) = tableEntry table index in (,) next <$> copyFrom arena from offset len
            4 -> case readVarint blob (position + 1) of
                (# count, next #) -> do
                    o <- byte offset 0x7b
                    (after, end) <- members count next o True
                    (,) after <$> byte end 0x7d
            5 -> case readVarint blob (position + 1) of
                (# count, next #) -> do
                    o <- byte offset 0x5b
                    (after, end) <- items count next o True
                    (,) after <$> byte end 0x5d
            _ -> case readVarint blob (position + 1) of
                (# len, next #) -> (,) (next + len) <$> copyFrom blob next offset len
    members :: Int -> Int -> Int -> Bool -> IO (Int, Int)
    members count position offset leading
        | count <= 0 = pure (position, offset)
        | otherwise = do
            o <- if leading then pure offset else byte offset 0x2c
            (afterKey, o') <- case readVarint blob position of
                (# tagged, next #)
                    | even tagged -> let (from, len) = tableEntry table (tagged `shiftR` 1) in (,) next <$> copyFrom arena from o len
                    | otherwise -> let len = tagged `shiftR` 1 in (,) (next + len) <$> copyFrom blob next o len
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
    pieceValue (Piece table packed substitute) = packedValue table packed ((\(Replacement text _) -> text) <$> substitute)
