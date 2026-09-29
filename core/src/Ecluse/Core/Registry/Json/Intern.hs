-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE UnboxedTuples #-}

{- | The first copy of each key and string that one document's retained values hold, found by the
bytes the lexer read before any text is built. Keep one table per read and drop it when the read
ends, so no table outlives the document it serves. The table hashes with SipHash-1-3 under a key
drawn for its read, so upstream text cannot choose which names collide.
-}
module Ecluse.Core.Registry.Json.Intern (
    -- * Names as read
    Name (Plain),
    decodedName,
    foldName,
    nameText,
    nameBytes,

    -- * The table
    InternTable,
    SipKey (..),
    newTableKey,
    newInternTable,
    Entry (..),
    Interned (..),
    internName,
    tableTexts,
    sipHash,
) where

import Crypto.Random (getRandomBytes)
import Data.Aeson (Value (String))
import Data.Bits (rotateL, shiftL, (.|.))
import Data.ByteArray.Hash (SipKey (..))
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable (..))
import Data.JsonStream.Unescape (unsafeDecodeASCII)
import Data.Primitive.SmallArray (SmallArray, newSmallArray, runSmallArray, writeSmallArray)
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI

import Ecluse.Core.Registry.Json.Packed (encodedLength)

-- | A member name or string as the lexer read it: plain ASCII bytes, or text decoded from escapes or UTF-8.
data Name = Plain !ByteString | Decoded !Text ~ByteString

-- | A name decoded from escapes or UTF-8. Its bytes are encoded once, when first asked for.
decodedName :: Text -> Name
decodedName text = Decoded text (encodeUtf8 text)

-- | Take a name as its plain ASCII bytes or as its decoded text.
foldName :: (ByteString -> a) -> (Text -> a) -> Name -> a
foldName onPlain onDecoded = \case
    Plain bytes -> onPlain bytes
    Decoded text _ -> onDecoded text
{-# INLINE foldName #-}

-- | The name's text on an array of its own. Plain bytes are copied, so no text keeps its input chunk.
nameText :: Name -> Text
nameText = \case
    Plain bytes -> unsafeDecodeASCII bytes
    Decoded text _ -> text

-- | The name's UTF-8 bytes. Plain bytes are the input's own, so hold them only for the current lookup.
nameBytes :: Name -> ByteString
nameBytes = \case
    Plain bytes -> bytes
    Decoded _ bytes -> bytes

-- | A fresh key for one read's table, so no key outlives the table it seeds.
newTableKey :: IO SipKey
newTableKey = do
    bytes <- getRandomBytes 16 :: IO ByteString
    let word = BS.foldl' (\total byte -> total * 256 + fromIntegral byte) 0
        (first8, last8) = BS.splitAt 8 bytes
    pure (SipKey (word first8) (word last8))

{- | One table entry: the shared text, its shared string value, whether a member's value is kept as
read, its place in the table, and the length of its encoding.
-}
data Entry = Entry
    { entryText :: !Text
    , entryString :: !Value
    , entryKeeps :: !Bool
    , entryIndex :: !Int
    , entryLength :: !Int
    }

-- | The table's entry for a name, with the table that holds it.
data Interned = Interned !Entry !InternTable

-- | One read's table and its entry count. Each entry's text is also its key, and each key carries its hash.
data InternTable = InternTable !SipKey !(HashMap.HashMap Probe Entry) !Int

{- | A table for one document that keeps the values of the named members as read. Name the members
whose values differ in every release or file, so they never enter the table.
-}
newInternTable :: SipKey -> [Text] -> InternTable
newInternTable key = foldl' seed (InternTable key mempty 0)
  where
    seed table@(InternTable _ entries count) name
        | HashMap.member (probeOf key (encodeUtf8 name)) entries = table
        | otherwise = insertEntry (encodeUtf8 name) (Entry name (String name) True count (encodedLength name)) table

{- | The table's entry for a name. A name the table lacks gets an entry of its own text, which the
returned table holds from then on.
-}
internName :: Name -> InternTable -> Interned
internName name table@(InternTable key entries count) = case HashMap.lookup (Probe code (Slice probe)) entries of
    Just entry -> Interned entry table
    Nothing ->
        let text = nameText name
            entry = Entry text (String text) False count (encodedLength text)
         in Interned entry (InternTable key (HashMap.insert (Probe code (Owned text)) entry entries) (count + 1))
  where
    probe = nameBytes name
    code = fromIntegral (sipHash 1 3 key probe)
{-# INLINE internName #-}

insertEntry :: ByteString -> Entry -> InternTable -> InternTable
insertEntry bytes entry (InternTable key entries count) =
    InternTable key (HashMap.insert (Probe (fromIntegral (sipHash 1 3 key bytes)) (Owned (entryText entry))) entry entries) (count + 1)

probeOf :: SipKey -> ByteString -> Probe
probeOf key bytes = Probe (fromIntegral (sipHash 1 3 key bytes)) (Slice bytes)

-- | Every entry's text in index order. Each index below the table's count holds exactly one entry.
tableTexts :: InternTable -> SmallArray Text
tableTexts (InternTable _ entries count) = runSmallArray $ do
    slots <- newSmallArray count ""
    forM_ (HashMap.elems entries) $ \entry -> when (entryIndex entry < count) (writeSmallArray slots (entryIndex entry) (entryText entry))
    pure slots

-- A key with its hash computed once. A probe holds the bytes it looks up, and a stored key its entry's text.
data Probe = Probe {-# UNPACK #-} !Int !Bytes

data Bytes = Slice !ByteString | Owned !Text

instance Eq Probe where
    Probe left leftBytes == Probe right rightBytes = left == right && sameBytes leftBytes rightBytes

instance Hashable Probe where
    hashWithSalt salt (Probe code _) = hashWithSalt salt code
    hash (Probe code _) = code

sameBytes :: Bytes -> Bytes -> Bool
sameBytes = curry $ \case
    (Owned left, Owned right) -> left == right
    (Slice left, Slice right) -> left == right
    (Slice bytes, Owned text) -> sliceMatches bytes text
    (Owned text, Slice bytes) -> sliceMatches bytes text

sliceMatches :: ByteString -> Text -> Bool
sliceMatches bytes (TI.Text array offset len) = BS.length bytes == len && go 0
  where
    go !index
        | index >= len = True
        | BSU.unsafeIndex bytes index == TA.unsafeIndex array (offset + index) = go (index + 1)
        | otherwise = False

{- | SipHash with the given compression and finalisation rounds, from one to four each, over the
message's little-endian words. The table uses SipHash-1-3.
-}
sipHash :: Int -> Int -> SipKey -> ByteString -> Word64
{-# INLINE sipHash #-}
sipHash compression finalisation (SipKey k0 k1) bytes =
    absorb 0 (k0 `xor` 0x736f6d6570736575) (k1 `xor` 0x646f72616e646f6d) (k0 `xor` 0x6c7967656e657261) (k1 `xor` 0x7465646279746573)
  where
    size = BS.length bytes
    whole = size - size `rem` 8
    absorb !offset !v0 !v1 !v2 !v3
        | offset < whole = case inject (fullWord offset) v0 v1 v2 v3 of
            (# a, b, c, d #) -> absorb (offset + 8) a b c d
        | otherwise = case inject ((fromIntegral size `shiftL` 56) .|. wordAt offset (size - offset)) v0 v1 v2 v3 of
            (# a, b, c, d #) -> case rounds finalisation a b (c `xor` 0xff) d of
                (# a', b', c', d' #) -> a' `xor` b' `xor` c' `xor` d'
    inject !message !v0 !v1 !v2 !v3 = case rounds compression v0 v1 v2 (v3 `xor` message) of
        (# a, b, c, d #) -> (# a `xor` message, b, c, d #)
    byte offset = fromIntegral (BSU.unsafeIndex bytes offset) :: Word64
    fullWord !offset =
        byte offset
            .|. (byte (offset + 1) `shiftL` 8)
            .|. (byte (offset + 2) `shiftL` 16)
            .|. (byte (offset + 3) `shiftL` 24)
            .|. (byte (offset + 4) `shiftL` 32)
            .|. (byte (offset + 5) `shiftL` 40)
            .|. (byte (offset + 6) `shiftL` 48)
            .|. (byte (offset + 7) `shiftL` 56)
    wordAt !offset !count = go 0 0
      where
        go !index !acc
            | index >= count = acc
            | otherwise = go (index + 1) (acc .|. (byte (offset + index) `shiftL` (8 * index)))

-- Rounds are unrolled, so no state word is boxed between them.
rounds :: Int -> Word64 -> Word64 -> Word64 -> Word64 -> (# Word64, Word64, Word64, Word64 #)
rounds count v0 v1 v2 v3 = case count of
    1 -> sipRound v0 v1 v2 v3
    2 -> case sipRound v0 v1 v2 v3 of (# a, b, c, d #) -> sipRound a b c d
    3 -> case sipRound v0 v1 v2 v3 of (# a, b, c, d #) -> case sipRound a b c d of (# e, f, g, h #) -> sipRound e f g h
    _ -> case sipRound v0 v1 v2 v3 of (# a, b, c, d #) -> case sipRound a b c d of (# e, f, g, h #) -> case sipRound e f g h of (# i, j, k, l #) -> sipRound i j k l
{-# INLINE rounds #-}

sipRound :: Word64 -> Word64 -> Word64 -> Word64 -> (# Word64, Word64, Word64, Word64 #)
sipRound !v0 !v1 !v2 !v3 =
    let !a0 = v0 + v1
        !b0 = rotateL v1 13 `xor` a0
        !a1 = rotateL a0 32
        !c0 = v2 + v3
        !d0 = rotateL v3 16 `xor` c0
        !a2 = a1 + d0
        !d1 = rotateL d0 21 `xor` a2
        !c1 = c0 + b0
        !b1 = rotateL b0 17 `xor` c1
        !c2 = rotateL c1 32
     in (# a2, b1, c2, d1 #)
{-# INLINE sipRound #-}
