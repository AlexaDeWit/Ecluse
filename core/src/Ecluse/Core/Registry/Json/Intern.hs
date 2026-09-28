-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE UnboxedTuples #-}

{- | The first copy of each key and string that one document's retained values hold, found by the
bytes the lexer read before any text is built. Keep one table per read and drop it when the read
ends, so no table outlives the document it serves.
-}
module Ecluse.Core.Registry.Json.Intern (
    -- * Names as read
    Name (..),
    nameText,
    nameBytes,

    -- * The table
    InternTable,
    TableHash (..),
    SipKey (..),
    newInternTable,
    readTableHash,
    tableHashWith,
    Entry (..),
    Interned (..),
    internName,
    sipHash13,
    sipHash24,
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
import Data.Map.Strict qualified as Map

-- | A member name or string as the lexer read it: plain ASCII bytes, or text decoded from escapes or UTF-8.
data Name = Plain !ByteString | Decoded !Text

-- | The name's text on an array of its own. Plain bytes are copied, so no text keeps its input chunk.
nameText :: Name -> Text
nameText = \case
    Plain bytes -> unsafeDecodeASCII bytes
    Decoded text -> text

-- | The name's UTF-8 bytes. Plain bytes are the input's own, so hold them only for the current lookup.
nameBytes :: Name -> ByteString
nameBytes = \case
    Plain bytes -> bytes
    Decoded text -> encodeUtf8 text

-- | How the table finds a name: a fixed-seed hash, a keyed SipHash, or byte order with no hash.
data TableHash
    = FixedSeed
    | SipHash13 !SipKey
    | SipHash24 !SipKey
    | ByteOrder

-- | The hash a read uses, with a fresh key per read, so no key outlives the table it seeds.
readTableHash :: IO TableHash
readTableHash = do
    bytes <- getRandomBytes 16 :: IO ByteString
    let word = BS.foldl' (\total byte -> total * 256 + fromIntegral byte) 0
        (first8, last8) = BS.splitAt 8 bytes
    pure (tableHashWith (SipKey (word first8) (word last8)))

-- | The hash reads use, given the key a keyed hash takes.
tableHashWith :: SipKey -> TableHash
tableHashWith = SipHash13

-- | One table entry: the shared text, its shared string value, and whether a member's value is kept as read.
data Entry = Entry
    { entryText :: !Text
    , entryString :: !Value
    , entryKeeps :: !Bool
    }

-- | The table's entry for a name, with the table that holds it.
data Interned = Interned !Entry !InternTable

-- | One read's table. The hashed forms cache each key's hash, so a lookup hashes the probe once.
data InternTable
    = Hashed !TableHash !(HashMap.HashMap Probe Entry)
    | Ordered !(Map.Map ByteString Entry)

{- | A table for one document that keeps the values of the named members as read. Name the members
whose values differ in every release or file, so they never enter the table.
-}
newInternTable :: TableHash -> [Text] -> InternTable
newInternTable kind = foldl' seed blank
  where
    blank = case kind of
        ByteOrder -> Ordered mempty
        _ -> Hashed kind mempty
    seed table name = insertEntry (encodeUtf8 name) (Entry name (String name) True) table

{- | The table's entry for a name. A name the table lacks gets an entry of its own text, which the
returned table holds from then on.
-}
internName :: Name -> InternTable -> Interned
internName name table = case lookupEntry probe table of
    Just entry -> Interned entry table
    Nothing ->
        let text = nameText name
            entry = Entry text (String text) False
         in Interned entry (insertEntry (BS.copy probe) entry table)
  where
    probe = nameBytes name
{-# INLINE internName #-}

lookupEntry :: ByteString -> InternTable -> Maybe Entry
lookupEntry probe = \case
    Hashed kind entries -> HashMap.lookup (Probe (hashBytes kind probe) probe) entries
    Ordered entries -> Map.lookup probe entries

-- A stored key is a copy of its own, never a slice that keeps an input chunk alive.
insertEntry :: ByteString -> Entry -> InternTable -> InternTable
insertEntry stored entry = \case
    Hashed kind entries -> Hashed kind (HashMap.insert (Probe (hashBytes kind stored) stored) entry entries)
    Ordered entries -> Ordered (Map.insert stored entry entries)

hashBytes :: TableHash -> ByteString -> Int
hashBytes kind bytes = case kind of
    SipHash13 key -> fromIntegral (sipHash13 key bytes)
    SipHash24 key -> fromIntegral (sipHash24 key bytes)
    _ -> hash bytes

-- | SipHash-1-3, the table's keyed hash.
sipHash13 :: SipKey -> ByteString -> Word64
sipHash13 = sipHash 1 3

-- | SipHash-2-4, the reference form, which the table can use in place of SipHash-1-3.
sipHash24 :: SipKey -> ByteString -> Word64
sipHash24 = sipHash 2 4

-- SipHash with one to four compression and finalisation rounds, with the state in unboxed words.
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
            | otherwise = go (index + 1) (acc .|. (fromIntegral (BSU.unsafeIndex bytes (offset + index)) `shiftL` (8 * index)))

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

-- A key with its hash computed once, from its bytes.
data Probe = Probe {-# UNPACK #-} !Int !ByteString

instance Eq Probe where
    Probe left leftBytes == Probe right rightBytes = left == right && leftBytes == rightBytes

instance Hashable Probe where
    hashWithSalt salt (Probe code _) = hashWithSalt salt code
    hash (Probe code _) = code
