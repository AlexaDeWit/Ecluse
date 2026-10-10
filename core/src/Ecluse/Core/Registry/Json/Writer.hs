-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{- | The packed form's writer: a 'Build' that writes each value's opcodes into one scratch buffer per
read as the walk reads it, and copies each finished release or file into a blob of its own. An
object's members are written in source order, and at the object's end they are copied behind their
count in key order, the first member under each key kept.
-}
module Ecluse.Core.Registry.Json.Writer (
    Writer,
    newWriter,
    Frame,
    sealValue,
    Pick (..),
    decodePicked,
    decodeWhole,
    replacedMember,
    discard,
) where

import Control.Monad.ST (ST)
import Data.Aeson (Value (..), toEncoding)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Internal (Map (Bin, Tip))
import Data.Map.Strict qualified as Map
import Data.Primitive.Array (MutableArray, newArray, readArray, sizeofMutableArray, writeArray)
import Data.Primitive.Array qualified as Array
import Data.Primitive.ByteArray (ByteArray, copyMutableByteArray, indexByteArray, moveByteArray, writeByteArray)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Primitive.PrimArray (MutablePrimArray, PrimArray, copyMutablePrimArray, getSizeofMutablePrimArray, indexPrimArray, newPrimArray, readPrimArray, setPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)

import Ecluse.Core.Registry.Json.Intern (Entry, Name, entryIndex, entryString, entryText, foldName)
import Ecluse.Core.Registry.Json.Packed (Packed, TableStrings (..), decodeScalar, decodeWith, encodedLength, opArray, opFalse, opInline, opNull, opObject, opShared, opTrue, packed, packedBlob, readVarint, separators, valueEnd, varintSize, writeVarint)
import Ecluse.Core.Registry.Json.Scratch (Scratch, copyOut, decimalLength, newScratch, putAt, putByte, putDecimal, putEncodedText, putPlainBytes, putRawBytes, putVarint, reserve, rewindTo, scratchBuffer, scratchCursor)
import Ecluse.Core.Registry.Json.Shape (Build (..), MemberKey (..))
import Ecluse.Core.Registry.Json.Walk (Steps)

{- | One read's scratch, and what an open object has recorded of each member: its key, where its
value's bytes lie, and which earlier member under the same table key it hides.
-}
data Writer st = Writer
    { writerScratch :: Scratch st
    , writerSpans :: MutVar st (MutablePrimArray st Int)
    , writerKeys :: MutVar st (MutableArray st Text)
    , writerTop :: PrimVar st Int
    , writerSeen :: MutVar st (MutablePrimArray st Int)
    , writerStrings :: MutVar st (MutableArray st Value)
    , writerLengths :: MutVar st (MutablePrimArray st Int)
    , writerOrder :: MutVar st (MutablePrimArray st Int)
    , writerDepth :: PrimVar st Int
    , writerAdded :: Maybe (Entry, Entry)
    , writerMarks :: MutablePrimArray st Int
    , writerReplaced :: MutVar st (Maybe ByteArray)
    }

{- | A writer for one read. Each top-level object also holds the given member, a table key and a
string, in place of its own under that key. An object read in 'Keep' mode keeps its own instead.
-}
newWriter :: Maybe (Entry, Entry) -> ST st (Writer st)
newWriter added =
    Writer
        <$> newScratch 1024
        <*> (newPrimArray (spanWidth * 32) >>= newMutVar)
        <*> (newArray 32 "" >>= newMutVar)
        <*> newPrimVar 0
        <*> (newFilled 256 (-1) >>= newMutVar)
        <*> (newArray 256 Null >>= newMutVar)
        <*> (newFilled 256 0 >>= newMutVar)
        <*> (newPrimArray 64 >>= newMutVar)
        <*> newPrimVar 0
        <*> pure added
        <*> newMarks
        <*> newMutVar Nothing

newFilled :: Int -> Int -> ST st (MutablePrimArray st Int)
newFilled size value = do
    array <- newPrimArray size
    setPrimArray array 0 size value
    pure array

-- The marks: where the value the writer holds starts, and the span of the member the added member
-- replaced in it, or -1.
marksWidth, valueStart, replacedStart, replacedEnd :: Int
marksWidth = 3
valueStart = 0
replacedStart = 1
replacedEnd = 2

newMarks :: ST st (MutablePrimArray st Int)
newMarks = do
    marks <- newFilled marksWidth 0
    writePrimArray marks replacedStart (-1)
    pure marks

-- | An object being written: where its bytes start, and its first member's record.
data Frame = Frame !Int !Int

-- Spans are records of four integers: the key's table index or -1, where the value starts and ends,
-- and the member the key's earlier record pointed at.
spanWidth :: Int
spanWidth = 4

{- | A value the writer holds in its scratch, which a read passes on as a mark only. Each step runs
out of line, so a read's continuations hold the writer as one pointer.
-}
instance Build (Writer st) (ST st (Steps (ST st) s)) where
    type Built (Writer st) = ()
    type Fields (Writer st) = Frame
    type Items (Writer st) = Int
    sharedString writer entry next = writeShared writer entry >> next ()
    ownString writer name next = writeOwn writer name >> next ()
    integer writer number next = writeInteger writer number >> next ()
    whole writer value next = writeWhole writer value >> next ()
    emptyContainer writer next = writeEmptyArray writer >> next ()
    openObject writer next = openFrame writer >>= next
    beginMember writer key frame next = case key of
        SharedKey entry -> beginShared writer entry frame >>= next
        OwnKey _ -> beginOwn writer >> next False
    addMember writer key () frame next = case key of
        SharedKey entry -> endShared writer entry >> next frame
        OwnKey own -> endOwn writer (Key.toText own) >> next frame
    dropValue writer () next = dropMember writer >> next
    closeObject writer frame next = finishFrame writer frame >> next ()
    openArray writer next = openItems writer >>= next
    addItem _ () start next = next start
    closeArray writer count start next = finishItems writer count start >> next ()
    {-# INLINE sharedString #-}
    {-# INLINE ownString #-}
    {-# INLINE integer #-}
    {-# INLINE whole #-}
    {-# INLINE emptyContainer #-}
    {-# INLINE openObject #-}
    {-# INLINE beginMember #-}
    {-# INLINE addMember #-}
    {-# INLINE dropValue #-}
    {-# INLINE closeObject #-}
    {-# INLINE openArray #-}
    {-# INLINE addItem #-}
    {-# INLINE closeArray #-}

writeShared :: Writer st -> Entry -> ST st ()
writeShared writer entry = do
    register writer entry
    putByte (writerScratch writer) opShared
    putVarint (writerScratch writer) (entryIndex entry)
{-# OPAQUE writeShared #-}

writeOwn :: Writer st -> Name -> ST st ()
writeOwn writer = foldName (putOwnBytes (writerScratch writer)) (putOwnText (writerScratch writer))
{-# OPAQUE writeOwn #-}

writeInteger :: Writer st -> Int -> ST st ()
writeInteger writer number = do
    let scratch = writerScratch writer
    putByte scratch opInline
    putVarint scratch (decimalLength number)
    putDecimal scratch number
{-# OPAQUE writeInteger #-}

writeWhole :: Writer st -> Value -> ST st ()
writeWhole writer = putWhole (writerScratch writer)
{-# OPAQUE writeWhole #-}

writeEmptyArray :: Writer st -> ST st ()
writeEmptyArray writer = do
    putByte (writerScratch writer) opArray
    putByte (writerScratch writer) 0
{-# OPAQUE writeEmptyArray #-}

openFrame :: Writer st -> ST st Frame
openFrame writer = do
    start <- scratchCursor (writerScratch writer)
    base <- readPrimVar (writerTop writer)
    depth <- readPrimVar (writerDepth writer)
    writePrimVar (writerDepth writer) (depth + 1)
    pure (Frame start base)
{-# OPAQUE openFrame #-}

-- Reserve the member's record from where its value starts, and say whether the frame already holds the key.
beginShared :: Writer st -> Entry -> Frame -> ST st Bool
beginShared writer entry (Frame _ base) = do
    reserveMember writer
    at <- seenAt writer (entryIndex entry)
    pure $! at >= base
{-# OPAQUE beginShared #-}

beginOwn :: Writer st -> ST st ()
beginOwn = reserveMember
{-# OPAQUE beginOwn #-}

-- Complete the member's record under a table key, hiding any earlier record of the key.
endShared :: Writer st -> Entry -> ST st ()
endShared writer entry = do
    register writer entry
    top <- readPrimVar (writerTop writer)
    let !member = top - 1
    earlier <- seenAt writer (entryIndex entry)
    setSeen writer (entryIndex entry) member
    completeMember writer member (entryIndex entry) earlier (entryText entry)
{-# OPAQUE endShared #-}

endOwn :: Writer st -> Text -> ST st ()
endOwn writer text = do
    top <- readPrimVar (writerTop writer)
    completeMember writer (top - 1) (-1) (-1) text
{-# OPAQUE endOwn #-}

-- Forget the member begun last, and every byte of its value.
dropMember :: Writer st -> ST st ()
dropMember writer = do
    top <- readPrimVar (writerTop writer)
    spans <- readMutVar (writerSpans writer)
    readPrimArray spans (spanWidth * (top - 1) + 1) >>= rewindTo (writerScratch writer)
    writePrimVar (writerTop writer) (top - 1)
{-# OPAQUE dropMember #-}

finishFrame :: Writer st -> Frame -> ST st ()
finishFrame writer frame = do
    depth <- readPrimVar (writerDepth writer)
    writePrimVar (writerDepth writer) (depth - 1)
    when (depth == 1) (traverse_ (addTopMember writer frame) (writerAdded writer))
    closeFrame writer frame (depth == 1)
{-# OPAQUE finishFrame #-}

openItems :: Writer st -> ST st Int
openItems writer = do
    depth <- readPrimVar (writerDepth writer)
    writePrimVar (writerDepth writer) (depth + 1)
    scratchCursor (writerScratch writer)
{-# OPAQUE openItems #-}

finishItems :: Writer st -> Int -> Int -> ST st ()
finishItems writer count start = do
    depth <- readPrimVar (writerDepth writer)
    writePrimVar (writerDepth writer) (depth - 1)
    closeItems writer count start
{-# OPAQUE finishItems #-}

putOwnBytes :: Scratch st -> ByteString -> ST st ()
putOwnBytes scratch bytes = do
    putByte scratch opInline
    putVarint scratch (BS.length bytes + 2)
    putPlainBytes scratch bytes

putOwnText :: Scratch st -> Text -> ST st ()
putOwnText scratch text = putByte scratch opInline >> putEncodedText scratch id text

-- The tag of an object key written in full: its encoded length, doubled and odd. A table key's is even.
ownKey :: Int -> Int
ownKey len = 2 * len + 1

-- A value taken whole: aeson's bytes for a number, and object keys of their own.
putWhole :: Scratch st -> Value -> ST st ()
putWhole scratch = \case
    Null -> putByte scratch opNull
    Bool False -> putByte scratch opFalse
    Bool True -> putByte scratch opTrue
    String text -> putOwnText scratch text
    number@(Number _) -> do
        let bytes = LBS.toStrict (encodingToLazyByteString (toEncoding number))
        putByte scratch opInline
        putVarint scratch (BS.length bytes)
        putRawBytes scratch bytes
    Object fields -> do
        putByte scratch opObject
        putVarint scratch (KeyMap.size fields)
        forM_ (KeyMap.toAscList fields) $ \(key, value) -> do
            putEncodedText scratch ownKey (Key.toText key)
            putWhole scratch value
    Array values -> do
        putByte scratch opArray
        putVarint scratch (length values)
        traverse_ (putWhole scratch) values

-- Record the table string a blob is about to refer to, on the read's first reference to it.
register :: Writer st -> Entry -> ST st ()
register writer entry = do
    lengths <- readMutVar (writerLengths writer)
    size <- getSizeofMutablePrimArray lengths
    known <- if entryIndex entry < size then readPrimArray lengths (entryIndex entry) else pure 0
    when (known == 0) (claim writer entry)
{-# INLINE register #-}

{- Hold a table string and its encoded length under its index, with room for both. A slot no string
has claimed holds null, and a length of 0. -}
claim :: Writer st -> Entry -> ST st ()
claim writer entry = do
    let index = entryIndex entry
    strings <- readMutVar (writerStrings writer)
    lengths <- readMutVar (writerLengths writer)
    let held = sizeofMutableArray strings
    if index < held
        then record strings lengths
        else do
            let room = max (index + 1) (2 * held)
            grownStrings <- newArray room Null
            Array.copyMutableArray grownStrings 0 strings 0 held
            writeMutVar (writerStrings writer) grownStrings
            grownLengths <- newFilled room 0
            copyMutablePrimArray grownLengths 0 lengths 0 held
            writeMutVar (writerLengths writer) grownLengths
            record grownStrings grownLengths
  where
    record strings lengths = do
        writeArray strings (entryIndex entry) (entryString entry)
        writePrimArray lengths (entryIndex entry) (encodedLength (entryText entry))
{-# NOINLINE claim #-}

-- The member that last recorded the table key, or -1.
seenAt :: Writer st -> Int -> ST st Int
seenAt writer index = do
    seen <- readMutVar (writerSeen writer)
    size <- getSizeofMutablePrimArray seen
    if index < size then readPrimArray seen index else pure (-1)
{-# INLINE seenAt #-}

setSeen :: Writer st -> Int -> Int -> ST st ()
setSeen writer index value = do
    seen <- readMutVar (writerSeen writer)
    size <- getSizeofMutablePrimArray seen
    if index < size
        then writePrimArray seen index value
        else do
            grown <- newFilled (max (index + 1) (2 * size)) (-1)
            copyMutablePrimArray grown 0 seen 0 size
            writePrimArray grown index value
            writeMutVar (writerSeen writer) grown

-- Reserve a member record that starts at the cursor.
reserveMember :: Writer st -> ST st ()
reserveMember writer = do
    top <- readPrimVar (writerTop writer)
    spans <- readMutVar (writerSpans writer)
    size <- getSizeofMutablePrimArray spans
    spans' <-
        if spanWidth * (top + 1) <= size
            then pure spans
            else do
                grown <- newPrimArray (2 * size)
                copyMutablePrimArray grown 0 spans 0 size
                writeMutVar (writerSpans writer) grown
                pure grown
    keys <- readMutVar (writerKeys writer)
    when (top >= sizeofMutableArray keys) $ do
        grown <- newArray (2 * sizeofMutableArray keys) ""
        Array.copyMutableArray grown 0 keys 0 (sizeofMutableArray keys)
        writeMutVar (writerKeys writer) grown
    scratchCursor (writerScratch writer) >>= writePrimArray spans' (spanWidth * top + 1)
    writePrimVar (writerTop writer) (top + 1)

-- Complete a reserved record: its key's table index or -1, the record the key hides, and its key's text.
completeMember :: Writer st -> Int -> Int -> Int -> Text -> ST st ()
completeMember writer member index earlier text = do
    spans <- readMutVar (writerSpans writer)
    keys <- readMutVar (writerKeys writer)
    end <- scratchCursor (writerScratch writer)
    writePrimArray spans (spanWidth * member) index
    writePrimArray spans (spanWidth * member + 2) end
    writePrimArray spans (spanWidth * member + 3) earlier
    writeArray keys member text

-- The added member, written after the object's members, replaces any member under its key.
addTopMember :: Writer st -> Frame -> (Entry, Entry) -> ST st ()
addTopMember writer (Frame _ base) (key, value) = do
    earlier <- seenAt writer (entryIndex key)
    if earlier >= base
        then do
            let scratch = writerScratch writer
            start <- scratchCursor scratch
            writeShared writer value
            end <- scratchCursor scratch
            spans <- readMutVar (writerSpans writer)
            readPrimArray spans (spanWidth * earlier + 1) >>= writePrimArray (writerMarks writer) replacedStart
            readPrimArray spans (spanWidth * earlier + 2) >>= writePrimArray (writerMarks writer) replacedEnd
            writePrimArray spans (spanWidth * earlier + 1) start
            writePrimArray spans (spanWidth * earlier + 2) end
        else do
            writePrimArray (writerMarks writer) replacedStart (-1)
            reserveMember writer >> writeShared writer value >> endShared writer key

{- Copy the object's members behind its count in key order, the first member under each key kept,
and forget their records. A nested object moves to its start, and a top-level one stays past it. -}
closeFrame :: Writer st -> Frame -> Bool -> ST st ()
closeFrame writer (Frame start base) topLevel = do
    let scratch = writerScratch writer
    top <- readPrimVar (writerTop writer)
    let count = top - base
    keys <- readMutVar (writerKeys writer)
    order <- sortedSpans writer keys base count
    kept <- firstUnderEachKey keys order count
    spans <- readMutVar (writerSpans writer)
    output <- scratchCursor scratch
    putByte scratch opObject
    putVarint scratch kept
    copyMembers scratch keys spans order kept 0
    if topLevel
        then writePrimArray (writerMarks writer) valueStart output
        else do
            end <- scratchCursor scratch
            buffer <- scratchBuffer scratch
            moveByteArray buffer start buffer output (end - output)
            rewindTo scratch (start + end - output)
    restoreSeen writer spans base (top - 1)
    writePrimVar (writerTop writer) base

-- Write the kept members from the position on, each key before its value.
copyMembers :: Scratch st -> MutableArray st Text -> MutablePrimArray st Int -> MutablePrimArray st Int -> Int -> Int -> ST st ()
copyMembers scratch keys spans order kept !position = when (position < kept) $ do
    member <- readPrimArray order position
    index <- readPrimArray spans (spanWidth * member)
    from <- readPrimArray spans (spanWidth * member + 1)
    to <- readPrimArray spans (spanWidth * member + 2)
    if index >= 0
        then putVarint scratch (2 * index)
        else do
            readArray keys member >>= putEncodedText scratch ownKey
    putAt scratch (to - from) $ \buffer at -> copyMutableByteArray buffer at buffer from (to - from) $> at + (to - from)
    copyMembers scratch keys spans order kept (position + 1)

-- Point each table key an object recorded back at the record it hid, latest record first.
restoreSeen :: Writer st -> MutablePrimArray st Int -> Int -> Int -> ST st ()
restoreSeen writer spans base !member = when (member >= base) $ do
    index <- readPrimArray spans (spanWidth * member)
    when (index >= 0) (readPrimArray spans (spanWidth * member + 3) >>= setSeen writer index)
    restoreSeen writer spans base (member - 1)

-- The object's spans in key order, ties in source order, in the order workspace's first slots.
sortedSpans :: Writer st -> MutableArray st Text -> Int -> Int -> ST st (MutablePrimArray st Int)
sortedSpans writer keys base count = do
    order <- readMutVar (writerOrder writer)
    size <- getSizeofMutablePrimArray order
    work <-
        if 2 * count <= size
            then pure order
            else do
                grown <- newPrimArray (4 * count)
                writeMutVar (writerOrder writer) grown
                pure grown
    fillOrder work base count 0
    sortRuns keys work count 0
    final <- mergeRuns keys work count sortedRun 0
    when (final /= 0) (copyMutablePrimArray work 0 work count count)
    pure work

fillOrder :: MutablePrimArray st Int -> Int -> Int -> Int -> ST st ()
fillOrder work base count !position = when (position < count) $ do
    writePrimArray work position (base + position)
    fillOrder work base count (position + 1)

-- Whether the first member's key sorts after the second's.
sortsAfter :: MutableArray st Text -> Int -> Int -> ST st Bool
sortsAfter keys a b = do
    left <- readArray keys a
    right <- readArray keys b
    pure $! left > right
{-# INLINE sortsAfter #-}

-- The run each insertion sort orders before the runs merge.
sortedRun :: Int
sortedRun = 16

-- Sort each run of the first slots by insertion, from the run starting at the position on.
sortRuns :: MutableArray st Text -> MutablePrimArray st Int -> Int -> Int -> ST st ()
sortRuns keys work count !from = when (from < count) $ do
    insertRun keys work from (min count (from + sortedRun)) (from + 1)
    sortRuns keys work count (from + sortedRun)

insertRun :: MutableArray st Text -> MutablePrimArray st Int -> Int -> Int -> Int -> ST st ()
insertRun keys work from to !position = when (position < to) $ do
    item <- readPrimArray work position
    shiftInto keys work from item position
    insertRun keys work from to (position + 1)

-- Move later items up until the item's slot is found, and put it there.
shiftInto :: MutableArray st Text -> MutablePrimArray st Int -> Int -> Int -> Int -> ST st ()
shiftInto keys work from item !at
    | at <= from = writePrimArray work at item
    | otherwise = do
        previous <- readPrimArray work (at - 1)
        after <- sortsAfter keys previous item
        if after
            then writePrimArray work at previous >> shiftInto keys work from item (at - 1)
            else writePrimArray work at item

{- Merge runs of doubling width, alternating between the first half of the workspace and the second,
and return the half that holds the result. -}
mergeRuns :: MutableArray st Text -> MutablePrimArray st Int -> Int -> Int -> Int -> ST st Int
mergeRuns keys work count !width !source
    | width >= count = pure source
    | otherwise = do
        let target = if source == 0 then count else 0
        mergePairs keys work count width source target 0
        mergeRuns keys work count (2 * width) target

mergePairs :: MutableArray st Text -> MutablePrimArray st Int -> Int -> Int -> Int -> Int -> Int -> ST st ()
mergePairs keys work count width source target !from = when (from < count) $ do
    let middle = min count (from + width)
        end = min count (from + 2 * width)
    mergeTwo keys work (source + middle) (source + end) (source + from) (source + middle) (target + from)
    mergePairs keys work count width source target (from + 2 * width)

-- Merge the left run from i up to its end with the right run from j up to its end, into out on.
mergeTwo :: MutableArray st Text -> MutablePrimArray st Int -> Int -> Int -> Int -> Int -> Int -> ST st ()
mergeTwo keys work !leftEnd !rightEnd !i !j !out
    | i >= leftEnd = copyRange j rightEnd
    | j >= rightEnd = copyRange i leftEnd
    | otherwise = do
        a <- readPrimArray work i
        b <- readPrimArray work j
        after <- sortsAfter keys a b
        if after
            then writePrimArray work out b >> mergeTwo keys work leftEnd rightEnd i (j + 1) (out + 1)
            else writePrimArray work out a >> mergeTwo keys work leftEnd rightEnd (i + 1) j (out + 1)
  where
    copyRange from to = when (to > from) (copyMutablePrimArray work out work from (to - from))

-- Keep the first member under each key at the front of the order, and count them.
firstUnderEachKey :: MutableArray st Text -> MutablePrimArray st Int -> Int -> ST st Int
firstUnderEachKey keys order count
    | count <= 0 = pure 0
    | otherwise = do
        leading <- readPrimArray order 0
        firstKey <- readArray keys leading
        keepFirsts keys order count 1 1 firstKey

keepFirsts :: MutableArray st Text -> MutablePrimArray st Int -> Int -> Int -> Int -> Text -> ST st Int
keepFirsts keys order count !position !kept lastKey
    | position >= count = pure kept
    | otherwise = do
        member <- readPrimArray order position
        key <- readArray keys member
        if key == lastKey
            then keepFirsts keys order count (position + 1) kept lastKey
            else writePrimArray order kept member >> keepFirsts keys order count (position + 1) (kept + 1) key

-- Put the array's count in front of its items.
closeItems :: Writer st -> Int -> Int -> ST st ()
closeItems writer count start = do
    let scratch = writerScratch writer
        header = 1 + varintSize count
    end <- scratchCursor scratch
    reserve scratch header $ \buffer _ -> do
        moveByteArray buffer (start + header) buffer start (end - start)
        writeByteArray buffer start opArray
        void (writeVarint count buffer (start + 1))
    rewindTo scratch (end + header)

{- | Copy the value the writer holds into a packed value, and empty the scratch for the next one. Its
hole is the string at the path of member names, when the value holds one there.
-}
sealValue :: Writer st -> [Text] -> ST st Packed
sealValue writer path = do
    let scratch = writerScratch writer
        marks = writerMarks writer
    start <- readPrimArray marks valueStart
    end <- scratchCursor scratch
    blob <- copyOut scratch start end
    from <- readPrimArray marks replacedStart
    to <- readPrimArray marks replacedEnd
    replaced <- if from >= 0 then Just <$> copyOut scratch from to else pure Nothing
    discard writer
    writeMutVar (writerReplaced writer) replaced
    hole <- holeAt writer blob path 0
    -- The lengths are read here, before a later claim writes the array again.
    lengths <- readMutVar (writerLengths writer) >>= unsafeFreezePrimArray
    pure $! case encodedFrom lengths blob 0 of Measured encoded _ -> packed blob hole encoded

-- The bytes a render writes for a value of a sealed blob, and the position after the value. The two
-- return in registers as strict fields, where a pair would allocate for every value.
data Measured = Measured !Int !Int

-- Measure the value at a position, from the recorded length of each table string it names.
encodedFrom :: PrimArray Int -> ByteArray -> Int -> Measured
encodedFrom lengths blob position = case indexByteArray blob position :: Word8 of
    code
        | code == opNull -> Measured 4 (position + 1)
        | code == opFalse -> Measured 5 (position + 1)
        | code == opTrue -> Measured 4 (position + 1)
        | code == opShared -> case readVarint blob (position + 1) of
            (# index, next #) -> Measured (indexPrimArray lengths index) next
        | code == opObject -> case readVarint blob (position + 1) of
            (# count, next #) -> membersFrom lengths blob count next (separators count)
        | code == opArray -> case readVarint blob (position + 1) of
            (# count, next #) -> itemsFrom lengths blob count next (separators count)
        | otherwise -> case readVarint blob (position + 1) of (# len, next #) -> Measured len (next + len)

-- Add the given number of members from a position on to a total: each key with its colon, then its value.
membersFrom :: PrimArray Int -> ByteArray -> Int -> Int -> Int -> Measured
membersFrom lengths blob !count !at !total
    | count <= 0 = Measured total at
    | otherwise = case readVarint blob at of
        (# tagged, next #)
            | even tagged -> memberValue lengths blob count next (total + indexPrimArray lengths (tagged `div` 2) + 1)
            | otherwise -> memberValue lengths blob count (next + tagged `div` 2) (total + tagged `div` 2 + 1)

-- A member's value after its key, then the members left.
memberValue :: PrimArray Int -> ByteArray -> Int -> Int -> Int -> Measured
memberValue lengths blob !count !at !total = case encodedFrom lengths blob at of
    Measured len after -> membersFrom lengths blob (count - 1) after (total + len)

itemsFrom :: PrimArray Int -> ByteArray -> Int -> Int -> Int -> Measured
itemsFrom lengths blob !count !at !total
    | count <= 0 = Measured total at
    | otherwise = case encodedFrom lengths blob at of
        Measured len after -> itemsFrom lengths blob (count - 1) after (total + len)

-- | Forget the value the writer holds, and the member its top-level object replaced.
discard :: Writer st -> ST st ()
discard writer = do
    rewindTo (writerScratch writer) 0
    writePrimArray (writerMarks writer) valueStart 0
    writePrimArray (writerMarks writer) replacedStart (-1)
    writeMutVar (writerReplaced writer) Nothing

{- | The member the added member replaced in the value sealed last, as read, or nothing when that
value held none or the writer discarded a value since.
-}
replacedMember :: Writer st -> ST st (Maybe Value)
replacedMember writer = readMutVar (writerReplaced writer) >>= traverse (\blob -> newPrimVar 0 >>= decodeWith writer blob)

-- Where the string at the path of member names starts, or -1.
holeAt :: Writer st -> ByteArray -> [Text] -> Int -> ST st Int
holeAt writer blob path position = case path of
    []
        | code == opShared -> pure position
        | code == opInline, stringAt -> pure position
        | otherwise -> pure (-1)
    name : rest
        | code /= opObject -> pure (-1)
        | otherwise -> case readVarint blob (position + 1) of
            (# count, next #) -> search count next
      where
        search !remaining !at
            | remaining <= 0 = pure (-1)
            | otherwise = keyAt writer blob at $ \key valueAt ->
                if key == name then holeAt writer blob rest valueAt else search (remaining - 1) (valueEnd blob valueAt)
  where
    code = indexByteArray blob position :: Word8
    stringAt = case readVarint blob (position + 1) of (# _, next #) -> (indexByteArray blob next :: Word8) == 0x22

-- A member's key text and where its value starts.
keyAt :: Writer st -> ByteArray -> Int -> (Text -> Int -> ST st a) -> ST st a
keyAt writer blob at next = case readVarint blob at of
    (# tagged, after #)
        | even tagged -> do
            strings <- readMutVar (writerStrings writer)
            string <- readArray strings (tagged `div` 2)
            next (stringText string) after
        | otherwise -> case decodeScalar blob after (tagged `div` 2) of
            String text -> next text (after + tagged `div` 2)
            _ -> next "" (after + tagged `div` 2)
{-# INLINE keyAt #-}

-- | Which parts of a packed value a decode needs: all of it, or the named members of an object.
data Pick = Whole | Only [(Text, Pick)]

{- | The parts of a packed value the pick names, as aeson's tree sharing the read's strings. A value
that is not an object where the pick names members decodes whole.
-}
decodePicked :: Writer st -> Pick -> Packed -> ST st Value
decodePicked writer pick value = newPrimVar 0 >>= decodePart writer pick (packedBlob value)

decodePart :: Writer st -> Pick -> ByteArray -> PrimVar st Int -> ST st Value
decodePart writer pick blob at = case pick of
    Whole -> decodeWith writer blob at
    Only picks -> do
        position <- readPrimVar at
        if (indexByteArray blob position :: Word8) /= opObject
            then decodeWith writer blob at
            else case readVarint blob (position + 1) of
                (# count, members #) -> do
                    wanted <- countPicked writer picks blob count members 0
                    writePrimVar at members
                    picked <- buildPicked writer picks blob at wanted
                    writePrimVar at (valueEnd blob position)
                    pure $! Object (KeyMap.fromMap picked)

-- How many of the object's members, from the position on, the pick names.
countPicked :: Writer st -> [(Text, Pick)] -> ByteArray -> Int -> Int -> Int -> ST st Int
countPicked writer picks blob !remaining !position !found
    | remaining <= 0 = pure found
    | otherwise = keyAt writer blob position $ \text valueAt ->
        countPicked writer picks blob (remaining - 1) (valueEnd blob valueAt) (if isJust (pickFor text picks) then found + 1 else found)

-- The next picked members, the given number of them, as a balanced map built in key order.
buildPicked :: Writer st -> [(Text, Pick)] -> ByteArray -> PrimVar st Int -> Int -> ST st (Map.Map Key.Key Value)
buildPicked writer picks blob at !count
    | count <= 0 = pure Tip
    | otherwise = do
        let before = (count - 1) `div` 2
        left <- buildPicked writer picks blob at before
        position <- readPrimVar at
        member <- nextPicked writer picks blob at position
        right <- buildPicked writer picks blob at (count - 1 - before)
        pure $! case member of (name, value) -> Bin count name value left right

pickFor :: Text -> [(Text, Pick)] -> Maybe Pick
pickFor text = fmap snd . find ((== text) . fst)

-- The next member the pick names, skipping the others.
nextPicked :: Writer st -> [(Text, Pick)] -> ByteArray -> PrimVar st Int -> Int -> ST st (Key.Key, Value)
nextPicked writer picks blob at !position = keyAt writer blob position $ \text valueAt -> case pickFor text picks of
    Just pick -> do
        writePrimVar at valueAt
        member <- decodePart writer pick blob at
        pure (Key.fromText text, member)
    Nothing -> nextPicked writer picks blob at (valueEnd blob valueAt)

-- | A packed value decoded whole, sharing the read's strings.
decodeWhole :: Writer st -> Packed -> ST st Value
decodeWhole writer value = newPrimVar 0 >>= decodeWith writer (packedBlob value)

-- A decode reads the read's own entries, so decoded strings share the typed view's texts.
instance TableStrings Writer where
    tableValue writer index = do
        strings <- readMutVar (writerStrings writer)
        readArray strings index
    tableKey writer index = do
        strings <- readMutVar (writerStrings writer)
        string <- readArray strings index
        pure $! Key.fromText (stringText string)

-- A registered string's text. Every slot a blob refers to holds a string.
stringText :: Value -> Text
stringText = \case
    String text -> text
    _ -> ""
