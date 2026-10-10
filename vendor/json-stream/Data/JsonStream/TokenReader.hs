{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

-- | A cursor over the C lexer's result records.
--
-- 'Data.JsonStream.CLexer.tokenParser' returns a lazy list with one cell per token, and each
-- cell holds an element with up to two slices of the input. This module reads the same tokens,
-- in the same order, from the lexer's result array. A token allocates only the payload its
-- consumer uses, so a consumer that looks at a token's constructor and moves on allocates nothing.
--
-- The lexer runs over the same input pieces, from the same state, with the same result limits as
-- under 'Data.JsonStream.CLexer.tokenParser'. Both therefore produce the same tokens, wait for
-- input at the same places and fail at the same places.
module Data.JsonStream.TokenReader (
    Tokens
  , tokenReader
  , reusingTokenReader
  , Next (..)
  , Element (..)
  , nextToken
) where

import qualified Data.Aeson                as AE
import qualified Data.ByteString           as BS
import           Data.ByteString.Unsafe    (unsafeUseAsCString)
import           Data.Scientific           (scientific)
import           Data.Text.Internal.Unsafe (inlinePerformIO)
import           Foreign
import           Foreign.C.Types
import           GHC.ForeignPtr            (unsafeWithForeignPtr)
import           System.IO.Unsafe          (unsafeDupablePerformIO)

import           Data.JsonStream.CLexType
import           Data.JsonStream.CLexer    (Header (..), defHeader, estResultLimit, lexJson,
                                            numberDigitLimit, parseNumber, resultRecSize, substr)

-- | One token. 'StringRaw' is a whole string as it stands in the input, with whether it is ASCII
-- without escapes. A string that crosses input pieces arrives as 'StringContent' parts and one
-- 'StringEnd'.
data Element =
    ArrayBegin
  | ArrayEnd
  | ObjectBegin
  | ObjectEnd
  | StringContent !BS.ByteString
  | StringRaw !BS.ByteString !Bool
  | StringEnd
  | JValue !AE.Value
  | JInteger !CLong
  deriving (Show, Eq)

-- | What a cursor finds: a token with the cursor after it, the end of the input piece, or a
-- lexer failure.
data Next =
    PartialResult !Element !Tokens
  | TokMoreData (BS.ByteString -> Tokens)
  | TokFailed

-- | A position in the token sequence of one input.
data Tokens = Tokens !Batch {-# UNPACK #-} !Int

-- The results of one lexer call. A position counts half records: the record that ends a string
-- split across pieces yields the string's last part at its even position and the string's end at
-- its odd one.
data Batch = Batch {
    batchResults :: {-# UNPACK #-} !(ForeignPtr ())
  , batchEnd     :: {-# UNPACK #-} !Int -- First position past the results
  , batchChunk   :: !BS.ByteString
  , batchHeader  :: !Header -- Lexer state after the call
  , batchError   :: !Bool
  , batchNumbers :: [BS.ByteString] -- Parts of a number split across pieces, last part first
  , batchSlots   :: {-# UNPACK #-} !Int -- Records the result array holds
  , batchReuse   :: !Bool
}

-- | The start of an input. Every lexer call writes a result array of its own, so a cursor stays
-- valid for as long as it is held.
tokenReader :: Tokens
tokenReader = start False

-- | The start of an input whose lexer calls share one result array. A cursor is valid until
-- 'nextToken' has run on a cursor after it: use each cursor once, in order.
reusingTokenReader :: Tokens
reusingTokenReader = start True

start :: Bool -> Tokens
start reuse = Tokens Batch {
    batchResults = noResults
  , batchEnd = 0
  , batchChunk = BS.empty
  , batchHeader = defHeader
  , batchError = False
  , batchNumbers = []
  , batchSlots = 0
  , batchReuse = reuse
  } 0

noResults :: ForeignPtr ()
noResults = unsafeDupablePerformIO (newForeignPtr_ nullPtr)
{-# NOINLINE noResults #-}

-- | The token at the cursor.
nextToken :: Tokens -> Next
nextToken (Tokens batch pos)
  | pos < batchEnd batch = tokenAt batch pos
  | otherwise = afterBatch batch (batchNumbers batch)
{-# INLINE nextToken #-}

-- Every field read is strict, so no deferred read can see a reused result array.
tokenAt :: Batch -> Int -> Next
tokenAt batch pos
  | kind == resString =
      let !added = addedAt batch pos
      in if | added == -1 || added == 0 -> PartialResult (StringRaw (textAt batch pos) (added == -1)) next
            | pos .&. 1 == 0 -> PartialResult (StringContent (textAt batch pos)) (Tokens batch (pos + 1))
            | otherwise -> PartialResult StringEnd (Tokens batch (pos + 1))
  | kind == resOpenBrace = PartialResult ObjectBegin next
  | kind == resCloseBrace = PartialResult ObjectEnd next
  | kind == resOpenBracket = PartialResult ArrayBegin next
  | kind == resCloseBracket = PartialResult ArrayEnd next
  | kind == resNumberSmall =
      let !added = addedAt batch pos
          !digits = lengthAt batch pos
      in if | digits == 0 -> PartialResult (JInteger added) next
            | otherwise -> PartialResult (JValue (AE.Number (scientific (fromIntegral added) ((-1) * digits)))) next
  | kind == resTrue = PartialResult trueElement next
  | kind == resFalse = PartialResult falseElement next
  | kind == resNull = PartialResult nullElement next
  | kind == resStringPartial = PartialResult (StringContent (textAt batch pos)) next
  | kind == resNumber =
      let !added = addedAt batch pos
          !text = textAt batch pos
          whole | added == 0 = text -- Single one-part number
                | otherwise = BS.concat (reverse (text : batchNumbers batch))
      in case parseNumber whole of
           Just num -> PartialResult (JValue (AE.Number num)) next
           Nothing -> TokFailed
  | kind == resNumberPartial =
      let !added = addedAt batch pos
          !text = textAt batch pos
      in if | added == 0 -> afterBatch batch [text] -- First part of number
            | sum (map BS.length (batchNumbers batch)) > numberDigitLimit -> TokFailed -- Number too long
            | otherwise -> afterBatch batch (text : batchNumbers batch) -- Middle part of number
  | otherwise = error "Unsupported"
  where
    !kind = kindAt batch pos
    next = Tokens batch (pos + 2)
{-# INLINE tokenAt #-}

-- One heap object per literal, wherever 'nextToken' is inlined: a consumer that keeps the value
-- of every literal it reads holds each only once.
trueElement, falseElement, nullElement :: Element
trueElement = JValue (AE.Bool True)
falseElement = JValue (AE.Bool False)
nullElement = JValue AE.Null
{-# NOINLINE trueElement #-}
{-# NOINLINE falseElement #-}
{-# NOINLINE nullElement #-}

-- What follows the last result of a lexer call: failure after a lexer error, another call while
-- the piece has input left, or a wait for the next piece.
afterBatch :: Batch -> [BS.ByteString] -> Next
afterBatch batch numbers
  | batchError batch = TokFailed
  | hdrPosition header < hdrLength header = first (lexPiece batch numbers (batchChunk batch) header)
  | otherwise = TokMoreData more
  where
    header = batchHeader batch
    more piece = Tokens (lexPiece batch numbers piece newHeader) 0
      where
        newHeader = header {
            hdrPosition = 0
          , hdrLength = fromIntegral (BS.length piece)
          , hdrResultLimit = estResultLimit piece
          }
    first lexed
      | batchEnd lexed > 0 = tokenAt lexed 0
      | otherwise = afterBatch lexed (batchNumbers lexed)
{-# NOINLINE afterBatch #-}

-- Call the C lexer on a piece from the given state. A reusing reader writes into the result
-- array it holds when the array has room for the call's result limit.
lexPiece :: Batch -> [BS.ByteString] -> BS.ByteString -> Header -> Batch
lexPiece prior numbers piece header = unsafeDupablePerformIO $ -- At worst the call runs twice, with the same results
  alloca $ \hdrptr -> do
    poke hdrptr (header {hdrResultNum = 0, hdrLength = fromIntegral (BS.length piece)})
    let limit = fromIntegral (hdrResultLimit header)
        held = batchReuse prior && limit <= batchSlots prior
    results <- if held then return (batchResults prior)
                       else mallocForeignPtrBytes (limit * resultRecSize)
    code <- unsafeUseAsCString piece $ \input ->
      withForeignPtr results $ \records ->
        lexJson input hdrptr records
    after <- peek hdrptr
    return Batch {
        batchResults = results
      , batchEnd = 2 * fromIntegral (hdrResultNum after)
      , batchChunk = piece
      , batchHeader = after
      , batchError = code /= 0
      , batchNumbers = numbers
      , batchSlots = if held then batchSlots prior else limit
      , batchReuse = batchReuse prior
      }

-- Read one field of the result record at a position (see lexer.h)
peekField :: Storable a => Batch -> Int -> Int -> a
peekField batch pos offset = inlinePerformIO $ -- The array is read before any later lexer call writes it
  unsafeWithForeignPtr (batchResults batch) $ \results ->
    peekByteOff results (resultRecSize * (pos `unsafeShiftR` 1) + offset)
{-# INLINE peekField #-}

kindAt :: Batch -> Int -> LexResultType
kindAt batch pos = peekField batch pos 0
{-# INLINE kindAt #-}

lengthAt :: Batch -> Int -> Int
lengthAt batch pos = fromIntegral (peekField batch pos (2 * sizeOf (undefined :: CInt)) :: CInt)
{-# INLINE lengthAt #-}

addedAt :: Batch -> Int -> CLong
addedAt batch pos = peekField batch pos (4 * sizeOf (undefined :: CInt))
{-# INLINE addedAt #-}

-- The input the record points at
textAt :: Batch -> Int -> BS.ByteString
textAt batch pos = substr begin (lengthAt batch pos) (batchChunk batch)
  where
    begin = fromIntegral (peekField batch pos (sizeOf (undefined :: CInt)) :: CInt)
{-# INLINE textAt #-}
