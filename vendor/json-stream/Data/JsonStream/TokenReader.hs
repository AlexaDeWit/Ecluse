{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

-- | An effectful cursor over the C lexer's result records. Each reader owns its buffer.
-- Token payloads refer only to immutable input bytes and survive subsequent reads.
module Data.JsonStream.TokenReader (
    Tokens, newTokenReader, nextToken, supplyTokens, maxChunkBytes,
    Element (..), Next (..)
) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Scientific (scientific)
import Data.STRef
import Foreign
import Foreign.C.Types

import Data.JsonStream.CLexType
import Data.JsonStream.CLexer (Header (..), ResultRecord (..), defHeader, estResultLimit,
    lexJson, numberDigitLimit, parseNumber, readResult, resultLimitFor, resultRecSize, substr)

-- | The largest input piece accepted by the reader and its body driver.
maxChunkBytes :: Int
maxChunkBytes = 32768

maximumResultSlots, bufferGrowthFactor, positionsPerRecord, stringEndPosition :: Int
maximumResultSlots = fromIntegral (resultLimitFor maxChunkBytes)
bufferGrowthFactor = 2
positionsPerRecord = 2
stringEndPosition = 1

asciiStringFlag, escapedStringFlag, initialNumberPart :: CLong
asciiStringFlag = -1
escapedStringFlag = 0
initialNumberPart = 0

-- | A token's payload, without a pointer into the reusable result buffer.
data Element
    = ArrayBegin | ArrayEnd | ObjectBegin | ObjectEnd
    | StringContent !BS.ByteString | StringRaw !BS.ByteString !Bool | StringEnd
    | JValue !AE.Value | JInteger !CLong
    deriving (Eq, Show)

-- | The next token, a request for another input piece, or a lexer failure.
data Next = PartialResult !Element | TokMoreData | TokFailed
    deriving (Eq, Show)

-- | A reader's current position. Aliases advance the same reader, never an older batch.
data Tokens st = Tokens !(STRef st Batch) !(STRef st Int)

data Batch = Batch
    { batchResults :: !(ForeignPtr ())
    , batchEnd :: !Int
    , batchChunk :: !BS.ByteString
    , batchHeader :: !Header
    , batchError :: !Bool
    , batchNumbers :: [BS.ByteString]
    , batchSlots :: !Int
    }

-- | Allocate a reader with no input. Its result buffer grows only up to a 32 KiB input piece.
newTokenReader :: ST st (Tokens st)
newTokenReader = do
    results <- unsafeIOToST (newForeignPtr_ nullPtr)
    batch <- newSTRef (Batch results 0 BS.empty defHeader False [] 0)
    Tokens batch <$> newSTRef 0

-- | Supply input after 'TokMoreData'. Oversized or premature input makes the reader fail.
supplyTokens :: Tokens st -> BS.ByteString -> ST st ()
supplyTokens reader@(Tokens state position) bytes = do
    batch <- readSTRef state
    offset <- readSTRef position
    let header = batchHeader batch
    if batchError batch || offset < batchEnd batch || hdrPosition header < hdrLength header || BS.length bytes > maxChunkBytes
        then writeSTRef state batch{batchError = True, batchEnd = 0}
        else fill reader batch (batchNumbers batch) bytes header
            { hdrPosition = 0, hdrLength = fromIntegral (BS.length bytes), hdrResultLimit = estResultLimit bytes }

-- | Advance once. All result-record fields are read before the buffer can be reused.
nextToken :: Tokens st -> ST st Next
nextToken reader@(Tokens state position) = do
    batch <- readSTRef state
    offset <- readSTRef position
    if offset < batchEnd batch
        then do
            record <- unsafeIOToST (readResult (offset `quot` positionsPerRecord) (batchResults batch))
            tokenAt reader batch offset record
        else afterBatch reader batch (batchNumbers batch)
{-# INLINE nextToken #-}

tokenAt :: Tokens st -> Batch -> Int -> ResultRecord -> ST st Next
tokenAt reader@(Tokens _ position) batch offset (ResultRecord kind start len added)
    | kind == resString =
        if | added == asciiStringFlag || added == escapedStringFlag -> yield positionsPerRecord (StringRaw text (added == asciiStringFlag))
           | offset `rem` positionsPerRecord == 0 -> yield stringEndPosition (StringContent text)
           | otherwise -> yield stringEndPosition StringEnd
    | kind == resOpenBrace = yield positionsPerRecord ObjectBegin
    | kind == resCloseBrace = yield positionsPerRecord ObjectEnd
    | kind == resOpenBracket = yield positionsPerRecord ArrayBegin
    | kind == resCloseBracket = yield positionsPerRecord ArrayEnd
    | kind == resNumberSmall =
        if len == 0 then yield positionsPerRecord (JInteger added)
        else yield positionsPerRecord (JValue (AE.Number (scientific (fromIntegral added) (negate len))))
    | kind == resTrue = yield positionsPerRecord trueElement
    | kind == resFalse = yield positionsPerRecord falseElement
    | kind == resNull = yield positionsPerRecord nullElement
    | kind == resStringPartial = yield positionsPerRecord (StringContent text)
    | kind == resNumber =
        let whole = if added == initialNumberPart then text else BS.concat (reverse (text : batchNumbers batch))
        in maybe (failReader reader) (yield positionsPerRecord . JValue . AE.Number) (parseNumber whole)
    | kind == resNumberPartial =
        if | added == initialNumberPart -> afterBatch reader batch [text]
           | sum (map BS.length (batchNumbers batch)) > numberDigitLimit -> failReader reader
           | otherwise -> afterBatch reader batch (text : batchNumbers batch)
    | otherwise = failReader reader
  where
    text = substr start len (batchChunk batch)
    yield count !element = writeSTRef position (offset + count) >> pure (PartialResult element)
{-# INLINE tokenAt #-}

-- These values stay shared when the consumer disables full laziness.
trueElement, falseElement, nullElement :: Element
trueElement = JValue (AE.Bool True)
falseElement = JValue (AE.Bool False)
nullElement = JValue AE.Null
{-# NOINLINE trueElement #-}
{-# NOINLINE falseElement #-}
{-# NOINLINE nullElement #-}

failReader :: Tokens st -> ST st Next
failReader (Tokens state _) = do
    modifySTRef' state (\batch -> batch{batchError = True, batchEnd = 0})
    pure TokFailed

afterBatch :: Tokens st -> Batch -> [BS.ByteString] -> ST st Next
afterBatch reader@(Tokens state position) batch numbers
    | batchError batch = failReader reader
    | hdrPosition header < hdrLength header = do
        fill reader batch numbers (batchChunk batch) header
        nextToken reader
    | otherwise = do
        writeSTRef state batch{batchNumbers = numbers}
        writeSTRef position (batchEnd batch)
        pure TokMoreData
  where
    header = batchHeader batch

fill :: Tokens st -> Batch -> [BS.ByteString] -> BS.ByteString -> Header -> ST st ()
fill (Tokens state position) prior numbers bytes header = do
    batch <- unsafeIOToST $ alloca $ \headerPtr -> do
        let limit = fromIntegral (hdrResultLimit header)
            capacity = if limit <= batchSlots prior then batchSlots prior
                else min maximumResultSlots (max limit (bufferGrowthFactor * batchSlots prior))
        results <- if limit <= batchSlots prior
            then pure (batchResults prior)
            else mallocForeignPtrBytes (capacity * resultRecSize)
        poke headerPtr header{hdrResultNum = 0}
        code <- unsafeUseAsCString bytes $ \input ->
            withForeignPtr results $ \output -> lexJson input headerPtr output
        after <- peek headerPtr
        pure (Batch results (positionsPerRecord * fromIntegral (hdrResultNum after)) bytes after (code /= 0) numbers capacity)
    writeSTRef state batch
    writeSTRef position 0
