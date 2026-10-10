-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

#ifdef FOLD_NO_FULL_LAZINESS
{-# OPTIONS_GHC -fno-full-laziness #-}
#endif

module Main (main) where

import Control.DeepSeq (force, rnf)
import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (allocated_bytes, max_live_bytes), getRTSStats, getRTSStatsEnabled)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Read (readMaybe)

#if defined(PURE_CURSOR) && defined(OWNED_READER)
#error Select only one token consumption API
#endif

#ifdef PURE_CURSOR
import qualified Data.JsonStream.Lexer.Internal as Tokens
#elif defined(OWNED_READER)
import Control.Monad.ST (runST)
import qualified Data.JsonStream.TokenReader as Tokens
#else
import Data.JsonStream.CLexer (tokenParser)
import qualified Data.JsonStream.TokenParser as Tokens
#endif

data Totals = Totals !Word64 !Word64 !Word64
    deriving (Eq, Show)

startTotals :: Totals
startTotals = Totals 0 0 digestInitial

digestInitial, digestMultiplier :: Word64
digestInitial = 14695981039346656037
digestMultiplier = 1099511628211

arrayBeginTag, objectBeginTag, arrayEndTag, objectEndTag, stringEndTag :: Word64
arrayBeginTag = 1
objectBeginTag = 2
arrayEndTag = 3
objectEndTag = 4
stringEndTag = 5

asciiStringTag, encodedStringTag, stringContentTag, integerTag, valueTag :: Word64
asciiStringTag = 6
encodedStringTag = 7
stringContentTag = 8
integerTag = 9
valueTag = 10

readerMaxChunkBytes :: Int
readerMaxChunkBytes = 32768

mix :: Word64 -> Word64 -> Word64
mix previous value = (previous * digestMultiplier) + value

record :: Bool -> Totals -> Tokens.Element -> Totals
record verify (Totals count bytes checksum) element =
    let (tag, size, value) = fields element
     in Totals (count + 1) (bytes + size) (mix (mix checksum tag) value)
  where
    stringFields tag text =
        let size = fromIntegral (BS.length text)
            value = if verify then BS.foldl' (\acc byte -> mix acc (fromIntegral byte)) 0 text else size
         in (tag, size, value)
    fields token = case token of
        Tokens.ArrayBegin -> (arrayBeginTag, 0, 0)
        Tokens.ObjectBegin -> (objectBeginTag, 0, 0)
#if defined(OWNED_READER) || defined(PURE_CURSOR)
        Tokens.ArrayEnd -> (arrayEndTag, 0, 0)
        Tokens.ObjectEnd -> (objectEndTag, 0, 0)
        Tokens.StringEnd -> (stringEndTag, 0, 0)
        Tokens.StringRaw text ascii -> stringFields (if ascii then asciiStringTag else encodedStringTag) text
#else
        Tokens.ArrayEnd _ -> (arrayEndTag, 0, 0)
        Tokens.ObjectEnd _ -> (objectEndTag, 0, 0)
        Tokens.StringEnd _ -> (stringEndTag, 0, 0)
        Tokens.StringRaw text ascii _ -> stringFields (if ascii then asciiStringTag else encodedStringTag) text
#endif
        Tokens.StringContent text -> stringFields stringContentTag text
        Tokens.JInteger number -> (integerTag, 0, fromIntegral number)
        Tokens.JValue value -> rnf value `seq` (valueTag, 0, if verify then valueHash value else 0)

valueHash :: Aeson.Value -> Word64
valueHash = foldl' (\acc char -> mix acc (fromIntegral (ord char))) 0 . show

foldTokens :: Bool -> Int -> BS.ByteString -> Maybe Totals
#ifdef PURE_CURSOR
foldTokens verify pieceSize input = loop startTotals input False (Tokens.start BS.empty)
  where
    loop !totals rest ended cursor = case Tokens.next cursor of
        Tokens.Token element after -> loop (record verify totals element) rest ended after
        Tokens.Failed -> Nothing
        Tokens.More waiting
            | ended -> Just totals
            | BS.null rest -> loop totals rest True (Tokens.feed waiting BS.empty)
            | otherwise ->
                let (piece, remaining) = BS.splitAt pieceSize rest
                 in loop totals remaining False (Tokens.feed waiting piece)
#elif defined(OWNED_READER)
foldTokens verify pieceSize input = runST $ do
    reader <- Tokens.newTokenReader
    let loop !totals rest ended = do
            next <- Tokens.nextToken reader
            case next of
                Tokens.PartialResult element -> loop (record verify totals element) rest ended
                Tokens.TokFailed -> pure Nothing
                Tokens.TokMoreData
                    | ended -> pure (Just totals)
                    | BS.null rest -> Tokens.supplyTokens reader BS.empty >> loop totals rest True
                    | otherwise -> do
                        let (piece, remaining) = BS.splitAt pieceSize rest
                        Tokens.supplyTokens reader piece
                        loop totals remaining False
    loop startTotals input False
#else
foldTokens verify pieceSize input = loop startTotals input False (tokenParser BS.empty)
  where
    loop !totals rest ended next = case next of
        Tokens.PartialResult element more -> loop (record verify totals element) rest ended more
        Tokens.TokFailed -> Nothing
        Tokens.TokMoreData refill
            | ended -> Just totals
            | BS.null rest -> loop totals rest True (refill BS.empty)
            | otherwise ->
                let (piece, remaining) = BS.splitAt pieceSize rest
                 in loop totals remaining False (refill piece)
#endif
{-# NOINLINE foldTokens #-}

copyInput :: BS.ByteString -> IO BS.ByteString
copyInput source = BS.useAsCStringLen source BS.packCStringLen

runPasses :: Int -> Int -> BS.ByteString -> IO Word64
runPasses count pieceSize source = loop count 0
  where
    loop remaining !checksum
        | remaining <= 0 = pure checksum
        | otherwise = do
            input <- copyInput source
            result <- evaluate (foldTokens False pieceSize input)
            case result of
                Nothing -> die "token fold failed"
                Just (Totals tokens _ value) -> loop (remaining - 1) (checksum + tokens + value)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["verify", sizeText, path] | Just size <- readMaybe sizeText, size > 0, size <= readerMaxChunkBytes -> do
            input <- BS.readFile path
            print (foldTokens True size input)
        ["measure", countText, sizeText, path]
            | Just count <- readMaybe countText, count > 0
            , Just size <- readMaybe sizeText, size > 0, size <= readerMaxChunkBytes -> do
                enabled <- getRTSStatsEnabled
                unless enabled (die "RTS statistics require +RTS -T")
                input <- BS.readFile path >>= evaluate . force
                _ <- runPasses 1 size input
                performGC
                before <- getRTSStats
                started <- getMonotonicTimeNSec
                checksum <- runPasses count size input
                ended <- getMonotonicTimeNSec
                performGC
                after <- getRTSStats
                putStrLn (show count ++ "," ++ show size ++ "," ++ show (ended - started) ++ ","
                    ++ show (allocated_bytes after - allocated_bytes before) ++ ","
                    ++ show (max_live_bytes after) ++ "," ++ show checksum)
        _ -> die "Usage: token-fold (verify SIZE FILE | measure COUNT SIZE FILE)"
