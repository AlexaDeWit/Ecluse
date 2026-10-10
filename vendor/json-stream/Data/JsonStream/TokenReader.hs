{-# LANGUAGE BangPatterns #-}

-- | An owned Haskell token cursor over immutable input pieces.
module Data.JsonStream.TokenReader (
    Tokens, newTokenReader, nextToken, supplyTokens, maxChunkBytes,
    Element (..), Next (..)
) where

import Control.Monad.ST (ST)
import qualified Data.ByteString as BS
import Data.STRef

import Data.JsonStream.Lexer.Internal (Element (..))
import qualified Data.JsonStream.Lexer.Internal as Lexer

-- | The largest piece accepted by the owned reader and its body driver.
maxChunkBytes :: Int
maxChunkBytes = 32768

-- | A token, a request for input, or a terminal failure.
data Next = PartialResult !Element | TokMoreData | TokFailed
    deriving (Eq, Show)

-- | Aliases advance the same reader state.
newtype Tokens st = Tokens (STRef st Lexer.Cursor)

-- | Create a reader with no input or foreign storage.
newTokenReader :: ST st (Tokens st)
newTokenReader = Tokens <$> newSTRef (Lexer.start BS.empty)

-- | Supply another piece after exhaustion. Oversized or premature input fails the reader.
supplyTokens :: Tokens st -> BS.ByteString -> ST st ()
supplyTokens (Tokens state) bytes = modifySTRef' state $ \cursor ->
    if BS.length bytes > maxChunkBytes then Lexer.stopped else Lexer.feed cursor bytes

-- | Advance once, retaining only the current piece and any split number.
nextToken :: Tokens st -> ST st Next
nextToken (Tokens state) = do
    cursor <- readSTRef state
    case Lexer.next cursor of
        Lexer.Token value after -> do
            writeSTRef state after
            pure (PartialResult value)
        Lexer.More waiting -> writeSTRef state waiting >> pure TokMoreData
        Lexer.Failed -> writeSTRef state Lexer.stopped >> pure TokFailed
{-# INLINE nextToken #-}
