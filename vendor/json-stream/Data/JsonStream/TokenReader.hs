{-# LANGUAGE BangPatterns #-}

-- | An owned Haskell token cursor over immutable input pieces.
module Data.JsonStream.TokenReader (
    Tokens, newTokenReader, nextToken, supplyTokens, maxChunkBytes,
    Element (..), Next (..)
) where

import Control.Monad.ST (ST)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.STRef
import Foreign.C.Types (CLong)

import qualified Data.JsonStream.Lexer.Internal as Lexer
import qualified Data.JsonStream.TokenParser as List

-- | The largest piece accepted by the owned reader and its body driver.
maxChunkBytes :: Int
maxChunkBytes = 32768

-- | A token without leftover-input reporting, for a consumer that always advances.
data Element
    = ArrayBegin | ArrayEnd | ObjectBegin | ObjectEnd
    | StringContent !BS.ByteString | StringRaw !BS.ByteString !Bool | StringEnd
    | JValue !Aeson.Value | JInteger !CLong
    deriving (Eq, Show)

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
            pure (PartialResult (withoutContext value))
        Lexer.More waiting -> writeSTRef state waiting >> pure TokMoreData
        Lexer.Failed -> writeSTRef state Lexer.stopped >> pure TokFailed
{-# INLINE nextToken #-}

withoutContext :: List.Element -> Element
withoutContext value = case value of
    List.ArrayBegin -> ArrayBegin
    List.ArrayEnd _ -> ArrayEnd
    List.ObjectBegin -> ObjectBegin
    List.ObjectEnd _ -> ObjectEnd
    List.StringContent bytes -> StringContent bytes
    List.StringRaw bytes ascii _ -> StringRaw bytes ascii
    List.StringEnd _ -> StringEnd
    List.JValue scalar -> JValue scalar
    List.JInteger integer -> JInteger integer
{-# INLINE withoutContext #-}
