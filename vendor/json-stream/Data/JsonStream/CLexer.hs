-- | Compatibility entry point for json-stream's incremental token parser.
module Data.JsonStream.CLexer (tokenParser, unescapeText) where

import qualified Data.ByteString as BS
import qualified Data.JsonStream.Lexer.Internal as Lexer
import Data.JsonStream.TokenParser (TokenResult (..))
import qualified Data.JsonStream.TokenParser as List
import Data.JsonStream.Unescape (unescapeText)

-- | Preserve the parser's token, input-boundary, and leftover-input interface.
tokenParser :: BS.ByteString -> TokenResult
tokenParser = go . Lexer.start
  where
    go cursor = case Lexer.next cursor of
        Lexer.Token value after -> PartialResult (withContext value after) (go after)
        Lexer.More waiting -> TokMoreData (go . Lexer.feed waiting)
        Lexer.Failed -> TokFailed

withContext :: Lexer.Element -> Lexer.Cursor -> List.Element
withContext element after = case element of
    Lexer.ArrayBegin -> List.ArrayBegin
    Lexer.ArrayEnd -> List.ArrayEnd rest
    Lexer.ObjectBegin -> List.ObjectBegin
    Lexer.ObjectEnd -> List.ObjectEnd rest
    Lexer.StringContent bytes -> List.StringContent bytes
    Lexer.StringRaw bytes ascii -> List.StringRaw bytes ascii rest
    Lexer.StringEnd -> List.StringEnd rest
    Lexer.JValue scalar -> List.JValue scalar
    Lexer.JInteger integer -> List.JInteger integer
  where
    rest = Lexer.remaining after
{-# INLINE withContext #-}
