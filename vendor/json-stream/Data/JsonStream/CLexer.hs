-- | Compatibility entry point for json-stream's incremental token parser.
module Data.JsonStream.CLexer (tokenParser, unescapeText) where

import qualified Data.ByteString as BS
import qualified Data.JsonStream.Lexer.Internal as Lexer
import Data.JsonStream.TokenParser (TokenResult (..))
import Data.JsonStream.Unescape (unescapeText)

-- | Preserve the parser's token, input-boundary, and leftover-input interface.
tokenParser :: BS.ByteString -> TokenResult
tokenParser = go . Lexer.start
  where
    go cursor = case Lexer.next cursor of
        Lexer.Token value after -> PartialResult value (go after)
        Lexer.More waiting -> TokMoreData (go . Lexer.feed waiting)
        Lexer.Failed -> TokFailed
