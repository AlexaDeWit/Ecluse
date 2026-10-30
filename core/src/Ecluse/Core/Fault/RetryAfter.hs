-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded retry hints from an upstream response, without a default delay policy.
module Ecluse.Core.Fault.RetryAfter (
    parseRetryAfter,
    upstreamRetryCap,
    responseRetryAfter,
) where

import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Types (ResponseHeaders)

import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Text (readDecimalText)
import Ecluse.Core.Text.HttpDate (parseHttpDateAt)

-- | Return a positive delay capped by the caller, rounding dates up to whole seconds. Unusable hints yield 'Nothing'.
parseRetryAfter :: UTCTime -> RetryAfter -> Maybe ByteString -> Maybe RetryAfter
parseRetryAfter now (RetryAfter cap) header = do
    guard (cap > 0)
    raw <- T.dropAround (\c -> c == ' ' || c == '\t') . decodeUtf8 <$> header
    seconds <- cappedDecimal cap raw <|> (ceiling . (`diffUTCTime` now) <$> parseHttpDateAt now raw)
    guard (seconds > (0 :: Integer))
    pure (RetryAfter (fromInteger (min (toInteger cap) seconds)))

-- | The longest delay an upstream hint may set: ten minutes.
upstreamRetryCap :: RetryAfter
upstreamRetryCap = RetryAfter 600

-- | A response's @Retry-After@ under 'upstreamRetryCap', read at the current time.
responseRetryAfter :: ResponseHeaders -> IO (Maybe RetryAfter)
responseRetryAfter headers = do
    now <- getCurrentTime
    pure (parseRetryAfter now upstreamRetryCap (lookup "Retry-After" headers))

-- Strip leading zeroes before the length check, so numeric conversion never grows with the header.
cappedDecimal :: Int -> Text -> Maybe Integer
cappedDecimal cap raw = do
    guard (not (T.null raw) && T.all isDigit raw)
    let digits = T.dropWhile (== '0') raw
    if T.null digits
        then pure 0
        else
            if T.compareLength digits (BS.length (show cap)) == GT
                then pure (toInteger cap)
                else min (toInteger cap) <$> readDecimalText digits
