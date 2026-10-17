-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Whether a request's @Accept@ header admits a media type a route serves, the one question
the router answers @406 Not Acceptable@ on. Nothing is selected by quality: each route serves
one form, so a client that wants another is refused rather than given a substitute.

The reading follows RFC 9110. An absent @Accept@ admits anything, a range matches exactly, by
its type half under @type\/*@, or universally under @*\/*@, and a range parameter is ignored
except @q=0@, which rejects rather than ranks.
-}
module Ecluse.Core.Server.Accept (
    acceptsAny,
) where

import Data.ByteString.Char8 qualified as BS8
import Data.List (lookup)
import Network.HTTP.Types.Header (RequestHeaders, hAccept)

import Ecluse.Core.Server.FieldValue (listMembers, lowerAscii, memberParameters)

-- | Whether the request admits one of the media types a route serves. No @Accept@ header admits them all.
acceptsAny :: RequestHeaders -> NonEmpty ByteString -> Bool
acceptsAny headers served = case lookup hAccept headers of
    Nothing -> True
    Just raw -> any (\range -> any (admits range) served) (listMembers raw)

{- Whether one range admits one served type. The range's own parameters are ignored, because
Écluse serves one representation per route and negotiates no parameter, but @q=0@ rejects. -}
admits :: ByteString -> ByteString -> Bool
admits member served =
    not (any isZeroQuality parameters) && matches range served
  where
    (range, parameters) = memberParameters member
    isZeroQuality (name, value) = name == "q" && isZero value
    -- A quality is a fixed-point number, so every zero spelling ("0", "0.", "0.000") rejects.
    isZero value = BS8.all (\c -> c == '0' || c == '.') value && not (BS8.null value)

-- Whether a parameterless range names a served type: exactly, by its type half, or universally.
matches :: ByteString -> ByteString -> Bool
matches range served
    | range == "*/*" = True
    | lowerAscii range == lowerAscii served = True
    | otherwise = case BS8.break (== '/') range of
        (typeHalf, subtype) -> subtype == "/*" && lowerAscii typeHalf == lowerAscii (BS8.takeWhile (/= '/') served)
