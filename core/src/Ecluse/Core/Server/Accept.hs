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
import Data.Char (toLower)
import Data.List (lookup)
import Network.HTTP.Types.Header (RequestHeaders, hAccept)

{- | Whether the request admits at least one of the media types a route serves. A request with
no @Accept@ header admits every one of them.
-}
acceptsAny :: RequestHeaders -> NonEmpty ByteString -> Bool
acceptsAny headers served = case lookup hAccept headers of
    Nothing -> True
    Just raw -> any (\range -> any (admits range) served) (acceptRanges raw)

-- The ranges an @Accept@ value lists, each still carrying its parameters.
acceptRanges :: ByteString -> [ByteString]
acceptRanges = map trim . BS8.split ','

{- Whether one range admits one served type. The range's own parameters are ignored, because
Écluse serves one representation per route and negotiates no parameter, but @q=0@ rejects. -}
admits :: ByteString -> ByteString -> Bool
admits range served =
    not (rejected range) && matches (trim (BS8.takeWhile (/= ';') range)) served

-- Whether a range's parameters carry the @q=0@ rejection, in any of its spellings.
rejected :: ByteString -> Bool
rejected range = any isZeroQuality (drop 1 (map trim (BS8.split ';' range)))
  where
    isZeroQuality parameter = case BS8.break (== '=') parameter of
        (name, value) -> lower (trim name) == "q" && isZero (trim (BS8.drop 1 value))
    -- A quality is a fixed-point number, so every zero spelling ("0", "0.", "0.000") rejects.
    isZero value = BS8.all (\c -> c == '0' || c == '.') value && not (BS8.null value)

-- Whether a parameterless range names a served type: exactly, by its type half, or universally.
matches :: ByteString -> ByteString -> Bool
matches range served
    | range == "*/*" = True
    | lower range == lower served = True
    | otherwise = case BS8.break (== '/') range of
        (typeHalf, subtype) -> subtype == "/*" && lower typeHalf == lower (BS8.takeWhile (/= '/') served)

lower :: ByteString -> ByteString
lower = BS8.map toLower

trim :: ByteString -> ByteString
trim = BS8.dropWhile isSpace . BS8.dropWhileEnd isSpace
  where
    isSpace c = c == ' ' || c == '\t'
