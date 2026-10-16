-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Content-coding negotiation for served metadata, read from every @Accept-Encoding@ field
(RFC 9110, section 12.5.3). Écluse offers gzip and identity. Gzip wins a tie of positive
qualities, a malformed quality refuses its coding, and a refusal beats a duplicate offer. A
request that excludes both codings takes @406@. Identity-only metadata ignores the field.
-}
module Ecluse.Core.Server.Encoding (
    -- * The operator's setting
    MetadataCompression (..),

    -- * Negotiation
    ContentCoding (..),
    CodingChoice (..),
    chooseCoding,

    -- * Response headers
    gzipEncodingHeader,
    varyAcceptEncoding,
) where

import Data.ByteString.Char8 qualified as BS8
import Data.Char (isDigit)
import Network.HTTP.Types (Header, RequestHeaders, ResponseHeaders)
import Network.HTTP.Types.Header (hAcceptEncoding, hContentEncoding, hVary)

import Ecluse.Core.Server.FieldValue (fieldValues, listMembers, lowerAscii, memberParameters)

-- | Whether served metadata may be gzipped for a client that accepts it.
data MetadataCompression
    = -- | Gzip a listing when the request's @Accept-Encoding@ prefers it.
      GzipWhenAccepted
    | -- | Serve every listing in identity coding and ignore @Accept-Encoding@.
      IdentityOnly
    deriving stock (Eq, Show)

-- | A content coding Écluse can serve a listing in.
data ContentCoding = IdentityCoding | GzipCoding
    deriving stock (Eq, Show)

-- | The coding a request selects, or the refusal when it excludes every coding Écluse serves.
data CodingChoice
    = ServeCoding ContentCoding
    | NoAcceptableCoding
    deriving stock (Eq, Show)

-- A quality in thousandths, 0 to 1000. Zero refuses its coding.
type Quality = Int

-- The offers one request makes, after duplicates of each coding are combined.
data Offers = Offers
    { offerGzip :: Maybe Quality
    , offerIdentity :: Maybe Quality
    , offerAny :: Maybe Quality
    }

-- | Select a listing's coding. Identity stays acceptable unless excluded, and outranks gzip only by an explicit higher quality.
chooseCoding :: MetadataCompression -> RequestHeaders -> CodingChoice
chooseCoding policy headers = case policy of
    IdentityOnly -> ServeCoding IdentityCoding
    GzipWhenAccepted -> decide (requestOffers headers)

decide :: Offers -> CodingChoice
decide offers = case offerIdentity offers <|> offerAny offers of
    Nothing
        | gzip > 0 -> ServeCoding GzipCoding
        | otherwise -> ServeCoding IdentityCoding
    Just plain
        | gzip > 0 && gzip >= plain -> ServeCoding GzipCoding
        | plain > 0 -> ServeCoding IdentityCoding
        | otherwise -> NoAcceptableCoding
  where
    gzip = fromMaybe 0 (offerGzip offers <|> offerAny offers)

requestOffers :: RequestHeaders -> Offers
requestOffers headers =
    foldl' offer (Offers Nothing Nothing Nothing) (concatMap listMembers (fieldValues hAcceptEncoding headers))

offer :: Offers -> ByteString -> Offers
offer offers member = case lowerAscii coding of
    "gzip" -> offers{offerGzip = combine (offerGzip offers)}
    "x-gzip" -> offers{offerGzip = combine (offerGzip offers)}
    "identity" -> offers{offerIdentity = combine (offerIdentity offers)}
    "*" -> offers{offerAny = combine (offerAny offers)}
    _ -> offers
  where
    (coding, parameters) = memberParameters member
    quality = maybe 1000 (fromMaybe 0 . parseQuality) (listToMaybe [value | (name, value) <- parameters, name == "q"])
    combine = Just . maybe quality (\previous -> if previous == 0 || quality == 0 then 0 else max previous quality)

-- RFC 9110's qvalue: "0" with up to three decimals, or "1" with up to three zero decimals.
parseQuality :: ByteString -> Maybe Quality
parseQuality raw = case BS8.uncons raw of
    Just (whole, rest)
        | whole `elem` ['0', '1']
        , Just decimals <- fraction rest -> do
            let value = (if whole == '1' then 1000 else 0) + thousandths decimals
            value <$ guard (value <= 1000)
    _ -> Nothing
  where
    fraction rest = case BS8.uncons rest of
        Nothing -> Just ""
        Just ('.', digits) | BS8.length digits <= 3 && BS8.all isDigit digits -> Just digits
        _ -> Nothing
    thousandths digits = BS8.foldl' (\total c -> total * 10 + fromEnum c - fromEnum '0') 0 (BS8.take 3 (digits <> "000"))

-- | The @Content-Encoding@ header of a gzip listing.
gzipEncodingHeader :: Header
gzipEncodingHeader = (hContentEncoding, "gzip")

-- | Add @Accept-Encoding@ to a response's @Vary@, keeping the field names already listed.
varyAcceptEncoding :: ResponseHeaders -> ResponseHeaders
varyAcceptEncoding headers
    | any covers varied = headers
    | otherwise = case break ((== hVary) . fst) headers of
        (before, (name, value) : after) -> before <> ((name, value <> ", Accept-Encoding") : after)
        _ -> headers <> [(hVary, "Accept-Encoding")]
  where
    varied = map lowerAscii (concatMap listMembers (fieldValues hVary headers))
    covers member = member == "*" || member == "accept-encoding"
