-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A hand-rolled recogniser for IP literals, feeding the internal-range block.

'parseIpLiteral' turns a host string into an 'IpAddr' (dotted-quad IPv4 or the IPv6
forms a host realistically carries), or 'Nothing' for a DNS name. The recogniser is
lenient on the IPv4 dotted-quad by design. It coerces each octet exactly as
@inet_aton@, and hence a libc resolver, does: leading-zero octal, @0x@ hex. The policy
layer therefore tests the address the proxy would actually dial, rather than a decimal
misreading. That policy layer ("Ecluse.Core.Security.Host") delegates range membership
to @iproute@. Recognising the literal stays here on purpose, because delegating it to a
library would change that lenient boundary. See 'parseIpLiteral' for the exact grammar
and the boundaries left unmodelled.
-}
module Ecluse.Core.Security.IpLiteral (
    -- * IP literals
    IpAddr (..),
    parseIpLiteral,
) where

import Data.Text qualified as T

import Ecluse.Core.Text (readDecimalText, readHexText)

{- | An IP literal recognised from a host, for the internal-range block. The constructors
are exported so "Ecluse.Core.Security.Host" can convert one to an @iproute@ @IP@ value.
-}
data IpAddr
    = -- | An IPv4 address as its four octets.
      IpV4 Word8 Word8 Word8 Word8
    | -- | An IPv6 address, normalised to its eight 16-bit groups.
      IpV6 [Word16]

{- | Parse a host as an IP literal, or 'Nothing' for a DNS name. It covers dotted-quad
IPv4 and the IPv6 forms a host realistically carries, enough for the addresses
'Ecluse.Core.Security.Host.isBlockedIP' blocks. Anything else counts as a name the host
allowlist still constrains: the short @inet_aton@ forms (@2130706433@, @127.1@), a
malformed octet, an overflowing IPv6 group, and a zone id.

The dotted-quad is deliberately lenient, coercing each octet exactly as @inet_aton@ and
hence a libc resolver does (see 'octetInetAton'), so the block tests the address the proxy
would actually dial. A leading-zero octet is octal, so @0012.0.0.1@ is @10.0.0.1@ and the
block catches it. A stricter parser would let an octal or hex spelling of an internal
address reach the resolving fetch as a name, silently narrowing the SSRF gate.
-}
parseIpLiteral :: Text -> Maybe IpAddr
parseIpLiteral host = case T.uncons host of
    Nothing -> Nothing -- empty host: not a literal
    Just _ -> if T.any (== ':') host then parseIPv6 host else parseIPv4 octetInetAton host

{- Parse a four-part dotted-quad into its octets with the supplied octet parser: the host
literal passes the @inet_aton@-faithful 'octetInetAton', and the embedded IPv4-in-IPv6
form passes the strict-decimal 'octetDecimal'. Only the four-part form counts, since
'parseIpLiteral' treats the short forms as names.
-}
parseIPv4 :: (Text -> Maybe Word8) -> Text -> Maybe IpAddr
parseIPv4 octet host = case T.splitOn "." host of
    [a, b, c, d] -> IpV4 <$> octet a <*> octet b <*> octet c <*> octet d
    _ -> Nothing

{- An IPv4 octet under @inet_aton@'s per-part base rules, the coercion a libc resolver
applies: a @0x@ prefix is hexadecimal, a leading @0@ is octal, anything else is decimal.
The value must still fit @0..255@, and a digit outside the chosen base (the @8@ in @08@)
fails, so such a spelling is not a literal at all, exactly as glibc refuses it.
-}
octetInetAton :: Text -> Maybe Word8
octetInetAton tok = do
    n <- value
    if n <= 255 then Just (fromInteger n) else Nothing
  where
    value :: Maybe Integer
    value = case T.uncons tok of
        Just ('0', rest)
            | T.toLower (T.take 1 rest) == "x" -> readHexText (T.drop 1 rest)
            | not (T.null rest) ->
                if isOctal tok then readMaybe ("0o" <> toString tok) else Nothing
        _ -> readDecimalText tok

{- An IPv4 octet as a strict decimal run in @0..255@: the spelling inside an IPv4-in-IPv6
literal, where @inet_aton@'s base coercion does not apply, so the value is >= 0.
-}
octetDecimal :: Text -> Maybe Word8
octetDecimal t = do
    n <- readDecimalText t :: Maybe Integer
    if n <= 255 then Just (fromInteger n) else Nothing

{- Parse an IPv6 literal into its eight 16-bit groups: the full eight-group form, or a
@::@-compressed form optionally ending in an embedded dotted-quad IPv4. Enough for the
@::1@, @fe80::\/10@, and @::ffff:0:0\/96@ addresses the block covers.
-}
parseIPv6 :: Text -> Maybe IpAddr
parseIPv6 host = case T.splitOn "::" host of
    [single] -> exactlyEightGroups =<< parseV6Side single
    [before, after] -> do
        hd <- parseV6Side before
        tl <- parseV6Side after
        expandCompressedV6 hd tl
    _ -> Nothing -- more than one "::" is illegal

{- The colon-separated groups of one side of the @::@. The final token may be a
dotted-quad IPv4 (RFC 4291 §2.2.3), which expands to two 16-bit groups, so
@::ffff:169.254.169.254@ decodes rather than passing for a name. An interior dotted token
fails 'parseV6Group', which rejects the whole parse.
-}
parseV6Side :: Text -> Maybe [Word16]
parseV6Side t
    | T.null t = Just []
    | otherwise = parseV6Tokens (T.splitOn ":" t)

parseV6Tokens :: [Text] -> Maybe [Word16]
parseV6Tokens [] = Just []
parseV6Tokens [tok]
    | T.any (== '.') tok = parseEmbeddedV4 tok
    | otherwise = (: []) <$> parseV6Group tok
parseV6Tokens (tok : rest) = (:) <$> parseV6Group tok <*> parseV6Tokens rest

-- A trailing dotted-quad IPv4 as its two 16-bit groups (high pair, low pair).
parseEmbeddedV4 :: Text -> Maybe [Word16]
parseEmbeddedV4 t = case parseIPv4 octetDecimal t of
    Just (IpV4 a b c d) -> Just [pair a b, pair c d]
    _ -> Nothing
  where
    pair hi lo = fromIntegral hi * 256 + fromIntegral lo

{- A group is a non-empty all-hex run that fits in 16 bits. 'readHexText' takes no sign and
no @0x@ prefix, so a parsed value is >= 0 and @0x1@ is not a group.
-}
parseV6Group :: Text -> Maybe Word16
parseV6Group t = do
    n <- readHexText t :: Maybe Integer
    if n <= 0xFFFF then Just (fromInteger n) else Nothing

{- Fill the compressed form's zero run. "::" stands for at least one all-zero group.
The explicit groups on either side must therefore total at most 7, leaving room to
fill to 8.
-}
expandCompressedV6 :: [Word16] -> [Word16] -> Maybe IpAddr
expandCompressedV6 hd tl =
    let present = length hd + length tl
     in if present <= 7
            then Just (IpV6 (hd <> replicate (8 - present) 0 <> tl))
            else Nothing

-- Exactly the full eight-group form. Anything else is malformed.
exactlyEightGroups :: [Word16] -> Maybe IpAddr
exactlyEightGroups gs@[_, _, _, _, _, _, _, _] = Just (IpV6 gs)
exactlyEightGroups _ = Nothing

-- Whether @t@ is a non-empty run of octal digits (0..7). @Data.Text.Read@ ships no octal
-- reader, so the leading-zero @inet_aton@ octal octet keeps its own gate.
isOctal :: Text -> Bool
isOctal t = not (T.null t) && T.all (`elem` ['0' .. '7']) t
