-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A hand-rolled recogniser for IP literals, feeding the internal-range block.

'parseIpLiteral' turns a host string into an 'IpAddr' (dotted-quad IPv4 or the
IPv6 forms a host realistically carries), or 'Nothing' for a DNS name. The
recogniser is deliberately __lenient__ on the IPv4 dotted-quad, coercing each
octet exactly as @inet_aton@ and hence a libc resolver does (leading-zero octal,
@0x@ hex), so the policy layer tests the address that would actually be dialled
rather than a decimal misreading. Range membership is delegated to @iproute@ by
the policy layer ("Ecluse.Core.Security.Host"); recognising the literal stays here
on purpose, because delegating it to a library would change that lenient boundary.
See 'parseIpLiteral' for the exact grammar and the boundaries left unmodelled.
-}
module Ecluse.Core.Security.IpLiteral (
    -- * IP literals
    IpAddr (..),
    parseIpLiteral,

    -- * Lexical predicates (internal for testing)
    isDecimal,
    isHex,
) where

import Data.Text qualified as T

{- | An IP literal recognised from a host, for the internal-range block. The
constructors are exposed so the policy layer ("Ecluse.Core.Security.Host") can
convert it to an @iproute@ @IP@ value for the range-membership test; the type
carries no instances of its own.
-}
data IpAddr
    = -- | An IPv4 address as its four octets.
      IpV4 Word8 Word8 Word8 Word8
    | -- | An IPv6 address, normalised to its eight 16-bit groups.
      IpV6 [Word16]

{- | Parse a host as an IP literal, or 'Nothing' for a DNS name. Handles dotted-
quad IPv4 and the IPv6 forms a host realistically carries -- full eight-group form,
@::@-compressed forms (including @::1@), and a trailing embedded IPv4 (the
@a.b.c.d@ in @::ffff:a.b.c.d@) -- which is enough to recognise the loopback,
link-local, and IPv4-mapped addresses 'Ecluse.Core.Security.Host.isBlockedIP' blocks. It is deliberately
__not__ a complete IPv6 parser (no zone ids); an unrecognised literal is treated
as a name, which the host allowlist still constrains.

Only range __membership__ is delegated to @iproute@ ('Ecluse.Core.Security.Host.isBlockedIP'); recognising
the literal stays hand-rolled __on purpose__. This recogniser is deliberately
__lenient__ on the IPv4 dotted-quad: it accepts the ambiguous octet spellings a
strict IP library rejects and coerces each octet exactly as @inet_aton@ -- and
hence a libc resolver -- does, so the block tests the address that would actually be
dialled. A @0x@\/@0X@-prefixed octet is hexadecimal, a leading-zero octet is
__octal__, and anything else is decimal. A leading-zero octet is therefore /not/
its decimal digits: @0012.0.0.1@ is octal @10.0.0.1@ (RFC1918, blocked), whereas
@010.0.0.1@ is octal @8.0.0.1@ and @0127.0.0.1@ is octal @87.0.0.1@ (both public,
not blocked) -- matching the resolver rather than a decimal misreading. A stricter
parser that rejected these spellings would let an octal\/hex spelling of an
internal address skip the block and reach the resolving fetch as a name, silently
__narrowing__ the SSRF gate.

Two boundaries are deliberately not modelled here; such a host is simply treated as a
name, which the host allowlist constrains. First, the __short__ @inet_aton@ forms with
fewer than four parts (a bare 32-bit number @2130706433@ \/ @0x7f000001@, or a @127.1@)
are not literals here. Second, a malformed octet (an invalid-octal @08@, where 8 is not
an octal digit, or an overflowing @0400@\/@256@\/@0x100@) is not a literal, exactly as a
resolver rejects it. A malformed IPv6 group that overflows 16 bits (@fe80::1ffff@) is
likewise not a literal here. Delegating literal /parsing/ to a library would change this
lenient/strict boundary, so it is kept here.
-}
parseIpLiteral :: Text -> Maybe IpAddr
parseIpLiteral host = case T.uncons host of
    Nothing -> Nothing -- empty host: not a literal
    Just _ -> if T.any (== ':') host then parseIPv6 host else parseIPv4 octetInetAton host

{- Parse a four-part dotted-quad @a.b.c.d@ into its octets, each coerced to @0..255@
by the supplied octet parser. The top-level host literal passes the
@inet_aton@-faithful 'octetInetAton' (leading-zero octal and @0x@ hex), and the
embedded IPv4-in-IPv6 form passes the strict-decimal 'octetDecimal'; only the
four-part form is recognised (see 'parseIpLiteral' for the short forms treated as names).
-}
parseIPv4 :: (Text -> Maybe Word8) -> Text -> Maybe IpAddr
parseIPv4 octet host = case T.splitOn "." host of
    [a, b, c, d] -> IpV4 <$> octet a <*> octet b <*> octet c <*> octet d
    _ -> Nothing

{- An IPv4 octet under @inet_aton@'s per-part base rules -- the coercion a libc
resolver ('getAddrInfo') applies, so the internal-range block tests the address a
resolver would actually dial. A @0x@\/@0X@ prefix is hexadecimal, a leading @0@
(with at least one more digit) is octal, and anything else is decimal; the parsed
value must still fit @0..255@, so an overflowing part (@0400@ = 256, @0x100@ = 256)
is rejected exactly as a resolver rejects it. The base-digit check keeps 'readMaybe'
from accepting signs or whitespace and rejects a digit outside the chosen base (the
@8@ in @08@ is not octal), so such a spelling is not a literal -- matching glibc,
which refuses it rather than coercing it.
-}
octetInetAton :: Text -> Maybe Word8
octetInetAton tok = do
    n <- value
    if n <= 255 then Just (fromInteger n) else Nothing
  where
    value :: Maybe Integer
    value = case T.uncons tok of
        Just ('0', rest)
            | T.toLower (T.take 1 rest) == "x" ->
                let hex = T.drop 1 rest
                 in if isHex hex then readMaybe ("0x" <> toString hex) else Nothing
            | not (T.null rest) ->
                if isOctal tok then readMaybe ("0o" <> toString tok) else Nothing
        _ -> if isDecimal tok then readMaybe (toString tok) else Nothing

{- An IPv4 octet as a non-empty all-decimal run in @0..255@: the strict spelling
used inside an IPv4-in-IPv6 literal (@::ffff:a.b.c.d@), where the embedded form is
not subject to @inet_aton@'s base coercion. The digit check keeps 'readMaybe' from
accepting signs\/whitespace, so a parsed value is >= 0.
-}
octetDecimal :: Text -> Maybe Word8
octetDecimal t = do
    n <- if isDecimal t then readMaybe (toString t) else Nothing :: Maybe Integer
    if n <= 255 then Just (fromInteger n) else Nothing

{- Parse an IPv6 literal -- either the full eight-group form or a @::@-compressed
form (at most one @::@), optionally ending in an embedded dotted-quad IPv4 -- into
its eight 16-bit groups. Enough to recognise the @::1@, @fe80::\/10@, and
@::ffff:0:0\/96@ addresses we block; rejects anything malformed.
-}
parseIPv6 :: Text -> Maybe IpAddr
parseIPv6 host = case T.splitOn "::" host of
    [single] -> exactlyEightGroups =<< parseV6Side single
    [before, after] -> do
        hd <- parseV6Side before
        tl <- parseV6Side after
        expandCompressedV6 hd tl
    _ -> Nothing -- more than one "::" is illegal

{- The colon-separated groups of one side of the @::@; "" → no groups. The final
token may be a dotted-quad IPv4 (RFC 4291 §2.2.3, e.g. the @169.254.169.254@ in
@::ffff:169.254.169.254@), which expands to its two 16-bit groups so an
IPv4-mapped literal in its canonical dotted form is decoded rather than
mistaken for a name. Only the last token may be dotted; an interior dotted
token fails 'parseV6Group' (no hex '.') and the whole parse is rejected.
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

{- A group is a non-empty all-hex run that fits in 16 bits. The hex check
keeps 'readMaybe' from accepting signs, so a parsed value is >= 0.
-}
parseV6Group :: Text -> Maybe Word16
parseV6Group t = do
    n <- if isHex t then readMaybe ("0x" <> toString t) else Nothing :: Maybe Integer
    if n <= 0xFFFF then Just (fromInteger n) else Nothing

{- Fill the compressed form's zero run: "::" stands for at least one all-zero
group, so the explicit groups on either side must total at most 7 (leaving room
to fill to 8).
-}
expandCompressedV6 :: [Word16] -> [Word16] -> Maybe IpAddr
expandCompressedV6 hd tl =
    let present = length hd + length tl
     in if present <= 7
            then Just (IpV6 (hd <> replicate (8 - present) 0 <> tl))
            else Nothing

-- Exactly the full eight-group form; anything else is malformed.
exactlyEightGroups :: [Word16] -> Maybe IpAddr
exactlyEightGroups gs@[_, _, _, _, _, _, _, _] = Just (IpV6 gs)
exactlyEightGroups _ = Nothing

-- Whether @t@ is a non-empty run of decimal digits (no sign or whitespace).
isDecimal :: Text -> Bool
isDecimal t = not (T.null t) && T.all (`elem` ['0' .. '9']) t

-- Whether @t@ is a non-empty run of octal digits (0..7).
isOctal :: Text -> Bool
isOctal t = not (T.null t) && T.all (`elem` ['0' .. '7']) t

-- Whether @t@ is a non-empty run of hexadecimal digits.
isHex :: Text -> Bool
isHex t = not (T.null t) && T.all isHexDigit t
  where
    isHexDigit c = c `elem` (['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F'])
