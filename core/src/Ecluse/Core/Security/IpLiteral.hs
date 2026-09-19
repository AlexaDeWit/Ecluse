-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A hand-rolled recogniser for IP literals, feeding the internal-range block.

'parseIpLiteral' turns a host into an 'IpAddr' or 'Nothing' for a DNS name. Its dotted-quad is
lenient by design, coercing each octet exactly as @inet_aton@ and hence a libc resolver does,
so the policy layer tests the address the proxy would actually dial rather than a decimal
misreading. Delegating the recognition to a library would move that boundary, so only range
membership goes to @iproute@, in "Ecluse.Core.Security.Host".
-}
module Ecluse.Core.Security.IpLiteral (
    -- * IP literals
    IpAddr (..),
    parseIpLiteral,
) where

import Data.Text qualified as T

import Ecluse.Core.Text (readDecimalText, readHexText)

{- | An IP literal recognised from a host. The constructors are exported so
"Ecluse.Core.Security.Host" can convert one to an @iproute@ @IP@ value.
-}
data IpAddr
    = -- | An IPv4 address as its four octets.
      IpV4 Word8 Word8 Word8 Word8
    | -- | An IPv6 address, normalised to its eight 16-bit groups.
      IpV6 [Word16]

{- | Parse a host as an IP literal, or 'Nothing' for a DNS name the host allowlist still
constrains: a short @inet_aton@ form (@2130706433@, @127.1@), a bad octet, or a zone id.
-}
parseIpLiteral :: Text -> Maybe IpAddr
parseIpLiteral host = case T.uncons host of
    Nothing -> Nothing -- empty host: not a literal
    Just _ -> if T.any (== ':') host then parseIPv6 host else parseIPv4 octetInetAton host

{- The host literal passes the @inet_aton@-faithful 'octetInetAton' and the embedded
IPv4-in-IPv6 form the strict-decimal 'octetDecimal'. Only the four-part form counts. -}
parseIPv4 :: (Text -> Maybe Word8) -> Text -> Maybe IpAddr
parseIPv4 octet host = case T.splitOn "." host of
    [a, b, c, d] -> IpV4 <$> octet a <*> octet b <*> octet c <*> octet d
    _ -> Nothing

{- An octet under @inet_aton@'s per-part base rules: @0x@ is hexadecimal, a leading @0@ octal,
anything else decimal. A digit outside the chosen base (the @8@ in @08@) fails, as glibc does. -}
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

{- The full eight-group form, or a @::@-compressed one optionally ending in an embedded
dotted-quad IPv4. Enough for the addresses the internal-range block covers. -}
parseIPv6 :: Text -> Maybe IpAddr
parseIPv6 host = case T.splitOn "::" host of
    [single] -> exactlyEightGroups =<< parseV6Side single
    [before, after] -> do
        hd <- parseV6Side before
        tl <- parseV6Side after
        expandCompressedV6 hd tl
    _ -> Nothing -- more than one "::" is illegal

{- One side of the @::@. Its final token may be a dotted-quad IPv4 (RFC 4291), which expands to
two groups, so @::ffff:169.254.169.254@ decodes rather than passing for a name. -}
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

{- Fill the compressed form's zero run. "::" stands for at least one all-zero group, so the
explicit groups must total at most 7. -}
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
