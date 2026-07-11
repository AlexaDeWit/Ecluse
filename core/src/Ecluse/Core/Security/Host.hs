{- | Outbound-request guards for the proxy's data plane: defending where the proxy fetches.

Écluse builds outbound HTTP requests from two untrusted sources -- __client-supplied
package identifiers__ (the request path) and __upstream-supplied artifact
locations__ (a packument's @dist.tarball@). This module provides the pure guard layer
that keeps the proxy from being steered by hostile input.

__Where the proxy fetches:__ 'isAllowedUpstreamHost' restricts outbound fetches
to the configured upstream hosts, and 'isBlockedTarget' rejects internal address
ranges (cloud instance metadata, loopback, RFC1918) that the proxy's network
position can otherwise reach. Together they are the SSRF gate: a target must be
both on the allowlist /and/ not an internal address.
-}
module Ecluse.Core.Security.Host (
    -- * Outbound host allowlist
    LoweredHostSet,
    lowerCaseHosts,
    isAllowedUpstreamHost,

    -- * Internal-range block
    isBlockedTarget,
    isBlockedIP,
    parseIpLiteral,
    parseBlockedRange,
    hostAddress,
    splitHostPort,

    -- * Tarball-host policy
    TarballHostPolicy (..),
    Origin (..),
    tarballHostAllowed,
    TarballHostGate (..),
    tarballHostGate,

    -- * Internal for testing
    isHex,
    isDecimal,
) where

import Data.IP (
    IP (IPv4, IPv6),
    IPRange (IPv4Range, IPv6Range),
    fromIPv6b,
    isMatchedTo,
    toIPv4,
    toIPv6,
 )
import Data.Set qualified as Set
import Data.Text qualified as T

{- | A set of host strings normalised to lower case, the form the host guards
('isAllowedUpstreamHost' and 'isBlockedTarget') compare against.

The type is __opaque, and 'lowerCaseHosts' is its only constructor__: a value of
this type therefore carries the proof that every host in it is already
lower-cased, so the guards lower only the /incoming/ host and the case-insensitive
match cannot be bypassed by an un-normalised configuration set.
-}
newtype LoweredHostSet = LoweredHostSet (Set Text)
    deriving stock (Eq, Show)

{- | Normalise a set of configured host strings to the canonical key form the host
guards take, yielding a 'LoweredHostSet'.

A plain DNS name is folded to lower case (hostnames are case-insensitive), so the
guards match an incoming host against the configuration regardless of how either
was spelled. An entry that parses as an __IP literal__ is additionally rendered to its
single canonical literal (see 'canonicalHostKey'), so equivalent spellings of one
address (compressed versus expanded IPv6, differing case) collapse to one key. An
operator who opts in @0:0:0:0:0:0:0:1@ therefore matches a literal @::1@ rather than
missing it on a textual difference.
-}
lowerCaseHosts :: Set Text -> LoweredHostSet
lowerCaseHosts = LoweredHostSet . Set.map canonicalHostKey

{- | Whether @host@ is one of the configured upstream hosts.

The first guard on every outbound fetch: the proxy talks to its configured
private\/public upstreams and mirror target, and __nothing else__ -- so a target
host derived from a packument's @dist.tarball@ (or anywhere else) is fetched only
if it appears in @allowed@. The match is exact on the bare host (no port, no
scheme -- extract it with 'hostAddress' first) and __case-insensitive__, since
DNS hostnames are; an empty @host@ is never allowed. This is the allowlist half
of the SSRF gate; pair it with 'isBlockedTarget' for the internal-range half.

The allowlist is a 'LoweredHostSet', so it is already normalised and only the
incoming @host@ is folded here -- through the same 'canonicalHostKey' the set was
built with, so an IP-literal entry matches regardless of how either side spells the
address.
-}
isAllowedUpstreamHost :: LoweredHostSet -> Text -> Bool
isAllowedUpstreamHost (LoweredHostSet allowed) host =
    not (T.null host) && canonicalHostKey host `Set.member` allowed

{- | Whether @host@ is an internal address the proxy must not fetch.

A proxy sits in a privileged network position, so an attacker who can steer a
fetch (see the module header) aims it at addresses only the proxy can reach: the
cloud instance-metadata endpoint (@169.254.169.254@), loopback, or the private
network (RFC1918). This blocks, by parsing @host@ as a literal IP and testing it
against:

* __link-local__ @169.254.0.0\/16@ (which contains the @169.254.169.254@ metadata
  address) and IPv6 @fe80::\/10@;
* __loopback__ @127.0.0.0\/8@ and IPv6 @::1@;
* __unspecified \/ this-host__ @0.0.0.0\/8@ and IPv6 @::@ -- @0.0.0.0@ is not a
  no-op target: on Linux a connect to it reaches a loopback-bound service, so it
  is a loopback-equivalent that must be blocked alongside @127.0.0.0\/8@;
* __RFC1918 private__ @10.0.0.0\/8@, @172.16.0.0\/12@, and @192.168.0.0\/16@;
* __CGNAT shared__ @100.64.0.0\/10@ (RFC 6598) -- carrier-grade NAT space some
  cloud fabrics route internally;
* __IPv6 unique-local__ @fc00::\/7@ (RFC 4193) -- the private-network IPv6 analogue,
  which contains the AWS IMDSv6 metadata endpoint @fd00:ec2::254@;
* every range in @additionalRanges@, the operator-configured extension of this
  fixed set (@ECLUSE_ADDITIONAL_BLOCKED_RANGES@) -- a deployment's own internal
  space this module cannot know about in advance.

A @host@ that is not an IP literal (a DNS name) is __not__ blocked here:
name-based targets are constrained by the 'isAllowedUpstreamHost' allowlist
instead, and post-resolution IP filtering belongs to the resolving fetch layer,
not this pure check. Both guards apply -- an allowlisted host that resolves to an
internal literal is still caught when its address is tested here.
-}
isBlockedTarget :: [IPRange] -> Text -> Bool
isBlockedTarget additionalRanges host =
    maybe False (isBlockedIP additionalRanges . ipAddrToIP) (parseIpLiteral host)

{- | Whether an 'IP' falls in a blocked internal range: the fixed 'blockedRanges'
set together with the caller-supplied @additionalRanges@.

The single source of record for the internal-range decision, used by the literal
block ('isBlockedTarget') on the @dist.tarball@ host gate. An
IPv4-mapped IPv6 address (@::ffff:a.b.c.d@) is first decoded to its embedded IPv4
and tested against the IPv4 ranges: a mapped internal literal (e.g.
@::ffff:169.254.169.254@) is a recognised SSRF smuggling form, so it must be
caught by the IPv4 block rather than slip through as an unrelated IPv6 address.
-}
isBlockedIP :: [IPRange] -> IP -> Bool
isBlockedIP additionalRanges ip = any matches (blockedRanges <> additionalRanges)
  where
    decoded = decodeMappedV4 ip
    matches = \case
        IPv4Range r -> case decoded of
            IPv4 a -> a `isMatchedTo` r
            IPv6 _ -> False
        IPv6Range r -> case decoded of
            IPv6 a -> a `isMatchedTo` r
            IPv4 _ -> False

{- The internal ranges the proxy refuses to fetch from, as @iproute@ CIDR values:
the unspecified \/ this-host, loopback, link-local, RFC1918, CGNAT-shared, and
IPv6 unique-local blocks. Declared once and consulted by 'isBlockedIP' alone, so
the blocked set is a single cross-cutting invariant. @0.0.0.0\/8@ is blocked
because @0.0.0.0@ reaches a loopback-bound service on Linux; @169.254.0.0\/16@
contains the @169.254.169.254@ cloud-metadata endpoint; @fc00::\/7@ contains the
AWS IMDSv6 endpoint @fd00:ec2::254@. An operator cannot narrow this fixed set --
only extend it, via the @additionalRanges@ 'isBlockedIP' also consults.
-}
blockedRanges :: [IPRange]
blockedRanges =
    [ "0.0.0.0/8" -- unspecified / this-host (reaches loopback on Linux)
    , "10.0.0.0/8" -- RFC1918 private
    , "100.64.0.0/10" -- CGNAT shared (RFC 6598)
    , "127.0.0.0/8" -- loopback
    , "169.254.0.0/16" -- link-local (incl. 169.254.169.254 metadata)
    , "172.16.0.0/12" -- RFC1918 private
    , "192.168.0.0/16" -- RFC1918 private
    , "::/128" -- IPv6 unspecified
    , "::1/128" -- IPv6 loopback
    , "fe80::/10" -- IPv6 link-local
    , "fc00::/7" -- IPv6 unique-local (incl. AWS IMDSv6 fd00:ec2::254)
    ]

{- | Parse one operator-configured @ECLUSE_ADDITIONAL_BLOCKED_RANGES@ entry (a
single CIDR, e.g. @"203.0.113.0\/24"@ or @"2001:db8::\/32"@) into an 'IPRange', or
'Nothing' for anything malformed.

A __total__ wrapper over @iproute@'s own 'Read' instance for 'IPRange': that
instance's underlying parser (@parseIPRange@) already fails by returning no
parse rather than calling 'error', so 'readMaybe' over it is safe -- unlike the
partial 'IsString' instance ('blockedRanges' relies on for its own compile-time
literals, where a malformed literal would be a build-time error, never runtime
input). This is the only way the config decoder is meant to turn operator text
into an 'IPRange': a malformed entry must fail closed at boot, never be silently
dropped or accepted as an unblocked range.
-}
parseBlockedRange :: Text -> Maybe IPRange
parseBlockedRange = readMaybe . toString

{- Convert a recognised literal to an @iproute@ 'IP' for the membership test.
The four IPv4 octets become an 'IPv4', and the eight 16-bit groups an 'IPv6'. The
IPv4-mapped decode is left to 'isBlockedIP' ('decodeMappedV4'), so a mapped
literal is carried here as the IPv6 it textually is and decoded only at the point
of the range test.
-}
ipAddrToIP :: IpAddr -> IP
ipAddrToIP = \case
    IpV4 a b c d -> IPv4 (toIPv4 (map fromIntegral [a, b, c, d]))
    IpV6 groups -> IPv6 (toIPv6 (map fromIntegral groups))

{- The canonical comparison key for a host: a normalised string the host guards
match a 'LoweredHostSet' on. A host that parses as an IP literal is rendered to the
@iproute@ canonical literal through @IP@ 'show', so equivalent spellings of one
address collapse to one key: compressed versus expanded IPv6
(@::1@ is @0:0:0:0:0:0:0:1@), embedded IPv4, and hex case all canonicalise identically.
Anything that is not a literal (a DNS name) is merely case-folded, since hostnames are
case-insensitive.

This is the single canonicaliser feeding the host allowlist: 'lowerCaseHosts' builds
the configured set with it, and 'isAllowedUpstreamHost' folds the queried host with
it, so a configured entry matches a literal address whichever representation either
uses. Pointing both sides at one @show@ is what guarantees they render identically;
a second, separate canonicaliser could drift.
-}
canonicalHostKey :: Text -> Text
canonicalHostKey host = case parseIpLiteral host of
    Just addr -> show (ipAddrToIP addr)
    Nothing -> T.toLower host

{- Decode an IPv4-mapped (@::ffff:a.b.c.d@) or IPv4-compatible (@::a.b.c.d@)
IPv6 address to its embedded IPv4, so it is tested against the IPv4 ranges;
any other address is returned unchanged. Over the sixteen octets 'fromIPv6b'
yields, the mapped form is ten zeros then @ff ff@, and the compatible form
is twelve zeros. Testing an embedded internal literal against the IPv6 ranges
instead would let @::169.254.169.254@ through, so the decode is load-bearing
for the SSRF block.
-}
decodeMappedV4 :: IP -> IP
decodeMappedV4 = \case
    IPv6 v6 -> case fromIPv6b v6 of
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xFF, 0xFF, a, b, c, d] ->
            IPv4 (toIPv4 [a, b, c, d])
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, b, c, d] ->
            IPv4 (toIPv4 [a, b, c, d])
        _ -> IPv6 v6
    ip -> ip

{- An IP literal, parsed from a host for internal-range testing. Internal to
this module; converted to an @iproute@ 'IP' by 'ipAddrToIP' for the membership
test, so it carries no instances.
-}
data IpAddr
    = -- An IPv4 address as its four octets.
      IpV4 Word8 Word8 Word8 Word8
    | -- An IPv6 address, normalised to its eight 16-bit groups.
      IpV6 [Word16]

{- | Extract the bare host from a URI or @host[:port]@ authority.

A convenience for the SSRF gate: an outbound target is usually a full URL or an
authority, but 'isAllowedUpstreamHost' and 'isBlockedTarget' compare the bare
host. This strips a @scheme:\/\/@ prefix, any @userinfo\@@, any @:port@ suffix,
and any @\/path@\/@?query@\/@#fragment@ tail, lower-casing the result. It is a
pragmatic extractor for comparison, __not__ a full RFC 3986 parser; a value with
no recognisable host yields the empty string, which both guards treat as
not-allowed. IPv6 literals in brackets (@[::1]:443@) are returned without the
brackets -- the bracket-aware @host[:port]@ split is 'splitHostPort', shared with
the SQS endpoint parser so the two cannot drift on an authority edge case; a
malformed authority (an opening bracket with no close) yields the empty string,
the same fail-safe the guards apply to it.
-}
hostAddress :: Text -> Text
hostAddress raw =
    let afterScheme = afterFirst "://" raw
        authority = T.takeWhile (`notElem` ['/', '?', '#']) afterScheme
        afterUserinfo = afterLast "@" authority
     in T.toLower (maybe "" fst (splitHostPort afterUserinfo))
  where
    -- The text after @needle@'s __first__ occurrence, or all of @hay@ if absent.
    -- The scheme separator is matched at its first occurrence so the extracted
    -- authority is the one http-client actually dials, not a later "://" inside a
    -- path or query: a crafted dist.tarball like
    -- "https://169.254.169.254/x?u=https://ok.example" must gate on 169.254.169.254
    -- (the host connected to), never on the host after the last "://".
    afterFirst :: Text -> Text -> Text
    afterFirst needle hay = fromMaybe hay (T.stripPrefix needle (snd (T.breakOn needle hay)))

    -- The text after @needle@'s last occurrence, or all of @hay@ if absent. Used for
    -- the userinfo "@" boundary, where the last "@" in the authority separates
    -- userinfo from host (matching URL parsers).
    afterLast :: Text -> Text -> Text
    afterLast needle hay =
        let (pre, post) = T.breakOnEnd needle hay
         in if T.null pre then hay else post

{- | Split a @host[:port]@ authority into its bare host and the raw @":port"@
remainder (empty when no port is present), bracket-aware so an IPv6 literal's
inner colons are never mistaken for the port separator.

The single canonical authority split feeding both the data-plane host extractor
('hostAddress') and the SQS endpoint parser ('Ecluse.Composition.parseEndpointUrl'),
so the two re-implementations the @[::1]:port@ edge cases tripped on cannot drift
again. A @[…]@ IPv6 literal is split on its closing bracket -- the host is returned
without the brackets and the remainder is whatever follows (a @":port"@ or empty) --
so an inner @::@ is never read as the port separator; a bare authority is split on
its first @':'@. An opening bracket with __no__ close is a malformed authority and
yields 'Nothing', which 'hostAddress' folds to the empty (not-allowed) host and the
endpoint parser surfaces as a malformed-URL boot error.
-}
splitHostPort :: Text -> Maybe (Text, Text)
splitHostPort authority
    | T.null authority = Nothing
    | otherwise = case T.stripPrefix "[" authority of
        Just rest -> case T.breakOn "]" rest of
            (_, "") -> Nothing -- an opening bracket with no close: malformed
            (inner, afterBracket) -> Just (inner, T.drop 1 afterBracket)
        Nothing -> case T.breakOn ":" authority of
            ("", _) -> Nothing
            (h, "") -> Just (h, "")
            (h, p) -> if p == ":" then Just (h, "") else Just (h, p)

{- | Parse a host as an IP literal, or 'Nothing' for a DNS name. Handles dotted-
quad IPv4 and the IPv6 forms a host realistically carries -- full eight-group form,
@::@-compressed forms (including @::1@), and a trailing embedded IPv4 (the
@a.b.c.d@ in @::ffff:a.b.c.d@) -- which is enough to recognise the loopback,
link-local, and IPv4-mapped addresses 'isBlockedIP' blocks. It is deliberately
__not__ a complete IPv6 parser (no zone ids); an unrecognised literal is treated
as a name, which the host allowlist still constrains.

Only range __membership__ is delegated to @iproute@ ('isBlockedIP'); recognising
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

{- | Whether a tarball may be fetched from a host that differs from the upstream
that served the packument.

An upstream's @dist.tarball@ is server-chosen data (see
@docs\/architecture\/security.md@ → "Why @dist.tarball@ is honoured"), so a
compromised or hostile upstream can name __any__ host as the artifact location.
This policy bounds the axis of that risk the host allowlist leaves open: /where/ the
bytes are fetched. Even an allowlisted-but-/different/ host is a wider fetch surface than
the packument's own source, and the safe reading of the allowlist is "same source unless
told otherwise".
-}
data TarballHostPolicy
    = {- | The secure default: a tarball is fetched only from the __same__ host
      that served the packument; a @dist.tarball@ on any other host is refused,
      even one otherwise on the allowlist.
      -}
      SameHostAsPackument
    | {- | The opt-in: a tarball may be fetched from __any allowlisted__ host (for a
      registry that legitimately serves artifacts from a separate CDN\/files host).
      This widens the fetch surface to the whole allowlist; it never escapes it or
      the internal-range block.
      -}
      AnyAllowlistedHost
    deriving stock (Eq, Show)

{- | The trust of the origin a @dist.tarball@ is being served from: the
operator-configured private upstream is 'TrustedOrigin', and the public upstream,
together with every artifact location an attacker could influence, is 'UntrustedOrigin'.

The distinction governs the __literal internal-range block__ alone (the cheap pure
defence-in-depth on the host gate). The trusted private origin is deliberately exempt
from it: a private registry may legitimately live on an internal address, and only an
untrusted target can be steered there. It never relaxes the host allowlist or the
same-host clause, which gate both origins identically, so a trusted origin's
@dist.tarball@ is still constrained to its own allowlisted host.
-}
data Origin
    = -- | The operator-configured private upstream: exempt from the literal internal-range block.
      TrustedOrigin
    | -- | The public upstream, and any attacker-influenceable target: subject to the literal internal-range block.
      UntrustedOrigin
    deriving stock (Eq, Show)

{- | Whether a @dist.tarball@ host may be fetched, given the origin's trust, the
policy, the host that served the packument, and the configured guards.

This is the policy half of the @dist.tarball@ defence; it never replaces the host
allowlist or the literal internal-range block but composes /on top/ of them, so the
answer is the conjunction of three independent checks and over-blocking is the
fail-safe:

* the @tarballHost@ must be on the host allowlist (@allowed@), as every outbound
  target is: a @dist.tarball@ host off the allowlist is refused regardless of
  policy;
* it must not be an internal-address literal (the fixed range set plus the
  operator-configured @additionalBlockedRanges@), the cheap pure defence-in-depth,
  but a 'TrustedOrigin' is __exempt__ from this clause (see 'Origin'); and
* under 'SameHostAsPackument' (the secure default) it must additionally __equal__
  the @packumentHost@ (the host that served the metadata), so a tarball on a
  /different/ host is refused even when that host is allowlisted. Under
  'AnyAllowlistedHost' that last clause is relaxed, leaving only the allowlist and
  (origin-aware) internal-range checks.

The allowlist and same-host clauses gate __both__ origins identically; only the
internal-range clause is origin-aware, so a 'TrustedOrigin' is never let past its own
allowlisted host or onto a /different/ host than its metadata under the default.

Hosts are compared by their canonical key (case-folded, and for an IP-literal the
single canonical literal; see 'canonicalHostKey'), as the host guards are. An
empty @tarballHost@ is never allowed (the allowlist already refuses it). The
@packumentHost@ is the bare host the metadata was fetched from (extract it with
'hostAddress'); only its equality to @tarballHost@ matters, so it need not itself
be re-validated here: it was already gated when the packument was fetched.
-}
tarballHostAllowed ::
    Origin ->
    TarballHostPolicy ->
    -- | The host allowlist (the same one every outbound fetch is gated by).
    LoweredHostSet ->
    {- | The operator-configured ranges extending the fixed internal-range block
    (untrusted origin).
    -}
    [IPRange] ->
    -- | The bare host that served the packument.
    Text ->
    -- | The bare host of the candidate @dist.tarball@.
    Text ->
    Bool
tarballHostAllowed origin policy allowed additionalBlockedRanges packumentHost tarballHost =
    isAllowedUpstreamHost allowed tarballHost
        && internalRangeOk
        && case policy of
            SameHostAsPackument -> canonicalHostKey tarballHost == canonicalHostKey packumentHost
            AnyAllowlistedHost -> True
  where
    -- The literal internal-range block is origin-aware: the trusted private origin is
    -- exempt, the untrusted origin is gated against the fixed set plus the operator's
    -- additional ranges.
    internalRangeOk :: Bool
    internalRangeOk = case origin of
        TrustedOrigin -> True
        UntrustedOrigin -> not (isBlockedTarget additionalBlockedRanges tarballHost)

{- | The mount-constant inputs to the per-request 'tarballHostAllowed' gate, extracted
__once__ from a mount's three configured upstream URLs so the serve path parses no URL
and builds no host set per request.

The serve-path tarball gate is on the hot artifact path (every private hit and every
public leg runs it), yet its allowlist and the private\/public upstream hosts never
change after boot -- they are fixed by the mount's configuration. Recovering them from
the base URLs on each request rebuilt a 'LoweredHostSet' and re-ran 'hostAddress' several
times per artifact; precomputing them here into a 'TarballHostGate' collapses that to a
few field reads. The only genuinely per-request host is the dynamic public
@dist.tarball@, still parsed at the call site.
-}
data TarballHostGate = TarballHostGate
    { thgAllowlist :: LoweredHostSet
    {- ^ The lowered allowlist of the mount's configured upstream hosts (public, private,
    and mirror target) -- the same set every outbound fetch is gated against
    (security.md invariant 2).
    -}
    , thgPrivateHost :: Text
    -- ^ The bare host of the private upstream, extracted once.
    , thgPublicHost :: Text
    -- ^ The bare host of the public upstream, extracted once.
    }
    deriving stock (Eq, Show)

{- | Build the 'TarballHostGate' from a mount's private, public, and mirror-target
upstream URLs: the allowlist is the lowered set of their bare hosts, and the private and
public hosts are each extracted once with 'hostAddress'. Called once per mount at the
composition root (and by test fixtures); the result is carried on the serve
dependencies so the per-request gate reads fields rather than re-parsing URLs.
-}
tarballHostGate :: Text -> Text -> Text -> TarballHostGate
tarballHostGate privateUrl publicUrl mirrorUrl =
    TarballHostGate
        { thgAllowlist = lowerCaseHosts (Set.fromList [privateHost, publicHost, hostAddress mirrorUrl])
        , thgPrivateHost = privateHost
        , thgPublicHost = publicHost
        }
  where
    privateHost = hostAddress privateUrl
    publicHost = hostAddress publicUrl
