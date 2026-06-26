{- | Outbound-request and response-bound guards for the proxy's data plane.

Écluse builds outbound HTTP requests from two untrusted sources — __client-supplied
package identifiers__ (the request path) and __upstream-supplied artifact
locations__ (a packument's @dist.tarball@) — and then parses whatever an upstream
returns. This module is the pure guard layer that keeps those steps from being
steered or exhausted by hostile input. It defends three boundaries:

* __Where the proxy fetches.__ 'isAllowedUpstreamHost' restricts outbound fetches
  to the configured upstream hosts, and 'isBlockedTarget' rejects internal address
  ranges (cloud instance metadata, loopback, RFC1918) that the proxy's network
  position can otherwise reach. Together they are the SSRF gate: a target must be
  both on the allowlist /and/ not an internal address.

* __How an upstream URL is derived.__ 'upstreamUrlFor' builds an artifact\/metadata
  URL from a configured base URL and an __already-parsed__ 'PackageName', never
  from raw client path segments, re-checking each name component with the router's
  own safety rule so traversal, encoded slashes, or an absolute URL cannot change
  the target.

* __How much an upstream may cost.__ A 'Limits' budget plus 'boundedRead' (abort a
  streamed body past 'maxBodyBytes') and 'checkVersionCount' \/ 'checkNestingDepth'
  (reject an oversized or deeply-nested parsed document) bound algorithmic-complexity
  DoS from a hostile or compromised upstream. Every limit __fails closed__: exceeding
  one yields 'Left', never a truncated or partial result.

The functions are pure and total; the streamed-body guard ('boundedRead') is
polymorphic over the producing monad so the streaming data plane can run it in
'IO' while tests drive it purely. They are __primitives__: the fetch and serve
layers compose them at the boundary (see @docs\/architecture\/registry-model.md@
→ "Registry Abstraction", @docs\/architecture\/web-layer.md@, and
@docs\/architecture\/hosting.md@ → "URL rewriting"). Path-component safety is
shared with the router's "Ecluse.Server.Route" ('isSafeComponent'); the threat
model these guards answer is recorded there too.
-}
module Ecluse.Security (
    -- * Outbound host allowlist
    LoweredHostSet,
    lowerCaseHosts,
    isAllowedUpstreamHost,

    -- * Internal-range block
    isBlockedTarget,
    isBlockedIP,
    hostOptedIn,
    hostAddress,
    splitHostPort,

    -- * Tarball-host policy
    TarballHostPolicy (..),
    Origin (..),
    tarballHostAllowed,

    -- * Identifier → URL safety
    upstreamUrlFor,
    UrlError (..),

    -- * Response bounds
    Limits (..),
    defaultLimits,
    LimitError (..),
    boundedRead,
    checkVersionCount,
    checkNestingDepth,
) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.IP (
    IP (IPv4, IPv6),
    IPRange (IPv4Range, IPv6Range),
    fromIPv6b,
    isMatchedTo,
    toIPv4,
    toIPv6,
 )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V

import Ecluse.Package (PackageInfo, PackageName, infoVersions, renderPackageName)
import Ecluse.Server.Route (encodeComponent, isSafeComponent)
import Ecluse.Text (joinUrlPath)

-- ── outbound host allowlist ──────────────────────────────────────────────────

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
was spelled. An entry that parses as an __IP literal__ is additionally rendered to
the single canonical literal the resolved-address recheck produces (see
'canonicalHostKey'), so equivalent spellings of one address — compressed versus
expanded IPv6, differing case — collapse to one key. An operator who opts in
@0:0:0:0:0:0:0:1@ therefore matches a resolved @::1@ rather than missing it on a
textual difference.
-}
lowerCaseHosts :: Set Text -> LoweredHostSet
lowerCaseHosts = LoweredHostSet . Set.map canonicalHostKey

{- | Whether @host@ is one of the configured upstream hosts.

The first guard on every outbound fetch: the proxy talks to its configured
private\/public upstreams and mirror target, and __nothing else__ — so a target
host derived from a packument's @dist.tarball@ (or anywhere else) is fetched only
if it appears in @allowed@. The match is exact on the bare host (no port, no
scheme — extract it with 'hostAddress' first) and __case-insensitive__, since
DNS hostnames are; an empty @host@ is never allowed. This is the allowlist half
of the SSRF gate; pair it with 'isBlockedTarget' for the internal-range half.

The allowlist is a 'LoweredHostSet', so it is already normalised and only the
incoming @host@ is folded here — through the same 'canonicalHostKey' the set was
built with, so an IP-literal entry matches regardless of how either side spells the
address.
-}
isAllowedUpstreamHost :: LoweredHostSet -> Text -> Bool
isAllowedUpstreamHost (LoweredHostSet allowed) host =
    not (T.null host) && canonicalHostKey host `Set.member` allowed

-- ── internal-range block ─────────────────────────────────────────────────────

{- | Whether @host@ is an internal address the proxy must not fetch, /unless/ it
is explicitly opted in.

A proxy sits in a privileged network position, so an attacker who can steer a
fetch (see the module header) aims it at addresses only the proxy can reach: the
cloud instance-metadata endpoint (@169.254.169.254@), loopback, or the private
network (RFC1918). This blocks, by parsing @host@ as a literal IP and testing it
against:

* __link-local__ @169.254.0.0\/16@ (which contains the @169.254.169.254@ metadata
  address) and IPv6 @fe80::\/10@;
* __loopback__ @127.0.0.0\/8@ and IPv6 @::1@;
* __unspecified \/ this-host__ @0.0.0.0\/8@ and IPv6 @::@ — @0.0.0.0@ is not a
  no-op target: on Linux a connect to it reaches a loopback-bound service, so it
  is a loopback-equivalent that must be blocked alongside @127.0.0.0\/8@;
* __RFC1918 private__ @10.0.0.0\/8@, @172.16.0.0\/12@, and @192.168.0.0\/16@;
* __CGNAT shared__ @100.64.0.0\/10@ (RFC 6598) — carrier-grade NAT space some
  cloud fabrics route internally;
* __IPv6 unique-local__ @fc00::\/7@ (RFC 4193) — the private-network IPv6 analogue,
  which contains the AWS IMDSv6 metadata endpoint @fd00:ec2::254@.

A host in @allowedInternal@ is __never__ blocked (matched case-insensitively, as
DNS and the host allowlist are) — the deliberate opt-in for a private upstream that
genuinely lives on an internal address. As a 'LoweredHostSet' it is already
lower-cased, so only the incoming @host@ is folded for the comparison. A @host@
that is not an IP literal (a DNS name) is __not__ blocked here: name-based targets
are constrained by the 'isAllowedUpstreamHost' allowlist instead, and
post-resolution IP filtering belongs to the resolving fetch layer, not this pure
check. Both guards apply — an allowlisted host that resolves to an internal literal
is still caught when its address is tested here.
-}
isBlockedTarget :: LoweredHostSet -> Text -> Bool
isBlockedTarget allowedInternal host =
    not (hostOptedIn allowedInternal host)
        && maybe False (isBlockedIP . ipAddrToIP) (parseIpLiteral host)

{- | Whether @host@ is opted in to the internal-range block — the deliberate
exemption for a private upstream that genuinely lives on an internal address.

The opt-in half of 'isBlockedTarget', shared with the resolved-address recheck in
"Ecluse.Security.Egress" so the literal block and the connection-time block honour
the exemption identically. The match folds case (as DNS and the host allowlist do)
and, for an IP-literal, collapses equivalent spellings to one canonical key (see
'canonicalHostKey'): the @allowedInternal@ set was built with that same key, so an
opt-in matches a resolved or literal address whichever representation either uses.
-}
hostOptedIn :: LoweredHostSet -> Text -> Bool
hostOptedIn (LoweredHostSet allowedInternal) host =
    canonicalHostKey host `Set.member` allowedInternal

{- | Whether an 'IP' falls in a blocked internal range.

The single source of record for the internal-range decision, shared by the
literal block here ('isBlockedTarget') and the resolved-address recheck in
"Ecluse.Security.Egress" so both gate against __identical__ ranges. An
IPv4-mapped IPv6 address (@::ffff:a.b.c.d@) is first decoded to its embedded IPv4
and tested against the IPv4 ranges: a mapped internal literal (e.g.
@::ffff:169.254.169.254@) is a recognised SSRF smuggling form, so it must be
caught by the IPv4 block rather than slip through as an unrelated IPv6 address.
-}
isBlockedIP :: IP -> Bool
isBlockedIP ip = any matches blockedRanges
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
AWS IMDSv6 endpoint @fd00:ec2::254@.
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
@iproute@ canonical literal — the /same/ form the resolved-address recheck in
"Ecluse.Security.Egress" renders a connected address to, since both go through this
@IP@ 'show' — so equivalent spellings of one address collapse to one key:
compressed versus expanded IPv6 (@::1@ ≡ @0:0:0:0:0:0:0:1@), embedded IPv4, and
hex case all canonicalise identically. Anything that is not a literal (a DNS name)
is merely case-folded, since hostnames are case-insensitive.

This is the single canonicaliser feeding __both__ sides of the internal-range
opt-in: 'lowerCaseHosts' builds the set with it, and 'hostOptedIn' folds the
queried host with it, so an operator's opt-in matches a resolved or literal address
whichever representation either uses. Pointing the opt-in key and the guard's
rendered key at one @show@ is what guarantees they are identical — a second,
separate canonicaliser could drift.
-}
canonicalHostKey :: Text -> Text
canonicalHostKey host = case parseIpLiteral host of
    Just addr -> show (ipAddrToIP addr)
    Nothing -> T.toLower host

{- Decode an IPv4-mapped IPv6 address (@::ffff:a.b.c.d@) to its embedded IPv4, so
it is tested against the IPv4 ranges; any other address is returned unchanged.
Over the sixteen octets 'fromIPv6b' yields, the mapped form is ten zero octets,
then @ff ff@, then the four IPv4 octets. Testing a mapped internal literal against
the IPv6 ranges instead would let @::ffff:169.254.169.254@ through, so the decode
is load-bearing for the SSRF block.
-}
decodeMappedV4 :: IP -> IP
decodeMappedV4 = \case
    IPv6 v6 -> case fromIPv6b v6 of
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xFF, 0xFF, a, b, c, d] ->
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
brackets — the bracket-aware @host[:port]@ split is 'splitHostPort', shared with
the SQS endpoint parser so the two cannot drift on an authority edge case; a
malformed authority (an opening bracket with no close) yields the empty string,
the same fail-safe the guards apply to it.
-}
hostAddress :: Text -> Text
hostAddress raw =
    let afterScheme = afterLast "://" raw
        authority = T.takeWhile (`notElem` ['/', '?', '#']) afterScheme
        afterUserinfo = afterLast "@" authority
     in T.toLower (maybe "" fst (splitHostPort afterUserinfo))
  where
    -- The text after @needle@'s last occurrence, or all of @hay@ if absent.
    -- ('T.breakOnEnd' yields @(hay, "")@ when the needle is absent — its prefix
    -- is non-empty exactly when the needle was found, since it includes it.)
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
again. A @[…]@ IPv6 literal is split on its closing bracket — the host is returned
without the brackets and the remainder is whatever follows (a @":port"@ or empty) —
so an inner @::@ is never read as the port separator; a bare authority is split on
its first @':'@. An opening bracket with __no__ close is a malformed authority and
yields 'Nothing', which 'hostAddress' folds to the empty (not-allowed) host and the
endpoint parser surfaces as a malformed-URL boot error.
-}
splitHostPort :: Text -> Maybe (Text, Text)
splitHostPort authority = case T.stripPrefix "[" authority of
    Just rest -> case T.breakOn "]" rest of
        (_, "") -> Nothing -- an opening bracket with no close: malformed
        (inner, afterBracket) -> Just (inner, T.drop 1 afterBracket)
    Nothing -> Just (T.breakOn ":" authority)

{- Parse a host as an IP literal, or 'Nothing' for a DNS name. Handles dotted-
quad IPv4 and the IPv6 forms a host realistically carries — full eight-group form,
@::@-compressed forms (including @::1@), and a trailing embedded IPv4 (the
@a.b.c.d@ in @::ffff:a.b.c.d@) — which is enough to recognise the loopback,
link-local, and IPv4-mapped addresses 'isBlockedIP' blocks. It is deliberately
__not__ a complete IPv6 parser (no zone ids); an unrecognised literal is treated
as a name, which the host allowlist still constrains.

Only range __membership__ is delegated to @iproute@ ('isBlockedIP'); recognising
the literal stays hand-rolled __on purpose__. This recogniser is deliberately
__lenient__ — it accepts ambiguous bypass spellings a strict IP library rejects,
notably leading-zero octets (@0127.0.0.1@, @010.0.0.1@), and __blocks__ them by
parsing them as the address they coerce to on a typical resolver. A stricter
parser would reject those spellings as non-literals, so they would skip the
internal-range block and reach the resolving fetch as names — silently
__narrowing__ the SSRF gate. Conversely a malformed group that overflows 16 bits
(@fe80::1ffff@) is __not__ a literal here, so it stays a name the allowlist
constrains. Delegating literal /parsing/ to a library would change both of these,
so it is kept here.
-}
parseIpLiteral :: Text -> Maybe IpAddr
parseIpLiteral host = case T.uncons host of
    Nothing -> Nothing -- empty host: not a literal
    Just _ -> if T.any (== ':') host then parseIPv6 host else parseIPv4 host

-- Parse a strict dotted-quad @a.b.c.d@ with each octet in @0..255@.
parseIPv4 :: Text -> Maybe IpAddr
parseIPv4 host = case T.splitOn "." host of
    [a, b, c, d] -> IpV4 <$> octet a <*> octet b <*> octet c <*> octet d
    _ -> Nothing
  where
    -- An octet is a non-empty all-decimal run in 0..255. The digit check keeps
    -- 'readMaybe' from accepting signs/whitespace, so a parsed value is >= 0.
    octet :: Text -> Maybe Word8
    octet t = do
        n <- if isDecimal t then readMaybe (toString t) else Nothing :: Maybe Integer
        if n <= 255 then Just (fromInteger n) else Nothing

{- Parse an IPv6 literal — either the full eight-group form or a @::@-compressed
form (at most one @::@), optionally ending in an embedded dotted-quad IPv4 — into
its eight 16-bit groups. Enough to recognise the @::1@, @fe80::\/10@, and
@::ffff:0:0\/96@ addresses we block; rejects anything malformed.
-}
parseIPv6 :: Text -> Maybe IpAddr
parseIPv6 host =
    case T.splitOn "::" host of
        [single] -> exactly8 =<< groupsOf single
        [before, after] -> do
            hd <- groupsOf before
            tl <- groupsOf after
            -- "::" stands for at least one all-zero group, so the explicit groups
            -- on either side must total at most 7 (leaving room to fill to 8).
            let present = length hd + length tl
            if present <= 7
                then Just (IpV6 (hd <> replicate (8 - present) 0 <> tl))
                else Nothing
        _ -> Nothing -- more than one "::" is illegal
  where
    -- The colon-separated groups of one side; "" → no groups. The final token
    -- may be a dotted-quad IPv4 (RFC 4291 §2.2.3, e.g. the @169.254.169.254@ in
    -- @::ffff:169.254.169.254@), which expands to its two 16-bit groups so an
    -- IPv4-mapped literal in its canonical dotted form is decoded rather than
    -- mistaken for a name. Only the last token may be dotted; an interior dotted
    -- token fails 'group16' (no hex '.') and the whole parse is rejected.
    groupsOf :: Text -> Maybe [Word16]
    groupsOf t
        | T.null t = Just []
        | otherwise = groups (T.splitOn ":" t)

    groups :: [Text] -> Maybe [Word16]
    groups [] = Just []
    groups [tok]
        | T.any (== '.') tok = embeddedV4 tok
        | otherwise = (: []) <$> group16 tok
    groups (tok : rest) = (:) <$> group16 tok <*> groups rest

    -- A trailing dotted-quad IPv4 as its two 16-bit groups (high pair, low pair).
    embeddedV4 :: Text -> Maybe [Word16]
    embeddedV4 t = case parseIPv4 t of
        Just (IpV4 a b c d) -> Just [pair a b, pair c d]
        _ -> Nothing
      where
        pair hi lo = fromIntegral hi * 256 + fromIntegral lo

    -- A group is a non-empty all-hex run that fits in 16 bits. The hex check
    -- keeps 'readMaybe' from accepting signs, so a parsed value is >= 0.
    group16 :: Text -> Maybe Word16
    group16 t = do
        n <- if isHex t then readMaybe ("0x" <> toString t) else Nothing :: Maybe Integer
        if n <= 0xFFFF then Just (fromInteger n) else Nothing

    exactly8 :: [Word16] -> Maybe IpAddr
    exactly8 gs = if length gs == 8 then Just (IpV6 gs) else Nothing

-- Whether @t@ is a non-empty run of decimal digits (no sign or whitespace).
isDecimal :: Text -> Bool
isDecimal t = not (T.null t) && T.all (`elem` ['0' .. '9']) t

-- Whether @t@ is a non-empty run of hexadecimal digits.
isHex :: Text -> Bool
isHex t = not (T.null t) && T.all isHexDigit t
  where
    isHexDigit c = c `elem` (['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F'])

-- ── tarball-host policy ───────────────────────────────────────────────────────

{- | Whether a tarball may be fetched from a host that differs from the upstream
that served the packument.

An upstream's @dist.tarball@ is server-chosen data (see
@docs\/architecture\/security.md@ → "Why @dist.tarball@ is honoured"), so a
compromised or hostile upstream can name __any__ host as the artifact location.
This policy bounds the third axis of that risk — /where/ the bytes are fetched —
that the host allowlist and the resolved-IP block leave open: even an
allowlisted-but-/different/ host is a wider fetch surface than the packument's own
source, and the safe reading of the allowlist is "same source unless told
otherwise".
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

{- | The trust of the origin a @dist.tarball@ is being served from, mirroring the
connection-layer trust split (see "Ecluse.Security.Egress"): the operator-configured
private upstream is 'TrustedOrigin', and the public upstream — together with every
artifact location an attacker could influence — is 'UntrustedOrigin'.

The distinction governs the __internal-range block__ alone. The trusted private
origin is deliberately exempt from it (a private registry may legitimately live on
an internal address, and only an untrusted target can be steered there), exactly as
the trusted origin's connections use the unguarded 'Ecluse.Security.Egress.newTrustedTlsManager'
while untrusted ones carry the resolved-IP recheck of
'Ecluse.Security.Egress.newGuardedTlsManager' (@security.md@ invariant 3). It never
relaxes the host allowlist or the same-host clause — those gate both origins
identically — so a trusted origin's @dist.tarball@ is still constrained to its own
allowlisted host.
-}
data Origin
    = -- | The operator-configured private upstream: exempt from the internal-range block.
      TrustedOrigin
    | {- | The public upstream, and any attacker-influenceable target: subject to the
      internal-range block (and the resolved-IP recheck at connect time).
      -}
      UntrustedOrigin
    deriving stock (Eq, Show)

{- | Whether a @dist.tarball@ host may be fetched, given the origin's trust, the
policy, the host that served the packument, and the configured guards.

This is the policy half of the @dist.tarball@ defence; it never replaces the host
allowlist or the internal-range block but composes /on top/ of them, so the
answer is the conjunction of three independent checks and over-blocking is the
fail-safe:

* the @tarballHost@ must be on the host allowlist (@allowed@), as every outbound
  target is — a @dist.tarball@ host off the allowlist is refused regardless of
  policy;
* it must not be an internal address (subject to the per-host @allowedInternal@
  opt-in), as every untrusted outbound target is — but a 'TrustedOrigin' is __exempt__
  from this clause (its connections likewise carry no resolved-IP recheck; see
  'Origin' and @security.md@ invariant 3); and
* under 'SameHostAsPackument' (the secure default) it must additionally __equal__
  the @packumentHost@ — the host that served the metadata — so a tarball on a
  /different/ host is refused even when that host is allowlisted. Under
  'AnyAllowlistedHost' that last clause is relaxed, leaving only the allowlist and
  (origin-aware) internal-range checks.

The allowlist and same-host clauses gate __both__ origins identically; only the
internal-range clause is origin-aware, so a 'TrustedOrigin' is never let past its own
allowlisted host or onto a /different/ host than its metadata under the default.

Hosts are compared by their canonical key (case-folded, and for an IP-literal the
single canonical literal — see 'canonicalHostKey'), as the host guards are. An
empty @tarballHost@ is never allowed (the allowlist already refuses it). The
@packumentHost@ is the bare host the metadata was fetched from (extract it with
'hostAddress'); only its equality to @tarballHost@ matters, so it need not itself
be re-validated here — it was already gated when the packument was fetched.
-}
tarballHostAllowed ::
    Origin ->
    TarballHostPolicy ->
    -- | The host allowlist (the same one every outbound fetch is gated by).
    LoweredHostSet ->
    -- | The hosts deliberately opted in to the internal-range block (untrusted origin).
    LoweredHostSet ->
    -- | The bare host that served the packument.
    Text ->
    -- | The bare host of the candidate @dist.tarball@.
    Text ->
    Bool
tarballHostAllowed origin policy allowed allowedInternal packumentHost tarballHost =
    isAllowedUpstreamHost allowed tarballHost
        && internalRangeOk
        && case policy of
            SameHostAsPackument -> canonicalHostKey tarballHost == canonicalHostKey packumentHost
            AnyAllowlistedHost -> True
  where
    -- The internal-range block is origin-aware: the trusted private origin is exempt
    -- (mirroring its unguarded connection manager), the untrusted origin is gated
    -- subject to the per-host opt-in.
    internalRangeOk :: Bool
    internalRangeOk = case origin of
        TrustedOrigin -> True
        UntrustedOrigin -> not (isBlockedTarget allowedInternal tarballHost)

-- ── identifier → URL safety ──────────────────────────────────────────────────

-- | Why building an upstream URL from an identifier was refused.
data UrlError
    = {- | A name component (scope or base name) is unsafe to interpolate — see
      'Ecluse.Server.Route.isSafeComponent'. Carries the offending component.
      -}
      UnsafeComponent Text
    | -- | The configured base URL is empty, so no URL can be formed.
      EmptyBaseUrl
    deriving stock (Eq, Show)

{- | Build an upstream URL for a package from a configured base URL and an
__already-parsed__ 'PackageName'.

This is the only sanctioned way to derive an upstream URL for a package: the
target is @{baseUrl}\/{path}@, where @path@ is built from the package's structural
components and @baseUrl@ is __configuration__, never a client-supplied path. The
client never chooses the host or the path prefix — only which (validated) package
— so @..\/@ traversal, an encoded slash, an absolute URL, or a CRLF in the original
request cannot steer the fetch elsewhere (see the module header).

The path is built with two complementary defences. First, although a 'PackageName'
is normally produced by the router's already-safe parse, its smart constructor does
no validation, so this __re-checks every structural component__ (scope and base
name) with the router's own 'Ecluse.Server.Route.isSafeComponent' — a name carrying
a @\'\/\'@, @\'\\\\\'@, control character, or a @"."@\/@".."@ component is refused
with 'UnsafeComponent' rather than interpolated. Second, each accepted component is
then __percent-encoded__ ('Ecluse.Server.Route.encodeComponent') around the
structural @\'\@\'@ sigil and @%2F@ scope separator this builder writes — so a
@\'%\'@, @\'?\'@, @\'#\'@, or other reserved byte the denylist accepts (notably a
once-decoded @%2e%2e%2f@) cannot reach the upstream URL raw. A scoped
@\@scope\/name@ therefore yields exactly one @%2F@ (the separator written here, not
an encoding of a component), with no double-encoding. An empty @baseUrl@ is refused
with 'EmptyBaseUrl'. A single trailing slash on @baseUrl@ is tolerated so the join
never doubles it.
-}
upstreamUrlFor :: Text -> PackageName -> Either UrlError Text
upstreamUrlFor baseUrl name
    | T.null baseUrl = Left EmptyBaseUrl
    | otherwise = case firstUnsafe (componentsOf parts) of
        Just bad -> Left (UnsafeComponent bad)
        Nothing -> Right (joinUrlPath baseUrl (encodePath parts))
  where
    parts = nameParts name

    firstUnsafe :: [Text] -> Maybe Text
    firstUnsafe = find (not . isSafeComponent)

{- The structural decomposition of a package name for URL building: a scope with a
base name, or a single component, recovered by splitting the rendered name on the
@\'\/\'@ scope separator so a legitimate scoped name's own separator is not judged
as unsafe content. One source of truth for both the safety re-check ('componentsOf')
and the encoded path ('encodePath'), so the two cannot disagree about where the
component boundaries are. A leading @\'\@\'@ with no @\'\/\'@ (or an empty base) is
a single component (the @\@foo@ fallback).
-}
data NameParts
    = -- A scoped name: scope and base, each a component to check and encode.
      Scoped Text Text
    | -- An unscoped (or @\@@-leading, separator-free) name: one component.
      Single Text

nameParts :: PackageName -> NameParts
nameParts name =
    let rendered = renderPackageName name
     in case T.stripPrefix "@" rendered of
            Just scopeAndBase ->
                let (scope, base) = T.breakOn "/" scopeAndBase
                 in if T.null base
                        then Single rendered
                        else Scoped scope (T.drop 1 base)
            Nothing -> Single rendered

-- The components each of which must independently pass 'isSafeComponent'.
componentsOf :: NameParts -> [Text]
componentsOf = \case
    Scoped scope base -> [scope, base]
    Single c -> [c]

{- The encoded on-the-wire path for the components: the @\'\@\'@ sigil and the
@%2F@ scope separator are written here, around each percent-encoded component, so a
legitimate scoped name carries exactly one @%2F@ and no component byte can alter the
URL's shape. -}
encodePath :: NameParts -> Text
encodePath = \case
    Scoped scope base -> "@" <> encodeComponent scope <> "%2F" <> encodeComponent base
    Single c -> encodeComponent c

-- ── response bounds ──────────────────────────────────────────────────────────

{- | Resource budget for a single upstream response. Every field is a hard
ceiling enforced fail-closed: exceeding one aborts with a 'LimitError' rather
than returning a truncated or partially-parsed result. These bound the
algorithmic-complexity DoS a hostile or compromised upstream can inflict by
returning a huge or pathological document.
-}
data Limits = Limits
    { maxBodyBytes :: Int
    {- ^ Largest response body, in bytes, 'boundedRead' will accumulate before
    aborting. Bounds memory on the metadata path (artifacts are streamed, not
    buffered).
    -}
    , maxVersionCount :: Int
    {- ^ Largest number of versions a parsed packument may carry
    ('checkVersionCount'); bounds per-version rule evaluation.
    -}
    , maxNestingDepth :: Int
    {- ^ Deepest JSON nesting a decoded document may reach ('checkNestingDepth');
    bounds stack\/CPU on pathologically nested input.
    -}
    }
    deriving stock (Eq, Show)

{- | Sane defaults for 'Limits'. Generous enough for real registry documents and
tight enough to fail closed on pathological input: a 16 MiB metadata body, 100k
versions, and 64 levels of JSON nesting. Override per deployment as needed.
-}
defaultLimits :: Limits
defaultLimits =
    Limits
        { maxBodyBytes = 16 * 1024 * 1024
        , maxVersionCount = 100_000
        , maxNestingDepth = 64
        }

-- | Which 'Limits' ceiling a response exceeded.
data LimitError
    = -- | The body exceeded 'maxBodyBytes'; carries the configured ceiling.
      BodyTooLarge Int
    | {- | The packument carried more than 'maxVersionCount' versions; carries the
      count seen and the ceiling.
      -}
      TooManyVersions Int Int
    | -- | JSON nesting exceeded 'maxNestingDepth'; carries the ceiling.
      TooDeeplyNested Int
    deriving stock (Eq, Show)

{- | Read a streamed body chunk-by-chunk, aborting as soon as the accumulated
size would exceed 'maxBodyBytes'. Polymorphic over the producing monad so the
streaming fetch can run it in 'IO' while tests drive it purely.

@readChunk@ is a chunk producer following the @http-client@ @BodyReader@ contract:
each call yields the next chunk, and an __empty__ 'ByteString' signals end of
input. 'boundedRead' pulls chunks until EOF and returns the concatenated body, or
stops at the first chunk that pushes the running total past 'maxBodyBytes' and
returns @'Left' ('BodyTooLarge' …)@ — __fail-closed__, never a truncated body. A
zero or negative 'maxBodyBytes' rejects any non-empty body. The bound is checked
__before__ a chunk is retained, so memory never exceeds the limit plus one chunk.
-}
boundedRead :: (Monad m) => Limits -> m ByteString -> m (Either LimitError ByteString)
boundedRead limits readChunk = go 0 []
  where
    cap = maxBodyBytes limits
    go !seen acc = do
        chunk <- readChunk
        if BS.null chunk
            then pure (Right (BS.concat (reverse acc)))
            else
                let seen' = seen + BS.length chunk
                 in if seen' > cap
                        then pure (Left (BodyTooLarge cap))
                        else go seen' (chunk : acc)

{- | Reject a parsed packument carrying more than 'maxVersionCount' versions,
returning it unchanged when within budget.

Applied after a document is projected to 'Ecluse.Package.PackageInfo' but before
per-version rule evaluation, so the cost of evaluating rules over every version is
bounded by configuration rather than by what an upstream returns. Counts the
'Ecluse.Package.infoVersions' map; on breach returns @'Left' ('TooManyVersions'
count cap)@, otherwise the document unchanged so it threads through a parse
pipeline.
-}
checkVersionCount :: Limits -> PackageInfo -> Either LimitError PackageInfo
checkVersionCount limits info =
    if count > cap
        then Left (TooManyVersions count cap)
        else Right info
  where
    cap = maxVersionCount limits
    count = Map.size (infoVersions info)

{- | Reject a decoded JSON document nested deeper than 'maxNestingDepth',
returning it unchanged when within budget.

Run on the __already-decoded__ 'Value' — after the parser has produced it, before
the document is projected to domain types — so a pathologically nested payload is
refused before any deep /domain/ traversal. It is therefore __not__ the defence
against an unbounded structure: the structure is already /bounded-by-body-size/ by
the time it reaches here, since the @maxBodyBytes@ cap on the streamed read precedes
the decode (a body the parser never finishes reading never produces a 'Value'). This
guard bounds the __traversal cost__ of a within-size-but-deeply-nested document — the
stack\/CPU a recursive walk of it would spend — which the body cap alone does not
bound (a small body can still nest deeply). Depth counts container nesting: a scalar
is depth @1@, and each enclosing 'Object'\/'Array' adds one. An empty container
counts as a leaf (depth @1@), since it forces no descent. Traversal short-circuits at
the first sub-tree to breach the ceiling, so a deeply-nested branch costs no more than
the ceiling to reject.
-}
checkNestingDepth :: Limits -> Value -> Either LimitError Value
checkNestingDepth limits value =
    if within cap value
        then Right value
        else Left (TooDeeplyNested cap)
  where
    cap = maxNestingDepth limits

    -- True iff @v@ fits within @budget@ remaining levels. Decrement per nested
    -- container and fail fast at zero, so a huge subtree is not fully walked.
    within :: Int -> Value -> Bool
    within budget v =
        budget >= 1 && case v of
            Object o -> all (within (budget - 1)) (KeyMap.elems o)
            Array xs -> V.all (within (budget - 1)) xs
            String _ -> True
            Number _ -> True
            Bool _ -> True
            Null -> True
