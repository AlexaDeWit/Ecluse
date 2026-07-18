-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Outbound-request guards for the proxy's data plane: defending where the proxy fetches.

Écluse builds outbound HTTP requests from two untrusted sources -- __client-supplied
package identifiers__ (the request path) and __upstream-supplied artifact
locations__ (a packument's @dist.tarball@). This module provides the pure guard layer
that keeps the proxy from being steered by hostile input.

__Where the proxy fetches:__ 'isAllowedUpstreamHost' restricts outbound fetches
to the configured upstream @host:port@ pairs, and 'isBlockedTarget' rejects internal
address ranges (cloud instance metadata, loopback, RFC1918) that the proxy's network
position can otherwise reach. Together they are the SSRF gate: a target must be
both on the allowlist /and/ not an internal address. The two compare different
projections of a target on purpose: authorisation compares the full authority
('HostPort', the host with its effective port, 443 when none is written), because
the fetch dials the port too; the internal-range block classifies the bare host
alone, because an address is internal regardless of port.
-}
module Ecluse.Core.Security.Host (
    -- * Outbound host:port allowlist
    AllowedHostPorts,
    allowedHostPorts,
    isAllowedUpstreamHost,

    -- * Internal-range block
    isBlockedTarget,
    isBlockedIP,
    parseBlockedRange,

    -- * Tarball-host gate
    Origin (..),
    tarballHostAllowed,
    TarballHostGate (..),
    tarballHostGate,
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

import Ecluse.Core.Security.Authority (HostPort (..), hostPortAddress)
import Ecluse.Core.Security.IpLiteral (IpAddr (IpV4, IpV6), parseIpLiteral)

{- | The @host:port@ pairs the host guards authorise, each host normalised to its
canonical key.

The type is __opaque, and 'allowedHostPorts' is its only constructor__: a value of
this type therefore carries the proof that every entry is already canonicalised, so
'isAllowedUpstreamHost' canonicalises only the /incoming/ host and the match cannot
be bypassed by an un-normalised configuration set. Each entry authorises exactly
its own pair: an entry built from a URL with no explicit port authorises port 443
alone ('hostPortAddress' bakes the default in), never the same host on any other
port.
-}
newtype AllowedHostPorts = AllowedHostPorts (Set HostPort)
    deriving stock (Eq, Show)

{- | Normalise a set of configured upstream authorities to the canonical key form
the host guards take, yielding an 'AllowedHostPorts'.

A plain DNS name is folded to lower case (hostnames are case-insensitive), so the
guards match an incoming host against the configuration regardless of how either
was spelled. A host that parses as an __IP literal__ is additionally rendered to its
single canonical literal (see 'canonicalHostKey'), so equivalent spellings of one
address (compressed versus expanded IPv6, differing case) collapse to one key. An
operator who opts in @0:0:0:0:0:0:0:1@ therefore matches a literal @::1@ rather than
missing it on a textual difference. Ports are already numeric and pass through
untouched.
-}
allowedHostPorts :: Set HostPort -> AllowedHostPorts
allowedHostPorts = AllowedHostPorts . Set.map canonicalEntry
  where
    canonicalEntry (HostPort host port) = HostPort (canonicalHostKey host) port

{- | Whether @target@ dials one of the configured upstream authorities.

The first guard on every outbound fetch: the proxy talks to its configured
private\/public upstreams and mirror target, and __nothing else__ -- so a target
derived from a packument's @dist.tarball@ (or anywhere else) is fetched only if its
host __and effective port__ appear together in @allowed@. Matching the pair rather
than the host alone is load-bearing: the fetch dials the full authority, so an
allowlisted host on an attacker-chosen port (@registry.npmjs.org:9443@) is an
unauthorised target, not a variant of an authorised one. The host match is exact
and __case-insensitive__, since DNS hostnames are; ports compare numerically, and
'hostPortAddress' already folded an absent port and a written @:443@ to the same
value. An empty host is never allowed. This is the allowlist half of the SSRF
gate; pair it with 'isBlockedTarget' for the internal-range half.

The allowlist is an 'AllowedHostPorts', so it is already normalised and only the
incoming host is folded here -- through the same 'canonicalHostKey' the set was
built with, so an IP-literal entry matches regardless of how either side spells the
address.
-}
isAllowedUpstreamHost :: AllowedHostPorts -> HostPort -> Bool
isAllowedUpstreamHost (AllowedHostPorts allowed) (HostPort host port) =
    not (T.null host) && HostPort (canonicalHostKey host) port `Set.member` allowed

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
  fixed set (@ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES@) -- a deployment's own internal
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
block ('isBlockedTarget') on the @dist.tarball@ host gate. An IPv6 address that
embeds an IPv4 address is first decoded to that embedded IPv4 and tested against
the IPv4 ranges: an embedded internal literal (e.g. @::ffff:169.254.169.254@, or
its NAT64 spelling @64:ff9b::a9fe:a9fe@) is a recognised SSRF smuggling form, so
it must be caught by the IPv4 block rather than slip through as an unrelated
IPv6 address.

The decoded embeddings are exactly the fixed-prefix forms: IPv4-mapped
@::ffff:a.b.c.d@ and IPv4-compatible @::a.b.c.d@ (RFC 4291), the NAT64
well-known prefix @64:ff9b::\/96@ (RFC 6052), and the NAT64 local-use prefix
@64:ff9b:1::\/48@ (RFC 8215). An RFC 6052 network-specific translation prefix
cannot be enumerated here: it is operator-chosen from the operator's own unicast
space, so nothing in the address marks it as an embedding. An operator whose
fabric translates under such a prefix extends the block with @additionalRanges@
(@ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES@) instead.
-}
isBlockedIP :: [IPRange] -> IP -> Bool
isBlockedIP additionalRanges ip = any matches (blockedRanges <> additionalRanges)
  where
    decoded = decodeEmbeddedV4 ip
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

{- | Parse one operator-configured @ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES@ entry (a
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
embedded-IPv4 decode is left to 'isBlockedIP' ('decodeEmbeddedV4'), so an
embedding literal is carried here as the IPv6 it textually is and decoded only at
the point of the range test.
-}
ipAddrToIP :: IpAddr -> IP
ipAddrToIP = \case
    IpV4 a b c d -> IPv4 (toIPv4 (map fromIntegral [a, b, c, d]))
    IpV6 groups -> IPv6 (toIPv6 (map fromIntegral groups))

{- The canonical comparison key for a host: a normalised string the host guards
match an 'AllowedHostPorts' entry on. A host that parses as an IP literal is rendered
to the @iproute@ canonical literal through @IP@ 'show', so equivalent spellings of one
address collapse to one key: compressed versus expanded IPv6
(@::1@ is @0:0:0:0:0:0:0:1@), embedded IPv4, and hex case all canonicalise identically.
Anything that is not a literal (a DNS name) is merely case-folded, since hostnames are
case-insensitive.

This is the single canonicaliser feeding the host allowlist: 'allowedHostPorts' builds
the configured set with it, and 'isAllowedUpstreamHost' folds the queried host with
it, so a configured entry matches a literal address whichever representation either
uses. Pointing both sides at one @show@ is what guarantees they render identically;
a second, separate canonicaliser could drift.
-}
canonicalHostKey :: Text -> Text
canonicalHostKey host = case parseIpLiteral host of
    Just addr -> show (ipAddrToIP addr)
    Nothing -> T.toLower host

{- Decode an IPv6 address carrying one of the fixed-prefix IPv4 embeddings
'isBlockedIP' documents to its embedded IPv4 (the low 32 bits), so it is tested
against the IPv4 ranges; any other address is returned unchanged. Over the
sixteen octets 'fromIPv6b' yields: the IPv4-mapped form is ten zeros then
@ff ff@, the IPv4-compatible form is twelve zeros, the NAT64 well-known prefix
is @00 64 ff 9b@ then eight zeros, and the RFC 8215 local-use prefix is
@00 64 ff 9b 00 01@ with the middle six octets unconstrained (every \/96 within
the \/48 embeds in the low 32 bits). Testing an embedded internal literal
against the IPv6 ranges instead would let @::169.254.169.254@ or
@64:ff9b::a9fe:a9fe@ through (no embedding prefix falls in a blocked IPv6
range), so the decode is load-bearing for the SSRF block.
-}
decodeEmbeddedV4 :: IP -> IP
decodeEmbeddedV4 = \case
    IPv6 v6 -> case fromIPv6b v6 of
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xFF, 0xFF, a, b, c, d] ->
            IPv4 (toIPv4 [a, b, c, d])
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, b, c, d] ->
            IPv4 (toIPv4 [a, b, c, d])
        [0x00, 0x64, 0xFF, 0x9B, 0, 0, 0, 0, 0, 0, 0, 0, a, b, c, d] ->
            IPv4 (toIPv4 [a, b, c, d])
        [0x00, 0x64, 0xFF, 0x9B, 0x00, 0x01, _, _, _, _, _, _, a, b, c, d] ->
            IPv4 (toIPv4 [a, b, c, d])
        _ -> IPv6 v6
    ip -> ip

{- | The trust of the origin a @dist.tarball@ is being served from: the
operator-configured private upstream is 'TrustedOrigin', and the public upstream,
together with every artifact location an attacker could influence, is 'UntrustedOrigin'.

The distinction governs the __literal internal-range block__ alone (the cheap pure
defence-in-depth on the host gate). The trusted private origin is deliberately exempt
from it: a private registry may legitimately live on an internal address, and only an
untrusted target can be steered there. It never relaxes the host allowlist or the
same-authority clause, which gate both origins identically, so a trusted origin's
@dist.tarball@ is still constrained to its own allowlisted @host:port@ pair.
-}
data Origin
    = -- | The operator-configured private upstream: exempt from the literal internal-range block.
      TrustedOrigin
    | -- | The public upstream, and any attacker-influenceable target: subject to the literal internal-range block.
      UntrustedOrigin
    deriving stock (Eq, Show)

{- | Whether a @dist.tarball@ authority may be fetched, given the origin's trust,
the policy, the authority that served the packument, and the configured guards.

This is the policy half of the @dist.tarball@ defence; it never replaces the host
allowlist or the literal internal-range block but composes /on top/ of them, so the
answer is the conjunction of three independent checks and over-blocking is the
fail-safe:

* the target must be on the @host:port@ allowlist (@allowed@), as every outbound
  target is: a @dist.tarball@ authority off the allowlist is refused outright;
* its host must not be an internal-address literal (the fixed range set plus the
  operator-configured @additionalBlockedRanges@), the cheap pure defence-in-depth,
  but a 'TrustedOrigin' is __exempt__ from this clause (see 'Origin'); and
* it must __equal__ the packument origin's authority, host and port both -- an
  upstream's @dist.tarball@ is server-chosen data (see
  @docs\/architecture\/security.md@ → "Why @dist.tarball@ is honoured"), so a
  tarball on a /different/ host, or on the same host at a different port, is
  refused even when that pair is allowlisted. The one equivalence is the
  ecosystem's own canonical artifact hosts (@ecosystemHosts@, adapter-declared:
  npm has none, PyPI's is @files.pythonhosted.org@): a host the ecosystem serves
  artifact bytes from __by design__ passes the same-authority clause, while
  staying allowlist-gated and internal-range-gated like any other target.

The allowlist and same-authority clauses gate __both__ origins identically; only the
internal-range clause is origin-aware, so a 'TrustedOrigin' is never let past its own
allowlisted authority or onto a /different/ one than its metadata.

Hosts are compared by their canonical key (case-folded, and for an IP-literal the
single canonical literal; see 'canonicalHostKey'), as the host guards are; ports
compare numerically, with no written port meaning 443 ('hostPortAddress'). Either
side arriving as 'Nothing' -- a URL from which no dialable authority could be
extracted -- refuses the fetch: an authority the gate cannot compare is an
authority it never authorises. The packument side is the authority the metadata
was fetched from; only its equality to the target matters, so it need not itself
be re-validated here: it was already gated when the packument was fetched.
-}
tarballHostAllowed ::
    -- | The ecosystem's canonical artifact authorities, same-host-equivalent.
    AllowedHostPorts ->
    Origin ->
    -- | The @host:port@ allowlist (the same one every outbound fetch is gated by).
    AllowedHostPorts ->
    {- | The operator-configured ranges extending the fixed internal-range block
    (untrusted origin).
    -}
    [IPRange] ->
    -- | The authority that served the packument, when one could be extracted.
    Maybe HostPort ->
    -- | The authority of the candidate @dist.tarball@, when one could be extracted.
    Maybe HostPort ->
    Bool
tarballHostAllowed ecosystemHosts origin allowed additionalBlockedRanges packumentOrigin tarballTarget =
    case (packumentOrigin, tarballTarget) of
        (Just packument, Just target) ->
            isAllowedUpstreamHost allowed target
                && internalRangeOk target
                && (sameAuthority target packument || isAllowedUpstreamHost ecosystemHosts target)
        -- No comparable authority on either side authorises nothing (fail closed).
        _ -> False
  where
    -- The literal internal-range block is origin-aware: the trusted private origin is
    -- exempt, the untrusted origin is gated against the fixed set plus the operator's
    -- additional ranges. It classifies the bare host: an address is internal
    -- regardless of the port it is dialled on.
    internalRangeOk :: HostPort -> Bool
    internalRangeOk target = case origin of
        TrustedOrigin -> True
        UntrustedOrigin -> not (isBlockedTarget additionalBlockedRanges (hpHost target))

-- Whether two authorities are one dial target: equal canonical host keys and equal
-- effective ports.
sameAuthority :: HostPort -> HostPort -> Bool
sameAuthority (HostPort host port) (HostPort host' port') =
    canonicalHostKey host == canonicalHostKey host' && port == port'

{- | The mount-constant inputs to the per-request 'tarballHostAllowed' gate, extracted
__once__ from a mount's three configured upstream URLs so the serve path parses no URL
and builds no host set per request.

The serve-path tarball gate is on the hot artifact path (every private hit and every
public leg runs it), yet its allowlist and the private\/public upstream authorities
never change after boot -- they are fixed by the mount's configuration. Recovering them
from the base URLs on each request rebuilt an 'AllowedHostPorts' and re-parsed the base
authorities several times per artifact; precomputing them here into a 'TarballHostGate'
collapses that to a few field reads. The only genuinely per-request authority is the
dynamic public @dist.tarball@, still parsed at the call site.
-}
data TarballHostGate = TarballHostGate
    { thgAllowlist :: AllowedHostPorts
    {- ^ The canonicalised @host:port@ allowlist of the mount's configured upstreams
    (public, private, and mirror target) plus the ecosystem's canonical artifact
    hosts -- the same set every outbound fetch is gated against (security.md
    invariant 2). An upstream URL that writes no port contributes its host at 443.
    -}
    , thgEcosystemHosts :: AllowedHostPorts
    {- ^ The ecosystem's canonical artifact authorities (the adapter supplies them;
    npm has none, PyPI's is @files.pythonhosted.org@): hosts the ecosystem serves
    artifact bytes from __by design__, the one same-host equivalence
    'tarballHostAllowed' grants. Also folded into 'thgAllowlist', and still
    internal-range-gated like any target.
    -}
    , thgPrivateHostPort :: Maybe HostPort
    {- ^ The private upstream's authority, extracted once; 'Nothing' when the configured
    URL yields no dialable authority, which authorises nothing (fail closed).
    -}
    , thgPublicHostPort :: Maybe HostPort
    -- ^ The public upstream's authority, extracted once; same fail-closed reading.
    }
    deriving stock (Eq, Show)

{- | Build the 'TarballHostGate' from the ecosystem's canonical artifact hosts
(empty for an ecosystem, like npm, that serves artifacts from its registry host) and a
mount's private, public, and mirror-target upstream URLs: the allowlist is the
canonicalised set of their @host:port@ pairs, and the private and public authorities
are each extracted once with 'hostPortAddress'.
Called once per mount at the composition root (and by test fixtures); the result is
carried on the serve dependencies so the per-request gate reads fields rather than
re-parsing URLs. A URL from which no authority extracts contributes no allowlist entry
and leaves its reference authority 'Nothing', so a misconfigured upstream authorises
nothing rather than something unintended; an __absent__ private upstream or mirror
target (a serve-only mount) composes identically, contributing nothing.
-}
tarballHostGate :: [Text] -> Maybe Text -> Text -> Maybe Text -> TarballHostGate
tarballHostGate ecosystemHostUrls privateUrl publicUrl mirrorUrl =
    TarballHostGate
        { thgAllowlist =
            allowedHostPorts
                ( Set.fromList
                    (catMaybes ([privateHostPort, publicHostPort, hostPortAddress =<< mirrorUrl] <> map hostPortAddress ecosystemHostUrls))
                )
        , thgEcosystemHosts = allowedHostPorts (Set.fromList (mapMaybe hostPortAddress ecosystemHostUrls))
        , thgPrivateHostPort = privateHostPort
        , thgPublicHostPort = publicHostPort
        }
  where
    privateHostPort = hostPortAddress =<< privateUrl
    publicHostPort = hostPortAddress publicUrl
