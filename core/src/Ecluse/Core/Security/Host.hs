-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Outbound-request guards for the data plane: where the proxy is allowed to fetch.

An outbound target comes from the client's request path or an upstream's @dist.tarball@, and
must pass both halves of the SSRF gate: 'isAllowedUpstreamHost' restricts a fetch to the
configured upstream @host:port@ pairs, and 'isBlockedTarget' rejects internal address
literals. They compare different projections on purpose. Authorisation compares the full
authority, because the fetch dials the port too, while the block classifies the bare host,
because an address is internal at any port.
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

    -- * Artifact-host gate
    Origin (..),
    tarballHostAllowed,
    artifactAuthorityHonoured,
    ecosystemArtifactAuthorities,
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

{- | The @host:port@ pairs the host guards authorise, canonicalised by 'allowedHostPorts', its
only constructor. An entry authorises exactly its own pair, port 443 when none was written.
-}
newtype AllowedHostPorts = AllowedHostPorts (Set HostPort)
    deriving stock (Eq, Show)

{- | Normalise configured upstream authorities to the key form the guards match on. Equivalent
spellings of one IP literal collapse, so an operator's @0:0:0:0:0:0:0:1@ matches an incoming @::1@.
-}
allowedHostPorts :: Set HostPort -> AllowedHostPorts
allowedHostPorts = AllowedHostPorts . Set.map canonicalEntry
  where
    canonicalEntry (HostPort host port) = HostPort (canonicalHostKey host) port

{- | The allowlist half of the SSRF gate. Matching the pair is load-bearing: an allowlisted
host on an attacker-chosen port (@registry.npmjs.org:9443@) is an unauthorised target.
-}
isAllowedUpstreamHost :: AllowedHostPorts -> HostPort -> Bool
isAllowedUpstreamHost (AllowedHostPorts allowed) (HostPort host port) =
    not (T.null host) && HostPort (canonicalHostKey host) port `Set.member` allowed

{- | Whether @host@ is an internal-address literal the proxy must not fetch. A DNS name is
not blocked here: the allowlist and the validating-TLS manager close that class.
-}
isBlockedTarget :: [IPRange] -> Text -> Bool
isBlockedTarget additionalRanges host =
    maybe False (isBlockedIP additionalRanges . ipAddrToIP) (parseIpLiteral host)

{- | Whether an 'IP' falls in a blocked internal range. An IPv6 address embedding an IPv4 one
decodes first (see 'decodeEmbeddedV4'), so an embedding literal cannot slip the IPv4 ranges.
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

-- An operator cannot narrow this fixed set, only extend it through the @additionalRanges@
-- 'isBlockedIP' also consults.
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

{- | Parse one operator-configured CIDR entry (@"203.0.113.0\/24"@) into an 'IPRange'. It goes
through @iproute@'s total 'Read', not its partial 'IsString', so a malformed entry fails closed.
-}
parseBlockedRange :: Text -> Maybe IPRange
parseBlockedRange = readMaybe . toString

-- The embedded-IPv4 decode stays with 'isBlockedIP', so an embedding literal rides through
-- here as the IPv6 it textually is.
ipAddrToIP :: IpAddr -> IP
ipAddrToIP = \case
    IpV4 a b c d -> IPv4 (toIPv4 (map fromIntegral [a, b, c, d]))
    IpV6 groups -> IPv6 (toIPv6 (map fromIntegral groups))

-- An IP literal renders through @iproute@'s 'show', so equivalent spellings collapse, and a
-- DNS name is only case-folded. Both guards fold through here, so neither can drift.
canonicalHostKey :: Text -> Text
canonicalHostKey host = case parseIpLiteral host of
    Just addr -> show (ipAddrToIP addr)
    Nothing -> T.toLower host

{- Decode the IPv4-mapped, IPv4-compatible, and NAT64 embeddings. No embedding prefix falls in
a blocked IPv6 range, so without this @::169.254.169.254@ would pass the SSRF block. -}
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

{- | The trust of the origin a @dist.tarball@ comes from. It governs the literal internal-range
block alone, since a private registry may live on an internal address.
-}
data Origin
    = -- | The operator-configured private upstream: exempt from the literal internal-range block.
      TrustedOrigin
    | -- | The public upstream, and any attacker-influenceable target: subject to the literal internal-range block.
      UntrustedOrigin
    deriving stock (Eq, Show)

{- | Whether a @dist.tarball@ authority may be fetched. An upstream's @dist.tarball@ is
server-chosen data, so the target must equal the packument authority, @ecosystemHosts@ aside.
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
    artifactAuthorityHonoured ecosystemHosts packumentOrigin tarballTarget
        -- A target no authority extracts from is unfetchable, so it authorises nothing.
        && maybe False fetchable tarballTarget
  where
    fetchable target =
        isAllowedUpstreamHost allowed target && internalRangeOk origin additionalBlockedRanges target

-- The block classifies the bare host, since an address is internal whatever port it is
-- dialled on. The trusted private origin is exempt (see 'Origin').
internalRangeOk :: Origin -> [IPRange] -> HostPort -> Bool
internalRangeOk origin additionalBlockedRanges target = case origin of
    TrustedOrigin -> True
    UntrustedOrigin -> not (isBlockedTarget additionalBlockedRanges (hpHost target))

{- | Whether an artifact's authority is honoured for a document the given authority served: the
same dial target, or one the ecosystem serves artifact bytes from by design.
-}
artifactAuthorityHonoured :: AllowedHostPorts -> Maybe HostPort -> Maybe HostPort -> Bool
artifactAuthorityHonoured ecosystemHosts packumentOrigin artifactTarget =
    case (packumentOrigin, artifactTarget) of
        (Just packument, Just target) ->
            sameAuthority target packument || isAllowedUpstreamHost ecosystemHosts target
        _ -> False

{- | The authority set of an ecosystem's declared artifact hosts, which the gate and each
adapter's projection both derive 'artifactAuthorityHonoured''s first argument through.
-}
ecosystemArtifactAuthorities :: [Text] -> AllowedHostPorts
ecosystemArtifactAuthorities = allowedHostPorts . Set.fromList . mapMaybe hostPortAddress

-- Whether two authorities are one dial target: equal canonical host keys and equal
-- effective ports.
sameAuthority :: HostPort -> HostPort -> Bool
sameAuthority (HostPort host port) (HostPort host' port') =
    canonicalHostKey host == canonicalHostKey host' && port == port'

{- | The mount-constant inputs to the per-request 'tarballHostAllowed' gate. The gate runs on
the hot artifact path, so only the dynamic public @dist.tarball@ authority is parsed per request.
-}
data TarballHostGate = TarballHostGate
    { thgAllowlist :: AllowedHostPorts
    {- ^ The mount's configured upstreams plus the ecosystem's artifact hosts, the same set
    every outbound fetch is gated against. A URL that writes no port contributes its host at 443.
    -}
    , thgEcosystemHosts :: AllowedHostPorts
    {- ^ The adapter's artifact authorities (npm has none, PyPI's is @files.pythonhosted.org@):
    the one same-host equivalence, still internal-range-gated like any target.
    -}
    , thgPrivateHostPort :: Maybe HostPort
    -- ^ The private upstream's authority. 'Nothing' authorises nothing (fail closed).
    , thgPublicHostPort :: Maybe HostPort
    -- ^ The public upstream's authority, with the same fail-closed reading.
    }
    deriving stock (Eq, Show)

{- | Build the gate from the ecosystem's artifact hosts and a mount's private, public, and
mirror-target URLs. A URL no authority extracts from authorises nothing (fail closed).
-}
tarballHostGate :: [Text] -> Maybe Text -> Text -> Maybe Text -> TarballHostGate
tarballHostGate ecosystemHostUrls privateUrl publicUrl mirrorUrl =
    TarballHostGate
        { thgAllowlist =
            allowedHostPorts
                ( Set.fromList
                    (catMaybes ([privateHostPort, publicHostPort, hostPortAddress =<< mirrorUrl] <> map hostPortAddress ecosystemHostUrls))
                )
        , thgEcosystemHosts = ecosystemArtifactAuthorities ecosystemHostUrls
        , thgPrivateHostPort = privateHostPort
        , thgPublicHostPort = publicHostPort
        }
  where
    privateHostPort = hostPortAddress =<< privateUrl
    publicHostPort = hostPortAddress publicUrl
