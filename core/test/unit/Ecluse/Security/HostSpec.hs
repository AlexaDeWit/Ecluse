-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Security.HostSpec (spec) where

import Data.IP (IPRange)
import Data.Set qualified as Set

import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Security (
    AllowedHostPorts,
    HostPort (HostPort, hpHost),
    Origin (TrustedOrigin, UntrustedOrigin),
    TarballHostPolicy (..),
    allowedHostPorts,
    hostAddress,
    hostPortAddress,
    isAllowedUpstreamHost,
    isBlockedTarget,
    isDecimal,
    parseBlockedRange,
    parseIpLiteral,
    splitHostPort,
    tarballHostAllowed,
    tarballHostAllowedFor,
    tarballHostGate,
    thgAllowlist,
    thgEcosystemHosts,
 )

{- | The raw configured upstream authorities (lower-/mixed-case on purpose), before
normalisation. Kept unwrapped so a case can extend it (e.g. the SSRF gate's
allowlisted-internal case) before normalising it through 'allowedHostPorts'. Every
entry is portless, so each authorises its host on 443 alone.
-}
upstreamHosts :: Set.Set HostPort
upstreamHosts = Set.fromList [hp "registry.npmjs.org", hp "Private.Internal.Example.com"]

{- | The configured upstreams, normalised through 'allowedHostPorts' -- the only way
to obtain the 'AllowedHostPorts' the host guards take.
-}
upstreams :: AllowedHostPorts
upstreams = allowedHostPorts upstreamHosts

-- | An authority on the https default port: what a URL with no written port dials.
hp :: Text -> HostPort
hp host = HostPort host 443

-- | An authority on an explicit port.
hpAt :: Text -> Word16 -> HostPort
hpAt = HostPort

spec :: Spec
spec = do
    hostAllowlistSpec
    internalRangeSpec
    classificationCorpusSpec
    hostAddressSpec
    hostPortAddressSpec
    splitHostPortSpec
    ssrfGateSpec
    tarballHostPolicySpec
    ecosystemHostSpec
    allowedHostPortsSpec
    propertiesSpec
    parseBlockedRangeSpec
    isDecimalSpec
    parseIpLiteralSpec

isDecimalSpec :: Spec
isDecimalSpec = describe "isDecimal" $ do
    it "returns True for a string with only decimal digits" $
        isDecimal "1234567890" `shouldBe` True
    it "returns False for an empty string" $
        isDecimal "" `shouldBe` False
    it "returns False for a string with spaces" $
        isDecimal "123 456" `shouldBe` False
    it "returns False for a string with letters" $
        isDecimal "123a456" `shouldBe` False
    it "returns False for a string with a sign" $
        isDecimal "-123" `shouldBe` False
    it "returns False for a string with a decimal point" $
        isDecimal "123.456" `shouldBe` False
    it "returns True for a single digit" $
        isDecimal "0" `shouldBe` True

hostAllowlistSpec :: Spec
hostAllowlistSpec = describe "isAllowedUpstreamHost" $ do
    it "accepts a configured upstream host on the default port" $
        isAllowedUpstreamHost upstreams (hp "registry.npmjs.org") `shouldBe` True
    it "rejects an attacker-chosen host not on the allowlist" $
        isAllowedUpstreamHost upstreams (hp "evil.example.com") `shouldBe` False
    it "rejects a look-alike subdomain of an allowed host" $
        -- An allowlist is exact: a host that merely *ends with* an allowed name
        -- (registry.npmjs.org.evil.com) must not slip through.
        isAllowedUpstreamHost upstreams (hp "registry.npmjs.org.evil.com") `shouldBe` False
    it "matches case-insensitively (DNS is case-insensitive)" $
        isAllowedUpstreamHost upstreams (hp "Registry.NPMJS.org") `shouldBe` True
    it "rejects the empty host" $
        isAllowedUpstreamHost upstreams (HostPort "" 443) `shouldBe` False
    it "rejects every host when the allowlist is empty" $
        isAllowedUpstreamHost (allowedHostPorts Set.empty) (hp "registry.npmjs.org") `shouldBe` False

    describe "the port dimension" $ do
        it "rejects an allowlisted host on a nonstandard port when the entry carries no port" $
            -- The membership half of the #779 vector: an entry without a port
            -- authorises 443 alone, so an allowlisted host at an attacker-chosen
            -- port never inherits the host's authorisation.
            isAllowedUpstreamHost upstreams (hpAt "registry.npmjs.org" 9443) `shouldBe` False
        it "authorises exactly the pair an explicit host:port entry names" $ do
            let allowed = allowedHostPorts (Set.singleton (hpAt "quay.internal.example.com" 9443))
            isAllowedUpstreamHost allowed (hpAt "quay.internal.example.com" 9443) `shouldBe` True
            -- A nonstandard-port entry is not a host-wide grant: the same host on
            -- the default port needs its own entry.
            isAllowedUpstreamHost allowed (hp "quay.internal.example.com") `shouldBe` False
        it "treats an explicit 443 entry as the same authority as a portless target" $
            isAllowedUpstreamHost (allowedHostPorts (Set.singleton (hpAt "registry.npmjs.org" 443))) (hp "registry.npmjs.org")
                `shouldBe` True
        it "matches an IP-literal entry across spellings at the same port" $
            -- canonicalHostKey collapses IPv6 spellings; the port rides along untouched.
            isAllowedUpstreamHost (allowedHostPorts (Set.singleton (hpAt "0:0:0:0:0:0:0:1" 8443))) (hpAt "::1" 8443)
                `shouldBe` True

internalRangeSpec :: Spec
internalRangeSpec = describe "isBlockedTarget" $ do
    let noOptIn = []

    describe "blocks internal IPv4 ranges" $ do
        it "blocks the cloud instance-metadata address 169.254.169.254" $
            isBlockedTarget noOptIn "169.254.169.254" `shouldBe` True
        it "blocks the rest of link-local 169.254.0.0/16" $
            isBlockedTarget noOptIn "169.254.1.1" `shouldBe` True
        it "blocks loopback 127.0.0.1" $
            isBlockedTarget noOptIn "127.0.0.1" `shouldBe` True
        it "blocks the whole 127.0.0.0/8 loopback block" $
            isBlockedTarget noOptIn "127.255.255.254" `shouldBe` True
        it "blocks RFC1918 10.0.0.0/8" $
            isBlockedTarget noOptIn "10.1.2.3" `shouldBe` True
        it "blocks RFC1918 172.16.0.0/12 (low edge)" $
            isBlockedTarget noOptIn "172.16.0.1" `shouldBe` True
        it "blocks RFC1918 172.16.0.0/12 (high edge)" $
            isBlockedTarget noOptIn "172.31.255.254" `shouldBe` True
        it "blocks RFC1918 192.168.0.0/16" $
            isBlockedTarget noOptIn "192.168.1.1" `shouldBe` True
        it "blocks the unspecified / this-host address 0.0.0.0 (loopback-equivalent on Linux)" $
            isBlockedTarget noOptIn "0.0.0.0" `shouldBe` True
        it "blocks the rest of the 0.0.0.0/8 this-host block" $
            isBlockedTarget noOptIn "0.1.2.3" `shouldBe` True
        it "blocks CGNAT shared 100.64.0.0/10 (low edge)" $
            isBlockedTarget noOptIn "100.64.0.0" `shouldBe` True
        it "blocks CGNAT shared 100.64.0.0/10 (high edge)" $
            isBlockedTarget noOptIn "100.127.255.254" `shouldBe` True

    describe "blocks internal IPv6 addresses" $ do
        it "blocks the IPv6 unspecified address ::" $
            isBlockedTarget noOptIn "::" `shouldBe` True
        it "blocks IPv6 loopback ::1" $
            isBlockedTarget noOptIn "::1" `shouldBe` True
        it "blocks IPv6 link-local fe80::/10" $
            isBlockedTarget noOptIn "fe80::1" `shouldBe` True
        it "blocks IPv6 link-local at the top of fe80::/10 (febf)" $
            isBlockedTarget noOptIn "febf::1" `shouldBe` True
        it "blocks fully-expanded IPv6 loopback" $
            isBlockedTarget noOptIn "0:0:0:0:0:0:0:1" `shouldBe` True
        it "blocks IPv6 unique-local fc00::/7 (low edge, fc00)" $
            isBlockedTarget noOptIn "fc00::1" `shouldBe` True
        it "blocks IPv6 unique-local fc00::/7 (high edge, fdff)" $
            isBlockedTarget noOptIn "fdff::1" `shouldBe` True
        it "blocks the AWS IMDSv6 metadata endpoint fd00:ec2::254" $
            -- The IPv6 analogue of 169.254.169.254; an SSRF aimed at IPv6 instance
            -- metadata must be caught alongside the IPv4 endpoint.
            isBlockedTarget noOptIn "fd00:ec2::254" `shouldBe` True
        it "does not block a public IPv6 address just below the ULA range (fbff)" $
            isBlockedTarget noOptIn "fbff::1" `shouldBe` False
        it "does not block a public IPv6 address just above the ULA range (fe00)" $
            -- fe00 is above fc00::/7 (fc00..fdff) and below link-local fe80::/10.
            isBlockedTarget noOptIn "fe00::1" `shouldBe` False

    describe "blocks IPv4-mapped IPv6 (::ffff:0:0/96)" $ do
        it "blocks the cloud instance-metadata address in mapped form (::ffff:169.254.169.254)" $
            isBlockedTarget noOptIn "::ffff:a9fe:a9fe" `shouldBe` True
        it "blocks mapped loopback (::ffff:127.0.0.1)" $
            isBlockedTarget noOptIn "::ffff:7f00:1" `shouldBe` True
        it "blocks mapped RFC1918 10/8 (::ffff:10.0.0.1)" $
            isBlockedTarget noOptIn "::ffff:a00:1" `shouldBe` True
        it "does not block a mapped public address (::ffff:1.1.1.1)" $
            isBlockedTarget noOptIn "::ffff:101:101" `shouldBe` False

    describe "blocks IPv4-mapped IPv6 in canonical dotted form (RFC 4291 §2.2.3)" $ do
        -- The dotted spelling is what a tool or attacker actually emits; it must
        -- be decoded too, not just the all-hex spelling above. This is the form
        -- that previously slipped past the internal-range block.
        it "blocks the instance-metadata address (::ffff:169.254.169.254)" $
            isBlockedTarget noOptIn "::ffff:169.254.169.254" `shouldBe` True
        it "blocks mapped loopback (::ffff:127.0.0.1)" $
            isBlockedTarget noOptIn "::ffff:127.0.0.1" `shouldBe` True
        it "blocks the fully-expanded mapped loopback (0:0:0:0:0:ffff:127.0.0.1)" $
            isBlockedTarget noOptIn "0:0:0:0:0:ffff:127.0.0.1" `shouldBe` True
        it "does not block a mapped public address (::ffff:1.1.1.1)" $
            isBlockedTarget noOptIn "::ffff:1.1.1.1" `shouldBe` False

    describe "blocks IPv4-compatible IPv6 (::/96)" $ do
        -- RFC 4291 §2.5.5.1: IPv4-compatible addresses are deprecated but still
        -- accepted by many stacks, including Écluse's parser; they must be
        -- decoded to their embedded IPv4 so the block catch them.
        it "blocks the instance-metadata address (::169.254.169.254)" $
            isBlockedTarget noOptIn "::169.254.169.254" `shouldBe` True
        it "blocks compatible loopback (::127.0.0.1)" $
            isBlockedTarget noOptIn "::127.0.0.1" `shouldBe` True
        it "blocks the fully-expanded compatible loopback (0:0:0:0:0:0:127.0.0.1)" $
            isBlockedTarget noOptIn "0:0:0:0:0:0:127.0.0.1" `shouldBe` True
        it "does not block a compatible public address (::1.1.1.1)" $
            isBlockedTarget noOptIn "::1.1.1.1" `shouldBe` False

    describe "blocks NAT64-embedded IPv4 under the well-known prefix (64:ff9b::/96, RFC 6052)" $ do
        -- On a fabric that runs NAT64, an address under the well-known prefix
        -- routes to its embedded IPv4, so it must be decoded and tested against
        -- the IPv4 ranges exactly as the mapped and compatible forms are.
        it "blocks the instance-metadata address (64:ff9b::a9fe:a9fe)" $
            isBlockedTarget noOptIn "64:ff9b::a9fe:a9fe" `shouldBe` True
        it "blocks the instance-metadata address in dotted form (64:ff9b::169.254.169.254)" $
            isBlockedTarget noOptIn "64:ff9b::169.254.169.254" `shouldBe` True
        it "blocks NAT64 loopback (64:ff9b::127.0.0.1)" $
            isBlockedTarget noOptIn "64:ff9b::127.0.0.1" `shouldBe` True
        it "blocks the fully-expanded NAT64 metadata address (64:ff9b:0:0:0:0:a9fe:a9fe)" $
            isBlockedTarget noOptIn "64:ff9b:0:0:0:0:a9fe:a9fe" `shouldBe` True
        it "does not block a NAT64 embedding of a public address (64:ff9b::1.1.1.1)" $
            isBlockedTarget noOptIn "64:ff9b::1.1.1.1" `shouldBe` False

    describe "blocks NAT64-embedded IPv4 under the local-use prefix (64:ff9b:1::/48, RFC 8215)" $ do
        -- The local-use prefix is a /48; any /96 within it embeds the IPv4 in
        -- the low 32 bits, so the decode holds across the middle bits.
        it "blocks the instance-metadata address (64:ff9b:1::169.254.169.254)" $
            isBlockedTarget noOptIn "64:ff9b:1::169.254.169.254" `shouldBe` True
        it "blocks an internal embedding under a non-zero /96 within the /48 (64:ff9b:1:aaaa::10.0.0.1)" $
            isBlockedTarget noOptIn "64:ff9b:1:aaaa::10.0.0.1" `shouldBe` True
        it "does not block a local-use embedding of a public address (64:ff9b:1::1.1.1.1)" $
            isBlockedTarget noOptIn "64:ff9b:1::1.1.1.1" `shouldBe` False

    describe "treats malformed IPv6 literals as names (not blocked)" $ do
        -- Each malformed form must fail to parse as an IP, so it is not mistaken
        -- for an internal literal; the allowlist would still gate a real name.
        it "rejects more than one '::'" $
            isBlockedTarget noOptIn "1::2::3" `shouldBe` False
        it "rejects a compressed literal that already has eight groups" $
            isBlockedTarget noOptIn "1:2:3:4:5:6:7:8::" `shouldBe` False
        it "rejects an out-of-range 16-bit group" $
            isBlockedTarget noOptIn "fe80::1ffff" `shouldBe` False
        it "rejects a non-hex group" $
            isBlockedTarget noOptIn "fe80::zz" `shouldBe` False
        it "rejects an uncompressed literal with the wrong group count" $
            isBlockedTarget noOptIn "1:2:3" `shouldBe` False
        it "does not block a non-internal compressed IPv6 address" $
            isBlockedTarget noOptIn "2001:db8::1" `shouldBe` False

    describe "permits public and non-IP targets" $ do
        it "does not block a public IPv4 address" $
            isBlockedTarget noOptIn "93.184.216.34" `shouldBe` False
        it "does not block 172.32.0.1 (just above the /12)" $
            isBlockedTarget noOptIn "172.32.0.1" `shouldBe` False
        it "does not block 11.0.0.1 (just above 10/8)" $
            isBlockedTarget noOptIn "11.0.0.1" `shouldBe` False
        it "does not block 1.0.0.0 (just above the 0/8 this-host block)" $
            isBlockedTarget noOptIn "1.0.0.0" `shouldBe` False
        it "does not block 100.63.255.255 (just below CGNAT 100.64/10)" $
            isBlockedTarget noOptIn "100.63.255.255" `shouldBe` False
        it "does not block 100.128.0.1 (just above CGNAT 100.64/10)" $
            isBlockedTarget noOptIn "100.128.0.1" `shouldBe` False
        it "does not block a DNS name (the allowlist constrains those)" $
            isBlockedTarget noOptIn "registry.npmjs.org" `shouldBe` False
        it "does not block a public IPv6 address" $
            isBlockedTarget noOptIn "2606:2800:220:1:248:1893:25c8:1946" `shouldBe` False
        it "treats a malformed octet (256) as a name, not an internal IP" $
            -- "10.0.0.256" is not a valid dotted-quad, so it is not parsed as the
            -- 10/8 literal it superficially resembles.
            isBlockedTarget noOptIn "10.0.0.256" `shouldBe` False
        it "treats a non-numeric octet as a name, not an internal IP" $
            isBlockedTarget noOptIn "10.0.0.x" `shouldBe` False
        it "treats a dotted-quad with too few octets as a name" $
            isBlockedTarget noOptIn "10.0.0" `shouldBe` False
        it "treats an empty octet as a name" $
            isBlockedTarget noOptIn "10..0.1" `shouldBe` False
        it "does not block the empty host" $
            -- The empty string parses as no IP literal, so it is not internal;
            -- the host allowlist independently rejects it.
            isBlockedTarget noOptIn "" `shouldBe` False

    describe "deliberately treats RFC 5737 documentation ranges as external" $ do
        -- A tripwire, not just coverage. The end-to-end suite (S53) stands the
        -- whole system up on a docker network whose subnet is TEST-NET-3
        -- (203.0.113.0/24): it points the proxy's gated *public* upstream at a
        -- stub on that range, relying on these documentation addresses being
        -- reachable rather than blocked. That is correct -- a documentation range
        -- never aliases a real service, so blocking it adds no SSRF protection --
        -- but a future blocklist audit (issue #178) that broadened the block to
        -- all reserved space would silently break e2e. If you are here because
        -- this failed: that broadening is a conscious choice; update the e2e
        -- harness (planning/slices/S53-e2e-ecosystem.md) to match.
        it "does not block TEST-NET-3 203.0.113.0/24 (the e2e network subnet)" $
            isBlockedTarget noOptIn "203.0.113.2" `shouldBe` False
        it "does not block TEST-NET-1 192.0.2.0/24" $
            isBlockedTarget noOptIn "192.0.2.1" `shouldBe` False
        it "does not block TEST-NET-2 198.51.100.0/24" $
            isBlockedTarget noOptIn "198.51.100.1" `shouldBe` False

    describe "operator-configured additional blocked ranges" $ do
        let testNet3 = ["203.0.113.0/24"] :: [IPRange]
        it "blocks a host matched by an additional range not in the fixed set" $
            isBlockedTarget testNet3 "203.0.113.5" `shouldBe` True
        it "leaves a host outside every additional range unblocked" $ do
            isBlockedTarget testNet3 "8.8.8.8" `shouldBe` False
            isBlockedTarget testNet3 "203.0.114.1" `shouldBe` False
        it "unions the additional ranges with the fixed set rather than replacing it" $
            -- A fixed-range address (10/8) is still blocked when an unrelated additional
            -- range is configured: additional ranges only ever widen the block.
            isBlockedTarget testNet3 "10.1.2.3" `shouldBe` True
        it "blocks an IPv6 host matched by an additional range" $
            isBlockedTarget ["2001:db8::/32"] "2001:db8::1" `shouldBe` True
        it "does not block a DNS name even when it lexically resembles a blocked range" $
            isBlockedTarget testNet3 "203.0.113.example.com" `shouldBe` False

    describe "coerces an IPv4 octet as inet_aton does (leading-zero octal, 0x hex)" $ do
        -- The literal block reads each octet in the base a libc resolver would, so it
        -- tests the address actually dialled rather than a decimal misreading. These
        -- expectations are validated against the real 'getAddrInfo' in the non-gating
        -- smoke oracle ("Ecluse.SecurityResolverOracleSpec").
        it "blocks 0012.0.0.1 -- octal 0012 = 10.0.0.1, an RFC1918 address" $
            -- The reported under-block: a decimal reading sees 12 (public) and lets it
            -- through; octal reads 10, the internal address the resolver actually dials.
            isBlockedTarget noOptIn "0012.0.0.1" `shouldBe` True
        it "blocks 0177.0.0.1 -- octal 0177 = 127.0.0.1, loopback" $
            isBlockedTarget noOptIn "0177.0.0.1" `shouldBe` True
        it "blocks 0x7f.0.0.1 -- hex 0x7f = 127.0.0.1, loopback" $
            isBlockedTarget noOptIn "0x7f.0.0.1" `shouldBe` True
        it "does not block 010.0.0.1 -- octal 010 = 8.0.0.1, a public address" $
            -- A decimal misreading over-blocks this as 10.0.0.1; octal is 8.0.0.1, which
            -- the resolver confirms is public, so the literal layer must not block it.
            isBlockedTarget noOptIn "010.0.0.1" `shouldBe` False
        it "does not block 0127.0.0.1 -- octal 0127 = 87.0.0.1, a public address" $
            isBlockedTarget noOptIn "0127.0.0.1" `shouldBe` False
        it "treats 08.0.0.1 as a name -- 8 is not an octal digit (a resolver rejects it)" $
            isBlockedTarget noOptIn "08.0.0.1" `shouldBe` False
        it "treats 0400.0.0.1 as a name -- octal 0400 = 256 overflows an octet" $
            isBlockedTarget noOptIn "0400.0.0.1" `shouldBe` False
        it "leaves the short 32-bit form 2130706433 to the connect-time recheck (not a literal here)" $
            -- inet_aton resolves this to 127.0.0.1, but the four-part recogniser does not
            -- model the short forms; the resolved-IP recheck in Ecluse.Core.Security.Egress
            -- is the backstop for it.
            isBlockedTarget noOptIn "2130706433" `shouldBe` False

{- | The blocked-vs-allowed classification of 'isBlockedTarget', pinned against an
__explicit expected table__ rather than any prior implementation. The internal
block recognises a host as a literal with a deliberately lenient hand-rolled
parser and delegates only the range membership to a library, so this corpus
guards that the gate neither narrows nor widens: every internal range blocks, the
IPv4-embedding smuggling forms (mapped, compatible, and NAT64) decode and block,
and the lenient/strict boundary spellings classify exactly as documented.

The boundary cases are the load-bearing ones. A leading-zero octet is coerced as
__octal__, exactly as a libc resolver does, so it is /not/ its decimal digits:
@0012.0.0.1@ is octal @10.0.0.1@ and is __blocked__ (RFC1918), whereas @010.0.0.1@
(octal @8.0.0.1@) and @0127.0.0.1@ (octal @87.0.0.1@) coerce to __public__
addresses and are __not__ blocked -- a decimal misreading would over-block those two
and, worse, /under/-block a sibling like @0012.0.0.1@. A @0x@ octet is hexadecimal
(@0x7f.0.0.1@ is @127.0.0.1@, blocked). @fe80::1ffff@ is __not__ blocked: its final
group overflows 16 bits, so it is not a literal here and stays a name the allowlist
constrains.
-}
classificationCorpusSpec :: Spec
classificationCorpusSpec =
    describe "isBlockedTarget classification corpus (explicit expected table)" $
        for_ corpus $ \(host, expected) ->
            it (renderCase host expected) $
                isBlockedTarget noOptIn host `shouldBe` expected
  where
    noOptIn = []
    renderCase host expected =
        toString $
            (if expected then "blocks " else "permits ")
                <> (if T.null host then "<empty>" else host)

    -- (host, expected-blocked). Grouped by intent; every internal range, every
    -- IPv4-embedding spelling, the lenient/strict boundary, and externals/names.
    corpus :: [(Text, Bool)]
    corpus =
        internalV4
            <> internalV6
            <> mappedV4
            <> nat64Embedded
            <> lenientBoundary
            <> externals
            <> names

    internalV4 =
        [ ("169.254.169.254", True) -- IMDSv4
        , ("169.254.1.1", True) -- link-local 169.254.0.0/16
        , ("127.0.0.1", True) -- loopback
        , ("127.255.255.254", True) -- loopback 127.0.0.0/8 high
        , ("10.1.2.3", True) -- RFC1918 10/8
        , ("172.16.0.1", True) -- RFC1918 172.16/12 low
        , ("172.31.255.254", True) -- RFC1918 172.16/12 high
        , ("192.168.1.1", True) -- RFC1918 192.168/16
        , ("0.0.0.0", True) -- unspecified / this-host
        , ("0.1.2.3", True) -- rest of 0.0.0.0/8
        , ("100.64.0.0", True) -- CGNAT 100.64/10 low
        , ("100.127.255.254", True) -- CGNAT 100.64/10 high
        ]

    internalV6 =
        [ ("::", True) -- unspecified
        , ("::1", True) -- loopback
        , ("0:0:0:0:0:0:0:1", True) -- loopback, fully expanded
        , ("fe80::1", True) -- link-local fe80::/10 low
        , ("febf::1", True) -- link-local fe80::/10 high
        , ("fc00::1", True) -- unique-local fc00::/7 low
        , ("fdff::1", True) -- unique-local fc00::/7 high
        , ("fd00:ec2::254", True) -- IMDSv6
        ]

    mappedV4 =
        [ ("::ffff:169.254.169.254", True) -- IMDSv4 mapped, dotted spelling
        , ("::ffff:a9fe:a9fe", True) -- IMDSv4 mapped, hex spelling
        , ("::ffff:127.0.0.1", True) -- mapped loopback
        , ("0:0:0:0:0:ffff:127.0.0.1", True) -- mapped loopback, fully expanded
        , ("::ffff:1.1.1.1", False) -- mapped public stays permitted
        , ("::169.254.169.254", True) -- IMDSv4 compatible
        , ("::127.0.0.1", True) -- compatible loopback
        , ("0:0:0:0:0:0:127.0.0.1", True) -- compatible loopback, fully expanded
        , ("::1.1.1.1", False) -- compatible public stays permitted
        ]

    nat64Embedded =
        [ ("64:ff9b::a9fe:a9fe", True) -- IMDSv4 under the NAT64 well-known prefix, hex spelling
        , ("64:ff9b::169.254.169.254", True) -- IMDSv4 under the well-known prefix, dotted spelling
        , ("64:ff9b::127.0.0.1", True) -- NAT64 loopback
        , ("64:ff9b::1.1.1.1", False) -- NAT64 public stays permitted
        , ("64:ff9b:1::169.254.169.254", True) -- IMDSv4 under the RFC 8215 local-use prefix
        , ("64:ff9b:1:aaaa::10.0.0.1", True) -- RFC1918 under a non-zero /96 within the /48
        , ("64:ff9b:1::1.1.1.1", False) -- local-use public stays permitted
        ]

    lenientBoundary =
        [ ("0012.0.0.1", True) -- octal 0012 = 10.0.0.1 (RFC1918) is blocked
        , ("0177.0.0.1", True) -- octal 0177 = 127.0.0.1 (loopback) is blocked
        , ("0x7f.0.0.1", True) -- hex 0x7f = 127.0.0.1 (loopback) is blocked
        , ("010.0.0.1", False) -- octal 010 = 8.0.0.1 is public, not blocked
        , ("0127.0.0.1", False) -- octal 0127 = 87.0.0.1 is public, not blocked
        , ("08.0.0.1", False) -- 8 is not an octal digit: not a literal here
        , ("0400.0.0.1", False) -- octal 0400 = 256 overflows an octet: not a literal
        , ("fe80::1ffff", False) -- over-16-bit group is not a literal
        ]

    externals =
        [ ("8.8.8.8", False)
        , ("1.1.1.1", False)
        , ("93.184.216.34", False)
        , ("172.32.0.1", False) -- just above the 172.16/12 block
        , ("11.0.0.1", False) -- just above 10/8
        , ("100.63.255.255", False) -- just below CGNAT
        , ("100.128.0.1", False) -- just above CGNAT
        , ("2606:4700::1111", False)
        , ("2001:db8::1", False)
        , ("fbff::1", False) -- just below fc00::/7
        , ("fe00::1", False) -- between fc00::/7 and fe80::/10
        ]

    names =
        [ ("registry.npmjs.org", False) -- a DNS name
        , ("", False) -- empty
        , ("10.0.0.256", False) -- octet out of range → not a literal
        , ("10.0.0.x", False) -- non-numeric octet → not a literal
        , ("10.0.0", False) -- too few octets → not a literal
        , ("2130706433", False) -- a bare 32-bit number: a short inet_aton form, not modelled here
        , ("1::2::3", False) -- two "::" → malformed
        , ("::ffff:1.2.3.4.5", False) -- mapped form with a bad embedded IPv4
        ]

hostAddressSpec :: Spec
hostAddressSpec = describe "hostAddress" $ do
    it "extracts the host from a full URL" $
        hostAddress "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
            `shouldBe` "registry.npmjs.org"
    it "strips a port" $
        hostAddress "https://registry.npmjs.org:8443/thing" `shouldBe` "registry.npmjs.org"
    it "strips userinfo (a credential-stuffing trick)" $
        -- "@" tricks: the real host is what follows the last '@', not the part
        -- before it (https://registry.npmjs.org@evil.com → evil.com).
        hostAddress "https://registry.npmjs.org@evil.com/path" `shouldBe` "evil.com"
    it "gates on the scheme authority, not a later :// in the path or query" $
        -- A crafted dist.tarball carrying a second "://" in its query must gate on
        -- the host actually dialled (the first authority), not the one after the
        -- last "://" (https://169.254.169.254/x?u=https://ok → 169.254.169.254).
        hostAddress "https://169.254.169.254/x?u=https://registry.npmjs.org"
            `shouldBe` "169.254.169.254"
    it "lower-cases the host" $
        hostAddress "https://Registry.NPMJS.org/x" `shouldBe` "registry.npmjs.org"
    it "extracts a bare host[:port] authority" $
        hostAddress "registry.npmjs.org:443" `shouldBe` "registry.npmjs.org"
    it "unwraps a bracketed IPv6 literal with a port" $
        hostAddress "http://[::1]:443/meta" `shouldBe` "::1"
    it "keeps a bare IPv6 literal intact" $
        hostAddress "[fe80::1]" `shouldBe` "fe80::1"
    it "yields empty for a value with no host" $
        hostAddress "" `shouldBe` ""
    it "handles a schemeless authority with userinfo and a path" $
        -- No "://" and a userinfo "@": both 'breakOnEnd' branches with the needle
        -- present-then-absent are exercised here.
        hostAddress "user:pass@registry.npmjs.org/x" `shouldBe` "registry.npmjs.org"
    it "drops a path on a schemeless bare host" $
        hostAddress "registry.npmjs.org/thing" `shouldBe` "registry.npmjs.org"
    it "composes with the SSRF guards to catch a metadata URL" $
        -- The realistic call shape: each guard tests its own projection of the
        -- URL, the bare host for the internal block and the host:port authority
        -- for the allowlist.
        let url = "http://169.254.169.254/latest/meta-data/"
         in (isBlockedTarget [] (hostAddress url), isAllowedUpstreamHost upstreams <$> hostPortAddress url)
                `shouldBe` (True, Just False)

{- | The authorisation-side extraction: the same authority stripping as
'hostAddress' with the effective port parsed rather than discarded. The default
443, the strict port grammar, and the IPv6 bracket discipline are each pinned
because each is load-bearing for the gate: a lenient port fallback or a mangled
colon split would alias an attacker-chosen authority onto an authorised one.
-}
hostPortAddressSpec :: Spec
hostPortAddressSpec = describe "hostPortAddress" $ do
    it "extracts the host and an explicit port from a full URL" $
        hostPortAddress "https://registry.npmjs.org:9443/thing/-/thing-1.0.0.tgz"
            `shouldBe` Just (hpAt "registry.npmjs.org" 9443)
    it "defaults a portless URL to 443 (egress is https-only)" $
        hostPortAddress "https://registry.npmjs.org/thing" `shouldBe` Just (hp "registry.npmjs.org")
    it "reads an explicit :443 as the same authority as no port" $
        hostPortAddress "https://registry.npmjs.org:443/x"
            `shouldBe` hostPortAddress "https://registry.npmjs.org/x"
    it "extracts a bare host[:port] authority" $
        hostPortAddress "registry.npmjs.org:8443" `shouldBe` Just (hpAt "registry.npmjs.org" 8443)
    it "lower-cases the host" $
        hostPortAddress "https://Registry.NPMJS.org:8443/x" `shouldBe` Just (hpAt "registry.npmjs.org" 8443)
    it "strips userinfo (a credential-stuffing trick)" $
        hostPortAddress "https://registry.npmjs.org@evil.com:8443/path" `shouldBe` Just (hpAt "evil.com" 8443)
    it "gates on the scheme authority, not a later :// in the path or query" $
        hostPortAddress "https://169.254.169.254/x?u=https://registry.npmjs.org:9443"
            `shouldBe` Just (hp "169.254.169.254")
    it "accepts the port range edges" $ do
        hostPortAddress "https://registry.npmjs.org:1/" `shouldBe` Just (hpAt "registry.npmjs.org" 1)
        hostPortAddress "https://registry.npmjs.org:65535/" `shouldBe` Just (hpAt "registry.npmjs.org" 65535)

    describe "IPv6 literals" $ do
        it "splits a bracketed literal with a port on the closing bracket" $
            hostPortAddress "https://[2606:4700::1111]:8443/x" `shouldBe` Just (hpAt "2606:4700::1111" 8443)
        it "defaults a bracketed literal without a port to 443" $
            hostPortAddress "https://[2606:4700::1111]/x" `shouldBe` Just (hp "2606:4700::1111")
        it "refuses an unbracketed literal whole rather than mangling it at a colon" $
            -- "2606:4700::1111" has no unambiguous host/port split; truncating it to
            -- a host "2606" would gate on an authority nothing dials.
            hostPortAddress "2606:4700::1111" `shouldBe` Nothing
        it "refuses an opening bracket with no close (malformed authority)" $
            hostPortAddress "https://[::1" `shouldBe` Nothing
        it "refuses junk between the closing bracket and the port" $
            hostPortAddress "https://[::1]x/" `shouldBe` Nothing
        it "refuses a bracketed literal with a written-but-empty port (fail closed)" $
            -- The bracketed analogue of "host:" -- http-client refuses a URL that
            -- writes a colon with no digits, so the gate refuses it too.
            hostPortAddress "https://[::1]:/x" `shouldBe` Nothing

    describe "strict port parsing (fail closed, never fall back to the default)" $ do
        it "refuses a non-numeric port" $
            hostPortAddress "https://registry.npmjs.org:9x9/" `shouldBe` Nothing
        it "refuses port 0" $
            hostPortAddress "https://registry.npmjs.org:0/" `shouldBe` Nothing
        it "refuses a port past 65535" $
            hostPortAddress "https://registry.npmjs.org:65536/" `shouldBe` Nothing
        it "refuses a signed port" $
            hostPortAddress "https://registry.npmjs.org:-443/" `shouldBe` Nothing
        it "refuses a trailing colon with no port digits (fail closed)" $
            -- A written-but-empty port is malformed, never the default: "host:" and
            -- "[::1]:" are both undialable, so the gate refuses both rather than
            -- folding "host:" to 443.
            hostPortAddress "https://registry.npmjs.org:/x" `shouldBe` Nothing
        it "refuses a leading-zero port so each port has one canonical spelling" $ do
            -- 0443 reads as 443 and 080 as 80 under a lenient decimal parse, aliasing
            -- a canonical port under a second spelling; the gate accepts one spelling.
            hostPortAddress "https://registry.npmjs.org:0443/" `shouldBe` Nothing
            hostPortAddress "https://registry.npmjs.org:080/" `shouldBe` Nothing
        it "accepts the canonical spelling of those ports" $ do
            hostPortAddress "https://registry.npmjs.org:443/" `shouldBe` Just (hp "registry.npmjs.org")
            hostPortAddress "https://registry.npmjs.org:80/" `shouldBe` Just (hpAt "registry.npmjs.org" 80)

    it "yields Nothing for a value with no host" $ do
        hostPortAddress "" `shouldBe` Nothing
        hostPortAddress ":8443" `shouldBe` Nothing

-- The bracket-aware @host[:port]@ split shared by 'hostAddress' and the SQS
-- endpoint parser. These assert host/port extraction only -- the split is purely
-- structural and carries no classification or gating (the OTLP and SQS endpoints
-- are trusted, operator-declared destinations; see #326).
splitHostPortSpec :: Spec
splitHostPortSpec = describe "splitHostPort" $ do
    it "splits a bracketed IPv6 literal with a port on the closing bracket" $
        -- The #292/#296 edge: an inner '::' is never read as the port separator.
        splitHostPort "[::1]:4566" `shouldBe` Just ("::1", ":4566")
    it "splits a longer bracketed IPv6 literal with a port" $
        splitHostPort "[fd00::1]:4318" `shouldBe` Just ("fd00::1", ":4318")
    it "keeps a bare bracketed IPv6 literal (no port) with an empty remainder" $
        splitHostPort "[fe80::1]" `shouldBe` Just ("fe80::1", "")
    it "splits a bare host with a port" $
        splitHostPort "localhost:4566" `shouldBe` Just ("localhost", ":4566")
    it "leaves a bare host (no port) with an empty remainder" $
        splitHostPort "localhost" `shouldBe` Just ("localhost", "")
    it "splits an IPv4 host with a port" $
        splitHostPort "127.0.0.1:8080" `shouldBe` Just ("127.0.0.1", ":8080")
    it "rejects an opening bracket with no close as malformed" $
        splitHostPort "[::1" `shouldBe` Nothing
    it "handles missing host (empty string)" $
        splitHostPort "" `shouldBe` Nothing
    it "handles missing host with a port" $
        splitHostPort ":4566" `shouldBe` Nothing
    it "handles missing port after colon" $
        splitHostPort "localhost:" `shouldBe` Just ("localhost", "")
    it "handles multiple colons in bare host" $
        splitHostPort "a:b:c" `shouldBe` Just ("a", ":b:c")
    it "handles empty brackets with port" $
        splitHostPort "[]:80" `shouldBe` Just ("", ":80")

{- | The actual outbound-fetch guarantee is the __conjunction__: a target is
fetched only if it is on the host allowlist /and/ not an internal address. The two
halves are exercised above; this pins the composition the data plane relies on, so
neither half can be silently weakened.
-}
ssrfGateSpec :: Spec
ssrfGateSpec = describe "composed SSRF gate (allowlist AND not-blocked)" $ do
    let noOptIn = []
        -- The allowlist authorises the host:port pair; the internal-range block
        -- classifies the bare host (an address is internal regardless of port).
        passesGate authority =
            isAllowedUpstreamHost upstreams authority && not (isBlockedTarget noOptIn (hpHost authority))

    it "admits a configured public upstream" $
        passesGate (hp "registry.npmjs.org") `shouldBe` True
    it "vetoes an allowlisted host that is an internal literal (block beats allowlist)" $
        -- Even if an operator allowlists an internal address, the internal-range
        -- block still rejects it: the guarantee is the conjunction, not either half.
        let allowed = allowedHostPorts (Set.insert (hp "169.254.169.254") upstreamHosts)
         in ( isAllowedUpstreamHost allowed (hp "169.254.169.254")
                && not (isBlockedTarget noOptIn "169.254.169.254")
            )
                `shouldBe` False
    it "refuses an IPv4-mapped IPv6 metadata literal (blocked by both halves)" $
        -- '::ffff:a9fe:a9fe' is 169.254.169.254 in IPv4-mapped form. The internal
        -- block now decodes the embedded IPv4 address and catches it directly, so
        -- the gate refuses it even if someone were to allowlist this literal form.
        passesGate (hp "::ffff:a9fe:a9fe") `shouldBe` False
    it "refuses a metadata authority extracted from a URL" $
        (passesGate <$> hostPortAddress "http://169.254.169.254/latest/meta-data/")
            `shouldBe` Just False

{- | The @dist.tarball@ host policy: under the secure default a tarball is fetched
only from the same authority (host and port) that served the packument; the opt-in
relaxes that to any allowlisted @host:port@ pair. Neither ever escapes the
allowlist; the internal-range block is __origin-aware__ -- the untrusted origin is
gated by it (subject to the per-host opt-in), the trusted private origin exempt
from it (mirroring the connection layer's unguarded manager, security.md
invariant 3). The deny paths are exercised hardest, since under-blocking on the
untrusted origin is a vulnerability.
-}
tarballHostPolicySpec :: Spec
tarballHostPolicySpec = describe "tarballHostAllowed" $ do
    let noOptIn = []
        -- Two allowlisted upstreams: the packument source and a separate CDN.
        allow = allowedHostPorts (Set.fromList [hp "registry.npmjs.org", hp "cdn.npmjs.org"])
        -- The untrusted public origin: the internal-range block applies (the existing
        -- policy/allowlist/internal-range coverage is over this origin).
        same policy packument target = tarballHostAllowed UntrustedOrigin policy allow noOptIn (Just packument) (Just target)
        -- A short alias: packument origin fixed to the npm registry on 443.
        decide policy = same policy (hp "registry.npmjs.org")

    describe "SameHostAsPackument (the secure default)" $ do
        it "admits a tarball on the same authority that served the packument" $
            decide SameHostAsPackument (hp "registry.npmjs.org") `shouldBe` True
        it "refuses a tarball on a different host, even one on the allowlist" $
            -- The crux of the default: an allowlisted-but-different CDN is refused.
            decide SameHostAsPackument (hp "cdn.npmjs.org") `shouldBe` False
        it "refuses a tarball on a host not on the allowlist" $
            decide SameHostAsPackument (hp "evil.example.com") `shouldBe` False
        it "matches the same-host clause case-insensitively (DNS is)" $
            decide SameHostAsPackument (hp "Registry.NPMJS.org") `shouldBe` True
        it "refuses an empty tarball host" $
            decide SameHostAsPackument (HostPort "" 443) `shouldBe` False
        it "refuses a look-alike suffix of the packument host" $
            -- registry.npmjs.org.evil.com is neither allowlisted nor equal.
            decide SameHostAsPackument (hp "registry.npmjs.org.evil.com") `shouldBe` False

    describe "AnyAllowlistedHost (the opt-in)" $ do
        it "admits a tarball on a different but allowlisted host" $
            decide AnyAllowlistedHost (hp "cdn.npmjs.org") `shouldBe` True
        it "still admits a tarball on the same authority" $
            decide AnyAllowlistedHost (hp "registry.npmjs.org") `shouldBe` True
        it "still refuses a tarball on a host not on the allowlist" $
            -- The opt-in relaxes which allowlisted pair, never the allowlist itself.
            decide AnyAllowlistedHost (hp "evil.example.com") `shouldBe` False

    describe "the port dimension (the gate authorises host and port as a pair)" $ do
        it "refuses a nonstandard-port dist.tarball under the default when the entry carries no port" $
            -- The #779 vector: dist.tarball = https://registry.npmjs.org:9443/...
            -- after a packument from https://registry.npmjs.org. The :9443 must
            -- reach both the allowlist and the same-authority clause, not be
            -- discarded before them.
            decide SameHostAsPackument (hpAt "registry.npmjs.org" 9443) `shouldBe` False
        it "refuses a nonstandard-port dist.tarball under the opt-in too" $
            same AnyAllowlistedHost (hp "registry.npmjs.org") (hpAt "cdn.npmjs.org" 9443) `shouldBe` False
        it "refuses a port mismatch between packument origin and tarball even with both pairs allowlisted" $
            -- Same host, both pairs allowlisted: the same-authority clause still
            -- refuses, because the origin dialled 443 and the tarball names 9443.
            let bothPorts = allowedHostPorts (Set.fromList [hp "registry.npmjs.org", hpAt "registry.npmjs.org" 9443])
             in tarballHostAllowed UntrustedOrigin SameHostAsPackument bothPorts noOptIn (Just (hp "registry.npmjs.org")) (Just (hpAt "registry.npmjs.org" 9443))
                    `shouldBe` False
        it "admits a nonstandard-port tarball when the origin and the entry both name that pair" $
            -- An operator whose upstream lives on a nonstandard port states the
            -- pair explicitly; the origin dialled it and the entry authorises it.
            let at9443 = allowedHostPorts (Set.singleton (hpAt "registry.internal.example.com" 9443))
             in tarballHostAllowed UntrustedOrigin SameHostAsPackument at9443 noOptIn (Just (hpAt "registry.internal.example.com" 9443)) (Just (hpAt "registry.internal.example.com" 9443))
                    `shouldBe` True
        it "admits a cross-host tarball under the opt-in only at its allowlisted pair" $ do
            let withCdnPort = allowedHostPorts (Set.fromList [hp "registry.npmjs.org", hpAt "cdn.npmjs.org" 8443])
                cdnAt port = tarballHostAllowed UntrustedOrigin AnyAllowlistedHost withCdnPort noOptIn (Just (hp "registry.npmjs.org")) (Just (hpAt "cdn.npmjs.org" port))
            cdnAt 8443 `shouldBe` True
            cdnAt 9443 `shouldBe` False
        it "refuses an unextractable tarball authority (fail closed)" $
            tarballHostAllowed UntrustedOrigin SameHostAsPackument allow noOptIn (Just (hp "registry.npmjs.org")) Nothing
                `shouldBe` False
        it "refuses an unextractable packument origin (fail closed)" $
            tarballHostAllowed UntrustedOrigin SameHostAsPackument allow noOptIn Nothing (Just (hp "registry.npmjs.org"))
                `shouldBe` False

    describe "the internal-range block beats either policy (untrusted origin)" $ do
        it "refuses an internal literal even when it equals the packument authority" $
            -- An operator could (mis)configure an internal upstream host; the
            -- internal block still vetoes a tarball pointed at it under the
            -- default. The allowlist must carry the literal for this to even reach
            -- the block clause.
            let allowInternal = allowedHostPorts (Set.singleton (hp "169.254.169.254"))
             in tarballHostAllowed UntrustedOrigin SameHostAsPackument allowInternal noOptIn (Just (hp "169.254.169.254")) (Just (hp "169.254.169.254"))
                    `shouldBe` False
        it "refuses an allowlisted internal literal under the opt-in too" $
            let allowInternal = allowedHostPorts (Set.singleton (hp "10.0.0.5"))
             in tarballHostAllowed UntrustedOrigin AnyAllowlistedHost allowInternal noOptIn (Just (hp "registry.npmjs.org")) (Just (hp "10.0.0.5"))
                    `shouldBe` False
        it "refuses an internal literal regardless of its port (the block classifies the host alone)" $
            -- The port never launders an internal address: 10.0.0.5:8443 is as
            -- internal as 10.0.0.5.
            let allowInternal = allowedHostPorts (Set.singleton (hpAt "10.0.0.5" 8443))
             in tarballHostAllowed UntrustedOrigin SameHostAsPackument allowInternal noOptIn (Just (hpAt "10.0.0.5" 8443)) (Just (hpAt "10.0.0.5" 8443))
                    `shouldBe` False
        it "still blocks a host matched only by an operator-configured additional range" $
            let allowInternal = allowedHostPorts (Set.singleton (hp "10.0.0.5"))
             in tarballHostAllowed UntrustedOrigin AnyAllowlistedHost allowInternal ["10.0.0.5/32"] (Just (hp "registry.npmjs.org")) (Just (hp "10.0.0.5"))
                    `shouldBe` False

    describe "the trusted private origin is exempt from the internal-range block" $ do
        -- The trusted origin mirrors the connection layer's unguarded manager
        -- (security.md invariant 3): a private registry may legitimately live on an
        -- internal address, so its same-host dist.tarball is admitted with no opt-in --
        -- where the untrusted origin would be refused. The allowlist and same-authority
        -- clauses still gate it, so the exemption never widens past its own pair.
        let allowInternal = allowedHostPorts (Set.singleton (hp "10.0.0.5"))
        it "admits a same-authority internal-literal tarball with no opt-in (where untrusted is refused)" $ do
            tarballHostAllowed TrustedOrigin SameHostAsPackument allowInternal noOptIn (Just (hp "10.0.0.5")) (Just (hp "10.0.0.5"))
                `shouldBe` True
            -- The same inputs on the untrusted origin are refused by the internal block.
            tarballHostAllowed UntrustedOrigin SameHostAsPackument allowInternal noOptIn (Just (hp "10.0.0.5")) (Just (hp "10.0.0.5"))
                `shouldBe` False
        it "still refuses a trusted tarball off the host allowlist (allowlist not relaxed)" $
            -- The exemption is the internal-range clause only; an off-allowlist host is
            -- still refused, so the trusted origin cannot be steered onto an arbitrary host.
            tarballHostAllowed TrustedOrigin AnyAllowlistedHost allowInternal noOptIn (Just (hp "10.0.0.5")) (Just (hp "192.168.0.9"))
                `shouldBe` False
        it "still refuses a cross-host trusted tarball under the secure default (same-host not relaxed)" $
            -- Two allowlisted internal hosts; under SameHostAsPackument the trusted
            -- origin's tarball must still equal its packument authority, so a different
            -- (allowlisted, internal) host is refused.
            let bothAllowed = allowedHostPorts (Set.fromList [hp "10.0.0.5", hp "10.0.0.6"])
             in tarballHostAllowed TrustedOrigin SameHostAsPackument bothAllowed noOptIn (Just (hp "10.0.0.5")) (Just (hp "10.0.0.6"))
                    `shouldBe` False
        it "still refuses a trusted port mismatch under the secure default (the pair must match)" $
            -- The trusted exemption never opens the port dimension: a trusted
            -- upstream's tarball on another port of its own host is refused.
            let bothPorts = allowedHostPorts (Set.fromList [hp "10.0.0.5", hpAt "10.0.0.5" 8443])
             in tarballHostAllowed TrustedOrigin SameHostAsPackument bothPorts noOptIn (Just (hp "10.0.0.5")) (Just (hpAt "10.0.0.5" 8443))
                    `shouldBe` False

allowedHostPortsSpec :: Spec
allowedHostPortsSpec = describe "allowedHostPorts" $ do
    it "folds configured-host case so a mixed-case entry matches a lowercase query" $
        -- 'allowedHostPorts' is the only constructor of the 'AllowedHostPorts' the
        -- guard takes, so this proves the guard relies on it for normalisation: a
        -- host configured in mixed case is matched by its lowercase form.
        isAllowedUpstreamHost (allowedHostPorts (Set.singleton (hp "Registry.NPMjs.ORG"))) (hp "registry.npmjs.org")
            `shouldBe` True
    it "normalises distinct casings of one host to the same allowlist" $
        -- Two spellings that differ only in case fold to equal 'AllowedHostPorts'
        -- values, so the normalisation is genuinely case-collapsing.
        allowedHostPorts (Set.fromList [hp "EXAMPLE.com", hp "example.COM"])
            `shouldBe` allowedHostPorts (Set.singleton (hp "example.com"))
    it "keeps the same host on distinct ports as distinct entries" $
        -- Normalisation collapses spellings, never ports: each pair authorises
        -- itself alone.
        allowedHostPorts (Set.fromList [hp "example.com", hpAt "example.com" 8443])
            `shouldNotBe` allowedHostPorts (Set.singleton (hp "example.com"))

-- The "public host matched by an additional range" arm is rare: it needs a public IP
-- literal whose own /32 is folded into the additional ranges, which lands in only ~5%
-- of generated cases. At hspec-hedgehog's default 100 tests the 'H.cover 2' floor below
-- straddled that rate and failed on unlucky seeds. Draw 1000 tests so the coverage
-- estimate is stable without weakening the floor; 'modifyMaxSuccess' is how
-- hspec-hedgehog sets Hedgehog's test count.
propertiesSpec :: Spec
propertiesSpec = modifyMaxSuccess (const 1000) $ describe "properties" $ do
    it "isBlockedTarget blocks an internal host, or one matched by an additional range" $
        hedgehog $ do
            -- Generate addresses across the blocked ranges plus public ones, and a
            -- random set of operator-configured additional ranges (each a /32 or /128
            -- naming one generated host). A random extra set almost never collides
            -- with the host by chance, so the host's own range is deliberately
            -- included half the time; the invariant is then checked directly.
            host <- forAll genMaybeInternalHost
            extra <- forAll (Gen.set (Range.linear 0 3) genMaybeInternalHost)
            includeHost <- forAll Gen.bool
            let hostRange = singleHostRange host
                additionalRanges =
                    mapMaybe singleHostRange (Set.toList extra)
                        <> maybeToList (guard includeHost *> hostRange)
                matchedByExtra = maybe False (`elem` additionalRanges) hostRange
            H.cover 5 "internal host" (looksInternal host)
            H.cover 5 "public host, unmatched" (not (looksInternal host) && not matchedByExtra)
            H.cover 2 "public host matched by an additional range" (not (looksInternal host) && matchedByExtra)
            isBlockedTarget additionalRanges host === (looksInternal host || matchedByExtra)

{- | An operator-configured single-host range naming exactly @host@ (a @\/32@ for an
IPv4 literal, a @\/128@ for IPv6), or 'Nothing' for a DNS name (which cannot be
expressed as a CIDR range). Used to turn 'genMaybeInternalHost's generated set into
plausible @additionalBlockedRanges@ that sometimes, but not always, name the
property's own @host@.
-}
singleHostRange :: Text -> Maybe IPRange
singleHostRange h
    | T.any (== ':') h = parseBlockedRange (h <> "/128")
    | otherwise = parseBlockedRange (h <> "/32")

{- | Whether a generated host string is one this module's ranges treat as
internal -- restated independently of the implementation so the property is not a
tautology. Matches the dotted-quad and IPv6 literals 'genMaybeInternalHost' emits.
-}
looksInternal :: Text -> Bool
looksInternal h =
    h == "::1"
        || "fe80:" `T.isPrefixOf` h
        || case T.splitOn "." h of
            [a, b, _, _] ->
                a == "127"
                    || (a == "169" && b == "254")
                    || a == "10"
                    || (a == "172" && octetIn b 16 31)
                    || (a == "192" && b == "168")
            _ -> False
  where
    octetIn t lo hi = maybe False (\n -> n >= lo && n <= hi) (readMaybe (toString t) :: Maybe Int)

{- | A host generator mixing internal-range IPv4/IPv6 literals with public
addresses and the odd DNS name, so the SSRF property drives both the blocked and
permitted arms.
-}
genMaybeInternalHost :: H.Gen Text
genMaybeInternalHost =
    Gen.choice
        [ -- link-local incl. the metadata address
          (\c d -> "169.254." <> show c <> "." <> show d) <$> octet <*> octet
        , -- loopback
          (\b c d -> "127." <> show b <> "." <> show c <> "." <> show d) <$> octet <*> octet <*> octet
        , -- RFC1918 10/8
          (\b c d -> "10." <> show b <> "." <> show c <> "." <> show d) <$> octet <*> octet <*> octet
        , -- RFC1918 172.16/12
          (\b c d -> "172." <> show (b :: Int) <> "." <> show c <> "." <> show d)
            <$> Gen.int (Range.linear 16 31)
            <*> octet
            <*> octet
        , -- RFC1918 192.168/16
          (\c d -> "192.168." <> show c <> "." <> show d) <$> octet <*> octet
        , pure "::1"
        , pure "fe80::1"
        , -- public IPv4 (1.x is not in any blocked range)
          (\b c d -> "1." <> show b <> "." <> show c <> "." <> show d) <$> octet <*> octet <*> octet
        , pure "registry.npmjs.org"
        ]
  where
    octet :: H.Gen Int
    octet = Gen.int (Range.linear 0 255)

parseIpLiteralSpec :: Spec
parseIpLiteralSpec = describe "parseIpLiteral" $ do
    it "returns Nothing for empty strings" $
        void (parseIpLiteral "") `shouldBe` Nothing

    it "returns Nothing for regular hostnames" $
        void (parseIpLiteral "registry.npmjs.org") `shouldBe` Nothing

    it "returns Just for standard IPv4" $
        void (parseIpLiteral "127.0.0.1") `shouldBe` Just ()

    it "returns Just for hex IPv4" $
        void (parseIpLiteral "0x7f.0.0.1") `shouldBe` Just ()

    it "returns Just for octal IPv4" $
        void (parseIpLiteral "0177.0.0.1") `shouldBe` Just ()

    it "returns Just for standard IPv6" $ do
        void (parseIpLiteral "::1") `shouldBe` Just ()
        void (parseIpLiteral "fe80::1") `shouldBe` Just ()

    it "returns Just for IPv4-mapped IPv6" $
        void (parseIpLiteral "::ffff:127.0.0.1") `shouldBe` Just ()

    it "returns Nothing for invalid short IPv4" $
        void (parseIpLiteral "127.0.0") `shouldBe` Nothing

    it "returns Nothing for large IPv4 octets" $
        void (parseIpLiteral "0400.0.0.1") `shouldBe` Nothing

    it "returns Nothing for malformed IPv6" $ do
        void (parseIpLiteral "fe80::1ffff") `shouldBe` Nothing
        void (parseIpLiteral "1::2::3") `shouldBe` Nothing

{- | 'parseBlockedRange' is the total decoder the config layer relies on for
@ECLUSE_ADDITIONAL_BLOCKED_RANGES@: a malformed entry must yield 'Nothing' (so the
decoder can fail the boot closed) rather than throwing, unlike the module's own
compile-time 'IPRange' literals.
-}
parseBlockedRangeSpec :: Spec
parseBlockedRangeSpec = describe "parseBlockedRange" $ do
    it "parses a valid IPv4 CIDR range" $
        parseBlockedRange "203.0.113.0/24" `shouldBe` Just "203.0.113.0/24"
    it "parses a valid IPv6 CIDR range" $
        parseBlockedRange "2001:db8::/32" `shouldBe` Just "2001:db8::/32"
    it "parses a single-host /32" $
        parseBlockedRange "10.0.0.5/32" `shouldBe` Just "10.0.0.5/32"
    it "treats a bare IP with no mask as an implicit single-host /32 (iproute's own reading)" $
        parseBlockedRange "203.0.113.0" `shouldBe` Just "203.0.113.0/32"
    it "returns Nothing for a DNS name" $
        parseBlockedRange "example.com/24" `shouldBe` Nothing
    it "returns Nothing for an out-of-range mask length" $
        parseBlockedRange "203.0.113.0/33" `shouldBe` Nothing
    it "returns Nothing for garbage input" $
        parseBlockedRange "not-a-range" `shouldBe` Nothing
    it "returns Nothing for the empty string" $
        parseBlockedRange "" `shouldBe` Nothing

{- Coverage of the ecosystem-host equivalence ('tarballHostAllowedFor'): an
ecosystem's canonical artifact host (PyPI's files host) is same-host-equivalent
under the secure default, while every other gate dimension (allowlist,
internal-range block, the policy for non-ecosystem hosts) is unchanged.
-}
ecosystemHostSpec :: Spec
ecosystemHostSpec = describe "tarballHostAllowedFor (ecosystem artifact hosts)" $ do
    let noOptIn = []
        filesHost = hp "files.pythonhosted.org"
        ecoHosts = allowedHostPorts (Set.fromList [filesHost])
        noEcoHosts = allowedHostPorts Set.empty
        -- The gate builder folds ecosystem hosts into the allowlist; mirror that here.
        allow = allowedHostPorts (Set.fromList [hp "pypi.org", filesHost])
        decide ecos policy target = tarballHostAllowedFor ecos UntrustedOrigin policy allow noOptIn (Just (hp "pypi.org")) (Just target)

    it "admits the ecosystem's canonical artifact host under SameHostAsPackument" $
        decide ecoHosts SameHostAsPackument filesHost `shouldBe` True

    it "still refuses a cross-host target that is not an ecosystem host" $
        decide ecoHosts SameHostAsPackument (hp "cdn.evil.example") `shouldBe` False

    it "changes nothing with no ecosystem hosts (npm's shape): cross-host stays refused" $
        decide noEcoHosts SameHostAsPackument filesHost `shouldBe` False

    it "still requires the ecosystem host to be allowlisted (fail closed off-list)" $ do
        let allowWithoutFiles = allowedHostPorts (Set.fromList [hp "pypi.org"])
        tarballHostAllowedFor ecoHosts UntrustedOrigin SameHostAsPackument allowWithoutFiles noOptIn (Just (hp "pypi.org")) (Just filesHost)
            `shouldBe` False

    it "still blocks an internal-range ecosystem host on the untrusted origin" $ do
        let internal = hp "10.0.0.5"
            ecoInternal = allowedHostPorts (Set.fromList [internal])
            allowInternal = allowedHostPorts (Set.fromList [hp "pypi.org", internal])
        tarballHostAllowedFor ecoInternal UntrustedOrigin SameHostAsPackument allowInternal noOptIn (Just (hp "pypi.org")) (Just internal)
            `shouldBe` False

    it "gate builder: ecosystem hosts enter the allowlist and the ecosystem set" $ do
        let gate = tarballHostGate ["https://files.pythonhosted.org"] Nothing "https://pypi.org" Nothing
        isAllowedUpstreamHost (thgAllowlist gate) filesHost `shouldBe` True
        isAllowedUpstreamHost (thgEcosystemHosts gate) filesHost `shouldBe` True
        isAllowedUpstreamHost (thgEcosystemHosts gate) (hp "pypi.org") `shouldBe` False
