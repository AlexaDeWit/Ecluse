-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Security.HostSpec (spec) where

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
    allowedHostPorts,
    hostPortAddress,
    isAllowedUpstreamHost,
    isBlockedTarget,
    parseBlockedRange,
    tarballHostAllowed,
    tarballHostGate,
    thgAllowlist,
    thgEcosystemHosts,
 )

{- | The raw configured upstream authorities, mixed case on purpose, so a case can extend them
before normalising through 'allowedHostPorts'. Every entry is portless, so each authorises 443
alone.
-}
upstreamHosts :: Set.Set HostPort
upstreamHosts = Set.fromList [hp "registry.npmjs.org", hp "Private.Internal.Example.com"]

{- | The configured upstreams, normalised through 'allowedHostPorts', the only way to
obtain the 'AllowedHostPorts' the host guards take.
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
    ssrfGateSpec
    tarballHostPolicySpec
    ecosystemHostSpec
    allowedHostPortsSpec
    propertiesSpec
    parseBlockedRangeSpec

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
            -- An entry without a port authorises 443 alone. An allowlisted host at an
            -- attacker-chosen port never inherits the host's authorisation.
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
            -- canonicalHostKey collapses IPv6 spellings. The port rides along untouched.
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
            -- The IPv6 analogue of 169.254.169.254. The block must catch an SSRF
            -- aimed at IPv6 instance metadata alongside the IPv4 endpoint.
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
        -- A tool or an attacker emits the dotted spelling, so the block must decode it as
        -- well as the all-hex spelling above.
        it "blocks the instance-metadata address (::ffff:169.254.169.254)" $
            isBlockedTarget noOptIn "::ffff:169.254.169.254" `shouldBe` True
        it "blocks mapped loopback (::ffff:127.0.0.1)" $
            isBlockedTarget noOptIn "::ffff:127.0.0.1" `shouldBe` True
        it "blocks the fully-expanded mapped loopback (0:0:0:0:0:ffff:127.0.0.1)" $
            isBlockedTarget noOptIn "0:0:0:0:0:ffff:127.0.0.1" `shouldBe` True
        it "does not block a mapped public address (::ffff:1.1.1.1)" $
            isBlockedTarget noOptIn "::ffff:1.1.1.1" `shouldBe` False

    describe "blocks IPv4-compatible IPv6 (::/96)" $ do
        -- RFC 4291 2.5.5.1 deprecates IPv4-compatible addresses, but many stacks still accept
        -- them, Ecluse's parser included, so the block decodes them to their embedded IPv4.
        it "blocks the instance-metadata address (::169.254.169.254)" $
            isBlockedTarget noOptIn "::169.254.169.254" `shouldBe` True
        it "blocks compatible loopback (::127.0.0.1)" $
            isBlockedTarget noOptIn "::127.0.0.1" `shouldBe` True
        it "blocks the fully-expanded compatible loopback (0:0:0:0:0:0:127.0.0.1)" $
            isBlockedTarget noOptIn "0:0:0:0:0:0:127.0.0.1" `shouldBe` True
        it "does not block a compatible public address (::1.1.1.1)" $
            isBlockedTarget noOptIn "::1.1.1.1" `shouldBe` False

    describe "blocks NAT64-embedded IPv4 under the well-known prefix (64:ff9b::/96, RFC 6052)" $ do
        -- On a fabric that runs NAT64, an address under the well-known prefix routes to its
        -- embedded IPv4, so the block decodes and tests that address against the IPv4 ranges.
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
        -- The local-use prefix is a /48. Any /96 within it embeds the IPv4 in the
        -- low 32 bits, so the decode holds across the middle bits.
        it "blocks the instance-metadata address (64:ff9b:1::169.254.169.254)" $
            isBlockedTarget noOptIn "64:ff9b:1::169.254.169.254" `shouldBe` True
        it "blocks an internal embedding under a non-zero /96 within the /48 (64:ff9b:1:aaaa::10.0.0.1)" $
            isBlockedTarget noOptIn "64:ff9b:1:aaaa::10.0.0.1" `shouldBe` True
        it "does not block a local-use embedding of a public address (64:ff9b:1::1.1.1.1)" $
            isBlockedTarget noOptIn "64:ff9b:1::1.1.1.1" `shouldBe` False

    describe "treats malformed IPv6 literals as names (not blocked)" $ do
        -- Each malformed form must fail to parse as an IP, so nothing mistakes it
        -- for an internal literal. The allowlist would still gate a real name.
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
            -- "10.0.0.256" is not a valid dotted-quad, so the parser does not read
            -- it as the 10/8 literal it resembles.
            isBlockedTarget noOptIn "10.0.0.256" `shouldBe` False
        it "treats a non-numeric octet as a name, not an internal IP" $
            isBlockedTarget noOptIn "10.0.0.x" `shouldBe` False
        it "treats a dotted-quad with too few octets as a name" $
            isBlockedTarget noOptIn "10.0.0" `shouldBe` False
        it "treats an empty octet as a name" $
            isBlockedTarget noOptIn "10..0.1" `shouldBe` False
        it "does not block the empty host" $
            -- The empty string parses as no IP literal, so it is not internal. The
            -- host allowlist rejects it independently.
            isBlockedTarget noOptIn "" `shouldBe` False

    describe "deliberately treats RFC 5737 documentation ranges as external" $ do
        -- A tripwire, not plain coverage. The e2e suite runs on a docker network in TEST-NET-3
        -- (203.0.113.0/24) and needs these ranges reachable. A documentation range never
        -- aliases a real service, so blocking it adds no SSRF protection.
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
            -- The block still catches a fixed-range address (10/8) alongside an
            -- unrelated additional range: additional ranges only ever widen the block.
            isBlockedTarget testNet3 "10.1.2.3" `shouldBe` True
        it "blocks an IPv6 host matched by an additional range" $
            isBlockedTarget ["2001:db8::/32"] "2001:db8::1" `shouldBe` True
        it "does not block a DNS name even when it lexically resembles a blocked range" $
            isBlockedTarget testNet3 "203.0.113.example.com" `shouldBe` False

    describe "coerces an IPv4 octet as inet_aton does (leading-zero octal, 0x hex)" $ do
        -- The block reads each octet in the base a libc resolver would, so it tests the
        -- address actually dialled. The smoke oracle ("Ecluse.Core.Security.HostSmokeSpec")
        -- validates these expectations against the real 'getAddrInfo'.
        it "blocks 0012.0.0.1 -- octal 0012 = 10.0.0.1, an RFC1918 address" $
            -- The reported under-block: a decimal reading sees 12 (public) and lets it
            -- through. Octal reads 10, the internal address the resolver actually dials.
            isBlockedTarget noOptIn "0012.0.0.1" `shouldBe` True
        it "blocks 0177.0.0.1 -- octal 0177 = 127.0.0.1, loopback" $
            isBlockedTarget noOptIn "0177.0.0.1" `shouldBe` True
        it "blocks 0x7f.0.0.1 -- hex 0x7f = 127.0.0.1, loopback" $
            isBlockedTarget noOptIn "0x7f.0.0.1" `shouldBe` True
        it "does not block 010.0.0.1 -- octal 010 = 8.0.0.1, a public address" $
            -- A decimal misreading over-blocks this as 10.0.0.1. Octal is 8.0.0.1, which
            -- the resolver confirms is public, so the literal layer must not block it.
            isBlockedTarget noOptIn "010.0.0.1" `shouldBe` False
        it "does not block 0127.0.0.1 -- octal 0127 = 87.0.0.1, a public address" $
            isBlockedTarget noOptIn "0127.0.0.1" `shouldBe` False
        it "treats 08.0.0.1 as a name -- 8 is not an octal digit (a resolver rejects it)" $
            isBlockedTarget noOptIn "08.0.0.1" `shouldBe` False
        it "treats 0400.0.0.1 as a name -- octal 0400 = 256 overflows an octet" $
            isBlockedTarget noOptIn "0400.0.0.1" `shouldBe` False
        it "does not block the short 32-bit form 2130706433 (not a four-part literal here)" $
            -- inet_aton resolves this to 127.0.0.1, but the four-part recogniser does not model
            -- the short forms. The allowlist and certificate validation constrain such names.
            isBlockedTarget noOptIn "2130706433" `shouldBe` False

{- | The blocked-vs-allowed classification of 'isBlockedTarget', pinned against an explicit
expected table rather than any prior implementation. A leading-zero octet coerces as octal, as a
libc resolver does: @0012.0.0.1@ is @10.0.0.1@ and blocks, while @010.0.0.1@ (@8.0.0.1@) and
@0127.0.0.1@ (@87.0.0.1@) are public and do not. A @0x@ octet is hexadecimal. @fe80::1ffff@
overflows 16 bits, so it stays a name the allowlist constrains.
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

    -- (host, expected-blocked). Grouped by intent: every internal range, every
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

{- | The outbound-fetch guarantee is the conjunction: Ecluse fetches a target only if the host
allowlist admits it and it is not an internal address.
-}
ssrfGateSpec :: Spec
ssrfGateSpec = describe "composed SSRF gate (allowlist AND not-blocked)" $ do
    let noOptIn = []
        -- The allowlist authorises the host:port pair. The internal-range block
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
        -- '::ffff:a9fe:a9fe' is 169.254.169.254 in IPv4-mapped form. The block decodes the
        -- embedded IPv4, so the gate refuses it even when an operator allowlists this form.
        passesGate (hp "::ffff:a9fe:a9fe") `shouldBe` False
    it "refuses a metadata authority extracted from a URL" $
        (passesGate <$> hostPortAddress "http://169.254.169.254/latest/meta-data/")
            `shouldBe` Just False

{- | The @dist.tarball@ host gate: Ecluse fetches a tarball only from the authority (host and
port) that served the packument, plus the ecosystem's declared artifact hosts, and never off the
allowlist. The internal-range block is origin-aware: it gates the untrusted origin and exempts the
trusted private origin (security.md invariant 3).
-}
tarballHostPolicySpec :: Spec
tarballHostPolicySpec = describe "tarballHostAllowed" $ do
    let noOptIn = []
        noEco = allowedHostPorts Set.empty
        -- Two allowlisted upstreams: the packument source and a separate CDN.
        allow = allowedHostPorts (Set.fromList [hp "registry.npmjs.org", hp "cdn.npmjs.org"])
        -- The untrusted public origin: the internal-range block applies (the
        -- allowlist and internal-range coverage above is over this origin).
        same packument target = tarballHostAllowed noEco UntrustedOrigin allow noOptIn (Just packument) (Just target)
        -- A short alias: packument origin fixed to the npm registry on 443.
        decide = same (hp "registry.npmjs.org")

    describe "the same-authority clause (unconditional)" $ do
        it "admits a tarball on the same authority that served the packument" $
            decide (hp "registry.npmjs.org") `shouldBe` True
        it "refuses a tarball on a different host, even one on the allowlist" $
            -- The crux of the gate: the gate refuses an allowlisted but different
            -- CDN. Only an adapter-declared ecosystem host is same-host-equivalent.
            decide (hp "cdn.npmjs.org") `shouldBe` False
        it "refuses a tarball on a host not on the allowlist" $
            decide (hp "evil.example.com") `shouldBe` False
        it "matches the same-host clause case-insensitively (DNS is)" $
            decide (hp "Registry.NPMJS.org") `shouldBe` True
        it "refuses an empty tarball host" $
            decide (HostPort "" 443) `shouldBe` False
        it "refuses a look-alike suffix of the packument host" $
            -- registry.npmjs.org.evil.com is neither allowlisted nor equal.
            decide (hp "registry.npmjs.org.evil.com") `shouldBe` False

    describe "the port dimension (the gate authorises host and port as a pair)" $ do
        it "refuses a nonstandard-port dist.tarball when the entry carries no port" $
            -- dist.tarball names registry.npmjs.org:9443 after a packument from that host on 443.
            -- The port must reach the allowlist and the same-authority clause undiscarded.
            decide (hpAt "registry.npmjs.org" 9443) `shouldBe` False
        it "refuses a port mismatch between packument origin and tarball even with both pairs allowlisted" $
            -- Same host, both pairs allowlisted: the same-authority clause still
            -- refuses, because the origin dialled 443 and the tarball names 9443.
            let bothPorts = allowedHostPorts (Set.fromList [hp "registry.npmjs.org", hpAt "registry.npmjs.org" 9443])
             in tarballHostAllowed noEco UntrustedOrigin bothPorts noOptIn (Just (hp "registry.npmjs.org")) (Just (hpAt "registry.npmjs.org" 9443))
                    `shouldBe` False
        it "admits a nonstandard-port tarball when the origin and the entry both name that pair" $
            -- An operator whose upstream lives on a nonstandard port states the pair
            -- explicitly. The origin dialled it and the entry authorises it.
            let at9443 = allowedHostPorts (Set.singleton (hpAt "registry.internal.example.com" 9443))
             in tarballHostAllowed noEco UntrustedOrigin at9443 noOptIn (Just (hpAt "registry.internal.example.com" 9443)) (Just (hpAt "registry.internal.example.com" 9443))
                    `shouldBe` True
        it "refuses an unextractable tarball authority (fail closed)" $
            tarballHostAllowed noEco UntrustedOrigin allow noOptIn (Just (hp "registry.npmjs.org")) Nothing
                `shouldBe` False
        it "refuses an unextractable packument origin (fail closed)" $
            tarballHostAllowed noEco UntrustedOrigin allow noOptIn Nothing (Just (hp "registry.npmjs.org"))
                `shouldBe` False

    describe "the internal-range block beats the other clauses (untrusted origin)" $ do
        it "refuses an internal literal even when it equals the packument authority" $
            -- The internal block still vetoes a tarball at a misconfigured internal upstream. The
            -- allowlist must carry the literal for the case to reach the block clause.
            let allowInternal = allowedHostPorts (Set.singleton (hp "169.254.169.254"))
             in tarballHostAllowed noEco UntrustedOrigin allowInternal noOptIn (Just (hp "169.254.169.254")) (Just (hp "169.254.169.254"))
                    `shouldBe` False
        it "refuses an internal literal regardless of its port (the block classifies the host alone)" $
            -- The port never launders an internal address: 10.0.0.5:8443 is as
            -- internal as 10.0.0.5.
            let allowInternal = allowedHostPorts (Set.singleton (hpAt "10.0.0.5" 8443))
             in tarballHostAllowed noEco UntrustedOrigin allowInternal noOptIn (Just (hpAt "10.0.0.5" 8443)) (Just (hpAt "10.0.0.5" 8443))
                    `shouldBe` False
        it "still blocks a host matched only by an operator-configured additional range" $
            let allowInternal = allowedHostPorts (Set.singleton (hp "10.0.0.5"))
             in tarballHostAllowed noEco UntrustedOrigin allowInternal ["10.0.0.5/32"] (Just (hp "10.0.0.5")) (Just (hp "10.0.0.5"))
                    `shouldBe` False

    describe "the trusted private origin is exempt from the internal-range block" $ do
        -- The trusted origin mirrors the connection layer's unguarded manager (security.md
        -- invariant 3). A private registry may live on an internal address, so the gate admits
        -- its same-host dist.tarball. The allowlist and same-authority clauses still gate it.
        let allowInternal = allowedHostPorts (Set.singleton (hp "10.0.0.5"))
        it "admits a same-authority internal-literal tarball (where untrusted is refused)" $ do
            tarballHostAllowed noEco TrustedOrigin allowInternal noOptIn (Just (hp "10.0.0.5")) (Just (hp "10.0.0.5"))
                `shouldBe` True
            -- The internal block refuses the same inputs on the untrusted origin.
            tarballHostAllowed noEco UntrustedOrigin allowInternal noOptIn (Just (hp "10.0.0.5")) (Just (hp "10.0.0.5"))
                `shouldBe` False
        it "still refuses a trusted tarball off the host allowlist (allowlist not relaxed)" $
            -- The exemption covers the internal-range clause only, so the gate still refuses an
            -- off-allowlist host.
            tarballHostAllowed noEco TrustedOrigin allowInternal noOptIn (Just (hp "10.0.0.5")) (Just (hp "192.168.0.9"))
                `shouldBe` False
        it "still refuses a cross-host trusted tarball (same-host not relaxed)" $
            -- The trusted origin's tarball must still equal its packument authority, so the gate
            -- refuses a different allowlisted internal host.
            let bothAllowed = allowedHostPorts (Set.fromList [hp "10.0.0.5", hp "10.0.0.6"])
             in tarballHostAllowed noEco TrustedOrigin bothAllowed noOptIn (Just (hp "10.0.0.5")) (Just (hp "10.0.0.6"))
                    `shouldBe` False
        it "still refuses a trusted port mismatch (the pair must match)" $
            -- The trusted exemption never opens the port dimension: the gate refuses
            -- a trusted upstream's tarball on another port of its own host.
            let bothPorts = allowedHostPorts (Set.fromList [hp "10.0.0.5", hpAt "10.0.0.5" 8443])
             in tarballHostAllowed noEco TrustedOrigin bothPorts noOptIn (Just (hp "10.0.0.5")) (Just (hpAt "10.0.0.5" 8443))
                    `shouldBe` False

allowedHostPortsSpec :: Spec
allowedHostPortsSpec = describe "allowedHostPorts" $ do
    it "folds configured-host case so a mixed-case entry matches a lowercase query" $
        -- 'allowedHostPorts' is the only constructor of the 'AllowedHostPorts' the guard
        -- takes, so the guard relies on it for normalisation.
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

-- The "public host matched by an additional range" arm hits about 5% of generated
-- cases, which straddled the 'H.cover 2' floor at hspec-hedgehog's default 100 tests.
-- Draw 1000 so the coverage estimate is stable without weakening the floor.
propertiesSpec :: Spec
propertiesSpec = modifyMaxSuccess (const 1000) $ describe "properties" $ do
    it "isBlockedTarget blocks an internal host, or one matched by an additional range" $
        hedgehog $ do
            -- A random additional-range set almost never names the generated host by chance, so
            -- the generator includes the host's own range half the time.
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

{- | An operator-configured single-host range naming exactly @host@, a @\/32@ for an IPv4 literal
and a @\/128@ for IPv6, or 'Nothing' for a DNS name, which no CIDR range can express.
-}
singleHostRange :: Text -> Maybe IPRange
singleHostRange h
    | T.any (== ':') h = parseBlockedRange (h <> "/128")
    | otherwise = parseBlockedRange (h <> "/32")

{- | Whether a generated host string is one this module's ranges treat as internal. It restates
the ranges independently of the implementation, so the property is not a tautology.
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

{- | A host generator mixing internal-range IPv4 and IPv6 literals with public addresses and the
odd DNS name, so the SSRF property drives both the blocked and the permitted arms.
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

{- | 'parseBlockedRange' is the total decoder the config layer relies on for
@ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES@. A malformed entry yields 'Nothing', so boot fails
closed rather than throwing as the module's own compile-time 'IPRange' literals do.
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

{- Coverage of the ecosystem-host equivalence in 'tarballHostAllowed'. An ecosystem's canonical
artifact host is same-host-equivalent under the secure default. Every other gate dimension holds:
the allowlist, the internal-range block, and the policy for non-ecosystem hosts.
-}
ecosystemHostSpec :: Spec
ecosystemHostSpec = describe "tarballHostAllowed (ecosystem artifact hosts)" $ do
    let noOptIn = []
        filesHost = hp "files.pythonhosted.org"
        ecoHosts = allowedHostPorts (Set.fromList [filesHost])
        noEcoHosts = allowedHostPorts Set.empty
        -- The gate builder folds ecosystem hosts into the allowlist. Mirror that here.
        allow = allowedHostPorts (Set.fromList [hp "pypi.org", filesHost])
        decide ecos target = tarballHostAllowed ecos UntrustedOrigin allow noOptIn (Just (hp "pypi.org")) (Just target)

    it "admits the ecosystem's canonical artifact host as same-host-equivalent" $
        decide ecoHosts filesHost `shouldBe` True

    it "still refuses a cross-host target that is not an ecosystem host" $
        decide ecoHosts (hp "cdn.evil.example") `shouldBe` False

    it "changes nothing with no ecosystem hosts (npm's shape): cross-host stays refused" $
        decide noEcoHosts filesHost `shouldBe` False

    it "still requires the ecosystem host to be allowlisted (fail closed off-list)" $ do
        let allowWithoutFiles = allowedHostPorts (Set.fromList [hp "pypi.org"])
        tarballHostAllowed ecoHosts UntrustedOrigin allowWithoutFiles noOptIn (Just (hp "pypi.org")) (Just filesHost)
            `shouldBe` False

    it "admits an ecosystem-host tarball only at its allowlisted pair (the port dimension holds)" $ do
        let filesAt8443 = hpAt "files.pythonhosted.org" 8443
            ecoAt = allowedHostPorts (Set.fromList [filesAt8443])
            allowAt = allowedHostPorts (Set.fromList [hp "pypi.org", filesAt8443])
            filesPort port = tarballHostAllowed ecoAt UntrustedOrigin allowAt noOptIn (Just (hp "pypi.org")) (Just (hpAt "files.pythonhosted.org" port))
        filesPort 8443 `shouldBe` True
        filesPort 9443 `shouldBe` False

    it "still blocks an internal-range ecosystem host on the untrusted origin" $ do
        let internal = hp "10.0.0.5"
            ecoInternal = allowedHostPorts (Set.fromList [internal])
            allowInternal = allowedHostPorts (Set.fromList [hp "pypi.org", internal])
        tarballHostAllowed ecoInternal UntrustedOrigin allowInternal noOptIn (Just (hp "pypi.org")) (Just internal)
            `shouldBe` False

    it "gate builder: ecosystem hosts enter the allowlist and the ecosystem set" $ do
        let gate = tarballHostGate ["https://files.pythonhosted.org"] Nothing "https://pypi.org" Nothing
        isAllowedUpstreamHost (thgAllowlist gate) filesHost `shouldBe` True
        isAllowedUpstreamHost (thgEcosystemHosts gate) filesHost `shouldBe` True
        isAllowedUpstreamHost (thgEcosystemHosts gate) (hp "pypi.org") `shouldBe` False
