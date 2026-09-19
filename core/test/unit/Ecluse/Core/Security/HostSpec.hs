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
import Ecluse.Security.Support (hp, hpAt, upstreamHosts, upstreams)

spec :: Spec
spec = do
    hostAllowlistSpec
    classificationCorpusSpec
    additionalRangesSpec
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

-- The one dimension the classification corpus does not reach: every row there runs against
-- the fixed range set alone.
additionalRangesSpec :: Spec
additionalRangesSpec = describe "isBlockedTarget (operator-configured additional ranges)" $ do
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

{- | Every classification 'isBlockedTarget' owes under the fixed range set, expected value
written out rather than derived from any implementation.
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
            <> documentationRanges
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
        , ("::ffff:7f00:1", True) -- mapped loopback, hex spelling
        , ("0:0:0:0:0:ffff:127.0.0.1", True) -- mapped loopback, fully expanded
        , ("::ffff:a00:1", True) -- mapped RFC1918 10/8, hex spelling
        , ("::ffff:1.1.1.1", False) -- mapped public stays permitted
        , ("::ffff:101:101", False) -- mapped public, hex spelling
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

    -- A tripwire, not plain coverage. The e2e suite runs on a docker network in TEST-NET-3
    -- and needs these ranges reachable. A documentation range never aliases a real service,
    -- so blocking it adds no SSRF protection.
    documentationRanges =
        [ ("203.0.113.2", False) -- TEST-NET-3 203.0.113.0/24, the e2e network subnet
        , ("192.0.2.1", False) -- TEST-NET-1 192.0.2.0/24
        , ("198.51.100.1", False) -- TEST-NET-2 198.51.100.0/24
        ]

    externals =
        [ ("8.8.8.8", False)
        , ("1.1.1.1", False)
        , ("93.184.216.34", False)
        , ("172.32.0.1", False) -- just above the 172.16/12 block
        , ("11.0.0.1", False) -- just above 10/8
        , ("1.0.0.0", False) -- just above the 0/8 this-host block
        , ("100.63.255.255", False) -- just below CGNAT
        , ("100.128.0.1", False) -- just above CGNAT
        , ("2606:4700::1111", False)
        , ("2606:2800:220:1:248:1893:25c8:1946", False) -- a fully written public IPv6 address
        , ("2001:db8::1", False)
        , ("fbff::1", False) -- just below fc00::/7
        , ("fe00::1", False) -- between fc00::/7 and fe80::/10
        ]

    -- Each of these must fail to parse as an IP, so nothing mistakes it for an internal
    -- literal. The allowlist still gates a real name.
    names =
        [ ("registry.npmjs.org", False) -- a DNS name
        , ("", False) -- empty
        , ("10.0.0.256", False) -- octet out of range → not a literal
        , ("10.0.0.x", False) -- non-numeric octet → not a literal
        , ("10.0.0", False) -- too few octets → not a literal
        , ("10..0.1", False) -- empty octet → not a literal
        , ("2130706433", False) -- a bare 32-bit number: a short inet_aton form, not modelled here
        , ("1::2::3", False) -- two "::" → malformed
        , ("1:2:3:4:5:6:7:8::", False) -- compressed though eight groups are already written
        , ("1:2:3", False) -- uncompressed with the wrong group count
        , ("fe80::zz", False) -- a non-hex group
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
