module Ecluse.SecuritySpec (spec) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), eitherDecodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Hedgehog (annotateShow, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available),
    CodeExecSignal (NoCodeOnInstall),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    Trust (Untrusted),
    mkPackageName,
    mkScope,
    renderPackageName,
 )
import Ecluse.Core.Registry.Npm.Project (Projection (NameMismatch, Projected), parsePackageInfoFromValue)
import Ecluse.Core.Security (
    LimitError (..),
    Limits (..),
    LoweredHostSet,
    Origin (TrustedOrigin, UntrustedOrigin),
    TarballHostPolicy (..),
    UrlError (..),
    boundedRead,
    checkNestingDepth,
    checkVersionCount,
    defaultLimits,
    hostAddress,
    isAllowedUpstreamHost,
    isBlockedTarget,
    lowerCaseHosts,
    splitHostPort,
    tarballHostAllowed,
    upstreamUrlFor,
 )
import Ecluse.Core.Version (Version, mkVersion)

-- ── fixtures ─────────────────────────────────────────────────────────────────

{- | The raw configured upstream hosts (lower-/mixed-case on purpose), before
normalisation. Kept unwrapped so a case can extend it (e.g. the SSRF gate's
allowlisted-internal case) before lowering it through 'lowerCaseHosts'.
-}
upstreamHosts :: Set.Set Text
upstreamHosts = Set.fromList ["registry.npmjs.org", "Private.Internal.Example.com"]

{- | The configured upstreams, normalised through 'lowerCaseHosts' — the only way
to obtain the 'LoweredHostSet' the host guards take.
-}
upstreams :: LoweredHostSet
upstreams = lowerCaseHosts upstreamHosts

-- | An unscoped npm package identity.
unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

-- | A scoped npm package identity (scope, base name).
scoped :: Text -> Text -> PackageName
scoped scope = mkPackageName Npm (Just (mkScope scope))

-- | An inert artifact; the version-count guard never inspects artifacts.
sampleArtifact :: Artifact
sampleArtifact =
    Artifact
        { artFilename = "thing-1.0.0.tgz"
        , artUrl = "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
        , artKind = Tarball
        , artHashes = []
        , artSize = Nothing
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }

-- | A minimal per-version snapshot; only name/version are meaningful here.
details :: PackageName -> Version -> PackageDetails
details name version =
    PackageDetails
        { pkgName = name
        , pkgVersion = version
        , pkgPublishedAt = Nothing
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = sampleArtifact :| []
        , pkgLicenses = ["MIT"]
        , pkgPublisher = Nothing
        , pkgMaintainers = []
        , pkgDependencies = []
        }

{- | A packument carrying @n@ distinct versions @0.0.1 .. 0.0.n@. Used to drive
'checkVersionCount' on either side of its ceiling.
-}
packumentWith :: Int -> PackageInfo
packumentWith n =
    let name = unscoped "thing"
        ver i = "0.0." <> show i
     in PackageInfo
            { infoName = name
            , infoVersions =
                Map.fromList
                    [ (ver i, details name (mkVersion Npm (ver i)))
                    | i <- [1 .. n]
                    ]
            , infoDistTags = Map.empty
            , infoPublishedAt = Map.empty
            }

{- | Drive 'boundedRead' purely: a 'State'-monad chunk producer that pops one
chunk per call and yields an empty 'ByteString' (the @BodyReader@ EOF signal) once
the list is exhausted. Exercises the @Monad m@ polymorphism without 'IO'.
-}
runBounded :: Limits -> [ByteString] -> Either LimitError ByteString
runBounded limits = evalState (boundedRead limits next)
  where
    next :: State [ByteString] ByteString
    next =
        get >>= \case
            [] -> pure BS.empty
            (c : cs) -> put cs >> pure c

-- ── spec ─────────────────────────────────────────────────────────────────────

spec :: Spec
spec = do
    hostAllowlistSpec
    internalRangeSpec
    classificationCorpusSpec
    hostAddressSpec
    splitHostPortSpec
    ssrfGateSpec
    tarballHostPolicySpec
    upstreamUrlSpec
    boundedReadSpec
    versionCountSpec
    nestingDepthSpec
    realPackumentSpec
    showInstancesSpec
    lowerCaseHostsSpec
    propertiesSpec

{- | The error\/config types derive 'Show' for diagnostics and test output; assert
each renders so the contract is exercised (and not silently dropped).
-}
showInstancesSpec :: Spec
showInstancesSpec = describe "Show instances" $ do
    it "renders LimitError values" $ do
        show (BodyTooLarge 10) `shouldBe` ("BodyTooLarge 10" :: Text)
        show (TooManyVersions 4 3) `shouldBe` ("TooManyVersions 4 3" :: Text)
        show (TooDeeplyNested 3) `shouldBe` ("TooDeeplyNested 3" :: Text)
    it "renders UrlError values" $ do
        show (UnsafeComponent "..") `shouldBe` ("UnsafeComponent \"..\"" :: Text)
        show EmptyBaseUrl `shouldBe` ("EmptyBaseUrl" :: Text)
    it "renders TarballHostPolicy values" $ do
        show SameHostAsPackument `shouldBe` ("SameHostAsPackument" :: Text)
        show AnyAllowlistedHost `shouldBe` ("AnyAllowlistedHost" :: Text)
    it "renders Limits" $
        show defaultLimits
            `shouldBe` ( "Limits {maxBodyBytes = 16777216, maxVersionCount = 100000, maxNestingDepth = 64}" ::
                            Text
                       )

-- ── outbound host allowlist ──────────────────────────────────────────────────

hostAllowlistSpec :: Spec
hostAllowlistSpec = describe "isAllowedUpstreamHost" $ do
    it "accepts a configured upstream host" $
        isAllowedUpstreamHost upstreams "registry.npmjs.org" `shouldBe` True
    it "rejects an attacker-chosen host not on the allowlist" $
        isAllowedUpstreamHost upstreams "evil.example.com" `shouldBe` False
    it "rejects a look-alike subdomain of an allowed host" $
        -- An allowlist is exact: a host that merely *ends with* an allowed name
        -- (registry.npmjs.org.evil.com) must not slip through.
        isAllowedUpstreamHost upstreams "registry.npmjs.org.evil.com" `shouldBe` False
    it "matches case-insensitively (DNS is case-insensitive)" $
        isAllowedUpstreamHost upstreams "Registry.NPMJS.org" `shouldBe` True
    it "rejects the empty host" $
        isAllowedUpstreamHost upstreams "" `shouldBe` False
    it "rejects every host when the allowlist is empty" $
        isAllowedUpstreamHost (lowerCaseHosts Set.empty) "registry.npmjs.org" `shouldBe` False

-- ── internal-range block ─────────────────────────────────────────────────────

internalRangeSpec :: Spec
internalRangeSpec = describe "isBlockedTarget" $ do
    let noOptIn = lowerCaseHosts Set.empty

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
        -- reachable rather than blocked. That is correct — a documentation range
        -- never aliases a real service, so blocking it adds no SSRF protection —
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

    describe "explicit per-host opt-in" $ do
        it "permits a deliberately-internal upstream that is opted in" $
            isBlockedTarget (lowerCaseHosts (Set.singleton "10.0.0.5")) "10.0.0.5" `shouldBe` False
        it "still blocks an internal address that is not the opted-in one" $
            isBlockedTarget (lowerCaseHosts (Set.singleton "10.0.0.5")) "10.0.0.6" `shouldBe` True
        it "honours an opt-in written in a different case (matched case-insensitively)" $
            -- 'lowerCaseHosts' normalises the opt-in set, so 'FE80::1' opts in
            -- 'fe80::1' rather than over-blocking it on a case mismatch.
            isBlockedTarget (lowerCaseHosts (Set.singleton "FE80::1")) "fe80::1" `shouldBe` False
        it "honours an IPv6 opt-in written in its expanded form against the compressed literal" $
            -- The opt-in is canonicalised to the same form a literal renders to, so
            -- the expanded '0:0:0:0:0:0:0:1' opts in the compressed '::1' rather than
            -- missing it on a textual mismatch.
            isBlockedTarget (lowerCaseHosts (Set.singleton "0:0:0:0:0:0:0:1")) "::1" `shouldBe` False
        it "honours an IPv6 opt-in written in its compressed form against the expanded literal" $
            -- The reverse direction: a compressed opt-in matches an expanded query,
            -- since both are canonicalised before comparison.
            isBlockedTarget (lowerCaseHosts (Set.singleton "::1")) "0:0:0:0:0:0:0:1" `shouldBe` False
        it "does not over-opt-in: a different IPv6 address than the canonical opt-in is still blocked" $
            isBlockedTarget (lowerCaseHosts (Set.singleton "0:0:0:0:0:0:0:1")) "fe80::1" `shouldBe` True
        it "leaves an IPv4 opt-in unaffected by canonicalisation" $
            isBlockedTarget (lowerCaseHosts (Set.singleton "10.0.0.5")) "10.0.0.5" `shouldBe` False

    describe "coerces an IPv4 octet as inet_aton does (leading-zero octal, 0x hex)" $ do
        -- The literal block reads each octet in the base a libc resolver would, so it
        -- tests the address actually dialled rather than a decimal misreading. These
        -- expectations are validated against the real 'getAddrInfo' in the non-gating
        -- smoke oracle ("Ecluse.SecurityResolverOracleSpec").
        it "blocks 0012.0.0.1 — octal 0012 = 10.0.0.1, an RFC1918 address" $
            -- The reported under-block: a decimal reading sees 12 (public) and lets it
            -- through; octal reads 10, the internal address the resolver actually dials.
            isBlockedTarget noOptIn "0012.0.0.1" `shouldBe` True
        it "blocks 0177.0.0.1 — octal 0177 = 127.0.0.1, loopback" $
            isBlockedTarget noOptIn "0177.0.0.1" `shouldBe` True
        it "blocks 0x7f.0.0.1 — hex 0x7f = 127.0.0.1, loopback" $
            isBlockedTarget noOptIn "0x7f.0.0.1" `shouldBe` True
        it "does not block 010.0.0.1 — octal 010 = 8.0.0.1, a public address" $
            -- A decimal misreading over-blocks this as 10.0.0.1; octal is 8.0.0.1, which
            -- the resolver confirms is public, so the literal layer must not block it.
            isBlockedTarget noOptIn "010.0.0.1" `shouldBe` False
        it "does not block 0127.0.0.1 — octal 0127 = 87.0.0.1, a public address" $
            isBlockedTarget noOptIn "0127.0.0.1" `shouldBe` False
        it "treats 08.0.0.1 as a name — 8 is not an octal digit (a resolver rejects it)" $
            isBlockedTarget noOptIn "08.0.0.1" `shouldBe` False
        it "treats 0400.0.0.1 as a name — octal 0400 = 256 overflows an octet" $
            isBlockedTarget noOptIn "0400.0.0.1" `shouldBe` False
        it "leaves the short 32-bit form 2130706433 to the connect-time recheck (not a literal here)" $
            -- inet_aton resolves this to 127.0.0.1, but the four-part recogniser does not
            -- model the short forms; the resolved-IP recheck in Ecluse.Core.Security.Egress
            -- is the backstop for it.
            isBlockedTarget noOptIn "2130706433" `shouldBe` False

-- ── classification corpus (the equivalence bar) ──────────────────────────────

{- | The blocked-vs-allowed classification of 'isBlockedTarget', pinned against an
__explicit expected table__ rather than any prior implementation. The internal
block recognises a host as a literal with a deliberately lenient hand-rolled
parser and delegates only the range membership to a library, so this corpus
guards that the gate neither narrows nor widens: every internal range blocks, the
IPv4-mapped smuggling forms decode and block, and the lenient/strict boundary
spellings classify exactly as documented.

The boundary cases are the load-bearing ones. A leading-zero octet is coerced as
__octal__, exactly as a libc resolver does, so it is /not/ its decimal digits:
@0012.0.0.1@ is octal @10.0.0.1@ and is __blocked__ (RFC1918), whereas @010.0.0.1@
(octal @8.0.0.1@) and @0127.0.0.1@ (octal @87.0.0.1@) coerce to __public__
addresses and are __not__ blocked — a decimal misreading would over-block those two
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
    noOptIn = lowerCaseHosts Set.empty
    renderCase host expected =
        toString $
            (if expected then "blocks " else "permits ")
                <> (if T.null host then "<empty>" else host)

    -- (host, expected-blocked). Grouped by intent; every internal range, both
    -- IPv4-mapped spellings, the lenient/strict boundary, and externals/names.
    corpus :: [(Text, Bool)]
    corpus =
        internalV4 <> internalV6 <> mappedV4 <> lenientBoundary <> externals <> names

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

-- ── host extraction ──────────────────────────────────────────────────────────

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
        -- The realistic call shape: extract, then test both guards.
        let h = hostAddress "http://169.254.169.254/latest/meta-data/"
         in (isBlockedTarget (lowerCaseHosts Set.empty) h, isAllowedUpstreamHost upstreams h)
                `shouldBe` (True, False)

-- ── canonical authority split ────────────────────────────────────────────────

-- The bracket-aware @host[:port]@ split shared by 'hostAddress' and the SQS
-- endpoint parser. These assert host/port extraction only — the split is purely
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

-- ── composed SSRF gate ───────────────────────────────────────────────────────

{- | The actual outbound-fetch guarantee is the __conjunction__: a target is
fetched only if it is on the host allowlist /and/ not an internal address. The two
halves are exercised above; this pins the composition the data plane relies on, so
neither half can be silently weakened.
-}
ssrfGateSpec :: Spec
ssrfGateSpec = describe "composed SSRF gate (allowlist AND not-blocked)" $ do
    let noOptIn = lowerCaseHosts Set.empty
        passesGate h = isAllowedUpstreamHost upstreams h && not (isBlockedTarget noOptIn h)

    it "admits a configured public upstream" $
        passesGate "registry.npmjs.org" `shouldBe` True
    it "vetoes an allowlisted host that is an internal literal (block beats allowlist)" $
        -- Even if an operator allowlists an internal address, the internal-range
        -- block still rejects it: the guarantee is the conjunction, not either half.
        let allowed = lowerCaseHosts (Set.insert "169.254.169.254" upstreamHosts)
         in ( isAllowedUpstreamHost allowed "169.254.169.254"
                && not (isBlockedTarget noOptIn "169.254.169.254")
            )
                `shouldBe` False
    it "refuses an IPv4-mapped IPv6 metadata literal (blocked by both halves)" $
        -- '::ffff:a9fe:a9fe' is 169.254.169.254 in IPv4-mapped form. The internal
        -- block now decodes the embedded IPv4 address and catches it directly, so
        -- the gate refuses it even if someone were to allowlist this literal form.
        passesGate "::ffff:a9fe:a9fe" `shouldBe` False
    it "refuses a metadata host extracted from a URL" $
        passesGate (hostAddress "http://169.254.169.254/latest/meta-data/") `shouldBe` False

-- ── tarball-host policy ───────────────────────────────────────────────────────

{- | The @dist.tarball@ host policy: under the secure default a tarball is fetched
only from the same host that served the packument; the opt-in relaxes that to any
allowlisted host. Neither ever escapes the allowlist; the internal-range block is
__origin-aware__ — the untrusted origin is gated by it (subject to the per-host
opt-in), the trusted private origin exempt from it (mirroring the connection layer's
unguarded manager, security.md invariant 3). The deny paths are exercised hardest,
since under-blocking on the untrusted origin is a vulnerability.
-}
tarballHostPolicySpec :: Spec
tarballHostPolicySpec = describe "tarballHostAllowed" $ do
    let noOptIn = lowerCaseHosts Set.empty
        -- Two allowlisted upstreams: the packument source and a separate CDN.
        allow = lowerCaseHosts (Set.fromList ["registry.npmjs.org", "cdn.npmjs.org"])
        -- The untrusted public origin: the internal-range block applies (the existing
        -- policy/allowlist/internal-range coverage is over this origin).
        same policy = tarballHostAllowed UntrustedOrigin policy allow noOptIn
        -- A short alias: packument host fixed to the npm registry.
        decide policy = same policy "registry.npmjs.org"

    describe "SameHostAsPackument (the secure default)" $ do
        it "admits a tarball on the same host that served the packument" $
            decide SameHostAsPackument "registry.npmjs.org" `shouldBe` True
        it "refuses a tarball on a different host, even one on the allowlist" $
            -- The crux of the default: an allowlisted-but-different CDN is refused.
            decide SameHostAsPackument "cdn.npmjs.org" `shouldBe` False
        it "refuses a tarball on a host not on the allowlist" $
            decide SameHostAsPackument "evil.example.com" `shouldBe` False
        it "matches the same-host clause case-insensitively (DNS is)" $
            decide SameHostAsPackument "Registry.NPMJS.org" `shouldBe` True
        it "refuses an empty tarball host" $
            decide SameHostAsPackument "" `shouldBe` False
        it "refuses a look-alike suffix of the packument host" $
            -- registry.npmjs.org.evil.com is neither allowlisted nor equal.
            decide SameHostAsPackument "registry.npmjs.org.evil.com" `shouldBe` False

    describe "AnyAllowlistedHost (the opt-in)" $ do
        it "admits a tarball on a different but allowlisted host" $
            decide AnyAllowlistedHost "cdn.npmjs.org" `shouldBe` True
        it "still admits a tarball on the same host" $
            decide AnyAllowlistedHost "registry.npmjs.org" `shouldBe` True
        it "still refuses a tarball on a host not on the allowlist" $
            -- The opt-in relaxes which allowlisted host, never the allowlist itself.
            decide AnyAllowlistedHost "evil.example.com" `shouldBe` False

    describe "the internal-range block beats either policy (untrusted origin)" $ do
        it "refuses an internal literal even when it equals the packument host" $
            -- An operator could (mis)configure an internal upstream host; the
            -- internal block still vetoes a tarball pointed at it under the
            -- default. The allowlist must carry the literal for this to even reach
            -- the block clause.
            let allowInternal = lowerCaseHosts (Set.singleton "169.254.169.254")
             in tarballHostAllowed UntrustedOrigin SameHostAsPackument allowInternal noOptIn "169.254.169.254" "169.254.169.254"
                    `shouldBe` False
        it "refuses an allowlisted internal literal under the opt-in too" $
            let allowInternal = lowerCaseHosts (Set.singleton "10.0.0.5")
             in tarballHostAllowed UntrustedOrigin AnyAllowlistedHost allowInternal noOptIn "registry.npmjs.org" "10.0.0.5"
                    `shouldBe` False
        it "honours an explicit internal opt-in for a deliberately-internal host" $
            -- With the host opted in to the internal block, an allowlisted internal
            -- tarball host is permitted under the relaxed policy.
            let allowInternal = lowerCaseHosts (Set.singleton "10.0.0.5")
                optIn = lowerCaseHosts (Set.singleton "10.0.0.5")
             in tarballHostAllowed UntrustedOrigin AnyAllowlistedHost allowInternal optIn "registry.npmjs.org" "10.0.0.5"
                    `shouldBe` True

    describe "the trusted private origin is exempt from the internal-range block" $ do
        -- The trusted origin mirrors the connection layer's unguarded manager
        -- (security.md invariant 3): a private registry may legitimately live on an
        -- internal address, so its same-host dist.tarball is admitted with no opt-in —
        -- where the untrusted origin would be refused. The allowlist and same-host
        -- clauses still gate it, so the exemption never widens past its own host.
        let allowInternal = lowerCaseHosts (Set.singleton "10.0.0.5")
        it "admits a same-host internal-literal tarball with no opt-in (where untrusted is refused)" $ do
            tarballHostAllowed TrustedOrigin SameHostAsPackument allowInternal noOptIn "10.0.0.5" "10.0.0.5"
                `shouldBe` True
            -- The same inputs on the untrusted origin are refused by the internal block.
            tarballHostAllowed UntrustedOrigin SameHostAsPackument allowInternal noOptIn "10.0.0.5" "10.0.0.5"
                `shouldBe` False
        it "still refuses a trusted tarball off the host allowlist (allowlist not relaxed)" $
            -- The exemption is the internal-range clause only; an off-allowlist host is
            -- still refused, so the trusted origin cannot be steered onto an arbitrary host.
            tarballHostAllowed TrustedOrigin AnyAllowlistedHost allowInternal noOptIn "10.0.0.5" "192.168.0.9"
                `shouldBe` False
        it "still refuses a cross-host trusted tarball under the secure default (same-host not relaxed)" $
            -- Two allowlisted internal hosts; under SameHostAsPackument the trusted
            -- origin's tarball must still equal its packument host, so a different
            -- (allowlisted, internal) host is refused.
            let bothAllowed = lowerCaseHosts (Set.fromList ["10.0.0.5", "10.0.0.6"])
                bothInternal = lowerCaseHosts (Set.fromList ["10.0.0.5", "10.0.0.6"])
             in tarballHostAllowed TrustedOrigin SameHostAsPackument bothAllowed bothInternal "10.0.0.5" "10.0.0.6"
                    `shouldBe` False

-- ── identifier → URL safety ──────────────────────────────────────────────────

upstreamUrlSpec :: Spec
upstreamUrlSpec = describe "upstreamUrlFor" $ do
    let base = "https://registry.npmjs.org"

    describe "builds a URL for a legitimate package" $ do
        it "joins the base URL and an unscoped name" $
            upstreamUrlFor base (unscoped "is-odd")
                `shouldBe` Right "https://registry.npmjs.org/is-odd"
        it "renders a scoped name in npm wire form (@scope%2Fname) under the base" $
            -- The scope separator is the structural '%2F' this builder writes — the
            -- same wire form the data plane uses — not a literal '/' that a segment
            -- splitter downstream could re-split.
            upstreamUrlFor base (scoped "babel" "code-frame")
                `shouldBe` Right "https://registry.npmjs.org/@babel%2Fcode-frame"
        it "accepts a name with interior dots (not over-rejected)" $
            upstreamUrlFor base (unscoped "lodash.merge")
                `shouldBe` Right "https://registry.npmjs.org/lodash.merge"
        it "tolerates a single trailing slash on the base without doubling it" $
            upstreamUrlFor "https://registry.npmjs.org/" (unscoped "is-odd")
                `shouldBe` Right "https://registry.npmjs.org/is-odd"
        it "emits exactly one %2F scope separator for a scoped name (no double-encoding)" $
            -- The structural '%2F' the scoped path carries is the separator this
            -- builder writes, never an encoding of a component's content: a
            -- legitimate '@scope/name' yields a single '%2F', the '@' is verbatim,
            -- and the hyphen in the base name is left literal.
            upstreamUrlFor base (scoped "babel" "code-frame")
                `shouldBe` Right "https://registry.npmjs.org/@babel%2Fcode-frame"

    describe "percent-encodes an accepted component so a once-decoded escape cannot reach the upstream raw" $ do
        it "re-encodes a literal '%' in an unscoped name (the %2e%2e%2f vector)" $
            -- 'foo%2e%2e%2fbar' passes the denylist (no literal '/'), so the
            -- defence in depth is to encode the '%' — the upstream must receive
            -- '%25', never a live '%2e%2e%2f' a decode-and-normalise CDN resolves
            -- to traversal.
            upstreamUrlFor base (unscoped "foo%2e%2e%2fbar")
                `shouldBe` Right "https://registry.npmjs.org/foo%252e%252e%252fbar"
        it "re-encodes a literal '%' hidden in the base name of a scoped package" $
            upstreamUrlFor base (scoped "babel" "code%2e%2eframe")
                `shouldBe` Right "https://registry.npmjs.org/@babel%2Fcode%252e%252eframe"
        it "encodes an accepted '?' (and the '=' after it) so it cannot inject an upstream query" $
            -- '?' becomes '%3F' and the reserved '=' '%3D', so the whole component
            -- is opaque path, never a query the upstream would parse.
            upstreamUrlFor base (unscoped "pkg?inject=1")
                `shouldBe` Right "https://registry.npmjs.org/pkg%3Finject%3D1"
        it "encodes an accepted '#' so it cannot inject an upstream fragment" $
            upstreamUrlFor base (unscoped "pkg#frag")
                `shouldBe` Right "https://registry.npmjs.org/pkg%23frag"

    describe "refuses to build a URL from a hostile identifier" $ do
        it "rejects a traversal segment in the base name" $
            upstreamUrlFor base (unscoped "..") `shouldBe` Left (UnsafeComponent "..")
        it "rejects a current-directory base name" $
            upstreamUrlFor base (unscoped ".") `shouldBe` Left (UnsafeComponent ".")
        it "rejects an embedded slash (would smuggle a path)" $
            upstreamUrlFor base (unscoped "foo/bar") `shouldBe` Left (UnsafeComponent "foo/bar")
        it "rejects an encoded-slash artdefact decoded to a real slash" $
            -- "@scope%2f..%2f" decodes to "scope/../"; recovered as a base name
            -- carrying '/' and '..', which must not be interpolated.
            upstreamUrlFor base (unscoped "scope/../") `shouldBe` Left (UnsafeComponent "scope/../")
        it "rejects an embedded backslash" $
            upstreamUrlFor base (unscoped "foo\\bar") `shouldBe` Left (UnsafeComponent "foo\\bar")
        it "rejects a CRLF-injecting name" $
            upstreamUrlFor base (unscoped "foo\r\nHost: evil")
                `shouldBe` Left (UnsafeComponent "foo\r\nHost: evil")
        it "rejects a control character in the name" $
            upstreamUrlFor base (unscoped "foo\0bar") `shouldBe` Left (UnsafeComponent "foo\0bar")
        it "rejects a traversal in the scope of a scoped name" $
            upstreamUrlFor base (scoped ".." "pkg") `shouldBe` Left (UnsafeComponent "..")
        it "rejects a slash hidden in the base name of a scoped package" $
            upstreamUrlFor base (scoped "babel" "code/frame")
                `shouldBe` Left (UnsafeComponent "code/frame")

    describe "handles an '@'-leading name with no scope separator" $ do
        it "accepts a bare '@'-prefixed name as a single component, percent-encoding the stray '@'" $
            -- A rendered name starting with '@' but carrying no '/' is treated as
            -- one component (the single-component fallback). With no structural
            -- scope frame, the '@' is component content, so it is percent-encoded
            -- ('%40') rather than emitted as a sigil.
            upstreamUrlFor base (unscoped "@foo")
                `shouldBe` Right "https://registry.npmjs.org/%40foo"
        it "still rejects a traversal hidden after an '@' scope prefix" $
            -- "@..\/b" splits to ["..", "b"]; the ".." component is caught.
            upstreamUrlFor base (unscoped "@../b") `shouldBe` Left (UnsafeComponent "..")

    describe "refuses an empty base URL" $
        it "rejects an empty configured base URL" $
            upstreamUrlFor "" (unscoped "is-odd") `shouldBe` Left EmptyBaseUrl

-- ── bounded read ─────────────────────────────────────────────────────────────

boundedReadSpec :: Spec
boundedReadSpec = describe "boundedRead" $ do
    let limits = defaultLimits{maxBodyBytes = 10}

    it "returns the whole body when within the byte budget" $
        runBounded limits ["hello", "12345"] `shouldBe` Right "hello12345"

    it "returns an empty body for an immediately-EOF reader" $
        runBounded limits [] `shouldBe` Right ""

    it "aborts fail-closed past the byte budget (never a partial body)" $
        -- 11 bytes against a 10-byte cap: a 'Left', not the first 10 bytes.
        runBounded limits ["hello", "world!"] `shouldBe` Left (BodyTooLarge 10)

    it "reports the configured ceiling in the error" $
        runBounded (defaultLimits{maxBodyBytes = 4}) ["abcde"]
            `shouldBe` Left (BodyTooLarge 4)

    it "accepts a body exactly at the budget" $
        runBounded limits ["1234567890"] `shouldBe` Right "1234567890"

    it "rejects any non-empty body under a zero budget" $
        runBounded (defaultLimits{maxBodyBytes = 0}) ["x"] `shouldBe` Left (BodyTooLarge 0)

    it "accepts an empty body even under a zero budget" $
        runBounded (defaultLimits{maxBodyBytes = 0}) [] `shouldBe` Right ""

    it "treats an empty chunk as EOF (the BodyReader contract), stopping early" $
        -- An empty 'ByteString' is the reader's end signal, so the chunk after it
        -- is never read — the body is just what preceded the empty chunk. This
        -- pins the @http-client@ @BodyReader@ semantics 'boundedRead' relies on.
        runBounded limits ["ab", "", "cd"] `shouldBe` Right "ab"

    it "passes a small body under the generous default budget" $
        -- Exercises 'defaultLimits' (the 16 MiB cap) directly.
        runBounded defaultLimits ["small", "body"] `shouldBe` Right "smallbody"

    it "stops reading once the budget is breached (does not drain the reader)" $ do
        -- An IORef-backed reader (the real S08 monad is IO) lets us observe that
        -- 'boundedRead' stops pulling chunks after it decides to abort.
        ref <- newIORef (["aaaa", "bbbb", "cccc", "dddd"] :: [ByteString])
        let next = atomicModifyIORef' ref $ \case
                [] -> ([], BS.empty)
                (c : cs) -> (cs, c)
        result <- boundedRead (defaultLimits{maxBodyBytes = 6}) next
        result `shouldBe` Left (BodyTooLarge 6)
        -- "aaaa"(4) fits; "bbbb" breaches at 8 > 6 and aborts, so "cccc"/"dddd"
        -- are never pulled — two chunks remain unread.
        remaining <- readIORef ref
        remaining `shouldBe` ["cccc", "dddd"]

-- ── version-count bound ──────────────────────────────────────────────────────

versionCountSpec :: Spec
versionCountSpec = describe "checkVersionCount" $ do
    let limits = defaultLimits{maxVersionCount = 3}

    it "passes a packument within the version budget (returns it unchanged)" $
        checkVersionCount limits (packumentWith 3) `shouldBe` Right (packumentWith 3)

    it "rejects a packument with too many versions, fail-closed" $
        checkVersionCount limits (packumentWith 4)
            `shouldBe` Left (TooManyVersions 4 3)

    it "passes an empty packument" $
        checkVersionCount limits (packumentWith 0) `shouldBe` Right (packumentWith 0)

    it "rejects a pathological huge-version-count document" $
        case checkVersionCount limits (packumentWith 5000) of
            Left (TooManyVersions seen cap) -> (seen, cap) `shouldBe` (5000, 3)
            other -> expectationFailure ("expected TooManyVersions, got " <> show other)

    it "passes a realistic packument under the default version budget" $
        -- Exercises 'defaultLimits' directly (not via a record override).
        checkVersionCount defaultLimits (packumentWith 25) `shouldBe` Right (packumentWith 25)

-- ── nesting-depth bound ──────────────────────────────────────────────────────

nestingDepthSpec :: Spec
nestingDepthSpec = describe "checkNestingDepth" $ do
    let limits = defaultLimits{maxNestingDepth = 3}

    it "passes a scalar (depth 1)" $
        checkNestingDepth limits (Number 1) `shouldBe` Right (Number 1)

    it "passes a document exactly at the depth budget" $
        -- {"a": {"b": 1}} is depth 3: object → object → scalar.
        let v = nestObject 3 in checkNestingDepth limits v `shouldBe` Right v

    it "rejects a document one level too deep, fail-closed" $
        checkNestingDepth limits (nestObject 4) `shouldBe` Left (TooDeeplyNested 3)

    it "rejects a deeply-nested array payload" $
        checkNestingDepth limits (nestArray 50) `shouldBe` Left (TooDeeplyNested 3)

    it "counts an empty container as a leaf (depth 1)" $
        checkNestingDepth (defaultLimits{maxNestingDepth = 1}) (Array V.empty)
            `shouldBe` Right (Array V.empty)

    it "passes a realistic shallow document under the default budget" $
        let doc = Object (KeyMap.fromList [("name", String "thing"), ("nested", nestObject 2)])
         in checkNestingDepth defaultLimits doc `shouldBe` Right doc

    it "accepts all JSON scalar kinds as leaves" $
        -- Object/Array carry every scalar constructor (String/Number/Bool/Null),
        -- so each leaf arm of the depth walk is exercised.
        let doc =
                Object
                    ( KeyMap.fromList
                        [ ("s", String "x")
                        , ("n", Number 1)
                        , ("b", Bool True)
                        , ("z", Null)
                        , ("xs", Array (V.fromList [Bool False, Null, Number 2]))
                        ]
                    )
         in checkNestingDepth defaultLimits doc `shouldBe` Right doc

-- ── real-world admissibility (the defaults must not false-positive) ──────────

{- | The whole point of the default 'Limits' (16 MiB body, 100k versions, depth 64)
is that they must __never__ refuse a legitimate trusted package. This drives the
exact sequence the data plane applies in @Ecluse.Core.Server.Pipeline.fetchEntry@ —
bounded read, depth check on the decoded document, projection, then version-count
check — over a __real, untrimmed__ packument committed under the fixtures directory,
and asserts the document is __admissible__ under the default budget at every step.

The fixture is @registry.npmjs.org/express@'s full packument (a large, widely-trusted
package: ~805 KB, 288 versions, JSON depth 7). It is a genuine capture, not
hand-trimmed — so a default that was accidentally too tight would fail here, exactly
the regression this guards. @react@ (the architect's example) is ~6.7 MB / 2841
versions, too large to commit comfortably; @express@ is the representative
large-but-committable choice. The live smoke tier
("Ecluse.RegistryProtocolSpec") validates @react@ and other large packuments against
__current__ data without committing megabytes.
-}
realPackumentSpec :: Spec
realPackumentSpec = describe "default Limits admit a real large trusted packument (no false positive)" $ do
    it "express: bounded read, decode, depth, projection, and version count all clear the defaults" $ do
        body <- readFileBS "core/test/unit/fixtures/npm/express.full.json"
        -- 1. Body size: the bounded read returns the whole body (within maxBodyBytes).
        bounded <- case runBounded defaultLimits [body] of
            Left err -> expectationFailure ("real packument refused by the body bound: " <> show err) >> pure ""
            Right b -> pure b
        bounded `shouldBe` body
        -- 2. Decode to a Value, then 3. depth-check it (within maxNestingDepth).
        value <- case eitherDecodeStrict bounded of
            Left e -> expectationFailure ("real packument did not decode: " <> e) >> pure (Object mempty)
            Right v -> pure v
        depthChecked <- case checkNestingDepth defaultLimits value of
            Left err -> expectationFailure ("real packument refused by the nesting bound: " <> show err) >> pure (Object mempty)
            Right v -> pure v
        -- 4. Project to the typed view (it really is a well-formed packument), then
        -- 5. version-count check it (within maxVersionCount).
        info <- case parsePackageInfoFromValue (unscoped "express") depthChecked of
            Left err -> expectationFailure ("real packument did not project: " <> show err) >> pure emptyInfo
            Right (Projected i) -> pure i
            Right (NameMismatch reported) ->
                expectationFailure ("real packument self-reported an unexpected name: " <> toString reported) >> pure emptyInfo
        case checkVersionCount defaultLimits info of
            Left err -> expectationFailure ("real packument refused by the version bound: " <> show err)
            Right admitted -> do
                renderPackageName (infoName admitted) `shouldBe` "express"
                -- A genuinely large version set, well under the 100k ceiling: proof the
                -- count bound clears a real package, not a toy one.
                Map.size (infoVersions admitted) `shouldSatisfy` (> 200)
                Map.size (infoVersions admitted) `shouldSatisfy` (<= maxVersionCount defaultLimits)

-- An empty 'PackageInfo' placeholder so a failed projection keeps the example total
-- (it has already failed via 'expectationFailure' before this is forced).
emptyInfo :: PackageInfo
emptyInfo =
    PackageInfo
        { infoName = unscoped "unused"
        , infoVersions = Map.empty
        , infoDistTags = Map.empty
        , infoPublishedAt = Map.empty
        }

-- ── lowerCaseHosts (the LoweredHostSet parser) ───────────────────────────────

lowerCaseHostsSpec :: Spec
lowerCaseHostsSpec = describe "lowerCaseHosts" $ do
    it "folds configured-host case so a mixed-case entry matches a lowercase query" $
        -- 'lowerCaseHosts' is the only constructor of the 'LoweredHostSet' the
        -- guard takes, so this proves the guard relies on it for normalisation: a
        -- host configured in mixed case is matched by its lowercase form.
        isAllowedUpstreamHost (lowerCaseHosts (Set.singleton "Registry.NPMjs.ORG")) "registry.npmjs.org"
            `shouldBe` True
    it "normalises distinct casings of one host to the same lowered set" $
        -- Two spellings that differ only in case fold to equal 'LoweredHostSet'
        -- values, so the normalisation is genuinely case-collapsing.
        lowerCaseHosts (Set.fromList ["EXAMPLE.com", "example.COM"])
            `shouldBe` lowerCaseHosts (Set.singleton "example.com")

-- ── properties ───────────────────────────────────────────────────────────────

propertiesSpec :: Spec
propertiesSpec = describe "properties" $ do
    it "boundedRead reconstructs the body iff it fits the budget" $
        hedgehog $ do
            -- Chunks are non-empty: a faithful @BodyReader@ emits an empty
            -- 'ByteString' only as its EOF signal, never as an interior chunk, so
            -- 'runBounded' (which stops at the first empty chunk) sees the whole
            -- list and 'BS.concat' is a faithful oracle for the body.
            chunks <- forAll (Gen.list (Range.linear 0 8) (Gen.bytes (Range.linear 1 6)))
            cap <- forAll (Gen.int (Range.linear 0 40))
            let total = BS.concat chunks
                result = runBounded (defaultLimits{maxBodyBytes = cap}) chunks
            annotateShow (BS.length total, cap)
            -- Non-vacuity: the generator must reach both the within- and
            -- over-budget arms often.
            H.cover 5 "within budget" (BS.length total <= cap)
            H.cover 5 "over budget" (BS.length total > cap)
            if BS.length total <= cap
                then result === Right total -- exact bytes, never truncated
                else result === Left (BodyTooLarge cap)

    it "isBlockedTarget blocks an internal host unless it is opted in" $
        hedgehog $ do
            -- Generate addresses across the blocked ranges plus public ones, and a
            -- random opt-in set, then check the invariant directly.
            host <- forAll genMaybeInternalHost
            optIn <- forAll (Gen.set (Range.linear 0 3) genMaybeInternalHost)
            H.cover 5 "internal host" (looksInternal host)
            H.cover 5 "public host" (not (looksInternal host))
            -- The generated hosts are already lowercase, so 'lowerCaseHosts' leaves
            -- them unchanged and the raw set is a faithful membership oracle.
            let blocked = isBlockedTarget (lowerCaseHosts optIn) host
            if host `Set.member` optIn
                then blocked === False -- opt-in always wins
                else blocked === looksInternal host

    it "an accepted upstream URL never contains a traversal/separator/injection artefact" $
        hedgehog $ do
            name <- forAll genName
            case upstreamUrlFor "https://registry.npmjs.org" name of
                Left _ -> H.success -- refused names are fine
                Right url -> do
                    -- An accepted URL is base ++ "/" ++ (one optional structural
                    -- "@…%2F…" scope frame over percent-encoded components), so
                    -- beyond the scheme its path must carry no "/../" or "/./"
                    -- smuggle, no backslash, no control character, and no live
                    -- escape, query, fragment, or space a component could inject.
                    let path = T.drop (T.length "https://registry.npmjs.org") url
                    annotateShow url
                    H.assert (not ("/../" `T.isInfixOf` path))
                    H.assert (not ("/./" `T.isInfixOf` path))
                    H.assert (not ("\\" `T.isInfixOf` path))
                    H.assert (T.all (\c -> c /= '\n' && c /= '\r' && c /= '\0') path)
                    -- No injectable delimiter survives unescaped: a query, fragment,
                    -- semicolon, or space a component carried is percent-encoded, so
                    -- none appears literally in the path.
                    H.assert (T.all (\c -> c `notElem` ['?', '#', ';', ' ']) path)
                    -- Every '%' is a well-formed '%XX' escape this builder wrote
                    -- (the '%2F' separator, or an encodeComponent escape) — a raw
                    -- '%' a component carried is itself re-encoded to '%25', so no
                    -- live escape (the once-decoded '%2e%2e%2f' vector) leaks.
                    H.assert (allEscapesWellFormed path)

{- | Whether every @\'%\'@ in the text begins a well-formed @%XX@ escape (two hex
digits). A raw @\'%\'@ that a component carried is re-encoded to @%25@, so a path
this builder produced has only well-formed escapes — the assertion that no live,
once-decoded escape (the @%2e%2e%2f@ vector) survives into the upstream URL.
-}
allEscapesWellFormed :: Text -> Bool
allEscapesWellFormed = go . toString
  where
    go ('%' : a : b : rest) = isHexDigit a && isHexDigit b && go rest
    go ('%' : _) = False -- a '%' not followed by two hex digits is a raw, unescaped '%'
    go (_ : rest) = go rest
    go [] = True
    isHexDigit c = c `elem` (['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F'])

-- | A scalar wrapped in @n-1@ nested single-key objects, giving total depth @n@.
nestObject :: Int -> Value
nestObject n
    | n <= 1 = Number 1
    | otherwise = Object (KeyMap.singleton "a" (nestObject (n - 1)))

-- | A scalar wrapped in @n-1@ nested single-element arrays, giving total depth @n@.
nestArray :: Int -> Value
nestArray n
    | n <= 1 = Number 1
    | otherwise = Array (V.singleton (nestArray (n - 1)))

{- | Whether a generated host string is one this module's ranges treat as
internal — restated independently of the implementation so the property is not a
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

{- | A package-name generator mixing benign names with hostile components
(traversal, slashes, control chars), exercising both arms of 'upstreamUrlFor'.
-}
genName :: H.Gen PackageName
genName = Gen.choice [unscoped <$> raw, scoped <$> rawScope <*> raw]
  where
    raw = Gen.frequency [(6, benign), (4, Gen.element hostile)]
    rawScope = Gen.frequency [(6, benign), (2, Gen.element hostile)]
    benign = Gen.text (Range.linear 1 8) (Gen.frequency [(8, Gen.alphaNum), (2, Gen.element ['.', '-', '_'])])
    hostile = ["..", ".", "a/b", "a\\b", "a\tb", "a\0b", "x/../y", "foo\r\nbar", ""]
