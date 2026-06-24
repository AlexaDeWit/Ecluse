module Ecluse.SecuritySpec (spec) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String))
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

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (
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
 )
import Ecluse.Security (
    LimitError (..),
    Limits (..),
    LoweredHostSet,
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
    tarballHostAllowed,
    upstreamUrlFor,
 )
import Ecluse.Version (Version, mkVersion)

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
    ssrfGateSpec
    tarballHostPolicySpec
    upstreamUrlSpec
    boundedReadSpec
    versionCountSpec
    nestingDepthSpec
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

    describe "explicit per-host opt-in" $ do
        it "permits a deliberately-internal upstream that is opted in" $
            isBlockedTarget (lowerCaseHosts (Set.singleton "10.0.0.5")) "10.0.0.5" `shouldBe` False
        it "still blocks an internal address that is not the opted-in one" $
            isBlockedTarget (lowerCaseHosts (Set.singleton "10.0.0.5")) "10.0.0.6" `shouldBe` True
        it "honours an opt-in written in a different case (matched case-insensitively)" $
            -- 'lowerCaseHosts' normalises the opt-in set, so 'FE80::1' opts in
            -- 'fe80::1' rather than over-blocking it on a case mismatch.
            isBlockedTarget (lowerCaseHosts (Set.singleton "FE80::1")) "fe80::1" `shouldBe` False

-- ── classification corpus (the equivalence bar) ──────────────────────────────

{- | The blocked-vs-allowed classification of 'isBlockedTarget', pinned against an
__explicit expected table__ rather than any prior implementation. The internal
block recognises a host as a literal with a deliberately lenient hand-rolled
parser and delegates only the range membership to a library, so this corpus
guards that the gate neither narrows nor widens: every internal range blocks, the
IPv4-mapped smuggling forms decode and block, and the lenient/strict boundary
spellings classify exactly as documented.

The boundary cases are the load-bearing ones. @0127.0.0.1@ and @010.0.0.1@ are
__blocked__: the recogniser accepts leading-zero octets and treats them as the
address they coerce to on a typical resolver, so they cannot skip the block as
unparsed names — a stricter parser that rejected them would narrow the gate.
@fe80::1ffff@ is __not__ blocked: its final group overflows 16 bits, so it is not
a literal here and stays a name the allowlist constrains.
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
        [ ("0127.0.0.1", True) -- leading-zero octet still blocks
        , ("010.0.0.1", True) -- leading-zero octet still blocks
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
allowlisted host. Neither ever escapes the allowlist or the internal-range block —
the deny paths are exercised hardest, since under-blocking here is a vulnerability.
-}
tarballHostPolicySpec :: Spec
tarballHostPolicySpec = describe "tarballHostAllowed" $ do
    let noOptIn = lowerCaseHosts Set.empty
        -- Two allowlisted upstreams: the packument source and a separate CDN.
        allow = lowerCaseHosts (Set.fromList ["registry.npmjs.org", "cdn.npmjs.org"])
        same policy = tarballHostAllowed policy allow noOptIn
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

    describe "the internal-range block beats either policy" $ do
        it "refuses an internal literal even when it equals the packument host" $
            -- An operator could (mis)configure an internal upstream host; the
            -- internal block still vetoes a tarball pointed at it under the
            -- default. The allowlist must carry the literal for this to even reach
            -- the block clause.
            let allowInternal = lowerCaseHosts (Set.singleton "169.254.169.254")
             in tarballHostAllowed SameHostAsPackument allowInternal noOptIn "169.254.169.254" "169.254.169.254"
                    `shouldBe` False
        it "refuses an allowlisted internal literal under the opt-in too" $
            let allowInternal = lowerCaseHosts (Set.singleton "10.0.0.5")
             in tarballHostAllowed AnyAllowlistedHost allowInternal noOptIn "registry.npmjs.org" "10.0.0.5"
                    `shouldBe` False
        it "honours an explicit internal opt-in for a deliberately-internal host" $
            -- With the host opted in to the internal block, an allowlisted internal
            -- tarball host is permitted under the relaxed policy.
            let allowInternal = lowerCaseHosts (Set.singleton "10.0.0.5")
                optIn = lowerCaseHosts (Set.singleton "10.0.0.5")
             in tarballHostAllowed AnyAllowlistedHost allowInternal optIn "registry.npmjs.org" "10.0.0.5"
                    `shouldBe` True

-- ── identifier → URL safety ──────────────────────────────────────────────────

upstreamUrlSpec :: Spec
upstreamUrlSpec = describe "upstreamUrlFor" $ do
    let base = "https://registry.npmjs.org"

    describe "builds a URL for a legitimate package" $ do
        it "joins the base URL and an unscoped name" $
            upstreamUrlFor base (unscoped "is-odd")
                `shouldBe` Right "https://registry.npmjs.org/is-odd"
        it "renders a scoped name as @scope/name under the base" $
            upstreamUrlFor base (scoped "babel" "code-frame")
                `shouldBe` Right "https://registry.npmjs.org/@babel/code-frame"
        it "accepts a name with interior dots (not over-rejected)" $
            upstreamUrlFor base (unscoped "lodash.merge")
                `shouldBe` Right "https://registry.npmjs.org/lodash.merge"
        it "tolerates a single trailing slash on the base without doubling it" $
            upstreamUrlFor "https://registry.npmjs.org/" (unscoped "is-odd")
                `shouldBe` Right "https://registry.npmjs.org/is-odd"

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
        it "accepts a bare '@'-prefixed name as a single safe component" $
            -- A rendered name starting with '@' but carrying no '/' is treated as
            -- one component (the 'nameComponents' fallback), and a stray '@' is
            -- harmless to interpolate.
            upstreamUrlFor base (unscoped "@foo")
                `shouldBe` Right "https://registry.npmjs.org/@foo"
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

    it "an accepted upstream URL never contains a traversal/separator artefact" $
        hedgehog $ do
            name <- forAll genName
            case upstreamUrlFor "https://registry.npmjs.org" name of
                Left _ -> H.success -- refused names are fine
                Right url -> do
                    -- An accepted URL's path is exactly base ++ "/" ++ rendered,
                    -- so beyond the scheme it must carry no "/../" or "/./" smuggle
                    -- and no control character.
                    let path = T.drop (T.length "https://registry.npmjs.org") url
                    annotateShow url
                    H.assert (not ("/../" `T.isInfixOf` path))
                    H.assert (not ("/./" `T.isInfixOf` path))
                    H.assert (not ("\\" `T.isInfixOf` path))
                    H.assert (T.all (\c -> c /= '\n' && c /= '\r' && c /= '\0') path)

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
