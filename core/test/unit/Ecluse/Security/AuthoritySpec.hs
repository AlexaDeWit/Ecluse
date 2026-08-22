-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Security.AuthoritySpec (spec) where

import Data.Set qualified as Set

import Test.Hspec

import Ecluse.Core.Security (
    AllowedHostPorts,
    HostPort (HostPort),
    allowedHostPorts,
    authorityLabel,
    hostAddress,
    hostPortAddress,
    isAllowedUpstreamHost,
    isBlockedTarget,
    splitHostPort,
 )

{- | The configured upstreams, normalised through 'allowedHostPorts'. Only the
composed-guard case uses it, to show that a metadata authority is off the allowlist.
-}
upstreams :: AllowedHostPorts
upstreams = allowedHostPorts (Set.fromList [hp "registry.npmjs.org", hp "Private.Internal.Example.com"])

-- | An authority on the https default port: what a URL with no written port dials.
hp :: Text -> HostPort
hp host = HostPort host 443

-- | An authority on an explicit port.
hpAt :: Text -> Word16 -> HostPort
hpAt = HostPort

spec :: Spec
spec = do
    hostAddressSpec
    hostPortAddressSpec
    authorityLabelSpec
    splitHostPortSpec

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
        -- A crafted dist.tarball can carry a second "://" in its query. The gate must
        -- read the host actually dialled (the first authority), not the one after the
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
        -- No "://" and a userinfo "@": this drives both 'breakOnEnd' branches, the
        -- needle present and then absent.
        hostAddress "user:pass@registry.npmjs.org/x" `shouldBe` "registry.npmjs.org"
    it "drops a path on a schemeless bare host" $
        hostAddress "registry.npmjs.org/thing" `shouldBe` "registry.npmjs.org"
    it "composes with the SSRF guards to catch a metadata URL" $
        -- The realistic call shape: each guard tests its own projection of the URL.
        -- The internal block gets the bare host, the allowlist the host:port
        -- authority.
        let url = "http://169.254.169.254/latest/meta-data/"
         in (isBlockedTarget [] (hostAddress url), isAllowedUpstreamHost upstreams <$> hostPortAddress url)
                `shouldBe` (True, Just False)

{- | The authorisation-side extraction: the same authority stripping as
'hostAddress', with the effective port parsed rather than discarded. These cases pin
the default 443, the strict port grammar, and the IPv6 bracket discipline, because
each is load-bearing for the gate. A lenient port fallback or a mangled colon split
would alias an attacker-chosen authority onto an authorised one.
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
            -- "2606:4700::1111" has no unambiguous host/port split. Truncating it to
            -- a host "2606" would gate on an authority nothing dials.
            hostPortAddress "2606:4700::1111" `shouldBe` Nothing
        it "refuses an opening bracket with no close (malformed authority)" $
            hostPortAddress "https://[::1" `shouldBe` Nothing
        it "refuses junk between the closing bracket and the port" $
            hostPortAddress "https://[::1]x/" `shouldBe` Nothing
        it "refuses a bracketed literal with a written-but-empty port (fail closed)" $
            -- The bracketed analogue of "host:". http-client refuses a URL that
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
            -- a canonical port under a second spelling. The gate accepts one spelling.
            hostPortAddress "https://registry.npmjs.org:0443/" `shouldBe` Nothing
            hostPortAddress "https://registry.npmjs.org:080/" `shouldBe` Nothing
        it "accepts the canonical spelling of those ports" $ do
            hostPortAddress "https://registry.npmjs.org:443/" `shouldBe` Just (hp "registry.npmjs.org")
            hostPortAddress "https://registry.npmjs.org:80/" `shouldBe` Just (hpAt "registry.npmjs.org" 80)

    it "yields Nothing for a value with no host" $ do
        hostPortAddress "" `shouldBe` Nothing
        hostPortAddress ":8443" `shouldBe` Nothing

{- The log-safe reduction that every log line and span attribute applies to a URL. What
it must never emit is the point. Userinfo and the query string can carry a credential. A
malformed authority yields no host at all, never a fragment of the input. -}
authorityLabelSpec :: Spec
authorityLabelSpec = describe "authorityLabel" $ do
    it "drops userinfo, path, query, and fragment, keeping host and port" $
        authorityLabel "https://deploy:hunter2@registry.npmjs.org/left-pad/-/left-pad-1.3.0.tgz?token=abc#frag"
            `shouldBe` "registry.npmjs.org:443"
    it "drops a pre-signed query string whole" $
        -- An S3-style pre-signed URL carries its credential in the query.
        authorityLabel "https://bucket.s3.amazonaws.com/all.zip?X-Amz-Signature=deadbeef&X-Amz-Credential=AKIA"
            `shouldBe` "bucket.s3.amazonaws.com:443"
    it "folds the https default port: a written :443 and no port render alike" $
        authorityLabel "https://registry.npmjs.org:443/x" `shouldBe` authorityLabel "https://registry.npmjs.org/x"
    it "keeps an explicit non-default port" $
        authorityLabel "https://registry.internal:9443/x" `shouldBe` "registry.internal:9443"
    it "re-brackets an IPv6 literal so the label reads back as one authority" $
        authorityLabel "https://[2606:4700::1111]:8443/thing" `shouldBe` "[2606:4700::1111]:8443"
    it "lower-cases the host, as the gate does" $
        authorityLabel "https://Registry.NPMJS.org/x" `shouldBe` "registry.npmjs.org:443"
    it "labels the authority actually dialled, not one hidden in the query" $
        authorityLabel "https://169.254.169.254/x?u=https://registry.npmjs.org"
            `shouldBe` "169.254.169.254:443"
    it "yields no host for a value carrying no dialable authority" $ do
        authorityLabel "" `shouldBe` "<unresolved>"
        authorityLabel "https://[::1/thing" `shouldBe` "<unresolved>"
        authorityLabel "https://deploy@/thing" `shouldBe` "<unresolved>"
        authorityLabel "https://registry.npmjs.org:0/x" `shouldBe` "<unresolved>"
        authorityLabel "https://registry.npmjs.org:https/x" `shouldBe` "<unresolved>"

-- The bracket-aware @host[:port]@ split shared by 'hostAddress' and the SQS
-- endpoint parser. These assert host/port extraction only. The split is purely
-- structural and carries no classification or gating: the OTLP and SQS endpoints
-- are trusted, operator-declared destinations (see #326).
splitHostPortSpec :: Spec
splitHostPortSpec = describe "splitHostPort" $ do
    it "splits a bracketed IPv6 literal with a port on the closing bracket" $
        -- The #292/#296 edge: the split never reads an inner '::' as the port separator.
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
