-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The egress posture for registry traffic: https-only by construction, with TLS
certificate validation as the endpoint-authentication boundary.

Every outbound registry URL the proxy dials is an 'RegistryUrl', built through the
https-only 'mkRegistryUrl'. A non-https registry endpoint cannot be represented. A
plain-HTTP target is therefore refused at the configuration boundary: a non-https
configured upstream fails closed at boot. A packument's @dist.tarball@ goes through
'resolveTarballUrl' before the proxy ever dials it. The data-plane
'Network.HTTP.Client.Manager' is the standard validating @tls@ manager. It checks the
certificate the dialled host presents against the system trust store, for the requested
name. An attacker who can steer a name to an internal or rebound address cannot make
that address present a CA-trusted certificate for the host. Certificate validation
therefore closes the credential-exfiltration and resolve-to-internal SSRF class, rather
than a resolved-IP pin.

Two complementary controls live alongside this and are not part of this module. The
outbound host allowlist ('Ecluse.Core.Security.isAllowedUpstreamHost') is the
load-bearing egress-policy control: the proxy dials only configured upstream
@host:port@ pairs. The pure literal internal-range block
('Ecluse.Core.Security.isBlockedTarget') is cheap defence-in-depth on the
@dist.tarball@ host gate. No data-plane request follows an upstream redirect, because
the shared 'Ecluse.Core.Registry.Request.finaliseRequest' pins @redirectCount = 0@ on
every request. No hop can therefore downgrade the scheme or escape the allowlist after
the URL is built.

A test- and dev-only loopback constructor lives in "Ecluse.Core.Security.Egress.DevHttp",
compiled only under the @dev-http-egress@ Cabal flag. The loopback test suites can then
dial an in-process @http:\/\/127.0.0.1@ server without weakening the production posture.
A release build does not compile it.
-}
module Ecluse.Core.Security.Egress (
    -- * The https-only egress URL
    RegistryUrl,
    mkRegistryUrl,
    registryUrlText,

    -- * Packument @dist.tarball@ normalisation
    resolveTarballUrl,
) where

import Data.Text qualified as T

import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress.Internal (RegistryUrl, mkRegistryUrl, registryUrlText)

{- | Resolve a packument's @dist.tarball@ URL against the https-only egress policy,
given the bare host the packument itself was served from.

An upstream's @dist.tarball@ is server-chosen data, so this normalises its scheme
before the proxy will dial it.

* An @https:\/\/@ target is kept, validated through 'mkRegistryUrl'.
* An @http:\/\/@ target on the same host as the packument is upgraded to @https:\/\/@.
  That is the legacy case of an older registry that still advertises plaintext
  artifact URLs on its own host.
* An @http:\/\/@ target on any other host is refused with a 'Left' carrying a reason.
  A foreign plaintext artifact location is dropped rather than dialled.
* Anything that is not an @http(s)@ URL is refused.

The 'Left' reason feeds the per-entry drop record. The 'Right' is the https
'RegistryUrl' the artifact is fetched from. Hosts compare on the bare host
('Ecluse.Core.Security.hostAddress'): the upgrade decision is a scheme normalisation
and not an authorisation, so the port plays no part here. This composes with, and never
replaces, the @host:port@ allowlist and the same-authority tarball policy that still
gate the resolved target at serve time. An upgraded-but-nonstandard-port target is
refused there.
-}
resolveTarballUrl :: Text -> Text -> Either Text RegistryUrl
resolveTarballUrl upstreamHost url
    | "https://" `T.isPrefixOf` lowered = mkRegistryUrl url
    | "http://" `T.isPrefixOf` lowered =
        if hostAddress url == upstreamHost
            then mkRegistryUrl ("https://" <> T.drop httpSchemeChars url)
            else Left ("dist.tarball is http on a host other than the upstream registry: " <> url)
    | otherwise = Left ("dist.tarball is not an https URL: " <> url)
  where
    lowered = T.toLower url
    -- The character length of the "http://" scheme prefix being re-schemed to https.
    -- It is a fixed literal count, written directly rather than through an O(n)
    -- 'T.length' on the constant. Dropping it from the original 'url' (not 'lowered')
    -- rewrites only the scheme and preserves the rest of the URL verbatim.
    httpSchemeChars = 7 :: Int
