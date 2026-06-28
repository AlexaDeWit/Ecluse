{- | The egress posture for registry traffic: __https-only by construction__, with
TLS certificate validation as the endpoint-authentication boundary.

Every outbound registry URL the proxy dials is an 'RegistryUrl', built through the
https-only 'mkRegistryUrl'. A non-https registry endpoint cannot be represented, so a
plain-HTTP target is refused at the configuration boundary (a non-https configured
upstream fails closed at boot) and a packument's @dist.tarball@ is normalised through
'resolveTarballUrl' before it is ever dialled. The data-plane 'Network.HTTP.Client.Manager'
is the standard validating @tls@ manager, so the certificate presented by the dialled
host is checked against the system trust store for the requested name. An attacker who
can steer a name to an internal or rebound address cannot make that address present a
CA-trusted certificate for the host, so the credential-exfiltration and
resolve-to-internal SSRF class is closed by certificate validation rather than by a
resolved-IP pin.

Two complementary controls live alongside this and are __not__ part of this module: the
outbound host allowlist ('Ecluse.Core.Security.isAllowedUpstreamHost'), which is the
load-bearing egress-policy control (the proxy dials only configured upstream hosts), and
the pure literal internal-range block ('Ecluse.Core.Security.isBlockedTarget'), kept as
cheap defence-in-depth on the @dist.tarball@ host gate. No data-plane request follows an
upstream redirect ('Ecluse.Core.Registry.Npm.withToken' pins @redirectCount = 0@), so there
is no hop that could downgrade the scheme or escape the allowlist after the URL is built.

A test- and dev-only loopback constructor lives in "Ecluse.Core.Security.Egress.DevHttp",
compiled only under the @dev-http-egress@ Cabal flag, so the loopback test suites can dial
an in-process @http:\/\/127.0.0.1@ server without weakening the production posture; a
release build does not compile it.
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

An upstream's @dist.tarball@ is server-chosen data, so its scheme is normalised before
the proxy will dial it:

* an @https:\/\/@ target is kept (validated through 'mkRegistryUrl');
* an @http:\/\/@ target __on the same host__ as the packument is __upgraded__ to
  @https:\/\/@, the legacy case of an older registry that still advertises plaintext
  artifact URLs on its own host;
* an @http:\/\/@ target on __any other__ host is refused (a 'Left' carrying a reason),
  so a foreign plaintext artifact location is dropped rather than dialled;
* anything that is not an @http(s)@ URL is refused.

The 'Left' reason feeds the per-entry drop record; the 'Right' is the https
'RegistryUrl' the artifact is fetched from. Hosts are compared by the same bare-host
extraction the allowlist uses ('Ecluse.Core.Security.hostAddress'). This composes with,
and never replaces, the host allowlist and the same-host tarball policy applied at serve
time.
-}
resolveTarballUrl :: Text -> Text -> Either Text RegistryUrl
resolveTarballUrl upstreamHost url
    | "https://" `T.isPrefixOf` lowered = mkRegistryUrl url
    | "http://" `T.isPrefixOf` lowered =
        if hostAddress url == upstreamHost
            then mkRegistryUrl ("https://" <> T.drop (T.length ("http://" :: Text)) url)
            else Left ("dist.tarball is http on a host other than the upstream registry: " <> url)
    | otherwise = Left ("dist.tarball is not an https URL: " <> url)
  where
    lowered = T.toLower url
