-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The egress posture for registry traffic: https-only by construction.

Every outbound registry URL is a 'RegistryUrl', so a plain-HTTP target cannot be represented
and a non-https configured upstream fails closed at boot. TLS certificate validation, not a
resolved-IP pin, is the endpoint-authentication boundary: an attacker who steers a name to an
internal address cannot make it present a CA-trusted certificate for the requested host. The
host allowlist ('Ecluse.Core.Security.isAllowedUpstreamHost'), the literal internal-range
block, and the @redirectCount = 0@ every request carries are complementary controls owned
elsewhere.
-}
module Ecluse.Core.Security.Egress (
    -- * The https-only egress URL
    RegistryUrl,
    mkRegistryUrl,
    mkConfiguredRegistryUrl,
    registryUrlText,

    -- * Packument @dist.tarball@ normalisation
    resolveTarballUrl,
) where

import Data.Text qualified as T

import Ecluse.Core.Security (authorityLabel, hostAddress)
import Ecluse.Core.Security.Egress.Internal (RegistryUrl, mkConfiguredRegistryUrl, mkRegistryUrl, registryUrlText)

{- | Resolve a packument's @dist.tarball@ against the https-only posture, given the bare host
the packument came from: plaintext upgrades to https only on that same host, any other
plaintext target is refused, and a refusal names the authority, never the URL, because an
upstream-supplied @dist.tarball@ can carry a credential. It authorises nothing on its own.
-}
resolveTarballUrl :: Text -> Text -> Either Text RegistryUrl
resolveTarballUrl upstreamHost url
    | "https://" `T.isPrefixOf` lowered = mkRegistryUrl url
    | "http://" `T.isPrefixOf` lowered =
        if hostAddress url == upstreamHost
            then mkRegistryUrl ("https://" <> T.drop httpSchemeChars url)
            else Left ("dist.tarball is http on a host other than the upstream registry: " <> authorityLabel url)
    | otherwise = Left ("dist.tarball is not an https URL: " <> authorityLabel url)
  where
    lowered = T.toLower url
    -- The character count of the "http://" prefix. Dropping it from the original @url@, not
    -- @lowered@, rewrites the scheme and preserves the rest of the URL verbatim.
    httpSchemeChars = 7 :: Int
