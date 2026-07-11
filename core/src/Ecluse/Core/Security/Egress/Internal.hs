-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The private construction boundary for 'RegistryUrl'.

This module is __not exposed__ from @ecluse-core@ (it is an @other-module@), so the
raw 'RegistryUrl' constructor is reachable only from inside the library. The public
"Ecluse.Core.Security.Egress" re-exports the type /abstractly/, with the https-only
'mkRegistryUrl' as the sole production builder; the test- and dev-only loopback
builder in "Ecluse.Core.Security.Egress.DevHttp" is compiled only under the
@dev-http-egress@ Cabal flag. A release build therefore carries no way to construct a
non-https registry target, in code or in configuration.
-}
module Ecluse.Core.Security.Egress.Internal (
    RegistryUrl (..),
    mkRegistryUrl,
    registryUrlText,
) where

import Data.Text qualified as T

{- | An outbound registry-egress URL that is __https by construction__. The only
production constructor, 'mkRegistryUrl', rejects any non-https scheme, so a
plain-HTTP registry target cannot be represented in a running system: every
configured upstream, mirror, and publication endpoint, and every @dist.tarball@
target, is one of these. Stored normalised (surrounding whitespace trimmed).
-}
newtype RegistryUrl = RegistryUrl Text
    deriving stock (Eq, Ord, Show)

{- | Build a 'RegistryUrl', accepting only an @https:\/\/@ URL (the scheme matched
case-insensitively, since URI schemes are). A non-https or empty value is rejected
with a reason that names the requirement, so the aggregating configuration layer can
fail closed at boot and report the offending value.

>>> mkRegistryUrl "https://registry.npmjs.org"
Right (RegistryUrl "https://registry.npmjs.org")

>>> mkRegistryUrl "http://registry.npmjs.org"
Left "registry URL must use https (got http://registry.npmjs.org)"
-}
mkRegistryUrl :: Text -> Either Text RegistryUrl
mkRegistryUrl raw
    | T.null trimmed = Left "expected a non-empty https URL"
    | "https://" `T.isPrefixOf` T.toLower trimmed = Right (RegistryUrl trimmed)
    | otherwise = Left ("registry URL must use https (got " <> trimmed <> ")")
  where
    trimmed = T.strip raw

-- | The underlying URL text.
registryUrlText :: RegistryUrl -> Text
registryUrlText (RegistryUrl u) = u
