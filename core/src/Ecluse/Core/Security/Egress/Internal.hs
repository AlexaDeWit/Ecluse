-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The private construction boundary for 'RegistryUrl'.

@ecluse-core@ does not expose this module (it is an @other-module@), so the raw
'RegistryUrl' constructor is reachable only from inside the library. The public
"Ecluse.Core.Security.Egress" re-exports the type /abstractly/, with the https-only
'mkRegistryUrl' and its configured-endpoint form 'mkConfiguredRegistryUrl' as the only
production builders. The test- and dev-only loopback builder in
"Ecluse.Core.Security.Egress.DevHttp" compiles only under the @dev-http-egress@ Cabal
flag. A release build therefore carries no way to construct a non-https registry
target, in code or in configuration.
-}
module Ecluse.Core.Security.Egress.Internal (
    RegistryUrl (..),
    mkRegistryUrl,
    mkConfiguredRegistryUrl,
    registryUrlText,
) where

import Data.Text qualified as T

import Ecluse.Core.Security.Authority (authoritySpan)

{- | An outbound registry-egress URL that is https by construction. Both production constructors
reject any other scheme, so a plain-HTTP registry target cannot be represented in a running system.
Stored with surrounding whitespace trimmed.
-}
newtype RegistryUrl = RegistryUrl Text
    deriving stock (Eq, Ord, Show)

{- | Build a 'RegistryUrl', accepting only an @https:\/\/@ URL, the scheme matched
case-insensitively. The configuration layer fails closed at boot on the 'Left' reason and reports
the offending value.

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

{- | Build a 'RegistryUrl' for an __operator-configured__ registry endpoint: 'mkRegistryUrl' under
the further rule that a configured endpoint carries no credential material. It refuses userinfo
(@https:\/\/user:token\@host\/@), a query string, and a fragment.

The refusals run before 'mkRegistryUrl', which quotes the value it rejects. A refusal reaches the
boot log, so it names the requirement and never the URL. An upstream-supplied @dist.tarball@ keeps
to 'mkRegistryUrl'. It may carry a signed query, the @host:port@ allowlist authorises it, and every
log line reduces it to its authority ('Ecluse.Core.Security.Authority.authorityLabel').

>>> mkConfiguredRegistryUrl "https://registry.npmjs.org"
Right (RegistryUrl "https://registry.npmjs.org")

>>> mkConfiguredRegistryUrl "https://deploy:hunter2@registry.npmjs.org"
Left "registry URL must not carry userinfo (a credential belongs in its own configuration key)"
-}
mkConfiguredRegistryUrl :: Text -> Either Text RegistryUrl
mkConfiguredRegistryUrl raw
    | "@" `T.isInfixOf` authoritySpan trimmed =
        Left "registry URL must not carry userinfo (a credential belongs in its own configuration key)"
    | "?" `T.isInfixOf` trimmed = Left "registry URL must not carry a query string"
    | "#" `T.isInfixOf` trimmed = Left "registry URL must not carry a fragment"
    | otherwise = mkRegistryUrl trimmed
  where
    trimmed = T.strip raw

-- | The underlying URL text.
registryUrlText :: RegistryUrl -> Text
registryUrlText (RegistryUrl u) = u
