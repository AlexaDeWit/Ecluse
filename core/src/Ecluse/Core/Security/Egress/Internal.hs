-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The private construction boundary for 'RegistryUrl'.

@ecluse-core@ does not expose this module (it is an @other-module@), so the raw constructor is
reachable only from inside the library. "Ecluse.Core.Security.Egress" re-exports the type
abstractly with the https-only builders, and the loopback builder in
"Ecluse.Core.Security.Egress.DevHttp" compiles only under the @dev-http-egress@ Cabal flag.
-}
module Ecluse.Core.Security.Egress.Internal (
    RegistryUrl (..),
    mkRegistryUrl,
    mkConfiguredRegistryUrl,
    registryUrlText,
) where

import Data.Text qualified as T

import Ecluse.Core.Security.Authority (refuseCredentialMaterial)

{- | An outbound registry-egress URL, https by construction and stored with surrounding
whitespace trimmed. A plain-HTTP registry target cannot be represented in a running system.
-}
newtype RegistryUrl = RegistryUrl Text
    deriving stock (Eq, Ord, Show)

{- | Build a 'RegistryUrl', accepting only an @https:\/\/@ URL, matched case-insensitively. The
configuration layer fails closed at boot on the 'Left' reason, which quotes the offending value.

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

{- | Build a 'RegistryUrl' for an __operator-configured__ endpoint. @refuseCredentialMaterial@
runs before 'mkRegistryUrl', which quotes what it rejects.

>>> mkConfiguredRegistryUrl "https://registry.npmjs.org"
Right (RegistryUrl "https://registry.npmjs.org")

>>> mkConfiguredRegistryUrl "https://deploy:hunter2@registry.npmjs.org"
Left "registry URL must not carry userinfo (a credential belongs in its own configuration key)"
-}
mkConfiguredRegistryUrl :: Text -> Either Text RegistryUrl
mkConfiguredRegistryUrl raw = do
    refuseCredentialMaterial "registry URL" trimmed
    mkRegistryUrl trimmed
  where
    trimmed = T.strip raw

-- | The underlying URL text.
registryUrlText :: RegistryUrl -> Text
registryUrlText (RegistryUrl u) = u
