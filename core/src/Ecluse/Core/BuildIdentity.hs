-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The proxy's own identity: the product name, the version the build declares, and the
@User-Agent@ every outbound request carries. The version is read from @Paths_ecluse@, so it
follows the @version:@ field of @ecluse.cabal@ that VERSIONING.md makes authoritative.

This is proxy identity rather than any ecosystem's protocol fact, so it lives outside every
adapter namespace. The module depends on nothing else in @Ecluse@, so any module may import
it without risking an import cycle.
-}
module Ecluse.Core.BuildIdentity (
    productName,
    productVersion,
    userAgent,
) where

import Data.Version (showVersion)
import Paths_ecluse (version)

-- | The name Écluse identifies itself by to another service.
productName :: Text
productName = "ecluse"

-- | The running build's version, as the cabal file declares it.
productVersion :: Text
productVersion = toText (showVersion version)

{- | The @User-Agent@ value for an outbound request, @ecluse\/\<version\>@.
'Ecluse.Core.Registry.Request.sealRequest' sets it once for every adapter.
-}
userAgent :: ByteString
userAgent = encodeUtf8 (productName <> "/" <> productVersion)
