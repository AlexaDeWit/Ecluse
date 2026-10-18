-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The product identity shared by outbound registry requests and advisory feeds.
module Ecluse.Core.BuildIdentity (
    productName,
    productVersion,
    userAgent,
    identifyRequest,
) where

import Data.Version (showVersion)
import Network.HTTP.Client (Request (requestHeaders))
import Network.HTTP.Types.Header (hUserAgent)
import Paths_ecluse (version)

-- | The name Écluse identifies itself by to another service.
productName :: Text
productName = "ecluse"

-- | The running build's version, as the cabal file declares it.
productVersion :: Text
productVersion = toText (showVersion version)

-- | The default identity, including the project's public URL.
userAgent :: ByteString
userAgent = encodeUtf8 (productName <> "/" <> productVersion <> " (+https://ecluse-proxy.com)")

-- | Identify an outbound request unless the caller supplies its own identity.
identifyRequest :: Request -> Request
identifyRequest request
    | any ((== hUserAgent) . fst) headers = request
    | otherwise = request{requestHeaders = (hUserAgent, userAgent) : headers}
  where
    headers = requestHeaders request
