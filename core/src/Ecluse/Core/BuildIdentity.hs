-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The product identity shared by outbound registry requests and advisory feeds.
module Ecluse.Core.BuildIdentity (
    OperatorContact,
    parseOperatorContact,
    renderUserAgent,
    identityManagerSettings,
    productName,
    productVersion,
    userAgent,
    identifyRequest,
) where

import Data.Text qualified as T
import Data.Version (showVersion)
import Network.HTTP.Client (ManagerSettings (managerModifyRequest), Request (requestHeaders))
import Network.HTTP.Types.Header (hUserAgent)
import Paths_ecluse (version)

-- | The name Écluse identifies itself by to another service.
productName :: Text
productName = "ecluse"

-- | The running build's version, as the cabal file declares it.
productVersion :: Text
productVersion = toText (showVersion version)

-- | An operator contact that cannot alter the HTTP comment or inject a header.
newtype OperatorContact = OperatorContact Text
    deriving stock (Eq, Show)

-- | Parse 1-256 printable ASCII characters, excluding comment delimiters and backslash.
parseOperatorContact :: Text -> Either Text OperatorContact
parseOperatorContact value
    | T.null value || T.compareLength value 256 == GT || T.any forbidden value =
        Left "must contain 1-256 printable ASCII characters, excluding parentheses and backslash"
    | otherwise = Right (OperatorContact value)
  where
    forbidden character = character < ' ' || character > '~' || character `elem` ['(', ')', '\\']

-- | Render a validated contact into the outbound identity before requests start.
renderUserAgent :: Maybe OperatorContact -> ByteString
renderUserAgent contact =
    encodeUtf8 (productName <> "/" <> productVersion <> " (+https://ecluse-proxy.com" <> suffix <> ")")
  where
    suffix = maybe "" (\(OperatorContact value) -> "; contact: " <> value) contact

-- | The default identity, including the project's public URL.
userAgent :: ByteString
userAgent = renderUserAgent Nothing

-- | Identify an outbound request unless the caller supplies its own identity.
identifyRequest :: Request -> Request
identifyRequest = identifyWith userAgent

-- | Chain request callbacks and replace only the default product identity or an absent identity.
identityManagerSettings :: ByteString -> ManagerSettings -> ManagerSettings
identityManagerSettings renderedAgent settings =
    settings{managerModifyRequest = fmap (identifyWith renderedAgent) . managerModifyRequest settings}

identifyWith :: ByteString -> Request -> Request
identifyWith renderedAgent request
    | any customIdentity headers = request
    | otherwise = request{requestHeaders = (hUserAgent, renderedAgent) : filter ((/= hUserAgent) . fst) headers}
  where
    headers = requestHeaders request
    customIdentity (name, value) = name == hUserAgent && value /= userAgent && value /= renderedAgent
