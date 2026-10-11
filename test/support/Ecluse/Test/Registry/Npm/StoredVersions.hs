-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Strict independent observations of a registry's stored document.
module Ecluse.Test.Registry.Npm.StoredVersions (
    storedObjectResponse,
    storedVersionKeys,
) where

import Data.Aeson (Object, eitherDecodeStrict, (.:))
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)

-- | Only HTTP 404 proves absence. Other failures retain their status or decoding error.
storedObjectResponse :: Key -> Int -> ByteString -> Either Text (Maybe Object)
storedObjectResponse field status body = case status of
    404 -> Right Nothing
    200 -> Just <$> first toText (eitherDecodeStrict body >>= parseEither (.: field))
    _ -> Left ("packument returned HTTP " <> show status)

-- | Preserve every exact raw version key from a readable response.
storedVersionKeys :: Int -> ByteString -> Either Text [Text]
storedVersionKeys status body =
    maybe [] (sort . map Key.toText . KeyMap.keys) <$> storedObjectResponse "versions" status body
