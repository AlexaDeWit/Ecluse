-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Strict independent observations of a registry's stored document.
module Ecluse.Test.Registry.Npm.StoredVersions (
    storedObjectResponse,
    storedVersionKeys,
    storedDocumentFacts,
) where

import Data.Aeson (Object, Value (Object), eitherDecodeStrict, encode, (.:))
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

-- | Each version key, @dist-tags@ entry, and @deprecated@ member of a readable response, as sorted facts.
storedDocumentFacts :: Int -> ByteString -> Either Text [Text]
storedDocumentFacts status body = do
    versions <- maybe [] KeyMap.toList <$> storedObjectResponse "versions" status body
    tags <- maybe [] KeyMap.toList <$> storedObjectResponse "dist-tags" status body
    pure . sort $ concatMap versionFacts versions <> ["dist-tag " <> Key.toText tag <> " " <> rendered target | (tag, target) <- tags]
  where
    versionFacts (version, document) =
        ("version " <> Key.toText version)
            : ["deprecated " <> Key.toText version <> " " <> rendered message | Object fields <- [document], Just message <- [KeyMap.lookup "deprecated" fields]]
    rendered :: Value -> Text
    rendered = decodeUtf8 . encode
