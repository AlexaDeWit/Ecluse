-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Snapshot fixtures share one byte digest between projection and assembly.
module Ecluse.Test.Snapshot (jsonSnapshot, projectJsonSnapshot, syntheticSnapshot) where

import Data.Aeson (Value, encode)

import Ecluse.Core.Snapshot (Snapshot (..), digestOf)
import Ecluse.Test.Support (expectRight)

-- | Treat a fixture's compact encoding as its upstream bytes.
jsonSnapshot :: Value -> Snapshot Value
jsonSnapshot value = Snapshot (digestOf (toStrict (encode value))) value

-- | Project and fingerprint the same fixture bytes.
projectJsonSnapshot :: (Show err) => (ByteString -> Either err a) -> Value -> IO (Snapshot a)
projectJsonSnapshot project value = do
    let body = toStrict (encode value)
    Snapshot (digestOf body) <$> expectRight (project body)

-- | Scope a synthetic domain fixture to its textual representation, without a wire adapter.
syntheticSnapshot :: (Show a) => a -> Snapshot a
syntheticSnapshot value = Snapshot (digestOf (encodeUtf8 (show value))) value
