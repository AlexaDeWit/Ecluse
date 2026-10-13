-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Accumulate qualification faults before writing a failed audit.
module Ecluse.OsvIndex.Audit (Audit, Fault (..), newAudit, recordFaults, attempt, decodeJson, decodeLines, auditFaults) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict')
import Data.ByteString qualified as BS
import UnliftIO.Exception (throwIO, tryAny)

-- | A fault keeps its evidence scope when later checks also fail.
data Fault = Fault {faultScope :: Text, faultDetail :: Text}
    deriving stock (Eq, Show, Generic)

instance FromJSON Fault
instance ToJSON Fault

newtype Audit = Audit (IORef [Fault])
newtype EvidenceFailure = EvidenceFailure Text
    deriving stock (Show)
instance Exception EvidenceFailure

-- | Collect failures until the report transaction finishes.
newAudit :: IO Audit
newAudit = Audit <$> newIORef []

-- | Retain every failed obligation without stopping independent checks.
recordFaults :: Audit -> Text -> [Text] -> IO ()
recordFaults (Audit ref) scope faults = modifyIORef' ref (<> map (Fault scope) faults)

-- | Convert read and decode failures into stored qualification faults.
attempt :: Audit -> Text -> IO a -> IO (Maybe a)
attempt audit scope action =
    tryAny action >>= \case
        Left exception -> Nothing <$ recordFaults audit scope [show exception]
        Right value -> pure (Just value)

-- | Malformed JSON raises a typed evidence failure.
decodeJson :: (FromJSON a) => FilePath -> IO a
decodeJson path = BS.readFile path >>= decodeBytes

-- | Every nonempty JSONL record must decode.
decodeLines :: (FromJSON a) => FilePath -> IO [a]
decodeLines path = BS.readFile path >>= traverse decodeBytes . filter (not . BS.null) . BS.split 10

decodeBytes :: (FromJSON a) => ByteString -> IO a
decodeBytes = either (throwIO . EvidenceFailure . toText) pure . eitherDecodeStrict'

-- | Return faults in qualification order.
auditFaults :: Audit -> IO [Fault]
auditFaults (Audit ref) = readIORef ref
