-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Withdrawal variants of committed advisory fixtures.
An independent overlap and an unrelated active control remain in every archive.
-}
module Ecluse.Test.Osv.Withdrawal (withdrawalBytes, withdrawalZip, maliciousZip, maliciousEntries) where

import Data.Aeson (Value (Object), eitherDecode, eitherDecodeStrict, encode, object, (.=))
import Data.Aeson qualified
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor, osvExportDirectory)
import Ecluse.Test.Osv (osvZipOf)

-- | Set only the withdrawal field of the committed active record, including malformed test values.
withdrawalBytes :: Maybe Value -> IO ByteString
withdrawalBytes withdrawn = do
    bytes <- readFileBS "test/fixtures/osv/withdrawal/active.json"
    case eitherDecodeStrict bytes of
        Right (Object fields) -> pure (LBS.toStrict (encode (Object (maybe fields (\v -> KM.insert "withdrawn" v fields) withdrawn))))
        Right _ -> fail "withdrawal fixture is not an object"
        Left err -> fail err

-- | Assemble a transition archive whose active controls keep its compiled output nonempty.
withdrawalZip :: Maybe Value -> IO LByteString
withdrawalZip withdrawn = do
    target <- withdrawalBytes withdrawn
    independent <- readFileLBS "test/fixtures/osv/withdrawal/independent.json"
    control <- readFileLBS "test/fixtures/osv/v1/GHSA-corpus-0001.json"
    osvZipOf [("active.json", LBS.fromStrict target), ("independent.json", independent), ("control.json", control)]

-- | Derive exact-point MAL records from the attributed capture, retaining its active row as a control.
maliciousZip :: Ecosystem -> [(Text, [Text])] -> Maybe Value -> IO LByteString
maliciousZip eco targets withdrawn = maliciousEntries eco targets withdrawn >>= osvZipOf

-- | The attributed capture and labelled variants before archive assembly.
maliciousEntries :: Ecosystem -> [(Text, [Text])] -> Maybe Value -> IO [(Text, LByteString)]
maliciousEntries eco targets withdrawn = do
    captured <- readFileLBS "test/fixtures/osv/malicious/MAL-2025-46969.json"
    fields <- case eitherDecode captured of
        Right (Object value) -> pure value
        Right _ -> fail "MAL capture is not an object"
        Left err -> fail err
    pure (("MAL-2025-46969.json", captured) : derived fields Nothing 0 ("malicious-fixture-control", ["1.0.0"]) : zipWith (derived fields withdrawn) [1 :: Int ..] targets)
  where
    derived fields withdrawal n (name, versions) =
        let ident = "MAL-derived-" <> show n
            affected = object ["package" .= object ["name" .= name, "ecosystem" .= osvExportDirectory (osvEcosystemFor eco)], "versions" .= versions]
            changed = KM.insert "id" (toJSONText ident) . KM.insert "affected" (toJSONList [affected]) . KM.insert "summary" (toJSONText "Derived inert test fixture") $ fields
            final = maybe changed (\value -> KM.insert "withdrawn" value changed) withdrawal
         in (ident <> ".json", encode (Object final))
    toJSONText = Data.Aeson.toJSON :: Text -> Value
    toJSONList = Data.Aeson.toJSON :: [Value] -> Value
