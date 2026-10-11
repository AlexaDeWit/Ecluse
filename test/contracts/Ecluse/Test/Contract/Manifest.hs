-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Preparation and report audits run separately from test entrypoints.
module Ecluse.Test.Contract.Manifest (
    writePlan,
    auditFiles,
    readIdentity,
    jsonLine,
    digest,
    failRunner,
    usesProductImage,
) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T

import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Types

-- | Emit validated metadata without starting a fixture or test.
writePlan :: Catalogue -> IO ()
writePlan catalogue = either (failRunner . T.unlines) (BS.putStr . jsonLine) $ makePlan catalogue

-- | Audit every fixed suite report against the prepared plan.
auditFiles :: Plan -> FilePath -> [FilePath] -> IO ()
auditFiles compiled path reportPaths = do
    (plan, expected) <- loadAuditPlan compiled path
    reports <- traverse (BS.readFile >=> decodeFileBytes) reportPaths
    let faults = auditReports expected plan reports
    unless (null faults) $ failRunner $ T.unlines faults
    putTextLn "Every required group and case passed with matching execution identities."

loadAuditPlan :: Plan -> FilePath -> IO (Plan, ExecutionIdentity)
loadAuditPlan compiled path = do
    bytes <- BS.readFile path
    plan <- decodeFileBytes bytes
    unless (plan == compiled) $ failRunner "downloaded plan differs from the compiled catalogue"
    expected <- readIdentity True (usesProductImage plan) $ digest bytes
    pure (plan, expected)

-- | Bind evidence to the immutable preparation identities.
readIdentity :: Bool -> Bool -> Text -> IO ExecutionIdentity
readIdentity forceRequired imageRequired planDigest = do
    explicit <- (== Just "1") <$> lookupEnv "ECLTEST_REQUIRED"
    ci <- (== Just "true") <$> lookupEnv "CI"
    let required = forceRequired || explicit || ci
    let field name fallback =
            lookupEnv name >>= \case
                Just value | not (null value) -> pure $ toText value
                _ | required -> failRunner $ "missing required execution identity: " <> toText name
                _ -> pure fallback
    source <- field "ECLTEST_SOURCE" "local"
    image <- if imageRequired then field "ECLTEST_IMAGE_ID" "local" else pure "not-applicable"
    architecture <- field "ECLTEST_ARCHITECTURE" "local"
    runId <- field "ECLTEST_RUN_ID" "local"
    declaredDigest <- field "ECLTEST_PLAN_DIGEST" planDigest
    unless (declaredDigest == planDigest) $ failRunner "plan digest differs from the compiled catalogue"
    pure $ ExecutionIdentity source image architecture planDigest runId

-- | Canonical JSON artifact bytes include a final newline.
jsonLine :: (ToJSON a) => a -> ByteString
jsonLine value = BL.toStrict (encode value) <> "\n"

-- | Hash the exact bytes distributed as the execution plan.
digest :: ByteString -> Text
digest bytes = show (hash bytes :: Digest SHA256)

decodeFileBytes :: (FromJSON a) => ByteString -> IO a
decodeFileBytes = either (failRunner . toText) pure . eitherDecodeStrict'

-- | Report an invalid declaration or artifact and stop the program.
failRunner :: Text -> IO a
failRunner message = putTextLn message >> exitFailure

-- | Only E2E declarations require an immutable product image.
usesProductImage :: Plan -> Bool
usesProductImage = any ((== E2E) . groupTier) . catalogueGroups . planCatalogue
