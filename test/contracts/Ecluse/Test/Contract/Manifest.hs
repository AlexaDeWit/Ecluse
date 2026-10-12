-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Preparation and report audits run separately from test entrypoints.
module Ecluse.Test.Contract.Manifest (
    writePlan,
    auditFiles,
    auditCompletionFiles,
    readIdentity,
    jsonLine,
    writeJsonFile,
    readPlanIdentity,
    digest,
    failRunner,
    usesProductImage,
) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T

import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory)

import Ecluse.Test.Contract.Completion (Completion, auditCompletion)
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

-- | Keep each tier's compiled plan and image identity while auditing one logical completion.
auditCompletionFiles :: Completion -> [(Plan, FilePath, [FilePath])] -> IO ()
auditCompletionFiles completion inputs = do
    executions <- forM inputs $ \(compiled, path, reportPaths) -> do
        bytes <- BS.readFile path
        plan <- decodeFileBytes bytes
        unless (plan == compiled) $ failRunner "downloaded plan differs from the compiled catalogue"
        expected <- readIdentityFields False True (usesProductImage plan) (digest bytes)
        reports <- traverse (BS.readFile >=> decodeFileBytes) reportPaths
        pure (expected, plan, reports)
    let faults = auditCompletion completion executions
    unless (null faults) $ failRunner $ T.unlines faults
    putTextLn "Every required completion law passed in its declared tier and execution."

loadAuditPlan :: Plan -> FilePath -> IO (Plan, ExecutionIdentity)
loadAuditPlan compiled path = do
    bytes <- BS.readFile path
    plan <- decodeFileBytes bytes
    unless (plan == compiled) $ failRunner "downloaded plan differs from the compiled catalogue"
    expected <- readIdentity True (usesProductImage plan) $ digest bytes
    pure (plan, expected)

-- | Bind evidence to the immutable preparation identities.
readIdentity :: Bool -> Bool -> Text -> IO ExecutionIdentity
readIdentity = readIdentityFields True

readIdentityFields :: Bool -> Bool -> Bool -> Text -> IO ExecutionIdentity
readIdentityFields checkDeclaredDigest forceRequired imageRequired planDigest = do
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
    architecture <- normaliseArchitecture <$> field "ECLTEST_ARCHITECTURE" "local"
    runId <- field "ECLTEST_RUN_ID" "local"
    declaredDigest <- if checkDeclaredDigest then field "ECLTEST_PLAN_DIGEST" planDigest else pure planDigest
    unless (declaredDigest == planDigest) $ failRunner "plan digest differs from the compiled catalogue"
    pure $ ExecutionIdentity source image architecture planDigest runId

-- | A producer binds its compiled plan directly rather than requiring a pre-existing digest variable.
readPlanIdentity :: Bool -> Text -> IO ExecutionIdentity
readPlanIdentity = readIdentityFields False False

-- | Atomic JSON writes preserve the last complete receipt if a producer stops between writes.
writeJsonFile :: (ToJSON a) => FilePath -> a -> IO ()
writeJsonFile path value = do
    createDirectoryIfMissing True $ takeDirectory path
    BS.writeFile (path <> ".tmp") (jsonLine value)
    renameFile (path <> ".tmp") path

normaliseArchitecture :: Text -> Text
normaliseArchitecture = \case
    "x86_64" -> "amd64"
    "X64" -> "amd64"
    "aarch64" -> "arm64"
    "ARM64" -> "arm64"
    architecture -> architecture

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
