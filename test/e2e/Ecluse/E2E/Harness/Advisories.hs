-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The emulated advisory store and whole-image Pilot compile-and-upload runs.
module Ecluse.E2E.Harness.Advisories (
    advisoryBucket,
    advisoryStoreEnv,
    createAdvisoryBucket,
    publishAdvisoryGeneration,
    publishAdvisoryExport,
    s3CredentialScope,
) where

import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Network.HTTP.Client (
    Request (method, requestHeaders),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseStatus,
 )
import Network.HTTP.Types (hAuthorization, statusCode)
import UnliftIO (handleAny)

import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName)
import Ecluse.E2E.Fixtures.Advisories (advisoryEpssPath, advisoryExportBase)
import Ecluse.E2E.Harness.Docker (RoleRun, advisoryDataDir, ministackAwsEnv, runRoleOnce)
import Ecluse.E2E.Harness.Stub (StubRoute (NpmPublic), publicStubRoute, stubUrl)
import Ecluse.E2E.Harness.Types (GlobalDataPlane, ministackPort)
import Ecluse.Test.Osv (CorpusVersion)
import Ecluse.Test.Poll (pollUntil)

-- | The bucket Pilot uploads compiled advisory artifacts to, and the proxy and Dredger read.
advisoryBucket :: Text
advisoryBucket = "ecluse-e2e-advisories"

-- | The shared store with a short poll interval for scenarios that replace a generation.
advisoryStoreEnv :: [(Text, Text)]
advisoryStoreEnv =
    [ ("ECLUSE_ADVISORIES__URL", "s3://" <> advisoryBucket)
    , ("ECLUSE_ADVISORIES__POLL_INTERVAL", "2")
    ]
        <> ministackAwsEnv

-- | Retry bucket creation while ministack warms up. Repeated owner requests are idempotent.
createAdvisoryBucket :: GlobalDataPlane -> IO ()
createAdvisoryBucket gdp = do
    port <- ministackPort gdp
    let endpoint = "http://127.0.0.1:" <> show port <> "/" <> advisoryBucket
    manager <- newManager defaultManagerSettings
    credentialScope <- s3CredentialScope
    created <- pollUntil 60 500000 id (attempt endpoint manager credentialScope)
    unless created (fail "ministack S3 never accepted the advisory bucket within the timeout")
  where
    attempt endpoint manager credentialScope =
        handleAny (\_ -> pure False) $ do
            base <- parseRequest (toString endpoint)
            resp <- httpLbs base{method = "PUT", requestHeaders = [(hAuthorization, encodeUtf8 credentialScope)]} manager
            -- 409 is BucketAlreadyOwnedByYou, which a retried create reports.
            pure (statusCode (responseStatus resp) `elem` [200, 409])

-- The emulator routes on the credential scope and verifies no signature.
s3CredentialScope :: IO Text
s3CredentialScope = do
    day <- formatTime defaultTimeLocale "%Y%m%d" <$> getCurrentTime
    pure
        ( "AWS4-HMAC-SHA256 Credential=test/"
            <> toText day
            <> "/us-east-1/s3/aws4_request, SignedHeaders=host, Signature="
            <> T.replicate 64 "0"
        )

-- | Compile and upload one npm corpus generation through the product image's Pilot.
publishAdvisoryGeneration :: GlobalDataPlane -> CorpusVersion -> IO RoleRun
publishAdvisoryGeneration gdp generation =
    publishAdvisoryExport gdp Npm (advisoryExportBase generation)

-- | Compile and upload one ecosystem's stub export, joined with the local EPSS slice.
publishAdvisoryExport :: GlobalDataPlane -> Ecosystem -> Text -> IO RoleRun
publishAdvisoryExport gdp ecosystem exportBase =
    runRoleOnce
        gdp
        (pilotEnv ecosystem <> advisorySourceEnv exportBase)
        ["pilot", "compile", "--ecosystem", toString (ecosystemName ecosystem), "--out", advisoryDataDir, "--upload"]

advisorySourceEnv :: Text -> [(Text, Text)]
advisorySourceEnv exportBase =
    [ ("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", publicUrl exportBase)
    , ("ECLUSE_ADVISORIES__EPSS_FEED_URL", publicUrl advisoryEpssPath)
    ]

publicUrl :: Text -> Text
publicUrl path = stubUrl NpmPublic <> path

-- Pilot requires an active mount and its server public URL even for one-shot compilation.
pilotEnv :: Ecosystem -> [(Text, Text)]
pilotEnv ecosystem =
    [ ("ECLUSE_SERVER__PORT", "4873")
    , ("ECLUSE_SERVER__PUBLIC_URL", "http://127.0.0.1:4873")
    , ("ECLUSE_MOUNTS__" <> T.toUpper (ecosystemName ecosystem) <> "__ENABLED", "true")
    , ("ECLUSE_MOUNTS__" <> T.toUpper (ecosystemName ecosystem) <> "__PUBLIC_UPSTREAM__REGISTRY__URL", stubUrl (publicStubRoute ecosystem))
    , ("ECLUSE_OBSERVABILITY__LOG_FORMAT", "json")
    , ("SSL_CERT_FILE", "/certs/bundle.pem")
    ]
        <> advisoryStoreEnv
