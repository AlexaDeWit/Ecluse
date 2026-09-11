-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory store for end-to-end scenarios: the emulated S3 bucket, the environment every
role reads it through, and Pilot's own compile-and-upload run against a committed corpus
generation.
-}
module Ecluse.E2E.Harness.Advisories (
    advisoryBucket,
    advisoryStoreEnv,
    createAdvisoryBucket,
    publishAdvisoryGeneration,
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

import Ecluse.E2E.Fixtures.Advisories (advisoryEpssPath, advisoryExportPath)
import Ecluse.E2E.Harness.Docker (RoleRun, advisoryDataDir, runRoleOnce)
import Ecluse.E2E.Harness.Types (GlobalDataPlane (gdpMiniPort))
import Ecluse.Test.Osv (CorpusVersion)
import Ecluse.Test.Poll (pollUntil)

-- | The bucket Pilot uploads compiled advisory artifacts to, and the proxy and Dredger read.
advisoryBucket :: Text
advisoryBucket = "ecluse-e2e-advisories"

{- | The advisory-store environment every role shares. The poll interval is short because one
scenario observes two generations, where a deployment waits out the shipped minute.
-}
advisoryStoreEnv :: [(Text, Text)]
advisoryStoreEnv =
    [ ("ECLUSE_ADVISORIES__URL", "s3://" <> advisoryBucket)
    , ("ECLUSE_ADVISORIES__POLL_INTERVAL", "2")
    , -- The AWS-SDK-standard generic override, which the S3 advisory client reads. The dummy
      -- keys sign a request the emulator does not validate.
      ("AWS_ENDPOINT_URL", "http://ministack:4566")
    , ("AWS_REGION", "us-east-1")
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    ]

{- | Create the advisory bucket in the ministack S3 emulator, retrying while it warms up.
@CreateBucket@ is idempotent for its own owner, so a retry after a partial answer is safe.
-}
createAdvisoryBucket :: GlobalDataPlane -> IO ()
createAdvisoryBucket gdp = do
    manager <- newManager defaultManagerSettings
    credentialScope <- s3CredentialScope
    created <- pollUntil 60 500000 id (attempt manager credentialScope)
    unless created (fail "ministack S3 never accepted the advisory bucket within the timeout")
  where
    endpoint = "http://127.0.0.1:" <> show (gdpMiniPort gdp) <> "/" <> advisoryBucket
    attempt manager credentialScope =
        handleAny (\_ -> pure False) $ do
            base <- parseRequest (toString endpoint)
            resp <- httpLbs base{method = "PUT", requestHeaders = [(hAuthorization, encodeUtf8 credentialScope)]} manager
            -- 409 is BucketAlreadyOwnedByYou, which a retried create reports.
            pure (statusCode (responseStatus resp) `elem` [200, 409])

{- The emulator routes a request to a service by the credential scope and verifies no signature,
so this header decides that the path-addressed PUT reaches S3 rather than another service. -}
s3CredentialScope :: IO Text
s3CredentialScope = do
    day <- formatTime defaultTimeLocale "%Y%m%d" <$> getCurrentTime
    pure
        ( "AWS4-HMAC-SHA256 Credential=test/"
            <> toText day
            <> "/us-east-1/s3/aws4_request, SignedHeaders=host, Signature="
            <> T.replicate 64 "0"
        )

{- | Compile one corpus generation through the product image's Pilot and upload it to the advisory
store, the cycle a scheduled export runs. Its archives come from the stub, so it reaches no feed.
-}
publishAdvisoryGeneration :: GlobalDataPlane -> CorpusVersion -> IO RoleRun
publishAdvisoryGeneration gdp generation =
    runRoleOnce
        gdp
        pilotEnv
        [ "pilot"
        , "compile"
        , "--ecosystem"
        , "npm"
        , "--source"
        , toString (stubUrl (advisoryExportPath generation))
        , "--epss-source"
        , toString (stubUrl advisoryEpssPath)
        , "--out"
        , advisoryDataDir
        , "--upload"
        ]

-- The public-upstream stub serves the advisory exports beside the package fixtures.
stubUrl :: Text -> Text
stubUrl path = "https://upstream/" <> path

{- Pilot's own environment. It declares one mount because a Pilot with no ecosystem has nothing to
compile and refuses to boot, and that mount makes server.publicUrl required. -}
pilotEnv :: [(Text, Text)]
pilotEnv =
    [ ("ECLUSE_SERVER__PORT", "4873")
    , ("ECLUSE_SERVER__PUBLIC_URL", "http://127.0.0.1:4873")
    , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM__REGISTRY__URL", "https://upstream/")
    , ("ECLUSE_OBSERVABILITY__LOG_FORMAT", "json")
    , ("SSL_CERT_FILE", "/certs/bundle.pem")
    ]
        <> advisoryStoreEnv
