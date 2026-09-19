-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Metadata diagnostics preserve bounded status fields and redact upstream credentials.
Dropped-entry diagnostics retain their existing detail limits.
-}
module Ecluse.Core.Server.Pipeline.DiagnosticsSpec (spec) where

import Data.Aeson (Value (Number), object, (.=))
import Data.Text qualified as T
import Katip (SimpleLogPayload, closeScribes)
import Katip.Monadic (KatipContextT, runKatipContextT)
import Test.Hspec

import Ecluse.Core.Package (
    InvalidEntry,
    InvalidEntryKind (InvalidDistTag, InvalidIndexFile, InvalidVersionManifest),
    mkInvalidEntry,
 )
import Ecluse.Core.Registry (UrlFormationError (EmptyBaseUrl, UnparseableUrl))
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataHttpFailure))
import Ecluse.Core.Server.Pipeline.Diagnostics (
    logDecodeFailure,
    logInvalidEntries,
    logMetadataFailure,
    logNameMismatch,
    logUpstreamUnformable,
 )
import Ecluse.Test.Log (captureStdout, jsonLogEnv)
import Ecluse.Test.Package (unscopedNpm)

-- | Pin log severity, status fields, and credential redaction.
spec :: Spec
spec = do
    metadataFailureSpec
    upstreamWarningSpec
    invalidEntriesSpec

upstreamWarningSpec :: Spec
upstreamWarningSpec = do
    describe "logDecodeFailure" $
        it "logs a WARNING tagged with this module and the package, naming the decode failure" $ do
            logged <- captureStdout $ do
                logEnv <- jsonLogEnv
                runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (logDecodeFailure (unscopedNpm "is-odd"))
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Server.Pipeline.Internal\""
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"is-odd\""
            logged `shouldSatisfy` T.isInfixOf "did not decode"

    describe "logNameMismatch" $
        it "logs a WARNING carrying both names and the origin when an upstream reports a different package" $ do
            -- No span is active here, so the line carries no @dd@ object. The serve path adds that
            -- correlation and is otherwise identical.
            logged <- captureStdout $ do
                logEnv <- jsonLogEnv
                runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (logNameMismatch (unscopedNpm "thing") "http://upstream.test" "other")
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Server.Pipeline.Internal\""
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"thing\""
            logged `shouldSatisfy` T.isInfixOf "\"upstreamName\":\"other\""
            logged `shouldSatisfy` T.isInfixOf "\"origin\":\"upstream.test:443\""
            logged `shouldSatisfy` T.isInfixOf "different package"

    describe "logUpstreamUnformable" $
        it "logs a WARNING naming the misconfigured origin and the URL fault, distinct from an outage" $ do
            logged <- captureStdout $ do
                logEnv <- jsonLogEnv
                runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (logUpstreamUnformable (unscopedNpm "is-odd") "http://upstream.test" EmptyBaseUrl)
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Server.Pipeline.Internal\""
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"is-odd\""
            logged `shouldSatisfy` T.isInfixOf "\"origin\":\"upstream.test:443\""
            logged `shouldSatisfy` T.isInfixOf "\"urlError\":\"EmptyBaseUrl\""
            logged `shouldSatisfy` T.isInfixOf "could not be formed"

    describe "logUpstreamUnformable (url minimisation)" $
        it "reduces the offending URL to its authority, dropping userinfo and query" $ do
            -- The URL a fault carries can be an upstream-supplied artifact location.
            -- That location can hold a credential in its userinfo or a signed query,
            -- so the rendered fault names the authority alone.
            let offending = UnparseableUrl "https://deploy:hunter2@upstream.test/base?token=abc"
            logged <- captureStdout $ do
                logEnv <- jsonLogEnv
                runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (logUpstreamUnformable (unscopedNpm "is-odd") "https://ops:s3cret@upstream.test/base?k=v" offending)
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"urlError\":\"UnparseableUrl upstream.test:443\""
            -- The origin field takes the same reduction on the same line, so it holds
            -- for every URL the payload names, not only the carried fault.
            logged `shouldSatisfy` T.isInfixOf "\"origin\":\"upstream.test:443\""
            logged `shouldSatisfy` (not . T.isInfixOf "hunter2")
            logged `shouldSatisfy` (not . T.isInfixOf "token=abc")
            logged `shouldSatisfy` (not . T.isInfixOf "s3cret")
            logged `shouldSatisfy` (not . T.isInfixOf "k=v")

metadataFailureSpec :: Spec
metadataFailureSpec = describe "logMetadataFailure" $ do
    for_ [(MetadataAbsent, 404, "Warning"), (MetadataHttpFailure 301, 301, "Warning"), (MetadataHttpFailure 400, 400, "Warning"), (MetadataHttpFailure 408, 408, "Error"), (MetadataHttpFailure 429, 429, "Error"), (MetadataHttpFailure 500, 500, "Error"), (MetadataHttpFailure 503, 503, "Error")] $ \(failure, code, severity) ->
        it ("logs " <> show failure <> " as " <> toString severity <> " without credentials") $ do
            logged <- runLog (logMetadataFailure (unscopedNpm "mix") "https://deploy:hunter2@registry.npmjs.org/path?sig=abc" failure)
            logged `shouldSatisfy` T.isInfixOf ("\"sev\":\"" <> severity <> "\"")
            logged `shouldSatisfy` T.isInfixOf ("\"status\":" <> show (code :: Int))
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"mix\""
            logged `shouldSatisfy` T.isInfixOf "\"upstream\":\"registry.npmjs.org:443\""
            let message = case failure of
                    MetadataAbsent -> "the upstream has no metadata for the requested package"
                    _ -> "the upstream refused the metadata read"
            logged `shouldSatisfy` T.isInfixOf message
            for_ ["hunter2", "deploy", "sig=abc", "/path"] $ \secret ->
                logged `shouldSatisfy` (not . T.isInfixOf secret)

    for_ [401, 403] $ \code ->
        it ("keeps access refusal HTTP " <> show code <> " at Warning") $ do
            logged <- runLog (logMetadataFailure (unscopedNpm "mix") upstream (MetadataAuthorisationFailure code))
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` T.isInfixOf "the upstream refused metadata access"

invalidEntriesSpec :: Spec
invalidEntriesSpec = describe "logInvalidEntries" $ do
    it "buckets the drop counts by kind, naming only the kinds seen" $ do
        logged <- runLog (logInvalidEntries (unscopedNpm "mix") upstream mixedDrops)
        logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
        logged `shouldSatisfy` T.isInfixOf "\"package\":\"mix\""
        logged `shouldSatisfy` T.isInfixOf "\"upstream\":\"registry.npmjs.org:443\""
        logged `shouldSatisfy` T.isInfixOf "\"version-manifest\":2"
        logged `shouldSatisfy` T.isInfixOf "\"dist-tag\":1"
        logged `shouldSatisfy` (not . T.isInfixOf "publish-time")

    it "buckets a second ecosystem's kind with no change to this renderer" $ do
        logged <- runLog (logInvalidEntries (unscopedNpm "mix") upstream [dropOf InvalidIndexFile (Number 1)])
        logged `shouldSatisfy` T.isInfixOf "\"index-file\":1"

    it "counts every dropped entry in the message" $ do
        logged <- runLog (logInvalidEntries (unscopedNpm "mix") upstream mixedDrops)
        logged `shouldSatisfy` T.isInfixOf "dropped 3 malformed entries"

    it "says entry, singular, for one drop" $ do
        logged <- runLog (logInvalidEntries (unscopedNpm "mix") upstream [dropOf InvalidDistTag (Number 5)])
        logged `shouldSatisfy` T.isInfixOf "dropped 1 malformed entry"

    it "renders each entry as kind, key, value, and reason" $ do
        logged <- runLog (logInvalidEntries (unscopedNpm "mix") upstream [dropOf InvalidDistTag (Number 5)])
        logged `shouldSatisfy` T.isInfixOf "dist-tag 2.0.0 = 5 (expected an object)"

    it "carries no credential from a URL-bearing dropped value" $ do
        -- The one line the drop record actually reaches. A credentialed dist.tarball inside a
        -- dropped version object must arrive here as an authority and nothing more.
        logged <- runLog (logInvalidEntries (unscopedNpm "mix") upstream [dropOf InvalidVersionManifest credentialedManifest])
        logged `shouldSatisfy` T.isInfixOf "registry.npmjs.org:443"
        logged `shouldSatisfy` (not . T.isInfixOf "hunter2")
        logged `shouldSatisfy` (not . T.isInfixOf "sig=abc")

runLog :: KatipContextT IO () -> IO Text
runLog action =
    captureStdout $ do
        logEnv <- jsonLogEnv
        runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty action
        void (closeScribes logEnv)

upstream :: Text
upstream = "https://registry.npmjs.org"

mixedDrops :: [InvalidEntry]
mixedDrops =
    [ dropOf InvalidVersionManifest (Number 1)
    , dropOf InvalidVersionManifest (Number 2)
    , dropOf InvalidDistTag (Number 5)
    ]

dropOf :: InvalidEntryKind -> Value -> InvalidEntry
dropOf kind value = mkInvalidEntry kind "2.0.0" value "expected an object"

credentialedManifest :: Value
credentialedManifest =
    object ["dist" .= object ["tarball" .= ("https://deploy:hunter2@registry.npmjs.org/x.tgz?sig=abc" :: Text)]]
