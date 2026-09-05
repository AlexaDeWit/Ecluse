-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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
import Ecluse.Core.Server.Pipeline.Diagnostics (logInvalidEntries)
import Ecluse.Test.Log (captureStdout, jsonLogEnv)
import Ecluse.Test.Package (unscopedNpm)

{- | The operator-facing dropped-entry line. It renders a value an upstream supplied, so it is
the last place a credential could surface.
-}
spec :: Spec
spec = invalidEntriesSpec

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

-- | Run one log action through a JSON scribe and hand back what it wrote.
runLog :: KatipContextT IO () -> IO Text
runLog action =
    captureStdout $ do
        logEnv <- jsonLogEnv
        runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty action
        void (closeScribes logEnv)

upstream :: Text
upstream = "https://registry.npmjs.org"

-- | Three drops across two kinds, so the bucketing has something to distinguish.
mixedDrops :: [InvalidEntry]
mixedDrops =
    [ dropOf InvalidVersionManifest (Number 1)
    , dropOf InvalidVersionManifest (Number 2)
    , dropOf InvalidDistTag (Number 5)
    ]

-- | Record a drop of the given kind and value, holding the key and reason fixed.
dropOf :: InvalidEntryKind -> Value -> InvalidEntry
dropOf kind value = mkInvalidEntry kind "2.0.0" value "expected an object"

-- | A dropped version object whose @dist.tarball@ carries a credential and a signature.
credentialedManifest :: Value
credentialedManifest =
    object ["dist" .= object ["tarball" .= ("https://deploy:hunter2@registry.npmjs.org/x.tgz?sig=abc" :: Text)]]
