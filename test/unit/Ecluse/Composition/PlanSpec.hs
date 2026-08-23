-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.PlanSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (BootError (MemoryPlanOverrideUnsafe, MissingAdapter, QueueUrlUnrecognised))
import Ecluse.Composition.MemoryPlan (MemoryPlan (mpOverrideViolations, mpQueueMemoryMaxDepth))
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    MirrorRuntimePlan (MirrorWith, NoMirroring),
    memoryQueueBootWarning,
 )
import Ecluse.Composition.Plan (BootPlan (..), configDocumentPath, defaultConfigPath, resolveBootPlan)
import Ecluse.Composition.Support (expectConfig, overrideEnv, staticEnvVars, withoutMirrorTargetUrl, withoutQueueUrl)
import Ecluse.Config (Config, mountPostureLines, resolvedKeyProvenance)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Rts (EffectiveAxis (..), EffectiveRuntimePlan (..), Provenance (FromCgroup, FromRts))

spec :: Spec
spec = describe "resolveBootPlan" $ do
    it "orders every boot line into the one list both entry points emit" $ do
        -- The golden the acceptance criterion rests on: the boot logs this list and
        -- check-config prints it, so the two transcripts cannot diverge.
        config <- expectConfig staticEnvVars Nothing
        plan <- expectPlan staticEnvVars Nothing config noCeiling
        bpLines plan
            `shouldBe` ["Config document: none at /etc/ecluse/config.yaml (defaults and environment only)"]
                <> resolvedKeyProvenance staticEnvVars Nothing
                <> [ "runtime: private connection pool 256 (computed from file-descriptor limit 1024)"
                   , "runtime: public connection pool 128 (computed from file-descriptor limit 1024)"
                   , "runtime: serve admission 20 (computed from 2 capabilities)"
                   , "memory plan: response byte cap 12582912" <> fallbackClause
                   , "memory plan: request byte cap 26214400" <> fallbackClause
                   , "memory plan: cache byte bound 268435456" <> fallbackClause
                   , "memory plan: cache entry bound 1024" <> fallbackClause
                   , "memory plan: memory-queue depth 50000" <> fallbackClause
                   , "memory plan: mirror artifact byte cap 536870912" <> fallbackClause
                   , "mirror queue: sqs, https://sqs.us-east-1.amazonaws.com/123456789012/mirror (region us-east-1)"
                   ]
                <> mountPostureLines config
        bpWarnings plan `shouldBe` []

    it "decides the mirror runtime, the memory plan, and both connection pools" $ do
        config <- expectConfig staticEnvVars Nothing
        plan <- expectPlan staticEnvVars Nothing config noCeiling
        case bpMirrorRuntime plan of
            MirrorWith (SqsBackend _) -> pass
            other -> expectationFailure ("expected an SQS mirror runtime, got: " <> show other)
        bpPrivateConnections plan `shouldBe` 256
        bpPublicConnections plan `shouldBe` 128
        mpQueueMemoryMaxDepth (bpMemoryPlan plan) `shouldBe` 50000
        mpOverrideViolations (bpMemoryPlan plan) `shouldBe` []

    it "warns on the in-memory queue rollover and names the depth it built with" $ do
        let envVars = withoutQueueUrl staticEnvVars
        config <- expectConfig envVars Nothing
        plan <- expectPlan envVars Nothing config noCeiling
        bpMirrorRuntime plan `shouldBe` MirrorWith MemoryBackend
        bpLines plan `shouldSatisfy` elem "mirror queue: in-memory (depth 50000)"
        bpWarnings plan `shouldBe` [memoryQueueBootWarning]

    it "reports a disabled mirror runtime in one wording for both entry points" $ do
        config <- expectConfig serveOnlyEnvVars Nothing
        plan <- expectPlan serveOnlyEnvVars Nothing config noCeiling
        bpMirrorRuntime plan `shouldBe` NoMirroring
        bpLines plan
            `shouldSatisfy` elem "mirror runtime disabled: no mount mirrors, so no queue is built and no worker starts"
        bpWarnings plan `shouldBe` []

    it "names the document an explicit ECLUSE_CONFIG points at" $ do
        let envVars = overrideEnv "ECLUSE_CONFIG" "/srv/ecluse.yaml" staticEnvVars
            document = "server:\n  helpMessage: from the document\n"
        config <- expectConfig envVars (Just document)
        plan <- expectPlan envVars (Just document) config noCeiling
        listToMaybe (bpLines plan) `shouldBe` Just "Config document: /srv/ecluse.yaml"
        configDocumentPath staticEnvVars `shouldBe` defaultConfigPath

    describe "refusals" $ do
        it "refuses a structural composition error" $ do
            let envVars = overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "true" staticEnvVars
            config <- expectConfig envVars Nothing
            resolveBootPlan envVars Nothing config noCeiling fdLimit
                `shouldBe` Left [MissingAdapter PyPI]

        it "refuses a queue URL whose shape names no backend" $ do
            let envVars = overrideEnv "ECLUSE_QUEUE__URL" "https://queue.example.test/q" staticEnvVars
            config <- expectConfig envVars Nothing
            resolveBootPlan envVars Nothing config noCeiling fdLimit
                `shouldBe` Left [QueueUrlUnrecognised "https://queue.example.test/q"]

        it "refuses an explicit memory override the shed ladder cannot work around" $ do
            -- A 1 GiB explicit cache on a 256 MiB pod. The override-free plan fits, so
            -- the pin is the named cause.
            let envVars = overrideEnv "ECLUSE_CACHE__MAX_BYTES" "1073741824" serveOnlyEnvVars
            config <- expectConfig envVars Nothing
            case resolveBootPlan envVars Nothing config tightPod fdLimit of
                Left [MemoryPlanOverrideUnsafe violations] ->
                    violations `shouldSatisfy` any (T.isInfixOf "cache.maxBytes")
                other -> expectationFailure ("expected a refused override, got: " <> show other)

-- | Resolve the boot plan for a fixture, failing the test on a refusal.
expectPlan :: [(String, String)] -> Maybe ByteString -> Config -> EffectiveRuntimePlan -> IO BootPlan
expectPlan envVars docBlob config effective =
    either
        (\errs -> fail ("boot plan refused: " <> show errs))
        pure
        (resolveBootPlan envVars docBlob config effective fdLimit)

-- | staticEnvVars with the mirror target and its write token dropped: the mount serves only.
serveOnlyEnvVars :: [(String, String)]
serveOnlyEnvVars =
    withoutMirrorTargetUrl (filter ((/= "ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN") . fst) staticEnvVars)

-- | A pinned file-descriptor soft limit, so both pool sizings are deterministic.
fdLimit :: Int
fdLimit = 1024

{- | A posture with no heap-ceiling datapoint, so the memory plan renders its shipped
fallbacks and every number in the golden is fixed.
-}
noCeiling :: EffectiveRuntimePlan
noCeiling =
    EffectiveRuntimePlan
        { erpCapabilities = EffectiveAxis{axDesired = 2, axObserved = 2, axProvenance = FromRts}
        , erpMaxHeapBytes = EffectiveAxis{axDesired = Nothing, axObserved = Nothing, axProvenance = FromRts}
        , erpAllocAreaBytes = 4 * mib
        , erpNurseryChunkBytes = Nothing
        , erpContainerMemoryBytes = Nothing
        }

-- | A 256 MiB pod on four capabilities: the computed tenants shed to fit, and nothing refuses.
tightPod :: EffectiveRuntimePlan
tightPod =
    noCeiling
        { erpCapabilities = EffectiveAxis{axDesired = 4, axObserved = 4, axProvenance = FromCgroup}
        , erpMaxHeapBytes =
            EffectiveAxis{axDesired = Just (256 * mib), axObserved = Just (256 * mib), axProvenance = FromCgroup}
        }

-- | The provenance clause every memory-plan line carries with no heap-ceiling datapoint.
fallbackClause :: Text
fallbackClause = " (built-in default; no heap-ceiling datapoint)"

mib :: Int
mib = 1024 * 1024
