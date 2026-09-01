-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.PlanSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (
    BootError (MemoryPlanOverrideUnsafe, MissingAdapter, QueueUrlUnrecognised, SplitRoleNeedsDurableQueue),
 )
import Ecluse.Composition.MemoryPlan (MemoryPlan (mpOverrideViolations, mpQueueMemoryMaxDepth))
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    MirrorRuntimePlan (MirrorWith, NoMirroring),
    memoryQueueBootWarning,
 )
import Ecluse.Composition.Plan (BootPlan (..), configDocumentPath, defaultConfigPath, resolveBootPlan)
import Ecluse.Composition.Support (expectConfig, fdLimit, noCeiling, overrideEnv, staticEnvVars, withoutMirrorTargetUrl, withoutQueueUrl)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline, BootWithoutPipeline), MirrorRole (ServeOnly))
import Ecluse.Config (Config, mountPostureLines, resolvedKeyProvenance)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Rts (EffectiveAxis (..), EffectiveRuntimePlan (..), Provenance (FromCgroup))

spec :: Spec
spec = describe "resolveBootPlan" $ do
    it "orders the plan's lines into the one list both entry points emit" $ do
        -- The golden the acceptance criterion rests on: the boot logs this list and
        -- check-config prints it, so the two transcripts cannot diverge.
        config <- expectConfig staticEnvVars Nothing
        plan <- expectPlan staticEnvVars Nothing config noCeiling
        bpLines plan
            `shouldBe` [ "runtime: private connection pool 256 (computed from file-descriptor limit 1024)"
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

    it "returns the preamble on the refusing path as well as the succeeding one" $ do
        -- A refusal that names a config key stays traceable to the layer that set it.
        let refusingEnv = overrideEnv "ECLUSE_QUEUE__URL" "https://queue.example.test/q" staticEnvVars
        ok <- expectConfig staticEnvVars Nothing
        refused <- expectConfig refusingEnv Nothing
        let (okPreamble, okPlan) = resolveBootPlan BootWithoutPipeline staticEnvVars Nothing ok noCeiling fdLimit
            (refusedPreamble, refusedPlan) = resolveBootPlan BootWithoutPipeline refusingEnv Nothing refused noCeiling fdLimit
        okPreamble `shouldBe` absentDocumentLine : resolvedKeyProvenance staticEnvVars Nothing
        refusedPreamble `shouldBe` absentDocumentLine : resolvedKeyProvenance refusingEnv Nothing
        void refusedPlan `shouldSatisfy` isLeft
        -- The plan's own lines never repeat the preamble, so no line has two emission sites.
        fmap (any (`elem` okPreamble) . bpLines) okPlan `shouldBe` Right False

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
        let (preamble, _) = resolveBootPlan BootWithoutPipeline envVars (Just document) config noCeiling fdLimit
        listToMaybe preamble `shouldBe` Just "Config document: /srv/ecluse.yaml"
        configDocumentPath staticEnvVars `shouldBe` defaultConfigPath

    it "trims the surrounding whitespace an ECLUSE_CONFIG value carries" $
        configDocumentPath (overrideEnv "ECLUSE_CONFIG" "  /etc/x.yaml  " staticEnvVars)
            `shouldBe` "/etc/x.yaml"

    it "falls back to the default path when ECLUSE_CONFIG is all whitespace" $
        configDocumentPath (overrideEnv "ECLUSE_CONFIG" "   " staticEnvVars)
            `shouldBe` defaultConfigPath

    describe "refusals" $ do
        it "refuses a structural composition error" $ do
            let envVars = overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "true" staticEnvVars
            config <- expectConfig envVars Nothing
            refusalsOf (resolveBootPlan BootWithoutPipeline envVars Nothing config noCeiling fdLimit)
                `shouldBe` Left [MissingAdapter PyPI]

        it "refuses a queue URL whose shape names no backend" $ do
            let envVars = overrideEnv "ECLUSE_QUEUE__URL" "https://queue.example.test/q" staticEnvVars
            config <- expectConfig envVars Nothing
            refusalsOf (resolveBootPlan BootWithoutPipeline envVars Nothing config noCeiling fdLimit)
                `shouldBe` Left [QueueUrlUnrecognised "https://queue.example.test/q"]

        it "refuses an explicit memory override the shed ladder cannot work around" $ do
            -- A 1 GiB explicit cache on a 256 MiB pod. The override-free plan fits, so
            -- the pin is the named cause.
            let envVars = overrideEnv "ECLUSE_CACHE__MAX_BYTES" "1073741824" serveOnlyEnvVars
            config <- expectConfig envVars Nothing
            case refusalsOf (resolveBootPlan BootWithoutPipeline envVars Nothing config tightPod fdLimit) of
                Left [MemoryPlanOverrideUnsafe violations] ->
                    violations `shouldSatisfy` any (T.isInfixOf "cache.maxBytes")
                other -> expectationFailure ("expected a refused override, got: " <> show other)

        it "refuses --no-worker over the in-memory queue rather than planning the role" $ do
            -- The config is otherwise complete, so nothing else would refuse: dropping the role
            -- guard would let this plan, and then boot, a runtime whose jobs nothing consumes.
            let envVars = withoutQueueUrl staticEnvVars
            config <- expectConfig envVars Nothing
            refusalsOf (resolveBootPlan (BootMirrorPipeline ServeOnly) envVars Nothing config noCeiling fdLimit)
                `shouldBe` Left [SplitRoleNeedsDurableQueue "ecluse proxy --no-worker"]

{- | A plan resolution reduced to its verdict. 'BootPlan' carries the cleared adapters, which are
records of functions, so the refusal is what an assertion compares.
-}
refusalsOf :: ([Text], Either [BootError] BootPlan) -> Either [BootError] ()
refusalsOf = void . snd

-- | Resolve the boot plan for a fixture, failing the test on a refusal.
expectPlan :: [(String, String)] -> Maybe ByteString -> Config -> EffectiveRuntimePlan -> IO BootPlan
expectPlan envVars docBlob config effective =
    either
        (\errs -> fail ("boot plan refused: " <> show errs))
        pure
        (snd (resolveBootPlan BootWithoutPipeline envVars docBlob config effective fdLimit))

-- | staticEnvVars with the mirror target and its write token dropped: the mount serves only.
serveOnlyEnvVars :: [(String, String)]
serveOnlyEnvVars =
    withoutMirrorTargetUrl (filter ((/= "ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN") . fst) staticEnvVars)

-- | The preamble's first line when no document exists at the default path.
absentDocumentLine :: Text
absentDocumentLine = "Config document: none at /etc/ecluse/config.yaml (defaults and environment only)"

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
