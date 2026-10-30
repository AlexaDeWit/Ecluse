-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

-- | Loader and mount-resolution contracts against the shipped defaults.
module Ecluse.ConfigSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (addUTCTime, nominalDay)
import Data.Yaml qualified as Yaml
import Test.Hspec

import Ecluse.Composition.Support (codeArtifactEnvVars, expectConfig, fixedNow, npmMountDoc, pubUrlEnv, staticEnvVars)
import Ecluse.Config (
    AppConfig (cfgQueue),
    Config (configApp, configMounts),
    ConfigError (MountMissingPrivateUpstream, ParseError, PolicyErrors, PublicUrlRequired, SharedPolicyErrors),
    Mount (mountPolicy, mountRegistries),
    MountMode (Mirrored, ServeOnly),
    MountRegistries (regMode),
    PolicyError (MalformedRule, MissingRuleType, SuppressUnknownRule),
    QueueSettings (qsMaxReceiveCount),
    RulePolicy (..),
    advisoryAgeLines,
    advisoryEpssLines,
    decodeDefaultConfig,
    loadConfig,
    mountAdvisoryDenials,
    mountDatabaseRequirement,
    mountEpssRequirement,
    mountPostureLines,
    renderConfigError,
    resolvedKeyProvenance,
    sourceDefaultNames,
 )
import Ecluse.Config.DefaultConfig (defaultConfigBytes)
import Ecluse.Config.Resolve (deepMerge)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI))
import Ecluse.Core.Osv.Schema (EpssRequirement (..))
import Ecluse.Core.Package (DependencySources (RegistryDependencies))
import Ecluse.Core.Queue (DeliveryBudget (DeliveryBudget), defaultDeliveryBudget)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (
    AllowIfRemediatesCveParams (..),
    Decision,
    DifferentialLimits (..),
    EvalContext (EvalContext),
    Fact (Known),
    FailureAlignment (..),
    PrecededRule (..),
    RemainingAdvisories (..),
    Rule (AllowIfRemediatesCve, DenyIfKnownMalicious),
    RuleEvidence (evDependencySources, evPublishedAt),
    RuleReach (AdmissionAndRevocation, AdmissionOnly),
    defaultDifferentialLimits,
    identityEvidence,
    revocationRules,
    ruleName,
 )
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Server.Readiness (DatabaseRequirement (DatabaseOptional, DatabaseRequired))
import Ecluse.Test.Package (pypiVersion, thingName, unscopedPyPI, v1_0_0)
import Ecluse.Test.Rules (admittedBy, blockedBy, evalRules, inertRuleDeps, withInstallScripts)
import Ecluse.Test.Support (expectRight)

-- | Configuration loading and operator diagnostics.
spec :: Spec
spec = do
    maliciousDefaultSpec
    remediationDefaultSpec
    describe "the embedded default configuration" $ do
        it "loads as a valid, self-contained backbone with no operator overlay" $
            loadConfig [] Nothing `shouldSatisfy` isRight

        it "leaves the Cargo template dormant and adds no archive-evidence default" $ do
            defaultConfig <- expectConfig [] Nothing
            Map.member Cargo (configMounts defaultConfig) `shouldBe` False
            enabled <- configFor "{\"mounts\":{\"cargo\":{\"enabled\":true}}}"
            fmap (map (ruleName . prRule) . mountPolicy) (Map.lookup Cargo (configMounts enabled))
                `shouldBe` Just ["AllowIfOlderThan"]

        it "ships exactly the expected shared baseline rules under their default names" $ do
            (_, catalogue) <- expectRight (decodeDefaultConfig defaultConfigBytes)
            Map.keys (policyRules catalogue) `shouldMatchList` ["deny-known-malicious", "min-age", "remediation-fast-track"]
            Map.keys (Map.withoutKeys (policyRules catalogue) sourceDefaultNames) `shouldMatchList` ["min-age"]

        it "applies every shared shipped rule at both phases" $ do
            (_, baseline) <- expectRight (decodeDefaultConfig defaultConfigBytes)
            map ruleReach (Map.elems (policyRules baseline))
                `shouldBe` [AdmissionAndRevocation, AdmissionAndRevocation, AdmissionAndRevocation]

        it "returns a typed YAML refusal for malformed default bytes" $
            decodeDefaultConfig "rules: [" `shouldSatisfy` parseRefusal "config/default.yaml is invalid YAML: "

        it "parses default settings before resolving default rules" $ do
            bytes <- defaultsWith (object ["server" .= object ["port" .= (-1 :: Int)], "rules" .= object ["unresolved" .= object ["ageSeconds" .= (60 :: Int)]]])
            decodeDefaultConfig bytes
                `shouldBe` Left [ParseError "Configuration parse error: Error in $.server.port: server.port must be a port in 0..65535 (0 = OS-assigned), got -1"]

        it "returns a typed parse refusal for malformed default rules" $ do
            bytes <- defaultsWith (object ["rules" .= ([] :: [Value])])
            decodeDefaultConfig bytes `shouldSatisfy` parseRefusal "Rules parse error: Error in $.rules: "

        it "resolves default rules against the empty policy" $ do
            bytes <- defaultsWith (object ["rules" .= object ["unresolved" .= object ["ageSeconds" .= (60 :: Int)]]])
            decodeDefaultConfig bytes `shouldBe` Left [PolicyErrors [MissingRuleType "unresolved"]]

        it "pins the shipped redelivery budget to the one a directly-built backend holds" $
            -- A drift changes when deployments and test doubles retire poison messages.
            case (loadConfig [] Nothing, defaultDeliveryBudget) of
                (Right cfg, DeliveryBudget budget) ->
                    qsMaxReceiveCount (cfgQueue (configApp cfg)) `shouldBe` budget
                (Left errs, _) -> expectationFailure ("the embedded defaults failed to load: " <> show errs)

    describe "the shipped npm mount policy" $ do
        it "activates no mount without an operator declaration" $
            fmap (Map.keys . configMounts) (loadConfig [] Nothing) `shouldBe` Right []

        for_ [("npm-only", "{\"mounts\":{\"npm\":{\"enabled\":true}}}"), ("both mounts", "{\"mounts\":{\"npm\":{\"enabled\":true},\"pypi\":{\"enabled\":true}}}")] $ \(label, doc) ->
            it ("denies install scripts on the active npm mount with " <> label) $ do
                cfg <- configFor doc
                fmap (map (ruleName . prRule) . mountPolicy) (Map.lookup Npm (configMounts cfg))
                    `shouldBe` Just ["DenyExternalDependencies", "DenyInstallTimeExecution", "AllowIfOlderThan"]
                decideOn cfg Npm (scriptEvidence Npm) >>= (\decision -> blockedBy decision `shouldBe` Just "DenyInstallTimeExecution")
                fmap (map (ruleName . prRule) . revocationRules . mountPolicy) (Map.lookup Npm (configMounts cfg))
                    `shouldBe` Just ["AllowIfOlderThan"]

        for_ [("PyPI-only", "{\"mounts\":{\"pypi\":{\"enabled\":true}}}"), ("both mounts", "{\"mounts\":{\"npm\":{\"enabled\":true},\"pypi\":{\"enabled\":true}}}")] $ \(label, doc) ->
            it ("admits an old PyPI release that runs code on install with " <> label) $ do
                cfg <- configFor doc
                fmap (map (ruleName . prRule) . mountPolicy) (Map.lookup PyPI (configMounts cfg))
                    `shouldBe` Just ["AllowIfOlderThan"]
                decideOn cfg PyPI (scriptEvidence PyPI) >>= (\decision -> admittedBy decision `shouldBe` Just "AllowIfOlderThan")

        it "disables the npm default in the document or environment" $
            for_ [configFor "{\"mounts\":{\"npm\":{\"rules\":{\"deny-scripts\":{\"enabled\":false}}}}}", expectConfig (pubUrlEnv <> [("ECLUSE_MOUNTS__NPM__RULES", "{\"deny-scripts\":{\"enabled\":false}}")]) Nothing] $ \load -> do
                cfg <- load
                Map.map limitedRules (configMounts cfg) `shouldBe` Map.singleton Npm ["DenyExternalDependencies"]
                decideOn cfg Npm (scriptEvidence Npm) >>= (\decision -> admittedBy decision `shouldBe` Just "AllowIfOlderThan")

        it "admits a named npm package with a pin above the deny's precedence" $ do
            cfg <- configFor "{\"mounts\":{\"npm\":{\"rules\":{\"pin\":{\"type\":\"AllowByIdentity\",\"identity\":\"thing@1.0.0\",\"precedence\":301}}}}}"
            decideOn cfg Npm (scriptEvidence Npm) >>= (\decision -> admittedBy decision `shouldBe` Just "AllowByIdentity")

        it "widens the shipped npm deny from the operator layer" $ do
            cfg <- configFor "{\"mounts\":{\"npm\":{\"rules\":{\"deny-scripts\":{\"appliesTo\":[\"admission\",\"revocation\"]}}}}}"
            Map.map limitedRules (configMounts cfg) `shouldBe` Map.singleton Npm ["DenyExternalDependencies"]
            fmap (map (ruleName . prRule) . revocationRules . mountPolicy) (Map.lookup Npm (configMounts cfg))
                `shouldBe` Just ["DenyInstallTimeExecution", "AllowIfOlderThan"]

        it "keeps the shipped mount layer above a shared entry of the same name" $ do
            cfg <- configFor "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":[\"admission\",\"revocation\"]}},\"mounts\":{\"npm\":{\"enabled\":true}}}"
            Map.map limitedRules (configMounts cfg) `shouldBe` Map.singleton Npm ["DenyExternalDependencies", "DenyInstallTimeExecution"]

        it "refuses disabling the npm-only name in the shared list or the PyPI mount" $ do
            loadConfig [] (Just "{\"rules\":{\"deny-scripts\":{\"enabled\":false}}}")
                `shouldBe` Left [SharedPolicyErrors [SuppressUnknownRule "deny-scripts"]]
            loadConfig pubUrlEnv (Just "{\"mounts\":{\"pypi\":{\"rules\":{\"deny-scripts\":{\"enabled\":false}}}}}")
                `shouldBe` Left [PolicyErrors [SuppressUnknownRule "deny-scripts"]]

        it "names the npm mount remedy when the shared layer disables its default" $
            for_ [loadConfig [] (Just "{\"rules\":{\"deny-scripts\":{\"enabled\":false}}}"), loadConfig [("ECLUSE_RULES", "{\"deny-scripts\":{\"enabled\":false}}")] Nothing] $ \result ->
                first (map renderConfigError) result
                    `shouldBe` Left ["rule \"deny-scripts\" disables a rule that no default defines. Disable the shipped npm default with enabled: false under mounts.npm.rules (ECLUSE_MOUNTS__NPM__RULES) instead.\n"]

        it "keeps generic refusal text for other absent rules beside the npm remedy" $
            first (map renderConfigError) (loadConfig [] (Just "{\"rules\":{\"absent\":{\"enabled\":false},\"deny-scripts\":{\"enabled\":false}}}"))
                `shouldBe` Left ["rule \"absent\" disables a rule that no default defines\nrule \"deny-scripts\" disables a rule that no default defines. Disable the shipped npm default with enabled: false under mounts.npm.rules (ECLUSE_MOUNTS__NPM__RULES) instead.\n"]

        it "keeps the generic typed refusal for an unrelated absent shared rule" $
            loadConfig [] (Just "{\"rules\":{\"absent\":{\"enabled\":false}}}")
                `shouldBe` Left [PolicyErrors [SuppressUnknownRule "absent"]]

        it "refuses a different type under the shipped npm name" $
            loadConfig pubUrlEnv (Just "{\"mounts\":{\"npm\":{\"rules\":{\"deny-scripts\":{\"type\":\"DenyByIdentity\",\"identity\":\"thing\"}}}}}")
                `shouldBe` Left [PolicyErrors [MalformedRule "deny-scripts" "\"type\" \"DenyByIdentity\" does not match the default rule it patches"]]

        it "disables the shipped dependency deny under the npm mount, keeping the script deny" $ do
            cfg <- configFor "{\"mounts\":{\"npm\":{\"rules\":{\"deny-external-dependencies\":{\"enabled\":false}}}}}"
            Map.map limitedRules (configMounts cfg) `shouldBe` Map.singleton Npm ["DenyInstallTimeExecution"]
            fmap (map (ruleName . prRule) . mountPolicy) (Map.lookup Npm (configMounts cfg))
                `shouldBe` Just ["DenyInstallTimeExecution", "AllowIfOlderThan"]

        it "refuses disabling the shipped dependency deny in the shared list, naming the npm remedy" $ do
            loadConfig [] (Just "{\"rules\":{\"deny-external-dependencies\":{\"enabled\":false}}}")
                `shouldBe` Left [SharedPolicyErrors [SuppressUnknownRule "deny-external-dependencies"]]
            first (map renderConfigError) (loadConfig [("ECLUSE_RULES", "{\"deny-external-dependencies\":{\"enabled\":false}}")] Nothing)
                `shouldBe` Left ["rule \"deny-external-dependencies\" disables a rule that no default defines. Disable the shipped npm default with enabled: false under mounts.npm.rules (ECLUSE_MOUNTS__NPM__RULES) instead.\n"]

        it "refuses a different type under the shipped dependency deny's name" $
            loadConfig pubUrlEnv (Just "{\"mounts\":{\"npm\":{\"rules\":{\"deny-external-dependencies\":{\"type\":\"DenyInstallTimeExecution\"}}}}}")
                `shouldBe` Left [PolicyErrors [MalformedRule "deny-external-dependencies" "\"type\" \"DenyInstallTimeExecution\" does not match the default rule it patches"]]

    describe "configuration type error paths" $ do
        forM_
            [ ("server.port", "ECLUSE_SERVER__PORT", "{\"server\":{\"port\":\"bad\"}}")
            , ("server.shutdownDrainTimeout", "ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT", "{\"server\":{\"shutdownDrainTimeout\":\"bad\"}}")
            , ("server.metadataGzip", "ECLUSE_SERVER__METADATA_GZIP", "{\"server\":{\"metadataGzip\":\"bad\"}}")
            , ("advisories.recordSourcePath", "ECLUSE_ADVISORIES__RECORD_SOURCE_PATH", "{\"advisories\":{\"recordSourcePath\":\"bad\"}}")
            , ("limits.maxVersionCount", "ECLUSE_LIMITS__MAX_VERSION_COUNT", "{\"limits\":{\"maxVersionCount\":\"bad\"}}")
            , ("limits.progressWindow", "ECLUSE_LIMITS__PROGRESS_WINDOW", "{\"limits\":{\"progressWindow\":\"bad\"}}")
            , ("limits.minProgressBytes", "ECLUSE_LIMITS__MIN_PROGRESS_BYTES", "{\"limits\":{\"minProgressBytes\":\"bad\"}}")
            , ("runtime.cores", "ECLUSE_RUNTIME__CORES", "{\"runtime\":{\"cores\":\"bad\"}}")
            ]
            $ \(field, envKey, doc) ->
                it ("names " <> toString field <> " through document and environment loads") $
                    forM_ [loadConfig [] (Just doc), loadConfig [(envKey, "bad")] Nothing] $ \result ->
                        first (any (T.isInfixOf ("$." <> field) . renderConfigError)) result
                            `shouldBe` Left True

        it "names a nested group with the wrong type" $
            first
                (any (T.isInfixOf "$.advisories.quietTime" . renderConfigError))
                (loadConfig [] (Just "{\"advisories\":{\"quietTime\":false}}"))
                `shouldBe` Left True

        it "omits supplied values from typed credential errors" $
            case loadConfig [("ECLUSE_SERVER", "{\"authToken\":[\"credential-sentinel\"]}")] Nothing of
                Left errs -> do
                    let messages = map renderConfigError errs
                    messages `shouldSatisfy` any (T.isInfixOf "server.authToken")
                    messages `shouldSatisfy` (not . any (T.isInfixOf "credential-sentinel"))
                Right _ -> expectationFailure "expected a credential type error"

    describe "mount modes (mirroring derived from the declared target)" $ do
        it "resolves a declared mirrorTarget to a mirrored mount" $ do
            cfg <- configFor (registryMountDoc [("privateUpstream", "https://priv.example.test"), ("mirrorTarget", "https://mirror.example.test")])
            modeOf cfg `shouldSatisfy` \case Just (Mirrored _) -> True; _ -> False
            mountPostureLines cfg `shouldSatisfy` any (T.isInfixOf "mirrored")

        it "resolves an absent mirrorTarget to a serve-only mount over the private merge" $ do
            cfg <- configFor (registryMountDoc [("privateUpstream", "https://priv.example.test")])
            modeOf cfg `shouldBe` (Just . ServeOnly . rightToMaybe . mkRegistryUrl) "https://priv.example.test"
            mountPostureLines cfg `shouldSatisfy` any (T.isInfixOf "serve-only")

        it "resolves enabled alone to the serve-only pure public gate" $ do
            cfg <- configFor "{\"mounts\":{\"npm\":{\"enabled\":true}}}"
            modeOf cfg `shouldBe` Just (ServeOnly Nothing)
            mountPostureLines cfg `shouldSatisfy` any (T.isInfixOf "pure public gate")

        it "switches a declared mount off under enabled: false (keys kept, nothing served)" $ do
            cfg <-
                configFor
                    "{\"mounts\":{\"npm\":{\"enabled\":false,\
                    \\"privateUpstream\":{\"registry\":{\"url\":\"https://priv.example.test\"}}}}}"
            Map.keys (configMounts cfg) `shouldBe` []

        it "requires the private upstream on a mirrored mount (the mirror must read back)" $
            loadConfig pubUrlEnv (Just (registryMountDoc [("mirrorTarget", "https://mirror.example.test")]))
                `shouldBe` Left [MountMissingPrivateUpstream Npm]

        it "requires server.publicUrl once any mount is active, aggregated with the mount errors" $ do
            -- Écluse rewrites served tarball URLs against the proxy's own base URL. A
            -- missing base URL fails here, not client by client at install time.
            loadConfig [] (Just "{\"mounts\":{\"npm\":{\"enabled\":true}}}")
                `shouldBe` Left [PublicUrlRequired]
            loadConfig [] (Just (registryMountDoc [("mirrorTarget", "https://mirror.example.test")]))
                `shouldBe` Left [PublicUrlRequired, MountMissingPrivateUpstream Npm]

        it "prints the Verdaccio store's declared deletion consent in the mount posture" $ do
            permitted <- configFor (verdaccioMountDoc "\"permitDeletion\":true,")
            mountPostureLines permitted `shouldSatisfy` any (T.isInfixOf "which permits deletion")
            withheld <- configFor (verdaccioMountDoc "")
            mountPostureLines withheld `shouldSatisfy` any (T.isInfixOf "which withholds deletion")

    describe "maintenance client posture" $ do
        it "appends a live-environment notice for CodeArtifact and Verdaccio" $ do
            codeArtifact <- expectConfig codeArtifactEnvVars Nothing
            verdaccio <- configFor (verdaccioMountDoc "")
            forM_ [codeArtifact, verdaccio] $ \cfg ->
                drop 1 (mountPostureLines cfg)
                    `shouldBe` [ "mount \"npm\": the store maintenance client is built at boot against the live environment. check-config does not attempt this build."
                               , upstreamProbeNotice
                               ]

        it "adds no maintenance notice for registry targets or serve-only mounts" $ do
            -- A mount with a private upstream carries the probe notice a boot makes and a
            -- checker does not, and a pure public gate carries its posture line alone.
            registry <- configFor (registryMountDoc [("privateUpstream", "https://priv.example.test"), ("mirrorTarget", "https://mirror.example.test")])
            private <- configFor (registryMountDoc [("privateUpstream", "https://priv.example.test")])
            public <- configFor "{\"mounts\":{\"npm\":{\"enabled\":true}}}"
            forM_ [registry, private] $ \cfg ->
                drop 1 (mountPostureLines cfg) `shouldBe` [upstreamProbeNotice]
            length (mountPostureLines public) `shouldBe` 1

    describe "the phases a mount's rules apply at" $ do
        it "keeps shared rules at both phases while the npm default applies only at admission" $ do
            cfg <- configFor "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\"}},\"mounts\":{\"npm\":{\"enabled\":true},\"pypi\":{\"enabled\":true}}}"
            Map.map (map ruleReach . mountPolicy) (configMounts cfg)
                `shouldBe` Map.fromList [(Npm, [AdmissionOnly, AdmissionOnly, AdmissionAndRevocation]), (PyPI, replicate 2 AdmissionAndRevocation)]

        it "limits a shared deny to admission only on the mount that says so" $ do
            cfg <- configFor "{\"rules\":{\"withdrawn\":{\"type\":\"DenyByIdentity\",\"identity\":\"thing\"}},\"mounts\":{\"npm\":{\"enabled\":true},\"pypi\":{\"enabled\":true,\"rules\":{\"withdrawn\":{\"appliesTo\":[\"admission\"]}}}}}"
            Map.map limitedRules (configMounts cfg) `shouldBe` Map.fromList [(Npm, ["DenyExternalDependencies", "DenyInstallTimeExecution"]), (PyPI, ["DenyByIdentity"])]

        it "widens on one mount a rule the layer below limits to admission" $ do
            cfg <- configFor "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":[\"admission\"]}},\"mounts\":{\"npm\":{\"enabled\":true,\"rules\":{\"deny-scripts\":{\"appliesTo\":[\"admission\",\"revocation\"]}}},\"pypi\":{\"enabled\":true}}}"
            Map.map limitedRules (configMounts cfg) `shouldBe` Map.fromList [(Npm, ["DenyExternalDependencies"]), (PyPI, ["DenyInstallTimeExecution"])]

        it "replaces the document's list whole from the environment" $ do
            let doc = "{\"mounts\":{\"npm\":{\"enabled\":true,\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":[\"admission\"]}}}}}"
            limited <- configFor doc
            widened <- expectConfig (pubUrlEnv <> [("ECLUSE_MOUNTS__NPM__RULES", "{\"deny-scripts\":{\"appliesTo\":[\"admission\",\"revocation\"]}}")]) (Just doc)
            Map.map limitedRules (configMounts limited) `shouldBe` Map.singleton Npm ["DenyExternalDependencies", "DenyInstallTimeExecution"]
            Map.map limitedRules (configMounts widened) `shouldBe` Map.singleton Npm ["DenyExternalDependencies"]

        it "switches off, from the environment, a shipped rule whose phases the document states" $ do
            -- The two layers merge into one entry, where enabled: false decides and appliesTo is not read.
            cfg <-
                expectConfig
                    (pubUrlEnv <> [("ECLUSE_RULES", "{\"min-age\":{\"enabled\":false}}")])
                    (Just "{\"rules\":{\"min-age\":{\"appliesTo\":[\"admission\",\"revocation\"]}},\"mounts\":{\"npm\":{\"enabled\":true}}}")
            Map.map (map (ruleName . prRule) . mountPolicy) (configMounts cfg)
                `shouldBe` Map.singleton Npm ["DenyExternalDependencies", "DenyInstallTimeExecution"]

        it "refuses an ambiguous setting on a mount's own rules, for npm and for PyPI" $
            for_ ["npm", "pypi"] $ \mount ->
                loadConfig pubUrlEnv (Just ("{\"mounts\":{\"" <> mount <> "\":{\"enabled\":true,\"rules\":{\"min-age\":{\"appliesTo\":[\"revocation\"]}}}}}"))
                    `shouldBe` Left [PolicyErrors [MalformedRule "min-age" "\"appliesTo\" must include \"admission\". A rule that applied at revocation alone would delete copies of versions the gate still admits"]]

        it "refuses appliesTo written with no value, in the document and in the environment" $ do
            let refused = first (any (T.isInfixOf "rule.appliesTo is written with no value" . renderConfigError))
                emptyInDocument = "rules:\n  deny-scripts:\n    type: DenyInstallTimeExecution\n    appliesTo:\nmounts:\n  npm:\n    enabled: true\n"
                nullInEnvironment = [("ECLUSE_MOUNTS__NPM__ENABLED", "true"), ("ECLUSE_RULES", "{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":null}}")]
            refused (loadConfig pubUrlEnv (Just emptyInDocument)) `shouldBe` Left True
            refused (loadConfig (pubUrlEnv <> nullInEnvironment) Nothing) `shouldBe` Left True

        it "refuses an empty string inside the list, in the document and in the environment" $ do
            let refusal = Left [PolicyErrors [MalformedRule "deny-scripts" "\"appliesTo\" names unknown phase \"\". The phases are \"admission\" and \"revocation\""]]
                inDocument = "rules:\n  deny-scripts:\n    type: DenyInstallTimeExecution\n    appliesTo: [\"\"]\nmounts:\n  npm:\n    enabled: true\n"
                inEnvironment = [("ECLUSE_MOUNTS__NPM__ENABLED", "true"), ("ECLUSE_RULES", "{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":[\"\"]}}")]
            loadConfig pubUrlEnv (Just inDocument) `shouldBe` refusal
            loadConfig (pubUrlEnv <> inEnvironment) Nothing `shouldBe` refusal

        it "names the setting and its layer in the provenance lines" $
            provenanceOf [] (Just "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":[\"admission\"]}}}")
                >>= (`shouldSatisfy` elem "config: rules.deny-scripts.appliesTo = [\"admission\"] (document)")

    describe "the advisory push-age limit reported at boot" $ do
        it "derives six days from the shipped seven-day quarantine, naming the rule" $ do
            cfg <- expectConfig (pubUrlEnv <> advisoryStoreEnv) (Just privateMountDoc)
            advisoryAgeLines cfg
                `shouldBe` ["mount \"npm\": advisory denial refuses on an advisory push older than 6 days, derived a day ahead of this mount's earliest AllowIfOlderThan quarantine of 7 days"]

        it "names an explicit maximum as its own basis" $ do
            cfg <- expectConfig (pubUrlEnv <> advisoryStoreEnv <> [("ECLUSE_ADVISORIES__MAX_AGE_SECONDS", "3600")]) (Just privateMountDoc)
            advisoryAgeLines cfg `shouldSatisfy` any (T.isInfixOf "1 hour, set by advisories.maxAgeSeconds")

        it "holds the floor under a two-day quarantine" $ do
            cfg <- expectConfig (pubUrlEnv <> advisoryStoreEnv <> [("ECLUSE_RULES", "{\"min-age\":{\"ageSeconds\":172800}}")]) (Just privateMountDoc)
            advisoryAgeLines cfg `shouldSatisfy` any (T.isInfixOf "3 days, the shipped floor")

        it "reports nothing for a mount whose rules read no advisory database" $ do
            cfg <- expectConfig (pubUrlEnv <> advisoryStoreEnv <> [("ECLUSE_RULES", "{\"remediation-fast-track\":{\"enabled\":false},\"deny-known-malicious\":{\"enabled\":false}}")]) (Just privateMountDoc)
            advisoryAgeLines cfg `shouldBe` []

        it "reports nothing at all without an advisory store, because nothing syncs" $ do
            cfg <- configFor privateMountDoc
            advisoryAgeLines cfg `shouldBe` []

    describe "the advisory database a mount's rules require" $ do
        it "names the deny rules that cannot decide without one" $ do
            cfg <- configFor "{\"rules\":{\"risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5}},\"mounts\":{\"npm\":{\"enabled\":true},\"pypi\":{\"enabled\":true,\"rules\":{\"risk\":{\"enabled\":false}}}}}"
            Map.map mountAdvisoryDenials (configMounts cfg) `shouldBe` Map.fromList [(Npm, ["DenyIfEpss"]), (PyPI, [])]
            Map.map mountDatabaseRequirement (configMounts cfg)
                `shouldBe` Map.fromList [(Npm, DatabaseRequired), (PyPI, DatabaseOptional)]

        it "leaves the source-free shipped policy needing no database, because no source activates its advisory rules" $ do
            cfg <- configFor privateMountDoc
            Map.map mountAdvisoryDenials (configMounts cfg) `shouldBe` Map.singleton Npm []
            Map.map mountDatabaseRequirement (configMounts cfg) `shouldBe` Map.singleton Npm DatabaseOptional

        it "names the MAL default and the remediation rule under deny once a source activates them, and only MAL under skip" $ do
            shipped <- expectConfig (pubUrlEnv <> advisoryStoreEnv) (Just privateMountDoc)
            Map.map mountAdvisoryDenials (configMounts shipped) `shouldBe` Map.singleton Npm ["DenyIfKnownMalicious", "AllowIfRemediatesCve"]
            skipping <- expectConfig (pubUrlEnv <> advisoryStoreEnv <> [("ECLUSE_RULES", "{\"remediation-fast-track\":{\"onUnavailable\":\"skip\"}}")]) (Just privateMountDoc)
            Map.map mountAdvisoryDenials (configMounts skipping) `shouldBe` Map.singleton Npm ["DenyIfKnownMalicious"]
            alone <- expectConfig (pubUrlEnv <> advisoryStoreEnv <> [("ECLUSE_RULES", "{\"deny-known-malicious\":{\"enabled\":false}}")]) (Just privateMountDoc)
            Map.map mountAdvisoryDenials (configMounts alone) `shouldBe` Map.singleton Npm ["AllowIfRemediatesCve"]
            Map.map mountDatabaseRequirement (configMounts alone) `shouldBe` Map.singleton Npm DatabaseRequired

    describe "mountEpssRequirement" $ do
        it "requires inherited EPSS rules only where the mount keeps them" $ do
            cfg <- configFor "{\"rules\":{\"risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5}},\"mounts\":{\"npm\":{\"enabled\":true},\"pypi\":{\"enabled\":true,\"rules\":{\"risk\":{\"enabled\":false}}}}}"
            Map.map mountEpssRequirement (configMounts cfg) `shouldBe` Map.fromList [(Npm, EpssRequired), (PyPI, EpssOptional)]

        it "requires mount additions regardless of name, skip alignment, or maximum threshold" $ do
            cfg <- configFor "{\"mounts\":{\"npm\":{\"enabled\":true,\"rules\":{\"renamed-risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":1,\"onUnavailable\":\"skip\"}}},\"pypi\":{\"enabled\":true}}}"
            Map.map mountEpssRequirement (configMounts cfg) `shouldBe` Map.fromList [(Npm, EpssRequired), (PyPI, EpssOptional)]

        it "does not require EPSS for the source-free resolved defaults" $ do
            cfg <- configFor privateMountDoc
            Map.map mountEpssRequirement (configMounts cfg) `shouldBe` Map.singleton Npm EpssOptional

        it "requires EPSS for the shipped differential a source activates, and not once it abstains or is off"
            $ for_
                [ ([], EpssRequired)
                , ([("ECLUSE_RULES", "{\"remediation-fast-track\":{\"remainingAdvisories\":\"abstain\"}}")], EpssOptional)
                , ([("ECLUSE_RULES", "{\"remediation-fast-track\":{\"enabled\":false}}")], EpssOptional)
                ]
            $ \(patch, expected) -> do
                cfg <- expectConfig (pubUrlEnv <> advisoryStoreEnv <> patch) (Just privateMountDoc)
                Map.map mountEpssRequirement (configMounts cfg) `shouldBe` Map.singleton Npm expected

    describe "the EPSS requirement reported at boot" $ do
        it "names each mount's requirement from its own resolved rules" $ do
            cfg <- expectConfig (pubUrlEnv <> advisoryStoreEnv) (Just "{\"rules\":{\"risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5}},\"mounts\":{\"npm\":{\"enabled\":true},\"pypi\":{\"enabled\":true,\"rules\":{\"risk\":{\"enabled\":false},\"remediation-fast-track\":{\"remainingAdvisories\":\"abstain\"}}}}}")
            advisoryEpssLines cfg
                `shouldBe` [ "mount \"npm\": EPSS enrichment is required, because these active rules read EPSS scores: AllowIfRemediatesCve, DenyIfEpss. A failed EPSS feed publishes no artifact, and consumers refuse one without enrichment"
                           , "mount \"pypi\": EPSS enrichment is optional, because no active rule reads EPSS scores. A failed EPSS feed publishes the OSV data with epss_status=unavailable"
                           ]

        it "requires EPSS without an advisory store, as a local compile does" $ do
            cfg <- configFor privateMountDoc
            advisoryEpssLines cfg
                `shouldBe` ["mount \"npm\": EPSS enrichment is required for a compile. With no advisories.url, no source activates the shipped remediation rule, so a compile cannot tell which rules its consumers run. A failed EPSS feed publishes no artifact"]

        it "names an operator's EPSS-reading rule without an advisory store" $ do
            cfg <- configFor "{\"rules\":{\"risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5,\"onUnavailable\":\"skip\"}},\"mounts\":{\"npm\":{\"enabled\":true}}}"
            advisoryEpssLines cfg
                `shouldBe` ["mount \"npm\": EPSS enrichment is required, because these active rules read EPSS scores: DenyIfEpss. A failed EPSS feed publishes no artifact, and consumers refuse one without enrichment"]

    describe "resolvedKeyProvenance" $ do
        it "labels each resolved key with the layer that supplied it" $ do
            provenance <- provenanceOf [("ECLUSE_SERVER__PORT", "4873")] (Just "{\"server\":{\"helpMessage\":\"ask platform-eng\"}}")
            provenance `shouldSatisfy` elem "config: server.port = 4873 (environment)"
            provenance `shouldSatisfy` elem "config: server.helpMessage = ask platform-eng (document)"
            provenance `shouldSatisfy` elem "config: observability.logFormat = json (default)"

        it "names the source path recording and the layer that set it" $ do
            provenanceOf [] Nothing >>= (`shouldSatisfy` elem "config: advisories.recordSourcePath = false (default)")
            provenanceOf [("ECLUSE_ADVISORIES__RECORD_SOURCE_PATH", "true")] Nothing
                >>= (`shouldSatisfy` elem "config: advisories.recordSourcePath = true (environment)")

        it "redacts secret-typed keys whatever layer supplies them" $ do
            provenance <- provenanceOf (("ECLUSE_SERVER__AUTH_TOKEN", "hunter2") : staticEnvVars) Nothing
            provenance `shouldSatisfy` elem "config: server.authToken = <redacted> (environment)"
            provenance
                `shouldSatisfy` elem "config: mounts.npm.mirrorTarget.registry.token = <redacted> (environment)"
            provenance `shouldSatisfy` (not . any (\line -> T.isInfixOf "hunter" line || T.isInfixOf "mirror-write-token" line))

    describe "the boot lines of a shipped rule an advisory source activates" $ do
        let npmDoc = Just "{\"mounts\":{\"npm\":{\"enabled\":true}}}"
            ruleLines name = filter (T.isPrefixOf ("config: rules." <> name <> "."))
            anyRuleLines name = filter (T.isInfixOf ("rules." <> name <> "."))
            marked = T.isSuffixOf ", inactive: no advisories.url)"
        it "marks every line of both rules inactive without advisories.url, and no other line" $ do
            provenance <- provenanceOf pubUrlEnv npmDoc
            for_ sourceDefaultNames $ \name -> do
                ruleLines name provenance `shouldSatisfy` (not . null)
                ruleLines name provenance `shouldSatisfy` all (T.isSuffixOf " (default, inactive: no advisories.url)")
            filter marked provenance `shouldBe` concatMap (`ruleLines` provenance) (toList sourceDefaultNames)
            ruleLines "min-age" provenance `shouldSatisfy` (not . null)
            provenance `shouldSatisfy` elem "config: rules.deny-known-malicious.onUnavailable = deny (default, inactive: no advisories.url)"
            provenance `shouldSatisfy` elem "config: rules.remediation-fast-track.remainingAdvisories = differential (default, inactive: no advisories.url)"

        it "keeps an operator patch's own layer and adds the marker, shared or on a mount" $ do
            provenance <-
                provenanceOf
                    (pubUrlEnv <> [("ECLUSE_RULES", "{\"deny-known-malicious\":{\"onUnavailable\":\"skip\"}}")])
                    (Just "{\"rules\":{\"remediation-fast-track\":{\"remainingAdvisories\":\"abstain\"}},\"mounts\":{\"npm\":{\"rules\":{\"deny-known-malicious\":{\"precedence\":310},\"remediation-fast-track\":{\"onUnavailable\":\"skip\"}}}}}")
            provenance `shouldSatisfy` elem "config: rules.deny-known-malicious.onUnavailable = skip (environment, inactive: no advisories.url)"
            provenance `shouldSatisfy` elem "config: rules.remediation-fast-track.remainingAdvisories = abstain (document, inactive: no advisories.url)"
            provenance `shouldSatisfy` elem "config: mounts.npm.rules.deny-known-malicious.precedence = 310 (document, inactive: no advisories.url)"
            provenance `shouldSatisfy` elem "config: mounts.npm.rules.remediation-fast-track.onUnavailable = skip (document, inactive: no advisories.url)"

        it "marks no line once advisories.url is configured" $ do
            provenance <- provenanceOf (pubUrlEnv <> advisoryStoreEnv) npmDoc
            for_ sourceDefaultNames $ \name -> ruleLines name provenance `shouldSatisfy` (not . null)
            provenance `shouldSatisfy` (not . any (T.isInfixOf "inactive"))

        for_
            [ ("deny-known-malicious", "remediation-fast-track", "{\"rules\":{\"deny-known-malicious\":{\"enabled\":true}},\"mounts\":{\"npm\":{\"enabled\":true}}}")
            , ("deny-known-malicious", "remediation-fast-track", "{\"rules\":{\"deny-known-malicious\":{\"type\":\"DenyIfKnownMalicious\"}},\"mounts\":{\"npm\":{\"enabled\":true}}}")
            , ("deny-known-malicious", "remediation-fast-track", "{\"mounts\":{\"npm\":{\"rules\":{\"deny-known-malicious\":{\"enabled\":true}}}}}")
            , ("remediation-fast-track", "deny-known-malicious", "{\"rules\":{\"remediation-fast-track\":{\"enabled\":true}},\"mounts\":{\"npm\":{\"enabled\":true}}}")
            , ("remediation-fast-track", "deny-known-malicious", "{\"rules\":{\"remediation-fast-track\":{\"type\":\"AllowIfRemediatesCve\"}},\"mounts\":{\"npm\":{\"enabled\":true}}}")
            , ("remediation-fast-track", "deny-known-malicious", "{\"mounts\":{\"npm\":{\"rules\":{\"remediation-fast-track\":{\"enabled\":true,\"onUnavailable\":\"skip\"}}}}}")
            ]
            $ \(activated, other, doc :: Text) ->
                it ("marks no line of a rule an operator activates: " <> toString doc) $ do
                    provenance <- provenanceOf pubUrlEnv (Just (encodeUtf8 doc))
                    ruleLines activated provenance `shouldSatisfy` (not . null)
                    anyRuleLines activated provenance `shouldSatisfy` (not . any marked)
                    ruleLines other provenance `shouldSatisfy` (\otherLines -> not (null otherLines) && all marked otherLines)

        for_
            [ ("deny-known-malicious", "remediation-fast-track", "{\"rules\":{\"deny-known-malicious\":{\"enabled\":false}},\"mounts\":{\"npm\":{\"enabled\":true}}}")
            , ("deny-known-malicious", "remediation-fast-track", "{\"mounts\":{\"npm\":{\"rules\":{\"deny-known-malicious\":{\"enabled\":false}}}}}")
            , ("remediation-fast-track", "deny-known-malicious", "{\"rules\":{\"remediation-fast-track\":{\"enabled\":false}},\"mounts\":{\"npm\":{\"enabled\":true}}}")
            , ("remediation-fast-track", "deny-known-malicious", "{\"mounts\":{\"npm\":{\"rules\":{\"remediation-fast-track\":{\"enabled\":false}}}}}")
            ]
            $ \(disabled, other, doc :: Text) ->
                it ("marks no line of a rule an operator disables, because a source would not run it: " <> toString doc) $ do
                    provenance <- provenanceOf pubUrlEnv (Just (encodeUtf8 doc))
                    anyRuleLines disabled provenance `shouldSatisfy` (\disabledLines -> not (null disabledLines) && not (any marked disabledLines))
                    provenance `shouldSatisfy` any (T.isInfixOf ("rules." <> disabled <> ".enabled = false (document)"))
                    ruleLines other provenance `shouldSatisfy` (\otherLines -> not (null otherLines) && all marked otherLines)

        it "keeps a shared rule's lines unmarked where one mount activates it, and marks the mount that leaves it to the source" $ do
            provenance <-
                provenanceOf
                    pubUrlEnv
                    (Just "{\"mounts\":{\"npm\":{\"rules\":{\"deny-known-malicious\":{\"enabled\":true}}},\"pypi\":{\"rules\":{\"deny-known-malicious\":{\"precedence\":310}}}}}")
            ruleLines "deny-known-malicious" provenance `shouldSatisfy` (\shared -> not (null shared) && not (any marked shared))
            provenance `shouldSatisfy` elem "config: mounts.npm.rules.deny-known-malicious.enabled = true (document)"
            provenance `shouldSatisfy` elem "config: mounts.pypi.rules.deny-known-malicious.precedence = 310 (document, inactive: no advisories.url)"
            ruleLines "remediation-fast-track" provenance `shouldSatisfy` (\shared -> not (null shared) && all marked shared)

        it "marks a shared rule's lines where one mount disables it and another leaves it to the source" $ do
            provenance <-
                provenanceOf
                    pubUrlEnv
                    (Just "{\"mounts\":{\"npm\":{\"rules\":{\"deny-known-malicious\":{\"enabled\":false}}},\"pypi\":{\"rules\":{\"deny-known-malicious\":{\"precedence\":310}}}}}")
            ruleLines "deny-known-malicious" provenance `shouldSatisfy` (\shared -> not (null shared) && all marked shared)
            provenance `shouldSatisfy` elem "config: mounts.npm.rules.deny-known-malicious.enabled = false (document)"
            provenance `shouldSatisfy` elem "config: mounts.pypi.rules.deny-known-malicious.precedence = 310 (document, inactive: no advisories.url)"

        for_ [("no mount", "{}"), ("the only mount switched off", "{\"mounts\":{\"npm\":{\"enabled\":false}}}")] $ \(label, doc :: Text) ->
            it ("marks no shared rule line where no mount serves: " <> label) $ do
                provenance <- provenanceOf pubUrlEnv (Just (encodeUtf8 doc))
                for_ sourceDefaultNames $ \name ->
                    ruleLines name provenance `shouldSatisfy` (\shared -> not (null shared) && not (any marked shared))

-- | The types of a mount's rules that are limited to admission, which the Dredger does not evaluate.
limitedRules :: Mount -> [Text]
limitedRules mount = [ruleName (prRule rule) | rule <- mountPolicy mount, ruleReach rule == AdmissionOnly]

-- | The provenance lines of the configuration these layers load.
provenanceOf :: [(String, String)] -> Maybe ByteString -> IO [Text]
provenanceOf envVars document = (\config -> resolvedKeyProvenance config envVars document) <$> expectConfig envVars document

-- | An advisory store, which the age lines report only once one is configured.
advisoryStoreEnv :: [(String, String)]
advisoryStoreEnv = [("ECLUSE_ADVISORIES__URL", "s3://advisories")]

-- | Load a config document under the client-facing base URL every active mount needs.
configFor :: ByteString -> IO Config
configFor doc = expectConfig pubUrlEnv (Just doc)

-- | The serve-only npm mount the advisory-age cases load, which carries the shipped rule policy.
privateMountDoc :: ByteString
privateMountDoc = registryMountDoc [("privateUpstream", "https://priv.example.test")]

-- | The line a mount with a private upstream carries, which both entry points report.
upstreamProbeNotice :: Text
upstreamProbeNotice =
    "mount \"npm\": the private upstream is asked at boot whether it, or a repository in its upstream chain, connects to a public registry. check-config does not make that call."

-- | An npm mount document declaring each named endpoint at its URL under the @registry@ tag.
registryMountDoc :: [(Text, Text)] -> ByteString
registryMountDoc = npmMountDoc . map endpoint
  where
    endpoint (key, url) = "\"" <> key <> "\":{\"registry\":{\"url\":\"" <> url <> "\"" <> write key <> "}}"
    -- The registry tag requires a static write token on a mirror target and admits none elsewhere.
    write key = if key == "mirrorTarget" then ",\"token\":\"t\"" else ""

-- | A mirrored npm mount on Verdaccio, with the given extra keys written under that tag.
verdaccioMountDoc :: Text -> ByteString
verdaccioMountDoc extra =
    encodeUtf8 $
        "{\"mounts\":{\"npm\":{\
        \\"privateUpstream\":{\"verdaccio\":{\"url\":\"https://priv.example.test\"}},\
        \\"mirrorTarget\":{\"verdaccio\":{"
            <> extra
            <> "\"url\":\"https://verdaccio.example.test\",\"token\":\"t\"}}}}}"

-- | The npm mount's resolved mode, when the config serves one.
modeOf :: Config -> Maybe MountMode
modeOf cfg = regMode . mountRegistries <$> Map.lookup Npm (configMounts cfg)

scriptEvidence :: Ecosystem -> RuleEvidence
scriptEvidence eco =
    withInstallScripts (identityEvidence name version){evPublishedAt = Known (Just (addUTCTime (negate (30 * nominalDay)) fixedNow)), evDependencySources = Known RegistryDependencies}
  where
    (name, version) = if eco == PyPI then (unscopedPyPI "thing", pypiVersion "1.0.0") else (thingName, v1_0_0)

decideOn :: Config -> Ecosystem -> RuleEvidence -> IO Decision
decideOn cfg eco evidence = case Map.lookup eco (configMounts cfg) of
    Nothing -> fail ("expected an active mount: " <> show eco)
    Just mount -> prepare inertRuleDeps (mountPolicy mount) >>= \rules -> evalRules (EvalContext fixedNow Nothing) rules evidence

defaultsWith :: Value -> IO ByteString
defaultsWith override = do
    ast <- expectRight (Yaml.decodeEither' defaultConfigBytes)
    pure (Yaml.encode (deepMerge ast override))

parseRefusal :: Text -> Either [ConfigError] a -> Bool
parseRefusal prefix = \case
    Left [ParseError message] -> prefix `T.isPrefixOf` message
    _ -> False

maliciousDefaultSpec :: Spec
maliciousDefaultSpec = describe "the source-dependent MAL default" $ do
    let source = [("ECLUSE_ADVISORIES__URL", "s3://advisories/ecluse")]
        malRules cfg = concatMap (filter ((== "DenyIfKnownMalicious") . ruleName . prRule) . mountPolicy) . Map.elems $ configMounts cfg
        load :: [(String, String)] -> Text -> Text -> IO Config
        load sourceEnv global mount = expectConfig (pubUrlEnv <> sourceEnv) (Just (encodeUtf8 ("{\"rules\":" <> global <> ",\"mounts\":{\"pypi\":{\"rules\":" <> mount <> "}}}")))
        off = "{\"deny-known-malicious\":{\"enabled\":false}}"
        enablePatch = "{\"deny-known-malicious\":{\"enabled\":true}}"
        partial = "{\"deny-known-malicious\":{\"precedence\":310,\"onUnavailable\":\"skip\",\"appliesTo\":[\"admission\"]}}"
        expected = PrecededRule 300 AdmissionAndRevocation (DenyIfKnownMalicious FailDeny)
    it "keeps the configured and source-free effective shared policies distinct" $ do
        (_, catalogue) <- expectRight (decodeDefaultConfig defaultConfigBytes)
        absent <- load [] "{}" "{}"
        present <- load source "{}" "{}"
        concatMap mountPolicy (Map.elems (configMounts absent)) `shouldMatchList` Map.elems (Map.withoutKeys (policyRules catalogue) sourceDefaultNames)
        concatMap mountPolicy (Map.elems (configMounts present)) `shouldMatchList` Map.elems (policyRules catalogue)
        malRules absent `shouldBe` []
        malRules present `shouldBe` [expected]
        map mountDatabaseRequirement (Map.elems (configMounts present)) `shouldBe` [DatabaseRequired]
        map mountEpssRequirement (Map.elems (configMounts present)) `shouldBe` [EpssRequired]
    for_ [[], source] $ \sourceEnv -> do
        it ("rejects malformed unavailable handling on a retained disabled MAL default " <> show sourceEnv) $
            loadConfig
                (pubUrlEnv <> sourceEnv)
                (Just "{\"rules\":{\"deny-known-malicious\":{\"enabled\":false}},\"mounts\":{\"pypi\":{\"rules\":{\"deny-known-malicious\":{\"onUnavailable\":\"maybe\"}}}}}")
                `shouldBe` Left [PolicyErrors [MalformedRule "deny-known-malicious" "\"onUnavailable\" must be \"deny\" or \"skip\", not \"maybe\""]]
        for_ [(off, "{}"), ("{}", off), (off, off), (off, partial)] $ \(global, mount) ->
            it ("retains disable through patches " <> show (sourceEnv, global, mount)) $
                load sourceEnv global mount >>= (\cfg -> malRules cfg `shouldBe` [])
        it ("allows an explicit mount enable after global disable " <> show sourceEnv) $
            load sourceEnv off enablePatch >>= (\cfg -> malRules cfg `shouldBe` [expected])
        it ("retains disabled knobs when explicitly re-enabled " <> show sourceEnv) $
            load sourceEnv partial enablePatch >>= (\cfg -> malRules cfg `shouldBe` [PrecededRule 310 AdmissionOnly (DenyIfKnownMalicious FailNoDecision)])
        it ("recognises a manual declaration " <> show sourceEnv) $
            load sourceEnv "{\"deny-known-malicious\":{\"type\":\"DenyIfKnownMalicious\"}}" "{}"
                >>= (\cfg -> malRules cfg `shouldBe` [expected])
        it ("does not disable a separately named manual rule " <> show sourceEnv) $
            load sourceEnv "{\"manual-mal\":{\"type\":\"DenyIfKnownMalicious\"}}" off
                >>= (\cfg -> malRules cfg `shouldBe` [expected])
    it "does not activate an absent source through partial knobs" $
        load [] partial "{}" >>= (\cfg -> malRules cfg `shouldBe` [])
    it "reads a document source before selecting the default" $ do
        cfg <- expectConfig pubUrlEnv (Just "{\"advisories\":{\"url\":\"s3://advisories/ecluse\"},\"mounts\":{\"pypi\":{\"enabled\":true}}}")
        malRules cfg `shouldBe` [expected]

-- The remediation default rides the MAL default's activation, so these cases mirror that table.
remediationDefaultSpec :: Spec
remediationDefaultSpec = describe "the source-dependent remediation default" $ do
    let source = [("ECLUSE_ADVISORIES__URL", "s3://advisories/ecluse")]
        both = Just "{\"mounts\":{\"npm\":{\"enabled\":true},\"pypi\":{\"enabled\":true}}}"
        fastLanes cfg = Map.map (mapMaybe (remediationOf . prRule) . mountPolicy) (configMounts cfg)
        remediationOf = \case
            AllowIfRemediatesCve params -> Just params
            _ -> Nothing
        onBoth params = Map.fromList [(Npm, params), (PyPI, params)]
        shipped = AllowIfRemediatesCveParams FailDeny (Differential defaultDifferentialLimits)
        sharedRules patch = [("ECLUSE_RULES", patch)]
    it "is inactive without a source, so a source-free mount decides as before" $ do
        cfg <- expectConfig pubUrlEnv both
        fastLanes cfg `shouldBe` onBoth []
        Map.map mountDatabaseRequirement (configMounts cfg) `shouldBe` onBoth DatabaseOptional

    it "is active with a source, under deny and the shipped differential limits" $ do
        cfg <- expectConfig (pubUrlEnv <> source) both
        fastLanes cfg `shouldBe` onBoth [shipped]
        Map.map mountAdvisoryDenials (configMounts cfg) `shouldBe` onBoth ["DenyIfKnownMalicious", "AllowIfRemediatesCve"]
        Map.map mountEpssRequirement (configMounts cfg) `shouldBe` onBoth EpssRequired

    for_ [("NPM", Npm, PyPI), ("PYPI", PyPI, Npm)] $ \(key, disabled, kept) ->
        it ("is deactivated by enabled: false through ECLUSE_MOUNTS__" <> key <> "__RULES alone") $ do
            cfg <- expectConfig (pubUrlEnv <> source <> [("ECLUSE_MOUNTS__" <> key <> "__RULES", "{\"remediation-fast-track\":{\"enabled\":false}}")]) both
            Map.lookup disabled (fastLanes cfg) `shouldBe` Just []
            Map.lookup kept (fastLanes cfg) `shouldBe` Just [shipped]

    for_ ["{\"remediation-fast-track\":{\"enabled\":true}}", "{\"remediation-fast-track\":{\"type\":\"AllowIfRemediatesCve\"}}"] $ \patch ->
        it ("is activated without a source by " <> patch) $ do
            cfg <- expectConfig (pubUrlEnv <> sharedRules patch) both
            fastLanes cfg `shouldBe` onBoth [shipped]
            Map.map mountDatabaseRequirement (configMounts cfg) `shouldBe` onBoth DatabaseRequired

    it "keeps the activation state under a partial patch" $ do
        let skip = sharedRules "{\"remediation-fast-track\":{\"onUnavailable\":\"skip\"}}"
        absent <- expectConfig (pubUrlEnv <> skip) both
        fastLanes absent `shouldBe` onBoth []
        present <- expectConfig (pubUrlEnv <> source <> skip) both
        fastLanes present `shouldBe` onBoth [AllowIfRemediatesCveParams FailNoDecision (Differential defaultDifferentialLimits)]
        Map.map mountAdvisoryDenials (configMounts present) `shouldBe` onBoth ["DenyIfKnownMalicious"]

    it "layers a mount's differential limits over a shared abstain" $ do
        cfg <-
            expectConfig
                ( pubUrlEnv
                    <> source
                    <> sharedRules "{\"remediation-fast-track\":{\"remainingAdvisories\":\"abstain\"}}"
                    <> [("ECLUSE_MOUNTS__PYPI__RULES", "{\"remediation-fast-track\":{\"remainingAdvisories\":\"differential\",\"maxRemainingCvss\":5}}")]
                )
                both
        fastLanes cfg
            `shouldBe` Map.fromList
                [ (Npm, [AllowIfRemediatesCveParams FailDeny AbstainOnRemaining])
                , (PyPI, [AllowIfRemediatesCveParams FailDeny (Differential defaultDifferentialLimits{dlMaxRemainingCvss = 5})])
                ]
        Map.map mountEpssRequirement (configMounts cfg) `shouldBe` Map.fromList [(Npm, EpssOptional), (PyPI, EpssRequired)]

    it "refuses a malformed patch on the inactive rule too" $
        loadConfig pubUrlEnv (Just "{\"rules\":{\"remediation-fast-track\":{\"remainingAdvisories\":\"partial\"}},\"mounts\":{\"npm\":{\"enabled\":true}}}")
            `shouldBe` Left [PolicyErrors [MalformedRule "remediation-fast-track" "\"remainingAdvisories\" must be \"abstain\" or \"differential\", not \"partial\""]]
