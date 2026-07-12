-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.CompositionSpec (spec) where

import Test.Hspec

import Ecluse (mountBindingFor)
import Ecluse.Composition (composeBindings, planMounts)
import Ecluse.Composition.BootError (BootError (..))
import Ecluse.Composition.Credential (initCredentialProviders)
import Ecluse.Composition.Support (
    expectConfig,
    expectEnv,
    expectProviders,
    fixedNow,
    noTokenEnvVars,
    staticEnvVars,
    withoutCredentialProvider,
    withoutMirrorTargetUrl,
 )
import Ecluse.Config (
    Config (configApp),
    ConfigError (..),
    CredentialBackend (..),
    PolicyError (UnknownRuleType),
    loadConfig,
    renderConfigError,
 )
import Ecluse.Core.Credential (unSecret)
import Ecluse.Core.Credential.Refresh (noCredentialReporters)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package (HashAlg (SHA1, SHA512), mkScope)
import Ecluse.Core.Package.Integrity (
    mkMinIntegrity,
    mkMinTrustedIntegrity,
 )
import Ecluse.Core.Security (Limits (maxBodyBytes, maxNestingDepth, maxVersionCount), TarballHostPolicy (AnyAllowlistedHost, SameHostAsPackument), defaultLimits)
import Ecluse.Core.Server.Context (
    MountBinding (bindingPackumentDeps, bindingPrefix, bindingPublishDeps),
    PackumentDeps (..),
    PublishDeps (..),
 )
import Ecluse.Core.Server.Response (appendHelp)
import Ecluse.Test.Package (defaultMinIntegrity, defaultMinTrustedIntegrity)
import Ecluse.Test.Rules (inertRuleDeps)

{- | Tests for the composition root's boot-time wiring. They exercise the two
promises of the slice: a valid configuration produces served mount bindings with
real packument-serve dependencies (so packuments are merged, not @501@ stubs), and
every boot problem -- an unresolved rule policy, a configured mount with no adapter,
a credential reference that does not resolve -- is a fail-fast, __aggregated__ boot
error. Pure of IO: the clock and the ecosystem-to-adapter resolver are injected, so
nothing here opens a socket. The sibling composition modules' specs live beside
them ("Ecluse.Composition.CredentialSpec", "Ecluse.Composition.MirrorQueueSpec",
"Ecluse.Composition.SizingSpec", "Ecluse.Composition.BootErrorSpec").
-}
spec :: Spec
spec = do
    composeBindingsSpec
    bootErrorSpec
    publishWiringSpec

expectDoc :: ByteString -> IO ByteString
expectDoc = pure

{- | A document mount keyed by the given ecosystem, naming the given credential
backend -- for the no-adapter and unresolved-credential boot-error cases.
-}
mountDoc :: Text -> Text -> ByteString
mountDoc eco credential =
    encodeUtf8
        ( "{\"mounts\":{\""
            <> eco
            <> "\":{\"privateUpstream\":\"https://priv\",\"publicUpstream\":\"https://pub\",\
               \\"mirrorTarget\":\"https://mir\",\"credentialProvider\":\""
            <> credential
            <> "\"}}}"
        )

-- Build the served bindings from an env + optional document through 'planMounts',
-- with the real adapter resolver, the fixed clock, and the env's static providers.
planFrom :: [(String, String)] -> Maybe ByteString -> IO (Either [BootError] [MountBinding])
planFrom envVars mDocBytes = do
    case loadConfig envVars mDocBytes of
        Left cfgErrs -> pure (Left (concatMap toBoot errs))
          where
            errs = cfgErrs
            toBoot (PolicyErrors es) = map PolicyBootError es
            toBoot (ParseError err) = [PolicyBootError (UnknownRuleType "parse" err)]
            toBoot missing@(MountMissingPrivateUpstream _) = [PolicyBootError (UnknownRuleType "mount" (renderConfigError missing))]
            toBoot missing@(MountMissingMirrorTarget _) = [PolicyBootError (UnknownRuleType "mount" (renderConfigError missing))]
        Right cfg -> do
            initCredentialProviders noCredentialReporters (configApp cfg) >>= \case
                Left pErrs -> pure (Left pErrs)
                Right providers -> planMounts mountBindingFor (pure fixedNow) (const inertRuleDeps) providers cfg

composeBindingsSpec :: Spec
composeBindingsSpec = describe "planMounts / composeBindings (config-driven serving)" $ do
    it "produces one npm binding with packument-serve deps wired (served, not a 501 stub)" $ do
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Left errs -> expectationFailure ("unexpected boot errors: " <> show errs)
            Right [binding] -> do
                bindingPrefix binding `shouldBe` ("npm" :| [])
                case bindingPackumentDeps binding of
                    Nothing -> expectationFailure "expected packument deps wired, got the 501 stub"
                    Just deps -> do
                        pdPrivateBaseUrl deps `shouldBe` "https://private.example.test"
                        pdPublicBaseUrl deps `shouldBe` "https://public.example.test"
                        -- With no ECLUSE_PUBLIC_URL the mount base falls back to the
                        -- derived relative prefix (the npm CLI cannot consume this;
                        -- the absolute form below is what a real client needs).
                        pdMountBaseUrl deps `shouldBe` "/npm"
                        -- The mirror-target endpoint is wired from the mount's config,
                        -- for the demand-driven mirror job's publish destination.
                        pdMirrorTarget deps `shouldBe` "https://mirror.example.test"
            Right other -> expectationFailure ("expected exactly one binding, got " <> show (length other))

    it "rewrites the tarball base to an absolute URL under ECLUSE_PUBLIC_URL" $ do
        -- With ECLUSE_PUBLIC_URL set, dist.tarball rewrites to an absolute URL a real
        -- npm client can fetch, instead of the npm-incompatible relative path.
        _ <- expectEnv (("ECLUSE_PUBLIC_URL", "https://proxy.example.test") : staticEnvVars)
        planFrom (("ECLUSE_PUBLIC_URL", "https://proxy.example.test") : staticEnvVars) Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMountBaseUrl deps `shouldBe` "https://proxy.example.test/npm"
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "drops a trailing slash on ECLUSE_PUBLIC_URL so the base joins with one separator" $ do
        _ <- expectEnv (("ECLUSE_PUBLIC_URL", "https://proxy.example.test/") : staticEnvVars)
        planFrom (("ECLUSE_PUBLIC_URL", "https://proxy.example.test/") : staticEnvVars) Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMountBaseUrl deps `shouldBe` "https://proxy.example.test/npm"
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "carries the resolved rule policy onto the binding's packument deps" $ do
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                -- 'PreparedRule' has no 'Show' (it carries an evaluator), so assert on
                -- the count rather than the rules themselves.
                Just deps -> null (pdRules deps) `shouldBe` False
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads the inbound edge token, clock, and help message onto the deps" $ do
        -- Forces the remaining 'PackumentDeps' fields: the inbound token (from
        -- ECLUSE_AUTH_TOKEN), the injected clock ('pdNow'), and the operator help
        -- message -- all wired by the composition root.
        env <- expectEnv (("ECLUSE_AUTH_TOKEN", "edge-secret") : ("ECLUSE_HELP_MESSAGE", "ask #platform") : staticEnvVars)
        providers <- expectProviders env
        config <- expectConfig (("ECLUSE_AUTH_TOKEN", "edge-secret") : ("ECLUSE_HELP_MESSAGE", "ask #platform") : staticEnvVars) Nothing
        composeBindings mountBindingFor (pure fixedNow) (const inertRuleDeps) providers config >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> do
                    fmap unSecret (pdInboundToken deps) `shouldBe` Just "edge-secret"
                    fmap (\help -> appendHelp (Just help) "denied") (pdHelp deps)
                        `shouldBe` Just "denied ask #platform"
                    served <- pdNow deps
                    served `shouldBe` fixedNow
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the tarball-host policy to same-host (secure default)" $ do
        -- With ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST unset, the deny-by-default
        -- reading of the allowlist is threaded onto every mount's deps.
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdTarballHostPolicy deps `shouldBe` SameHostAsPackument
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "relaxes the tarball-host policy when the operator opts in" $ do
        _ <- expectEnv (("ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST", "true") : staticEnvVars)
        planFrom (("ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST", "true") : staticEnvVars) Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdTarballHostPolicy deps `shouldBe` AnyAllowlistedHost
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults additionalBlockedRanges to empty onto every mount's deps" $ do
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdAdditionalBlockedRanges deps `shouldBe` []
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads the operator's global additionalBlockedRanges onto every mount's deps" $ do
        -- Global (not per-mount): which internal ranges exist on an operator's own
        -- network is a deployment-wide fact, so one list applies to every mount alike.
        let testEnvVars = ("ECLUSE_ADDITIONAL_BLOCKED_RANGES", "203.0.113.0/24") : staticEnvVars
        _ <- expectEnv testEnvVars
        planFrom testEnvVars Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdAdditionalBlockedRanges deps `shouldBe` ["203.0.113.0/24"]
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the response-bound budget to the secure defaults" $ do
        -- With no PROXY_MAX_* set, the deps carry Ecluse.Core.Security.defaultLimits -- the
        -- secure-default body/version/nesting ceilings (security.md invariant 4).
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdLimits deps `shouldBe` defaultLimits
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads the operator's response-bound overrides onto the deps" $ do
        -- The three PROXY_MAX_* knobs flow from the environment layer onto every
        -- mount's Limits budget, so a deployment tightens or loosens the bounds.
        let testEnvVars =
                ("ECLUSE_MAX_RESPONSE_BYTES", "2048")
                    : ("ECLUSE_MAX_VERSION_COUNT", "10")
                    : ("ECLUSE_MAX_NESTING_DEPTH", "16")
                    : staticEnvVars
        _ <- expectEnv testEnvVars
        planFrom testEnvVars Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> do
                    maxBodyBytes (pdLimits deps) `shouldBe` 2048
                    maxVersionCount (pdLimits deps) `shouldBe` 10
                    maxNestingDepth (pdLimits deps) `shouldBe` 16
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the public-integrity floor to SHA-256 onto the deps" $ do
        -- With ECLUSE_MIN_PUBLIC_INTEGRITY unset, every mount's deps carry the default
        -- SHA-256 floor the public admission gate enforces.
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMinIntegrity deps `shouldBe` defaultMinIntegrity
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads a raised public-integrity floor onto the deps" $ do
        sha512Floor <- either (fail . toString) pure (mkMinIntegrity SHA512)
        _ <- expectEnv (("ECLUSE_MIN_PUBLIC_INTEGRITY", "sha512") : staticEnvVars)
        planFrom (("ECLUSE_MIN_PUBLIC_INTEGRITY", "sha512") : staticEnvVars) Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMinIntegrity deps `shouldBe` sha512Floor
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the trusted-integrity floor to SHA-256 onto the deps" $ do
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMinTrustedIntegrity deps `shouldBe` defaultMinTrustedIntegrity
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads a loosened trusted-integrity floor (sha1) onto the deps" $ do
        -- The trusted floor is loosenable below SHA-256, so a SHA-1 value flows from
        -- config to the deps the trusted gate consults -- the asymmetry with the public
        -- floor's hard SHA-256 minimum.
        sha1Floor <- either (fail . toString) pure (mkMinTrustedIntegrity SHA1)
        _ <- expectEnv (("ECLUSE_MIN_TRUSTED_INTEGRITY", "sha1") : staticEnvVars)
        planFrom (("ECLUSE_MIN_TRUSTED_INTEGRITY", "sha1") : staticEnvVars) Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMinTrustedIntegrity deps `shouldBe` sha1Floor
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "composeBindings is the listener-free Config -> [MountBinding] builder under planMounts" $ do
        -- The builder over an already-loaded Config is the testable core (it opens no
        -- listener, only 'prepare's each mount's rules); planMounts is just loadConfig
        -- sequenced into it.
        env <- expectEnv staticEnvVars
        providers <- expectProviders env
        config <- expectConfig staticEnvVars Nothing
        composeBindings mountBindingFor (pure fixedNow) (const inertRuleDeps) providers config >>= \case
            Right bindings -> map bindingPrefix bindings `shouldBe` ["npm" :| []]
            Left errs -> expectationFailure ("unexpected boot errors: " <> show errs)

bootErrorSpec :: Spec
bootErrorSpec = describe "planMounts (fail fast at boot)" $ do
    it "fails on an unresolved rule policy (a typo'd rule type)" $ do
        _ <- expectEnv staticEnvVars
        _ <- expectDoc "{\"rules\":{\"oops\":{\"type\":\"Nope\"}}}"
        planFrom staticEnvVars (Just "{\"rules\":{\"oops\":{\"type\":\"Nope\"}}}") >>= \case
            Left errs -> errs `shouldBe` [PolicyBootError (UnknownRuleType "oops" "Nope")]
            Right _ -> expectationFailure "expected a policy boot error"

    it "fails on a configured mount whose ecosystem has no adapter" $ do
        -- Touching any pypi key activates the mount, so the env fixture must carry
        -- the private upstream the activation contract requires.
        let pypiEnv =
                ("ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM", "https://priv.example.test")
                    : ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET", "https://mir.example.test")
                    : ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET_TOKEN", "t")
                    : staticEnvVars
        _ <- expectEnv pypiEnv
        _ <- expectDoc (mountDoc "pypi" "static")
        planFrom pypiEnv (Just (mountDoc "pypi" "static")) >>= \case
            Left errs -> errs `shouldBe` [MissingAdapter PyPI]
            Right _ -> expectationFailure "expected boot failure"

    it "fails when an active mount declares no mirror target" $ do
        -- Activation implies a mirror write: the target must be declared explicitly
        -- (even when it equals the private upstream), so a mount without one is a
        -- rendered boot error, never an implied endpoint.
        let env = withoutMirrorTargetUrl staticEnvVars
        planFrom env Nothing >>= \case
            Left errs ->
                errs
                    `shouldBe` [PolicyBootError (UnknownRuleType "mount" (renderConfigError (MountMissingMirrorTarget Npm)))]
            Right _ -> expectationFailure "expected a missing-mirror-target boot error"

    it "fails on a mount missing codeartifact config" $ do
        let env = withoutCredentialProvider staticEnvVars
        _ <- expectEnv env
        _ <- expectDoc (mountDoc "npm" "codeartifact")
        planFrom env (Just (mountDoc "npm" "codeartifact")) >>= \case
            Left errs ->
                errs
                    `shouldMatchList` [ CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN"
                                      , CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER"
                                      , CodeArtifactConfigMissing "ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION"
                                      ]
            Right _ -> expectationFailure "expected an unresolved-credential boot error"

    it "aggregates a missing adapter and an unresolved credential on one mount" $ do
        -- Without a MIRROR_TARGET_TOKEN, PyPI's static credential is unresolved AND its adapter is missing.
        _ <- expectEnv staticEnvVars
        _ <- expectDoc (mountDoc "pypi" "static")
        planFrom staticEnvVars (Just (mountDoc "pypi" "static")) >>= \case
            Left errs ->
                errs
                    `shouldMatchList` [ UnresolvedCredential PyPI StaticCredential
                                      , MissingAdapter PyPI
                                      ]
            Right _ -> expectationFailure "expected aggregated boot errors"

    it "fails the env single-mount when no static provider is initialized" $ do
        -- Without ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN, the env single-mount's @static@ reference does
        -- not resolve, so even the default npm mount is an honest boot failure.
        _ <- expectEnv noTokenEnvVars
        planFrom noTokenEnvVars Nothing >>= \case
            Left errs -> errs `shouldBe` [UnresolvedCredential Npm StaticCredential]
            Right _ -> expectationFailure "expected an unresolved-credential boot error"

    it "fails when a publication target is set without a publish-scope allow-list" $ do
        -- ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET set but ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES empty: the anti-shadowing guard
        -- would have nothing to enforce, so the boot refuses rather than defaulting.
        _ <- expectEnv (("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test") : staticEnvVars)
        planFrom (("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test") : staticEnvVars) Nothing >>= \case
            Left errs -> errs `shouldBe` [PublishScopesMissing Npm]
            Right _ -> expectationFailure "expected a publish-scopes-missing boot error"

    it "fails when a static publish credential is set without a verifiable inbound edge" $ do
        -- ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN set with ECLUSE_AUTH_TOKEN unset (the default open edge):
        -- Écluse would substitute its own write credential for a caller who forwards none,
        -- so any unauthenticated client could publish within scope. The boot refuses rather
        -- than leaving the internal credential coupled to no edge.
        let testEnvVars =
                [ ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test")
                , ("ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES", "@acme")
                , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN", "publish-write-token")
                ]
                    <> staticEnvVars
        _ <- expectEnv testEnvVars
        planFrom testEnvVars Nothing >>= \case
            Left errs -> errs `shouldBe` [PublishStaticCredentialNeedsEdge Npm]
            Right _ -> expectationFailure "expected a publish-static-credential-needs-edge boot error"

    it "accumulates both publish boot errors when scopes are missing and the static credential has no edge" $ do
        -- A publication target with no ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES and a static credential behind an
        -- open edge trips both couplings at once: they surface together, in a stable order
        -- (scopes first, then the edge requirement), so the operator fixes both before the
        -- next boot rather than swatting them one reboot at a time.
        let testEnvVars =
                [ ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test")
                , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN", "publish-write-token")
                ]
                    <> staticEnvVars
        _ <- expectEnv testEnvVars
        planFrom testEnvVars Nothing >>= \case
            Left errs -> errs `shouldMatchList` [PublishScopesMissing Npm, PublishStaticCredentialNeedsEdge Npm]
            Right _ -> expectationFailure "expected both publish boot errors, accumulated"

publishWiringSpec :: Spec
publishWiringSpec = describe "planMounts (first-party publish deps)" $ do
    it "wires the publication target and scope allow-list onto the mount when configured" $ do
        let testEnv =
                [ ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test")
                , ("ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES", "@acme, @beta")
                ]
                    <> staticEnvVars
        _ <- expectEnv testEnv
        planFrom testEnv Nothing >>= \case
            Right [binding] -> case bindingPublishDeps binding of
                Just deps -> do
                    pubTargetUrl deps `shouldBe` "https://publish.example.test"
                    pubScopes deps `shouldBe` [mkScope "acme", mkScope "beta"]
                Nothing -> expectationFailure "expected the mount to carry publish deps"
            _ -> expectationFailure "expected a single wired binding"

    it "boots a static publish credential when a verifiable inbound edge is configured" $ do
        -- The safe pairing -- the positive control for the fail-loud boot test above: the
        -- same static publish credential boots once ECLUSE_AUTH_TOKEN gates the edge, so the
        -- internal credential is only ever reachable behind edge authentication.
        let testEnv =
                [ ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test")
                , ("ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES", "@acme")
                , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN", "publish-write-token")
                , ("ECLUSE_AUTH_TOKEN", "edge-token")
                ]
                    <> staticEnvVars
        _ <- expectEnv testEnv
        planFrom testEnv Nothing >>= \case
            Right [binding] -> case bindingPublishDeps binding of
                Just deps -> pubTargetUrl deps `shouldBe` "https://publish.example.test"
                Nothing -> expectationFailure "expected the mount to carry publish deps"
            _ -> expectationFailure "expected a single wired binding"

    it "leaves the publish path off (no publish deps) when no publication target is configured" $ do
        -- The opt-out: with no ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET the mount carries no publish deps,
        -- so a PUT /{pkg} is 405 -- there is no implicit write path.
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> case bindingPublishDeps binding of
                Nothing -> pure ()
                Just _ -> expectationFailure "expected no publish deps when no publication target is configured"
            _ -> expectationFailure "expected a single wired binding"
