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
    staticEnvVars,
    withoutMirrorTargetUrl,
 )
import Ecluse.Config (
    ConfigError (..),
    PolicyError (UnknownRuleType),
    loadConfig,
    renderConfigError,
 )
import Ecluse.Core.Credential (unSecret)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package (HashAlg (SHA1, SHA512), mkScope)
import Ecluse.Core.Package.Integrity (
    mkMinIntegrity,
    mkMinTrustedIntegrity,
 )
import Ecluse.Core.Package.Merge (DivergencePolicy (FailClosed))
import Ecluse.Core.Security (Limits (maxBodyBytes, maxNestingDepth, maxVersionCount), TarballHostPolicy (AnyAllowlistedHost, SameHostAsPackument), defaultLimits)
import Ecluse.Core.Server.Context (
    MirrorServePlan (MirrorOnAdmit, NoMirrorWrite),
    MountBinding (bindingPackumentDeps, bindingPrefix, bindingPublishDeps),
    PackumentDeps (..),
    PublishDeps (..),
 )
import Ecluse.Core.Server.Response (appendHelp)
import Ecluse.Test.Credential (noCredentialReporters)
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

{- | A complete document mount keyed by the given ecosystem, with a non-CodeArtifact
mirror target and a static write token so its credential resolves -- for the
no-adapter boot-error case (where the adapter, not the credential, is the fault).
-}
mountDoc :: Text -> ByteString
mountDoc eco =
    encodeUtf8
        ( "{\"mounts\":{\""
            <> eco
            <> "\":{\"privateUpstream\":\"https://priv\",\"publicUpstream\":\"https://pub\",\
               \\"mirrorTarget\":\"https://mir\",\"mirrorTargetToken\":\"t\"}}}"
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
            toBoot missing@(MirrorSettingWithoutWrite _ _) = [PolicyBootError (UnknownRuleType "mount" (renderConfigError missing))]
            toBoot missing@(MirrorCredentialTokenMissing _) = [PolicyBootError (UnknownRuleType "mount" (renderConfigError missing))]
            toBoot missing@(MirrorCredentialConflict _) = [PolicyBootError (UnknownRuleType "mount" (renderConfigError missing))]
        Right cfg -> do
            initCredentialProviders noCredentialReporters cfg >>= \case
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
                do
                    let deps = bindingPackumentDeps binding
                    pdPrivateBaseUrl deps `shouldBe` Just "https://private.example.test"
                    pdPublicBaseUrl deps `shouldBe` "https://public.example.test"
                    -- With no ECLUSE_PUBLIC_URL the mount base falls back to the
                    -- derived relative prefix (the npm CLI cannot consume this;
                    -- the absolute form below is what a real client needs).
                    pdMountBaseUrl deps `shouldBe` "/npm"
                    -- The mirror serve plan is wired from the mount's config: an
                    -- admitted public artifact enqueues toward the declared target.
                    pdMirror deps `shouldBe` MirrorOnAdmit "https://mirror.example.test"
            Right other -> expectationFailure ("expected exactly one binding, got " <> show (length other))

    it "rewrites the tarball base to an absolute URL under ECLUSE_PUBLIC_URL" $ do
        -- With ECLUSE_PUBLIC_URL set, dist.tarball rewrites to an absolute URL a real
        -- npm client can fetch, instead of the npm-incompatible relative path.
        _ <- expectEnv (("ECLUSE_PUBLIC_URL", "https://proxy.example.test") : staticEnvVars)
        planFrom (("ECLUSE_PUBLIC_URL", "https://proxy.example.test") : staticEnvVars) Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdMountBaseUrl deps `shouldBe` "https://proxy.example.test/npm"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "drops a trailing slash on ECLUSE_PUBLIC_URL so the base joins with one separator" $ do
        _ <- expectEnv (("ECLUSE_PUBLIC_URL", "https://proxy.example.test/") : staticEnvVars)
        planFrom (("ECLUSE_PUBLIC_URL", "https://proxy.example.test/") : staticEnvVars) Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdMountBaseUrl deps `shouldBe` "https://proxy.example.test/npm"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "carries the resolved rule policy onto the binding's packument deps" $ do
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> do
                -- 'PreparedRule' has no 'Show' (it carries an evaluator), so assert on
                -- the count rather than the rules themselves.
                let deps = bindingPackumentDeps binding
                null (pdRules deps) `shouldBe` False
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads the inbound edge token, clock, and help message onto the deps" $ do
        -- Forces the remaining 'PackumentDeps' fields: the inbound token (from
        -- ECLUSE_AUTH_TOKEN), the injected clock ('pdNow'), and the operator help
        -- message -- all wired by the composition root.
        config <- expectConfig (("ECLUSE_AUTH_TOKEN", "edge-secret") : ("ECLUSE_HELP_MESSAGE", "ask #platform") : staticEnvVars) Nothing
        providers <- expectProviders config
        composeBindings mountBindingFor (pure fixedNow) (const inertRuleDeps) providers config >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                fmap unSecret (pdInboundToken deps) `shouldBe` Just "edge-secret"
                fmap (\help -> appendHelp (Just help) "denied") (pdHelp deps)
                    `shouldBe` Just "denied ask #platform"
                served <- pdNow deps
                served `shouldBe` fixedNow
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the tarball-host policy to same-host (secure default)" $ do
        -- With ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST unset, the deny-by-default
        -- reading of the allowlist is threaded onto every mount's deps.
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdTarballHostPolicy deps `shouldBe` SameHostAsPackument
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "relaxes the tarball-host policy when the operator opts in" $ do
        _ <- expectEnv (("ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST", "true") : staticEnvVars)
        planFrom (("ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST", "true") : staticEnvVars) Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdTarballHostPolicy deps `shouldBe` AnyAllowlistedHost
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults additionalBlockedRanges to empty onto every mount's deps" $ do
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdAdditionalBlockedRanges deps `shouldBe` []
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads the operator's global additionalBlockedRanges onto every mount's deps" $ do
        -- Global (not per-mount): which internal ranges exist on an operator's own
        -- network is a deployment-wide fact, so one list applies to every mount alike.
        let testEnvVars = ("ECLUSE_ADDITIONAL_BLOCKED_RANGES", "203.0.113.0/24") : staticEnvVars
        _ <- expectEnv testEnvVars
        planFrom testEnvVars Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdAdditionalBlockedRanges deps `shouldBe` ["203.0.113.0/24"]
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the response-bound budget to the secure defaults" $ do
        -- With no ECLUSE_MAX_* set, the deps carry Ecluse.Core.Security.defaultLimits -- the
        -- secure-default body/version/nesting ceilings (security.md invariant 4).
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdLimits deps `shouldBe` defaultLimits
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads the operator's response-bound overrides onto the deps" $ do
        -- The three ECLUSE_MAX_* knobs flow from the environment layer onto every
        -- mount's Limits budget, so a deployment tightens or loosens the bounds.
        let testEnvVars =
                ("ECLUSE_MAX_RESPONSE_BYTES", "2048")
                    : ("ECLUSE_MAX_VERSION_COUNT", "10")
                    : ("ECLUSE_MAX_NESTING_DEPTH", "16")
                    : staticEnvVars
        _ <- expectEnv testEnvVars
        planFrom testEnvVars Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                maxBodyBytes (pdLimits deps) `shouldBe` 2048
                maxVersionCount (pdLimits deps) `shouldBe` 10
                maxNestingDepth (pdLimits deps) `shouldBe` 16
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the public-integrity floor to SHA-256 onto the deps" $ do
        -- With ECLUSE_MIN_PUBLIC_INTEGRITY unset, every mount's deps carry the default
        -- SHA-256 floor the public admission gate enforces.
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdMinIntegrity deps `shouldBe` defaultMinIntegrity
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads a raised public-integrity floor onto the deps" $ do
        sha512Floor <- either (fail . toString) pure (mkMinIntegrity SHA512)
        _ <- expectEnv (("ECLUSE_MIN_PUBLIC_INTEGRITY", "sha512") : staticEnvVars)
        planFrom (("ECLUSE_MIN_PUBLIC_INTEGRITY", "sha512") : staticEnvVars) Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdMinIntegrity deps `shouldBe` sha512Floor
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the trusted-integrity floor to SHA-256 onto the deps" $ do
        _ <- expectEnv staticEnvVars
        planFrom staticEnvVars Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdMinTrustedIntegrity deps `shouldBe` defaultMinTrustedIntegrity
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads a loosened trusted-integrity floor (sha1) onto the deps" $ do
        -- The trusted floor is loosenable below SHA-256, so a SHA-1 value flows from
        -- config to the deps the trusted gate consults -- the asymmetry with the public
        -- floor's hard SHA-256 minimum.
        sha1Floor <- either (fail . toString) pure (mkMinTrustedIntegrity SHA1)
        _ <- expectEnv (("ECLUSE_MIN_TRUSTED_INTEGRITY", "sha1") : staticEnvVars)
        planFrom (("ECLUSE_MIN_TRUSTED_INTEGRITY", "sha1") : staticEnvVars) Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdMinTrustedIntegrity deps `shouldBe` sha1Floor
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "refines the trusted floor and divergence policy per mount over the global defaults" $ do
        -- The two knobs describe trust in a particular registry, so a legacy mount's
        -- loosening must not leak onto other mounts: the mount key overrides, the
        -- global default stands elsewhere.
        sha1Floor <- either (fail . toString) pure (mkMinTrustedIntegrity SHA1)
        let env =
                ("ECLUSE_MOUNTS__NPM__MIN_TRUSTED_INTEGRITY", "sha1")
                    : ("ECLUSE_MOUNTS__NPM__DIVERGENCE_POLICY", "fail-closed")
                    : staticEnvVars
        planFrom env Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdMinTrustedIntegrity deps `shouldBe` sha1Floor
                pdDivergencePolicy deps `shouldBe` FailClosed
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "composeBindings is the listener-free Config -> [MountBinding] builder under planMounts" $ do
        -- The builder over an already-loaded Config is the testable core (it opens no
        -- listener, only 'prepare's each mount's rules); planMounts is just loadConfig
        -- sequenced into it.
        config <- expectConfig staticEnvVars Nothing
        providers <- expectProviders config
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
        _ <- expectDoc (mountDoc "pypi")
        planFrom pypiEnv (Just (mountDoc "pypi")) >>= \case
            Left errs -> errs `shouldBe` [MissingAdapter PyPI]
            Right _ -> expectationFailure "expected boot failure"

    it "refuses a leftover write token on a mount that declares no mirror target" $ do
        -- Mirroring is derived from the declared target: no mirrorTarget means
        -- serve-only. A write token left behind signals a misunderstanding (most
        -- likely a dropped target), so it is refused per key, never ignored.
        let env = withoutMirrorTargetUrl staticEnvVars
        planFrom env Nothing >>= \case
            Left errs ->
                errs
                    `shouldBe` [PolicyBootError (UnknownRuleType "mount" (renderConfigError (MirrorSettingWithoutWrite Npm "mirrorTargetToken")))]
            Right _ -> expectationFailure "expected a mirror-setting-without-write boot error"

    it "binds a serve-only mount (no mirror target): NoMirrorWrite deps over the private merge" $ do
        let env = filter ((/= "ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN") . fst) (withoutMirrorTargetUrl staticEnvVars)
        planFrom env Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdMirror deps `shouldBe` NoMirrorWrite
                pdPrivateBaseUrl deps `shouldBe` Just "https://private.example.test"
            other -> expectationFailure ("expected one serve-only binding, got " <> show (fmap length other))

    it "binds a pure public gate from enabled alone (no endpoint keys declared)" $ do
        -- The two-variable start: enabled activates the mount, the template public
        -- upstream serves, nothing is private and nothing mirrors.
        planFrom [("ECLUSE_MOUNTS__NPM__ENABLED", "true")] Nothing >>= \case
            Right [binding] -> do
                let deps = bindingPackumentDeps binding
                pdPrivateBaseUrl deps `shouldBe` Nothing
                pdMirror deps `shouldBe` NoMirrorWrite
                pdPublicBaseUrl deps `shouldBe` "https://registry.npmjs.org"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "fails when a publication target is set without a publish-scope allow-list" $ do
        -- ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET set but ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW empty: the anti-shadowing guard
        -- would have nothing to enforce, so the boot refuses rather than defaulting.
        _ <- expectEnv (("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test") : staticEnvVars)
        planFrom (("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test") : staticEnvVars) Nothing >>= \case
            Left errs -> errs `shouldBe` [PublishAllowMissing Npm]
            Right _ -> expectationFailure "expected a publish-scopes-missing boot error"

    it "fails when a static publish credential is set without a verifiable inbound edge" $ do
        -- ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN set with ECLUSE_AUTH_TOKEN unset (the default open edge):
        -- Écluse would substitute its own write credential for a caller who forwards none,
        -- so any unauthenticated client could publish within scope. The boot refuses rather
        -- than leaving the internal credential coupled to no edge.
        let testEnvVars =
                [ ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test")
                , ("ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW", "@acme")
                , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN", "publish-write-token")
                ]
                    <> staticEnvVars
        _ <- expectEnv testEnvVars
        planFrom testEnvVars Nothing >>= \case
            Left errs -> errs `shouldBe` [PublishStaticCredentialNeedsEdge Npm]
            Right _ -> expectationFailure "expected a publish-static-credential-needs-edge boot error"

    it "accumulates both publish boot errors when scopes are missing and the static credential has no edge" $ do
        -- A publication target with no ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW and a static credential behind an
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
            Left errs -> errs `shouldMatchList` [PublishAllowMissing Npm, PublishStaticCredentialNeedsEdge Npm]
            Right _ -> expectationFailure "expected both publish boot errors, accumulated"

publishWiringSpec :: Spec
publishWiringSpec = describe "planMounts (first-party publish deps)" $ do
    it "wires the publication target and scope allow-list onto the mount when configured" $ do
        let testEnv =
                [ ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test")
                , ("ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW", "@acme, @beta")
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
                , ("ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW", "@acme")
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
