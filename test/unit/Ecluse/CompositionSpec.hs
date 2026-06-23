module Ecluse.CompositionSpec (spec) where

import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime (UTCTime), fromGregorian)
import Test.Hspec

import Ecluse (mirrorWriteProvider, mountBindingFor)
import Ecluse.Composition (
    BootError (..),
    cacheConfigFor,
    composeBindings,
    initCredentialProviders,
    initializedBackends,
    lookupProvider,
    planMounts,
    renderBootError,
 )
import Ecluse.Config (
    Config,
    ConfigDoc,
    CredentialBackend (..),
    EnvConfig,
    PolicyError (UnknownRuleType),
    decodeDocument,
    loadConfig,
    parseEnvPure,
 )
import Ecluse.Credential (authSecret, currentToken, unSecret)
import Ecluse.Ecosystem (Ecosystem (..))
import Ecluse.Server.Cache (CacheConfig (cacheMaxEntries, cacheTtl))
import Ecluse.Server.Context (
    MountBinding (bindingPackumentDeps, bindingPrefix),
    PackumentDeps (..),
 )
import Ecluse.Server.Response (unHelpMessage)

{- | Tests for the composition root's boot-time wiring. They exercise the two
promises of the slice: a valid configuration produces served mount bindings with
real packument-serve dependencies (so packuments are merged, not @501@ stubs), and
every boot problem — an unresolved rule policy, a configured mount with no adapter,
a credential reference that does not resolve — is a fail-fast, __aggregated__ boot
error. Pure of IO: the clock and the ecosystem-to-adapter resolver are injected, so
nothing here opens a socket.
-}
spec :: Spec
spec = do
    credentialProvidersSpec
    mirrorWriteProviderSpec
    cacheConfigSpec
    composeBindingsSpec
    bootErrorSpec
    renderSpec

-- ── fixtures ──────────────────────────────────────────────────────────────────

-- | A fixed clock for the injected 'pdNow'; never advanced (no timing here).
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 6 23) 0

{- | A minimal valid environment with a static mirror-target token, so the env
single-mount's @static@ credential reference resolves.
-}
staticEnvVars :: [(String, String)]
staticEnvVars =
    [ ("PRIVATE_UPSTREAM_URL", "https://private.example.test")
    , ("PUBLIC_UPSTREAM_URL", "https://public.example.test")
    , ("MIRROR_TARGET_URL", "https://mirror.example.test")
    , ("MIRROR_QUEUE_URL", "https://sqs.example.test/q")
    , ("MIRROR_TARGET_TOKEN", "mirror-write-token")
    ]

-- | The same environment without the static write token (no provider initialized).
noTokenEnvVars :: [(String, String)]
noTokenEnvVars = filter ((/= "MIRROR_TARGET_TOKEN") . fst) staticEnvVars

expectEnv :: [(String, String)] -> IO EnvConfig
expectEnv = either (\errs -> fail ("env parse failed: " <> show errs)) pure . parseEnvPure

expectDoc :: ByteString -> IO ConfigDoc
expectDoc = either (\e -> fail ("document decode failed: " <> toString e)) pure . decodeDocument

{- | A document mount keyed by the given ecosystem, naming the given credential
backend — for the no-adapter and unresolved-credential boot-error cases.
-}
mountDoc :: Text -> Text -> ByteString
mountDoc eco credential =
    encodeUtf8
        ( "{\"mounts\":{\""
            <> eco
            <> "\":{\"privateUpstream\":\"https://priv\",\"publicUpstream\":\"https://pub\",\
               \\"mirrorTarget\":{\"url\":\"https://mir\",\"credential\":\""
            <> credential
            <> "\",\"queue\":\"sqs\"}}}}"
        )

-- Build the served bindings from an env + optional document through 'planMounts',
-- with the real adapter resolver, the fixed clock, and the env's static providers.
planFrom :: EnvConfig -> Maybe ConfigDoc -> IO (Either [BootError] [MountBinding])
planFrom env mDoc = do
    providers <- initCredentialProviders env
    pure (planMounts mountBindingFor (pure fixedNow) providers env mDoc)

-- ── credential providers ──────────────────────────────────────────────────────

credentialProvidersSpec :: Spec
credentialProvidersSpec = describe "initCredentialProviders" $ do
    it "initializes the static provider from MIRROR_TARGET_TOKEN when set" $ do
        env <- expectEnv staticEnvVars
        providers <- initCredentialProviders env
        initializedBackends providers `shouldBe` fromList [StaticCredential]

    it "yields the configured static token through the initialized provider" $ do
        env <- expectEnv staticEnvVars
        providers <- initCredentialProviders env
        case lookupProvider StaticCredential providers of
            Nothing -> expectationFailure "expected an initialized static provider"
            Just provider -> do
                tok <- currentToken provider
                unSecret (authSecret tok) `shouldBe` "mirror-write-token"

    it "initializes no provider when no static token is supplied" $ do
        env <- expectEnv noTokenEnvVars
        providers <- initCredentialProviders env
        initializedBackends providers `shouldBe` mempty
        -- 'CredentialProvider' has no 'Eq'\/'Show' (it is a record of an IO
        -- function), so resolution is asserted through 'isJust' on a 'Bool'.
        isJust (lookupProvider StaticCredential providers) `shouldBe` False
        -- The cloud-minted backends have no leaf in this build, so they never
        -- resolve regardless of configuration.
        isJust (lookupProvider CodeArtifactCredential providers) `shouldBe` False
        isJust (lookupProvider AdcCredential providers) `shouldBe` False

-- ── mirror-write credential selection ─────────────────────────────────────────

mirrorWriteProviderSpec :: Spec
mirrorWriteProviderSpec = describe "mirrorWriteProvider" $ do
    it "selects the initialized static provider as the mirror-write credential" $ do
        env <- expectEnv staticEnvVars
        providers <- initCredentialProviders env
        tok <- currentToken (mirrorWriteProvider providers)
        unSecret (authSecret tok) `shouldBe` "mirror-write-token"

    it "falls back to the empty placeholder when no provider is initialized" $ do
        env <- expectEnv noTokenEnvVars
        providers <- initCredentialProviders env
        tok <- currentToken (mirrorWriteProvider providers)
        unSecret (authSecret tok) `shouldBe` ""

-- ── config-derived cache settings ─────────────────────────────────────────────

cacheConfigSpec :: Spec
cacheConfigSpec = describe "cacheConfigFor" $
    it "maps the env cache tunables onto the metadata CacheConfig" $ do
        env <- expectEnv (("METADATA_CACHE_TTL_SECONDS", "45") : ("METADATA_CACHE_MAX_ENTRIES", "256") : staticEnvVars)
        cacheTtl (cacheConfigFor env) `shouldBe` (45 :: NominalDiffTime)
        cacheMaxEntries (cacheConfigFor env) `shouldBe` 256

-- ── config-driven serving ─────────────────────────────────────────────────────

composeBindingsSpec :: Spec
composeBindingsSpec = describe "planMounts / composeBindings (config-driven serving)" $ do
    it "produces one npm binding with packument-serve deps wired (served, not a 501 stub)" $ do
        env <- expectEnv staticEnvVars
        planFrom env Nothing >>= \case
            Left errs -> expectationFailure ("unexpected boot errors: " <> show errs)
            Right [binding] -> do
                bindingPrefix binding `shouldBe` ("npm" :| [])
                case bindingPackumentDeps binding of
                    Nothing -> expectationFailure "expected packument deps wired, got the 501 stub"
                    Just deps -> do
                        pdPrivateBaseUrl deps `shouldBe` "https://private.example.test"
                        pdPublicBaseUrl deps `shouldBe` "https://public.example.test"
                        -- The mount base is the derived relative prefix, so tarball
                        -- URLs rewrite back through the proxy mount.
                        pdMountBaseUrl deps `shouldBe` "/npm"
                        -- The mirror-target endpoint is wired from the mount's config,
                        -- for the demand-driven mirror job's publish destination.
                        pdMirrorTarget deps `shouldBe` "https://mirror.example.test"
            Right other -> expectationFailure ("expected exactly one binding, got " <> show (length other))

    it "carries the resolved rule policy onto the binding's packument deps" $ do
        env <- expectEnv staticEnvVars
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdRules deps `shouldSatisfy` (not . null)
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads the inbound edge token, clock, and help message onto the deps" $ do
        -- Forces the remaining 'PackumentDeps' fields: the inbound token (from
        -- PROXY_AUTH_TOKEN), the injected clock ('pdNow'), and the operator help
        -- message — all wired by the composition root.
        env <- expectEnv (("PROXY_AUTH_TOKEN", "edge-secret") : ("PROXY_HELP_MESSAGE", "ask #platform") : staticEnvVars)
        providers <- initCredentialProviders env
        config <- expectConfig env Nothing
        case composeBindings mountBindingFor (pure fixedNow) providers config of
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> do
                    fmap unSecret (pdInboundToken deps) `shouldBe` Just "edge-secret"
                    fmap unHelpMessage (pdHelp deps) `shouldBe` Just "ask #platform"
                    served <- pdNow deps
                    served `shouldBe` fixedNow
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "composeBindings is the pure Config -> [MountBinding] builder under planMounts" $ do
        -- The pure builder over an already-loaded Config is the testable core;
        -- planMounts is just loadConfig sequenced into it.
        env <- expectEnv staticEnvVars
        providers <- initCredentialProviders env
        config <- expectConfig env Nothing
        case composeBindings mountBindingFor (pure fixedNow) providers config of
            Right bindings -> map bindingPrefix bindings `shouldBe` ["npm" :| []]
            Left errs -> expectationFailure ("unexpected boot errors: " <> show errs)

-- ── boot fail-fast ────────────────────────────────────────────────────────────

bootErrorSpec :: Spec
bootErrorSpec = describe "planMounts (fail fast at boot)" $ do
    it "fails on an unresolved rule policy (a typo'd rule type)" $ do
        env <- expectEnv staticEnvVars
        doc <- expectDoc "{\"rules\":{\"oops\":{\"type\":\"Nope\"}}}"
        planFrom env (Just doc) >>= \case
            Left errs -> errs `shouldBe` [PolicyBootError (UnknownRuleType "oops" "Nope")]
            Right _ -> expectationFailure "expected a policy boot error"

    it "fails on a configured mount whose ecosystem has no adapter" $ do
        -- A pypi mount: pypi has no registry client or renderer in this build, so
        -- 'mountBindingFor' returns Nothing — a loud boot failure, never a drop.
        env <- expectEnv staticEnvVars
        doc <- expectDoc (mountDoc "pypi" "static")
        planFrom env (Just doc) >>= \case
            Left errs -> errs `shouldBe` [MissingAdapter PyPI]
            Right _ -> expectationFailure "expected a missing-adapter boot error"

    it "fails on a mount naming a credential source with no initialized provider" $ do
        -- A codeartifact mount with no cloud leaf in this build: the credential
        -- reference does not resolve, an honest boot failure.
        env <- expectEnv staticEnvVars
        doc <- expectDoc (mountDoc "npm" "codeartifact")
        planFrom env (Just doc) >>= \case
            Left errs -> errs `shouldBe` [UnresolvedCredential Npm CodeArtifactCredential]
            Right _ -> expectationFailure "expected an unresolved-credential boot error"

    it "aggregates a missing adapter and an unresolved credential on one mount" $ do
        -- A pypi mount naming codeartifact fails both checks; both surface in one run
        -- rather than one-at-a-time.
        env <- expectEnv staticEnvVars
        doc <- expectDoc (mountDoc "pypi" "codeartifact")
        planFrom env (Just doc) >>= \case
            Left errs ->
                errs
                    `shouldMatchList` [ UnresolvedCredential PyPI CodeArtifactCredential
                                      , MissingAdapter PyPI
                                      ]
            Right _ -> expectationFailure "expected aggregated boot errors"

    it "fails the env single-mount when no static provider is initialized" $ do
        -- Without MIRROR_TARGET_TOKEN, the env single-mount's @static@ reference does
        -- not resolve, so even the default npm mount is an honest boot failure.
        env <- expectEnv noTokenEnvVars
        planFrom env Nothing >>= \case
            Left errs -> errs `shouldBe` [UnresolvedCredential Npm StaticCredential]
            Right _ -> expectationFailure "expected an unresolved-credential boot error"

-- ── rendering ─────────────────────────────────────────────────────────────────

renderSpec :: Spec
renderSpec = describe "renderBootError" $
    it "renders each boot-error kind as a distinct operator-facing line" $ do
        renderBootError (PolicyBootError (UnknownRuleType "x" "Y")) `shouldSatisfy` infixed "unknown type"
        renderBootError (MissingAdapter PyPI) `shouldSatisfy` infixed "no adapter"
        renderBootError (UnresolvedCredential Npm CodeArtifactCredential)
            `shouldSatisfy` infixed "not initialized"
  where
    infixed :: Text -> Text -> Bool
    infixed needle hay = needle `T.isInfixOf` hay

-- Build a 'Config' from an env + optional document, failing the test on a policy
-- error (the composeBindings examples want a successfully-loaded config).
expectConfig :: EnvConfig -> Maybe ConfigDoc -> IO Config
expectConfig env mDoc =
    either (\errs -> fail ("config load failed: " <> show errs)) pure (loadConfig env mDoc)
