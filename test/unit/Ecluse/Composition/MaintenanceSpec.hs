-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MaintenanceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Composition.BootError (
    BootError (StoreMaintenanceUnavailable),
    StoreMaintenanceReason (ClientBuildFailed, DeletionNotPermitted, NoProtocolMaintenance),
    renderBootError,
 )
import Ecluse.Composition.Maintenance (
    ClearedBackend (ClearedCodeArtifact, ClearedProtocol),
    ClearedProtocolStore (cpsConsent),
    ResolveMaintenanceAdapter,
    buildStoreMaintenance,
    planStoreMaintenance,
    vetStoreBackends,
 )
import Ecluse.Composition.Support (
    clearedRepository,
    codeArtifactEnvVars,
    expectConfig,
    noMaintenanceBackend,
    overrideEnv,
    staticEnvVars,
    withoutMirrorTargetToken,
    withoutMirrorTargetUrl,
 )
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (
    Config (configMounts),
    DeletionConsent (DeletionWithheld),
    MountMap,
    StoreTag (TagVerdaccio),
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMaintenance), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterMaintenance (AdapterMaintenance, maintenanceAlphabet, maintenanceListing, maintenanceVersionDelete),
 )
import Ecluse.Core.Registry.Maintenance (
    CompletionNotion (CompletesOnCall),
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    DeleteCeiling (AtMost),
    NameAlphabet,
    RefillPosture (RefillPermitted),
    StoreFacts (..),
    StoreMaintenance (storeFacts, verifyConsent),
    noNameAlphabet,
 )
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance), defaultFakeStoreConfig, newFakeStore)

spec :: Spec
spec = do
    passSpec
    protocolSpec
    buildSpec
    planSpec

{- The rule as the boot applies it: over the loaded mounts, under each role. The deleting role
is the one that refuses, and the checker's warning for it is what a writing role leaves behind. -}
passSpec :: Spec
passSpec = describe "vetStoreBackends" $ do
    it "clears the deleting role the backend for each mirror target this build can sweep" $ do
        mounts <- mountsFor codeArtifactEnvVars
        let (advisories, outcome) = vetted MirrorPruner mounts
        advisories `shouldBe` []
        fmap (mapMaybe clearedRepository . Map.elems) outcome
            `shouldBe` Right ["mirror"]
        fmap Map.keys outcome `shouldBe` Right [Npm]

    it "refuses the deleting role a mirror target this build cannot sweep, naming the key" $ do
        mounts <- mountsFor staticEnvVars
        case vetted MirrorPruner mounts of
            ([], Left [err]) -> do
                err `shouldBe` noMaintenanceBackend
                renderBootError err `shouldSatisfy` T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET"
            other -> expectationFailure ("expected the one maintenance refusal, got: " <> show (refusalsOf other))

    it "clears a writing role no backend, and neither refuses nor advises on a target it cannot sweep" $ do
        -- Only the Dredger deletes, so only its pass reads the rule. The checker still names
        -- the Dredger's refusal for this configuration, so an operator learns of it once.
        mounts <- mountsFor staticEnvVars
        clearsNothing (vetted MirrorWriter mounts) `shouldBe` True

    it "clears nothing and refuses nothing for a mount that declares no mirror target" $ do
        mounts <- mountsFor (withoutMirrorTargetUrl (withoutMirrorTargetToken staticEnvVars))
        clearsNothing (vetted MirrorPruner mounts) `shouldBe` True

    it "clears a vendor store with no alphabet when this build ships the ecosystem no adapter" $ do
        -- Unreachable through a real boot, which refuses that mount as MissingAdapter first. The
        -- store still clears, walked as one bucket, rather than the pass inventing an alphabet.
        mounts <- mountsFor codeArtifactEnvVars
        fmap (map clearedAlphabet . Map.elems) (snd (runVet MirrorPruner (vetStoreBackends noAdapter mounts)))
            `shouldBe` Right [noNameAlphabet]

{- The protocol arm: a store with no vendor control plane, swept through the ecosystem's own
verbs. Consent and the ecosystem's verbs are separate refusals, and the writing role reads neither. -}
protocolSpec :: Spec
protocolSpec = describe "vetStoreBackends -- a store swept through the ecosystem protocol" $ do
    it "clears the deleting role a consenting Verdaccio target" $ do
        mounts <- mountsFor (verdaccioEnv "true")
        case vetted MirrorPruner mounts of
            ([], Right cleared) -> map protocolArm (Map.elems cleared) `shouldBe` [True]
            other -> expectationFailure ("expected one cleared protocol store, got: " <> show (refusalsOf other))

    it "refuses the deleting role a Verdaccio target carrying no deletion consent" $ do
        mounts <- mountsFor (verdaccioEnv "false")
        refusalsOf (vetted MirrorPruner mounts)
            `shouldBe` Just [StoreMaintenanceUnavailable Npm (DeletionNotPermitted TagVerdaccio)]

    it "names the consent key an operator must set in that refusal" $ do
        mounts <- mountsFor (verdaccioEnv "false")
        renderedRefusals (vetted MirrorPruner mounts)
            `shouldSatisfy` any (T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION")

    it "refuses the deleting role an ecosystem whose protocol spells no delete" $ do
        -- The rule must not turn on which ecosystem the mount names, so the adapter is
        -- injected and the refusal is the rule's own rather than the registry's.
        mounts <- mountsFor (verdaccioEnv "true")
        refusalsOf (runVet MirrorPruner (vetStoreBackends withoutMaintenance mounts))
            `shouldBe` Just [StoreMaintenanceUnavailable Npm NoProtocolMaintenance]

    it "names the protocol in that refusal" $ do
        mounts <- mountsFor (verdaccioEnv "true")
        renderedRefusals (runVet MirrorPruner (vetStoreBackends withoutMaintenance mounts))
            `shouldSatisfy` any (T.isInfixOf "npm protocol carries no package listing or version delete")

    it "boots every writing role on a configuration the Dredger refuses for either reason" $ do
        withheld <- mountsFor (verdaccioEnv "false")
        clearsNothing (vetted MirrorWriter withheld) `shouldBe` True
        consenting <- mountsFor (verdaccioEnv "true")
        clearsNothing (runVet MirrorWriter (vetStoreBackends withoutMaintenance consenting)) `shouldBe` True

{- The live build of a protocol store's handle. It opens a connection to nothing, so the facts
and the verdicts it supplies are readable without a store to dial. -}
buildSpec :: Spec
buildSpec = describe "buildStoreMaintenance -- a store swept through the ecosystem protocol" $ do
    it "supplies the backend's standing facts under the tag the store was declared with" $ do
        handle <- protocolHandleFor id
        let facts = storeFacts handle
        factBackend facts `shouldBe` "verdaccio"
        factDeleteCeiling facts `shouldBe` AtMost 1
        factRefill facts `shouldBe` RefillPermitted
        factCompletion facts `shouldBe` CompletesOnCall

    it "grants consent on the store the pass cleared" $ do
        handle <- protocolHandleFor id
        verifyConsent handle `shouldReturn` Right ConsentGranted

    it "withholds it, naming the key an operator sets, on a store carrying none" $ do
        handle <- protocolHandleFor (\store -> store{cpsConsent = DeletionWithheld})
        verifyConsent handle >>= \case
            Right (ConsentWithheld descriptor) ->
                descriptor `shouldSatisfy` T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION"
            other -> expectationFailure ("expected a withheld verdict, got: " <> show other)

{- The handle the live builder makes for the cleared Verdaccio store, under a caller's edit of
the witness the pass issued. -}
protocolHandleFor :: (ClearedProtocolStore -> ClearedProtocolStore) -> IO StoreMaintenance
protocolHandleFor edit = do
    cleared <- clearedBackendsFor (verdaccioEnv "true")
    case Map.elems cleared of
        [ClearedProtocol store] -> buildStoreMaintenance defaultLimits (ClearedProtocol (edit store))
        other -> fail ("expected one cleared protocol store, got " <> show (length other))

{- The environment tier over the cleared backends. It builds one handle per store, and its
refusals accumulate rather than stopping at the first store whose client cannot be built. -}
planSpec :: Spec
planSpec = describe "planStoreMaintenance" $ do
    it "builds one handle per cleared store, keyed by the mount that declares it" $ do
        backends <- clearedBackendsFor twoStoreEnv
        outcome <- planStoreMaintenance (\_ _ -> fakeMaintenance <$> newFakeStore defaultFakeStoreConfig) defaultLimits backends
        fmap Map.keys outcome `shouldBe` Right (Map.keys backends)

    it "reports a refusal for every store whose client the environment cannot build" $ do
        backends <- clearedBackendsFor twoStoreEnv
        Map.keys backends `shouldBe` [Npm, PyPI]
        outcome <- planStoreMaintenance (\_ _ -> throwIO NoStoreClient) defaultLimits backends
        case outcome of
            Right _ -> expectationFailure "expected both store builds to refuse"
            Left errs ->
                map withoutBacktrace errs
                    `shouldBe` [ StoreMaintenanceUnavailable Npm (ClientBuildFailed "NoStoreClient")
                               , StoreMaintenanceUnavailable PyPI (ClientBuildFailed "NoStoreClient")
                               ]
  where
    -- 'displayException' appends GHC's backtrace, so the assertion reads the reason's own line.
    withoutBacktrace = \case
        StoreMaintenanceUnavailable eco (ClientBuildFailed detail) ->
            StoreMaintenanceUnavailable eco (ClientBuildFailed (T.takeWhile (/= '\n') detail))
        err -> err

-- | The typed stand-in for amazonka's credential-discovery failure.
data NoStoreClient = NoStoreClient
    deriving stock (Show)

instance Exception NoStoreClient

-- The pass as the boot runs it, over this build's own adapter registry.
vetted :: RegistryRole -> MountMap -> ([Text], Either [BootError] (Map Ecosystem ClearedBackend))
vetted role mounts = runVet role (vetStoreBackends adapterFor mounts)

-- | An ecosystem this build ships no adapter for at all.
noAdapter :: ResolveMaintenanceAdapter
noAdapter _ = Nothing

-- | The bucket alphabet a cleared vendor store carries, and 'noNameAlphabet' for any other arm.
clearedAlphabet :: ClearedBackend -> NameAlphabet
clearedAlphabet = \case
    ClearedCodeArtifact _ alphabet -> alphabet
    ClearedProtocol{} -> noNameAlphabet

-- | This build's adapters with their maintenance slice emptied: an ecosystem that fills neither verb.
withoutMaintenance :: ResolveMaintenanceAdapter
withoutMaintenance eco =
    adapterFor eco <&> \adapter ->
        adapter
            { adapterMaintenance =
                AdapterMaintenance
                    { maintenanceListing = Nothing
                    , maintenanceVersionDelete = Nothing
                    , maintenanceAlphabet = noNameAlphabet
                    }
            }

-- Whether a cleared backend is the protocol arm, which is what a Verdaccio target resolves to.
protocolArm :: ClearedBackend -> Bool
protocolArm = \case
    ClearedProtocol{} -> True
    ClearedCodeArtifact{} -> False

-- A pass that logged nothing and cleared no store: what every writing role's pass looks like.
clearsNothing :: ([Text], Either [BootError] (Map Ecosystem ClearedBackend)) -> Bool
clearsNothing = \case
    ([], Right cleared) -> Map.null cleared
    _ -> False

refusalsOf :: ([Text], Either [BootError] a) -> Maybe [BootError]
refusalsOf = leftToMaybe . snd

renderedRefusals :: ([Text], Either [BootError] a) -> [Text]
renderedRefusals = maybe [] (map renderBootError) . refusalsOf

-- The resolved mounts an environment layer loads to: the input the rule reads.
mountsFor :: [(String, String)] -> IO MountMap
mountsFor env = configMounts <$> expectConfig env Nothing

-- The deleting role's cleared backends for an environment layer, failing the test on a refusal.
clearedBackendsFor :: [(String, String)] -> IO (Map Ecosystem ClearedBackend)
clearedBackendsFor env = do
    mounts <- mountsFor env
    either (\errs -> fail ("the backend rule refused: " <> show errs)) pure (snd (vetted MirrorPruner mounts))

-- | 'codeArtifactEnvVars' with a second mirrored mount, so the plan has two stores to build.
twoStoreEnv :: [(String, String)]
twoStoreEnv =
    overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "true" $
        overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL" pypiInternalEndpoint $
            overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__CODE_ARTIFACT__URL" pypiEndpoint codeArtifactEnvVars

-- | 'staticEnvVars' mirroring to a Verdaccio store, under the written deletion consent.
verdaccioEnv :: String -> [(String, String)]
verdaccioEnv permitDeletion =
    overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION" permitDeletion $
        overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__URL" "https://verdaccio.example.test/" $
            overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN" "write-token" $
                withoutMirrorTargetToken (withoutMirrorTargetUrl staticEnvVars)

pypiEndpoint :: (IsString s) => s
pypiEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/mirror/"

pypiInternalEndpoint :: (IsString s) => s
pypiInternalEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/internal/"
