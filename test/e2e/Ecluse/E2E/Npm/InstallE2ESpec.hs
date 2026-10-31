-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm policy, artifact protocol responses, and mirror metadata on the base topology. Fresh public
and mirror-only installation parity lives in "Ecluse.E2E.Npm.MirrorInstallE2ESpec".
-}
module Ecluse.E2E.Npm.InstallE2ESpec (nativeInstallationContract) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types (Group)
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

import Ecluse.E2E.Fixtures.Npm (
    allowPkg,
    denyPkg,
    deprecatedPkg,
    deprecationNotice,
    externalDependencyPkg,
    headPkg,
    latestPkg,
    mirrorAuthorFields,
    mirrorOmittedAuthorFields,
    mirrorPkg,
    mirrorRegistryDistFields,
    mirrorRegistryFields,
    psName,
    psVersion,
    tamperPkg,
 )
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Install (InstallDriver (..), RefusalDriver (..), installsObserved, refusesWithoutEffects)
import Ecluse.Test.Contract.Mirror (MirrorTagDriver (..), preservesLatest)
import Ecluse.Test.Contract.Serving (artifactHeadCase)

-- | Drive the product image with a real npm client and local stores.
nativeInstallationContract :: ConcreteFixture Group -> DeclaredGroup
nativeInstallationContract fixture = DeclaredGroup (fixtureMetadata fixture) $ \recorder -> aroundAll (timedFixture recorder (withFixture fixture)) (scenarios recorder)

scenarios :: PhaseRecorder -> SpecWith GlobalDataPlane
scenarios recorder = do
    describe "read-only and non-interfering scenarios (shared environment)" $ aroundAllWith (timedFixtureWith recorder withE2E) $ do
        describe "public surface -- install and policy" $ do
            it (caseLabel "npm/install/default-denial" "uses the shipped default to block install scripts and never mirrors the package") $ \e2e -> do
                logs <- proxyContainerLogs e2e
                logs `shouldSatisfy` T.isInfixOf "rule 2: DenyInstallTimeExecution (precedence 300, applies at admission only)"
                refusesWithoutEffects
                    False
                    RefusalDriver
                        { refusalInstall = npmRefused e2e (psName denyPkg)
                        , refusalObserve = verdaccioHasVersionNow e2e (psName denyPkg) (psVersion denyPkg)
                        , refusalSettle = awaitMirrorWindow
                        }

            it (caseLabel "npm/install/external-dependency-denial" "uses the shipped default to deny a version whose dependency names a git source, and never mirrors it") $ \e2e -> do
                logs <- proxyContainerLogs e2e
                logs `shouldSatisfy` T.isInfixOf "rule 1: DenyExternalDependencies (precedence 300, applies at admission only)"
                proxyStatus e2e (npmTarballPath (psName externalDependencyPkg) (psVersion externalDependencyPkg)) `shouldReturn` 403
                refusesWithoutEffects
                    False
                    RefusalDriver
                        { refusalInstall = npmRefused e2e (psName externalDependencyPkg)
                        , refusalObserve = verdaccioHasVersionNow e2e (psName externalDependencyPkg) (psVersion externalDependencyPkg)
                        , refusalSettle = awaitMirrorWindow
                        }

            it (caseLabel "npm/install/lifecycle-script-control" "runs no package lifecycle script during a harness install (defence in depth)") $ \e2e -> do
                installsObserved
                    InstallDriver
                        { installRun = installWithLifecycleProbe e2e
                        , installExpected = False
                        }

            it (caseLabel "npm/install/deprecation-relay" "relays a deprecated version's notice to the client, before and after the worker mirrors the version") $ \e2e -> do
                let name = psName deprecatedPkg
                    ver = psVersion deprecatedPkg
                    installsWithNotice = withNpmProject e2e $ \proj -> do
                        void $
                            npmInstallIn proj name
                                >>= shouldSucceedThroughProxy e2e
                                >>= shouldWarn ("npm warn deprecated " <> name <> "@" <> ver <> ": " <> deprecationNotice)
                        installedVersion proj name `shouldReturn` Just ver
                -- Only this case installs the package, so public alone holds it for the first read.
                verdaccioHasVersionNow e2e name ver `shouldReturn` False
                installsWithNotice
                verdaccioHasVersion e2e name ver `shouldReturn` True
                installsWithNotice

            it (caseLabel "npm/install/deprecation-negative-control" "rejects the deprecation warning assertion for a version without the member") $ \e2e ->
                withNpmProject e2e $ \proj -> do
                    installed <- npmInstallIn proj (psName allowPkg) >>= shouldSucceedThroughProxy e2e
                    shouldWarn ("npm warn deprecated " <> psName allowPkg <> "@" <> psVersion allowPkg <> ": " <> deprecationNotice) installed `shouldThrow` anyException

        describe "server and worker -- the integrity gate" $
            it (caseLabel "npm/install/integrity-refusal" "refuses to mirror an artifact whose bytes fail the integrity gate") $ \e2e -> do
                _ <- proxyGet e2e (npmTarballPath (psName tamperPkg) (psVersion tamperPkg))
                mirrored <- verdaccioMirroredWithinWindow e2e (psName tamperPkg) (psVersion tamperPkg)
                mirrored `shouldBe` False

        describe "protocol behaviours" $
            it (caseLabel "npm/install/head-no-effect" "answers HEAD on a tarball with its size but no body, and enqueues no mirror") $ \e2e -> do
                -- Only this case touches headPkg.
                response <- proxyHead e2e (npmTarballPath (psName headPkg) (psVersion headPkg))
                artifactHeadCase response
                mirrored <- verdaccioMirroredWithinWindow e2e (psName headPkg) (psVersion headPkg)
                mirrored `shouldBe` False

        describe "server and worker -- mirror metadata" $ do
            it (caseLabel "npm/install/metadata-preservation" "mirrors supported installation metadata and omits unknown and public-registry fields") $ \e2e -> do
                let name = psName mirrorPkg
                    ver = psVersion mirrorPkg
                verdaccioHasVersionNow e2e name ver `shouldReturn` False
                void $ npmInstall e2e (name <> "@" <> ver) >>= shouldSucceedThroughProxy e2e
                verdaccioHasVersion e2e name ver `shouldReturn` True
                stored <- verdaccioVersionObject e2e name ver
                stored `shouldSatisfy` isJust
                forM_ stored $ \version -> do
                    forM_ mirrorAuthorFields $ \(field, value) ->
                        (field, KeyMap.lookup field version) `shouldBe` (field, Just value)
                    forM_ mirrorOmittedAuthorFields $ \(field, _) ->
                        (field, KeyMap.lookup field version) `shouldBe` (field, Nothing)
                    KeyMap.lookup "author" version `shouldBe` Just (String ("See " <> stubUrl NpmPublic <> name))
                    forM_ mirrorRegistryFields $ \(field, _) ->
                        (field, KeyMap.lookup field version) `shouldBe` (field, Nothing)
                    KeyMap.lookup "name" version `shouldBe` Just (String name)
                    KeyMap.lookup "version" version `shouldBe` Just (String ver)
                    let dist = case KeyMap.lookup "dist" version of
                            Just (Object o) -> o
                            _ -> mempty
                    forM_ mirrorRegistryDistFields $ \(field, _) ->
                        (field, KeyMap.lookup field dist) `shouldBe` (field, Nothing)
                    KeyMap.member "integrity" dist `shouldBe` True
            it (caseLabel "npm/install/latest-preservation" "keeps the upstream latest on the mirror when an older version is mirrored after it") $ \e2e -> do
                let name = psName latestPkg
                withNpmProject e2e $ \project ->
                    preservesLatest
                        "2.0.0"
                        "1.0.0"
                        MirrorTagDriver
                            { tagInstallVersion = \version -> npmInstallIn project (name <> "@" <> version)
                            , tagInstallLatest = withNpmProject e2e $ \fresh ->
                                (,) <$> npmInstallIn fresh name <*> installedVersion fresh name
                            , tagAwaitVersion = \version -> do
                                void (verdaccioHasVersion e2e name version)
                                verdaccioVersions e2e name
                            , tagReadLatest = verdaccioLatest e2e name
                            , tagReadVersions = verdaccioVersions e2e name
                            }
        describe "first-party publish -- opt-in posture" $
            it (caseLabel "npm/install/publication-unconfigured" "answers a publish with 405 when no publication target is configured") $ \e2e -> do
                status <- proxyPut e2e ("/npm/" <> publishInScopeName)
                status `shouldBe` 405

    describe "MAL source requirements"
        $ for_
            [ ("mal-forced-source-required", "{\"deny-known-malicious\":{\"enabled\":true}}")
            , ("mal-manual-source-required", "{\"manual-mal\":{\"type\":\"DenyIfKnownMalicious\",\"onUnavailable\":\"skip\"}}")
            ]
        $ \(identifier, policy) ->
            it (caseLabel ("npm/install/" <> identifier) "refuses an active MAL rule without a source at check-config and boot") $ \plane ->
                -- "Écluse" in the check-config output shows the image writes non-ASCII text with no locale set.
                for_ [(["check-config"], ["Écluse"]), (["proxy"], [])] $ \(args, printed) -> do
                    run <- runRoleOnce plane [("ECLUSE_SERVER__PUBLIC_URL", "https://proxy.test"), ("ECLUSE_MOUNTS__NPM__ENABLED", "true"), ("ECLUSE_RULES", policy)] args
                    roleExit run `shouldSatisfy` (/= ExitSuccess)
                    for_ (["DenyIfKnownMalicious", "advisories.url"] <> printed) $ \text ->
                        roleOutput run `shouldSatisfy` T.isInfixOf text
