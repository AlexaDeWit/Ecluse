-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm mount on the base topology, driven by a real @npm@ client: install and policy, the
artifact route's protocol answers, the mirror round trip through the worker, and the @405@ that
refuses a publish when no publication target is configured. Every case shares one proxy, and the
mirrored-metadata case reads the store entry the lifecycle case before it wrote.
-}
module Ecluse.E2E.Npm.InstallE2ESpec (spec) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.KeyMap qualified as KeyMap

import Test.Hspec

import Ecluse.E2E.Fixtures.Npm (
    allowPkg,
    denyPkg,
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

-- | Drive the product image with a real npm client and local stores.
spec :: Spec
spec = whenE2EAvailable (aroundAll withGlobalDataPlane scenarios)

scenarios :: SpecWith GlobalDataPlane
scenarios = do
    describe "read-only and non-interfering scenarios (shared environment)" $ aroundAllWith withE2E $ do
        describe "public surface -- install and policy" $ do
            it "installs an allow-listed package end to end" $ \e2e -> do
                void $ npmInstall e2e (psName allowPkg) >>= shouldSucceed

            it "blocks a package that declares an install script, and never mirrors it" $ \e2e -> do
                void $ npmInstall e2e (psName denyPkg) >>= shouldFail
                mirrored <- verdaccioMirroredWithinWindow e2e (psName denyPkg) (psVersion denyPkg)
                mirrored `shouldBe` False

            it "runs no package lifecycle script during a harness install (defence in depth)" $ \e2e -> do
                (installed, scriptRan) <- installWithLifecycleProbe e2e
                void $ shouldSucceed installed
                scriptRan `shouldBe` False

        describe "server↔worker -- the integrity gate" $
            it "refuses to mirror an artifact whose bytes fail the integrity gate" $ \e2e -> do
                -- A tarball request enqueues a mirror on demand. The worker's digest gate must
                -- reject the tampered bytes, so the version never reaches the private mirror.
                _ <- proxyGet e2e (npmTarballPath (psName tamperPkg) (psVersion tamperPkg))
                mirrored <- verdaccioMirroredWithinWindow e2e (psName tamperPkg) (psVersion tamperPkg)
                mirrored `shouldBe` False

        describe "protocol behaviours" $
            it "answers HEAD on a tarball with its size but no body, and enqueues no mirror" $ \e2e -> do
                -- A HEAD relays the upstream headers with no body, so it declares a
                -- Content-Length yet enqueues no mirror. Only this case touches headPkg.
                (status, declared, bodyBytes) <- proxyHead e2e (npmTarballPath (psName headPkg) (psVersion headPkg))
                status `shouldBe` 200
                bodyBytes `shouldBe` 0
                declared `shouldSatisfy` maybe False (> 0)
                mirrored <- verdaccioMirroredWithinWindow e2e (psName headPkg) (psVersion headPkg)
                mirrored `shouldBe` False

        describe "server↔worker -- the full mirror lifecycle" $ do
            it "mirrors a package served from public, then installs it from the mirror with public down" $ \e2e -> do
                let name = psName mirrorPkg
                    ver = psVersion mirrorPkg
                presentBefore <- verdaccioHasVersionNow e2e name ver -- (1) a miss in the private mirror
                presentBefore `shouldBe` False
                withNpmProject e2e $ \proj -> do
                    void $ npmInstallIn proj name >>= shouldSucceed -- (2,3) served from public, writes the lockfile
                    mirrored <- verdaccioHasVersion e2e name ver -- (4) the worker mirrors it to private
                    mirrored `shouldBe` True
                    -- The lockfile pins its dependency too, so that mirror must land before public goes down.
                    verdaccioHasVersion e2e (psName allowPkg) (psVersion allowPkg) `shouldReturn` True
                    void $ withUpstreamPaused e2e (npmCiIn proj) >>= shouldSucceed -- (5) public down → from the mirror
            it "mirrors supported installation metadata and omits unknown and public-registry fields" $ \e2e -> do
                let name = psName mirrorPkg
                    ver = psVersion mirrorPkg
                -- The mirror lifecycle case above seeds the store. This reads the version object
                -- back straight from the store, so the assertion sees what the mirror holds.
                verdaccioHasVersion e2e name ver `shouldReturn` True
                stored <- verdaccioVersionObject e2e name ver
                stored `shouldSatisfy` isJust
                forM_ stored $ \version -> do
                    forM_ mirrorAuthorFields $ \(field, value) ->
                        (field, KeyMap.lookup field version) `shouldBe` (field, Just value)
                    forM_ mirrorOmittedAuthorFields $ \(field, _) ->
                        (field, KeyMap.lookup field version) `shouldBe` (field, Nothing)
                    KeyMap.lookup "author" version `shouldBe` Just (String ("See https://upstream/" <> name))
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
            it "keeps the upstream latest on the mirror when an older version is mirrored after it" $ \e2e -> do
                let name = psName latestPkg
                withNpmProject e2e $ \proj -> do
                    void $ npmInstallIn proj (name <> "@2.0.0") >>= shouldSucceedThroughProxy e2e
                    verdaccioHasVersion e2e name "2.0.0" `shouldReturn` True
                    verdaccioLatest e2e name `shouldReturn` Just "2.0.0"
                    void $ npmInstallIn proj (name <> "@1.0.0") >>= shouldSucceedThroughProxy e2e
                    verdaccioHasVersion e2e name "1.0.0" `shouldReturn` True
                    -- Completion order must not retag: 1.0.0 landing last stays behind 2.0.0.
                    verdaccioLatest e2e name `shouldReturn` Just "2.0.0"
                    verdaccioVersions e2e name `shouldReturn` ["1.0.0", "2.0.0"]
                withNpmProject e2e $ \proj -> do
                    -- The mirror's own tag is asserted on the store above, because one stub fronts
                    -- every registry name and cannot be paused for the public leg alone.
                    void $ npmInstallIn proj name >>= shouldSucceedThroughProxy e2e
                    installedVersion proj name `shouldReturn` Just "2.0.0"
        describe "first-party publish -- opt-in posture" $
            it "answers a publish with 405 when no publication target is configured" $ \e2e -> do
                -- The base topology declares no ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET, so PUT is not an allowed
                -- method. A raw PUT suffices because the 405 precedes any body read.
                status <- proxyPut e2e ("/npm/" <> publishInScopeName)
                status `shouldBe` 405
