{- | The end-to-end scenarios, driven through the real @npm@ CLI against the real
image. One environment is booted for the whole suite ('withE2E' under 'aroundAll');
each case drives the public surface and asserts a client- or mirror-observable
outcome. When the environment is unavailable (no docker / image), every case is
reported @pending@ rather than failed.

See @planning\/slices\/S53-e2e-ecosystem.md@ for the design and the full scenario
list. @HEAD@-no-body and graceful-drain are @pending@ here: they track in-flight
work (the #211\/#269 HEAD fix is not on this base; the #160 drain work) and activate
once that lands.
-}
module Ecluse.E2E.SuiteSpec (spec) where

import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

import Ecluse.E2E.Fixtures (PkgSpec, allowPkg, denyPkg, mirrorPkg, psName, psVersion, tamperPkg)
import Ecluse.E2E.Harness

spec :: Spec
spec = do
    unavailable <- runIO e2eUnavailable
    case unavailable of
        Just reason -> it "end-to-end suite (environment unavailable)" (pendingWith reason)
        Nothing -> aroundAll withE2E scenarios

scenarios :: SpecWith E2E
scenarios = do
    describe "public surface — install and policy" $ do
        it "installs an allow-listed package end to end" $ \e2e -> do
            res <- npmInstall e2e (psName allowPkg)
            npmExit res `shouldBe` ExitSuccess

        it "blocks a package that declares an install script, and never mirrors it" $ \e2e -> do
            res <- npmInstall e2e (psName denyPkg)
            npmExit res `shouldNotBe` ExitSuccess
            mirrored <- verdaccioHasVersion e2e (psName denyPkg) (psVersion denyPkg)
            mirrored `shouldBe` False

    describe "server↔worker — the mirror round-trip" $ do
        it "mirrors an installed package to the private mirror" $ \e2e -> do
            res <- npmInstall e2e (psName mirrorPkg)
            npmExit res `shouldBe` ExitSuccess
            mirrored <- verdaccioHasVersion e2e (psName mirrorPkg) (psVersion mirrorPkg)
            mirrored `shouldBe` True

        it "refuses to mirror an artifact whose bytes fail the integrity gate" $ \e2e -> do
            -- A tarball request enqueues a mirror (S15 demand-driven). The proxy serves
            -- the bytes, but the worker's strongest-digest gate must reject them, so the
            -- tampered version never reaches the private mirror.
            _ <- proxyGet e2e (tarballPath tamperPkg)
            mirrored <- verdaccioHasVersion e2e (psName tamperPkg) (psVersion tamperPkg)
            mirrored `shouldBe` False

    describe "protocol behaviours (tracking in-flight work)" $ do
        it "answers HEAD on a tarball without streaming the body" $ \_e2e ->
            pendingWith "activates with the #211/#269 HEAD fix (not on this base)"

        it "drains in-flight work on SIGTERM" $ \_e2e ->
            pendingWith "activates with the #160 graceful-drain work"

-- | The mount-relative tarball path for a fixture package's single version.
tarballPath :: PkgSpec -> Text
tarballPath p = "/npm/" <> psName p <> "/-/" <> psName p <> "-" <> psVersion p <> ".tgz"
