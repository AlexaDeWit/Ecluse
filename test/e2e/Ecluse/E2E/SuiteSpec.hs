{- | The end-to-end scenarios, driven through the real @npm@ CLI against the real
image. One environment is booted for the whole suite ('withE2E' under 'aroundAll');
each case drives the public surface and asserts a client- or mirror-observable
outcome. When the environment is unavailable (no docker / image), every case is
reported @pending@ rather than failed.

See @planning\/slices\/S53-e2e-ecosystem.md@ for the design and the full scenario
list. Graceful-drain is @pending@ here — it tracks the #160 drain work and activates
once the harness can signal the running container and observe the readiness flip.
-}
module Ecluse.E2E.SuiteSpec (spec) where

import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

import Ecluse.E2E.Fixtures (PkgSpec, allowPkg, denyPkg, headPkg, mirrorPkg, psName, psVersion, tamperPkg)
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
        it "mirrors a package served from public, then installs it from the mirror with public down" $ \e2e -> do
            -- The core resilience loop, end to end, as a real upstream-outage scenario.
            -- A package absent from the private mirror but present on public is served
            -- from public (an `npm install` succeeds and writes a lockfile); the worker
            -- mirrors it to the private mirror; then, with the public upstream PAUSED, an
            -- `npm ci` from that lockfile still installs it. `npm ci` fetches the artifact
            -- from the lockfile's `resolved` URL (the proxy's private-first tarball path)
            -- without re-resolving via the packument, so it never touches public — the
            -- success proves the bytes came from the mirror, not a still-reachable public.
            let name = psName mirrorPkg
                ver = psVersion mirrorPkg
            presentBefore <- verdaccioHasVersionNow e2e name ver -- (1) a miss in the private mirror
            presentBefore `shouldBe` False
            withNpmProject e2e $ \proj -> do
                installed <- npmInstallIn proj name -- (2,3) served from public; writes the lockfile
                npmExit installed `shouldBe` ExitSuccess
                mirrored <- verdaccioHasVersion e2e name ver -- (4) the worker mirrors it to private
                mirrored `shouldBe` True
                fromMirror <- withUpstreamPaused e2e (npmCiIn proj) -- (5) public down → from the mirror
                npmExit fromMirror `shouldBe` ExitSuccess

        it "refuses to mirror an artifact whose bytes fail the integrity gate" $ \e2e -> do
            -- A tarball request enqueues a mirror (S15 demand-driven). The proxy serves
            -- the bytes, but the worker's strongest-digest gate must reject them, so the
            -- tampered version never reaches the private mirror.
            _ <- proxyGet e2e (tarballPath tamperPkg)
            mirrored <- verdaccioHasVersion e2e (psName tamperPkg) (psVersion tamperPkg)
            mirrored `shouldBe` False

    describe "protocol behaviours" $ do
        it "answers HEAD on a tarball with its size but no body, and enqueues no mirror" $ \e2e -> do
            -- A HEAD goes through the same gating as the GET path but probes the upstream
            -- as a HEAD and relays the headers with no body (#211/#269): it reports a
            -- Content-Length yet streams zero body bytes, and — serving no bytes to
            -- back-fill — enqueues no mirror. Driven on a package only ever HEADed (never
            -- installed/GET), so the empty mirror is attributable to the HEAD alone.
            (status, declared, bodyBytes) <- proxyHead e2e (tarballPath headPkg)
            status `shouldBe` 200
            bodyBytes `shouldBe` 0
            declared `shouldSatisfy` maybe False (> 0)
            mirrored <- verdaccioHasVersion e2e (psName headPkg) (psVersion headPkg)
            mirrored `shouldBe` False

        it "drains in-flight work on SIGTERM" $ \_e2e ->
            pendingWith "activates with the #160 graceful-drain work"

-- | The mount-relative tarball path for a fixture package's single version.
tarballPath :: PkgSpec -> Text
tarballPath p = "/npm/" <> psName p <> "/-/" <> psName p <> "-" <> psVersion p <> ".tgz"
