{-# LANGUAGE UndecidableInstances #-}

{- | Compile the fixture corpus into a real @osv.db@ artifact.

This lives in @ecluse-test-support-app@ rather than @ecluse-test-support@
because it runs the corpus through Pilot's actual compiler, which is in the
@ecluse@ application library, and the core unit suite's partition forbids that
dependency (see the @ecluse-core-unit@ stanza note in @ecluse.cabal@).
App-tier suites depend on this library and exercise the real artifact; the
core suite keeps to a pure fake lookup.
-}
module Ecluse.Test.OsvDb (
    withFixtureOsvDb,
) where

import Conduit
import Control.Monad.Catch (MonadCatch, MonadMask)
import Katip (Environment (..), Katip (..), KatipContext (..), LogEnv, initLogEnv)
import Network.HTTP.Types.Status (status200)
import System.IO.Temp (withSystemTempDirectory)

import Ecluse.Pilot.Osv.Compile (compileOsvToSqlite)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Test.Osv (CorpusVersion, osvCorpusZip)
import Ecluse.Test.Stub (stubBaseUrl, withStub)

newtype CompileM a = CompileM {runCompileM :: ReaderT LogEnv (ResourceT IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource, MonadThrow, MonadCatch, MonadMask, PrimMonad, MonadUnliftIO)

instance Katip CompileM where
    getLogEnv = CompileM ask
    localLogEnv f (CompileM m) = CompileM (local f m)

instance KatipContext CompileM where
    getKatipContext = pure mempty
    localKatipContext _ m = m
    getKatipNamespace = pure mempty
    localKatipNamespace _ m = m

{- | Serve a corpus version through a local HTTP stub, compile it into a real
@osv.db@ with Pilot's pipeline, and hand the artifact's path to the
continuation. The artifact lives in a temporary directory that is gone when
the continuation returns.
-}
withFixtureOsvDb :: CorpusVersion -> (FilePath -> IO a) -> IO a
withFixtureOsvDb v use = do
    zipBytes <- osvCorpusZip v
    le <- initLogEnv "ecluse-test" (Environment "test")
    withSystemTempDirectory "ecluse-osv-fixture" $ \dir ->
        withStub status200 zipBytes $ \stub -> do
            dbFile <-
                runResourceT $
                    runReaderT
                        (runCompileM (compileOsvToSqlite telemetryDisabled dir "npm" (toString (stubBaseUrl stub) <> "/all.zip")))
                        le
            use dbFile
