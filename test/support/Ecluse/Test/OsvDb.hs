-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE UndecidableInstances #-}

{- | Compile the fixture corpus into a real @osv.db@ artifact.

Serves a corpus version through a local HTTP stub and runs it through Pilot's
actual compiler ('Ecluse.Core.Osv.Compile.compileOsvToSqlite', in @ecluse-core@),
so a suite exercises a genuine artifact rather than a hand-built one.
-}
module Ecluse.Test.OsvDb (
    withFixtureOsvDb,
) where

import Conduit
import Control.Monad.Catch (MonadCatch, MonadMask)
import Katip (Environment (..), Katip (..), KatipContext (..), LogEnv, initLogEnv)
import Network.HTTP.Types.Status (status200)
import System.IO.Temp (withSystemTempDirectory)

import Ecluse.Core.Osv.Compile (compileOsvToSqlite)
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
                        (runCompileM (compileOsvToSqlite Nothing dir "npm" (toString (stubBaseUrl stub) <> "/all.zip")))
                        le
            use dbFile
