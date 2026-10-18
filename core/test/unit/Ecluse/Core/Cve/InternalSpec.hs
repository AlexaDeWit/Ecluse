-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The read-only connection the artifact is opened on, and the row decode behind 'CveLookup'.
module Ecluse.Core.Cve.InternalSpec (spec) where

import Control.Exception (MaskingState (MaskedInterruptible, Unmasked), getMaskingState)
import Database.SQLite.Simple (Connection, Only (..), SQLError, close, execute_, open, query_)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldThrow)
import UnliftIO.Async (cancel, withAsync)
import UnliftIO.Exception (bracket, throwIO)

import Ecluse.Core.Cve (AdvisoryRange (..))
import Ecluse.Core.Cve.Internal (CveDbRejected (CveDbWrongEpoch), acceptConnection, toRange, verifyConnection)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Schema (EpssRequirement (..))
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Test.Osv (CorpusVersion (CorpusV1))
import Ecluse.Test.OsvDb (withFixtureOsvDb)

withHardenedConnection :: (Connection -> IO ()) -> IO ()
withHardenedConnection body =
    withFixtureOsvDb CorpusV1 $ \path ->
        bracket (open path) close $ \conn ->
            verifyConnection Npm EpssOptional conn >>= \case
                Left rejection -> fail ("fixture artifact unexpectedly rejected: " <> show rejection)
                Right () -> body conn

spec :: Spec
spec = do
    describe "connection ownership" $ do
        it "acquires and transfers masked while verification remains unmasked" $ do
            states <- newIORef []
            let record = getMaskingState >>= \maskingState -> modifyIORef' states (<> [maskingState])
            result <- acceptConnection (record >> open ":memory:") close (\_ -> record $> Right ()) (\conn () -> record >> close conn)
            result `shouldBe` Right ()
            readIORef states `shouldReturn` [MaskedInterruptible, Unmasked, MaskedInterruptible]

        it "closes a rejected connection once" $ do
            closes <- newIORef (0 :: Int)
            let release conn = close conn >> modifyIORef' closes (+ 1)
            result <- acceptConnection (open ":memory:") release (\_ -> pure (Left (CveDbWrongEpoch 0) :: Either CveDbRejected ())) (\_ () -> pass)
            result `shouldBe` Left (CveDbWrongEpoch 0)
            readIORef closes `shouldReturn` 1

        it "closes once when verification throws" $ do
            closes <- newIORef (0 :: Int)
            let release conn = close conn >> modifyIORef' closes (+ 1)
                verify _ = throwIO (TestContractEscape "verification") :: IO (Either CveDbRejected ())
            acceptConnection (open ":memory:") release verify (\_ () -> pass)
                `shouldThrow` (\(TestContractEscape _) -> True)
            readIORef closes `shouldReturn` 1

        it "closes once when cancellation interrupts verification" $ do
            closes <- newIORef (0 :: Int)
            verifying <- newEmptyMVar
            blocked <- newEmptyMVar
            let release conn = close conn >> modifyIORef' closes (+ 1)
                verify _ = putMVar verifying () >> takeMVar blocked $> Right ()
            withAsync (acceptConnection (open ":memory:") release verify (\_ () -> pass)) $ \worker -> do
                takeMVar verifying
                cancel worker
            readIORef closes `shouldReturn` 1

    describe "the hardened connection" $ do
        it "refuses writes outright, so no trigger can ever fire through it" $
            withHardenedConnection $ \conn ->
                execute_ conn "INSERT INTO meta (key, value) VALUES ('tampered', '1')"
                    `shouldThrow` \(_ :: SQLError) -> True

        it "validates cell sizes and reads through the pager, not a memory map" $
            withHardenedConnection $ \conn -> do
                cellCheck <- query_ conn "PRAGMA cell_size_check" :: IO [Only Int]
                mmap <- query_ conn "PRAGMA mmap_size" :: IO [Only Int]
                map fromOnly cellCheck `shouldBe` [1]
                map fromOnly mmap `shouldBe` [0]

    describe "toRange" $ do
        let boundOf fixed lastAffected = arUpperBound (toRange ("GHSA-decode", Just "1.0.0", fixed, lastAffected, Just 5.9, Just 0.5))

        it "reads a fixed_version column as an exclusive bound" $
            boundOf (Just "2.0.0") Nothing `shouldBe` FixedBefore "2.0.0"

        it "reads a last_affected_version column as an inclusive bound" $
            boundOf Nothing (Just "2.0.0") `shouldBe` LastAffected "2.0.0"

        it "reads a row with neither bound column as unbounded" $
            boundOf Nothing Nothing `shouldBe` Unbounded

        it "resolves a row that carries both bound columns as the fix" $
            -- The writer fills at most one column, so only a hand-built artifact reaches
            -- this. The decode must still yield exactly one bound.
            boundOf (Just "2.0.0") (Just "3.0.0") `shouldBe` FixedBefore "2.0.0"

        it "carries the row's identity, lower bound, and both scores through unchanged" $
            toRange ("GHSA-decode", Just "1.0.0", Just "2.0.0", Nothing, Just 5.9, Just 0.5)
                `shouldBe` AdvisoryRange "GHSA-decode" (Just 5.9) (Just "1.0.0") (FixedBefore "2.0.0") (Just 0.5)
