-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The read-only connection the artifact is opened on, and the row decode behind 'CveLookup'.
module Ecluse.Core.Cve.InternalSpec (spec) where

import Database.SQLite.Simple (Connection, Only (..), SQLError, close, execute_, query_)
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)

import Ecluse.Core.Cve (AdvisoryRange (..))
import Ecluse.Core.Cve.Internal (openHardenedConnection, toRange)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Schema (EpssRequirement (..))
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Test.Osv (CorpusVersion (CorpusV1))
import Ecluse.Test.OsvDb (withFixtureOsvDb)

-- Hand the body a hardened connection over the fixture artifact. A rejection of the
-- fixture is a loud test failure. The connection closes on the body's normal exit.
withHardenedConnection :: (Connection -> IO ()) -> IO ()
withHardenedConnection body =
    withFixtureOsvDb CorpusV1 $ \dbFile ->
        openHardenedConnection Npm EpssOptional dbFile >>= \case
            Left rejection -> fail ("fixture artifact unexpectedly rejected: " <> show rejection)
            Right conn -> body conn >> close conn

spec :: Spec
spec = do
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
