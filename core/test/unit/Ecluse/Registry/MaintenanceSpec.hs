-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.MaintenanceSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportProtocol, TransportTimeout), tfCause, transportFault)
import Ecluse.Core.Registry.Maintenance (
    DeleteCeiling (AtMost, NoCeiling),
    RetryAdvice (RetryFutile, RetryWorthwhile),
    StoreFault (..),
    VersionOutcome (VersionRemoved, VersionUnreached),
    chunksOfCeiling,
    deleteAll,
    pageAll,
    refusalCode,
    refusalDetail,
    storeRefusal,
    unreachedBatch,
 )
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

spec :: Spec
spec = do
    vocabularySpec
    pagingSpec
    chunkingSpec
    deleteDriveSpec

vocabularySpec :: Spec
vocabularySpec = do
    describe "storeRefusal" $ do
        it "keeps the backend's code and message" $ do
            let refusal = storeRefusal "NOT_FOUND" "no such version"
            refusalCode refusal `shouldBe` "NOT_FOUND"
            refusalDetail refusal `shouldBe` "no such version"

        it "bounds a pathological message to the shared log-line budget" $
            T.compareLength (refusalDetail (storeRefusal "X" (T.replicate 4000 "a"))) 512 `shouldBe` EQ

    describe "unreachedBatch" $ do
        it "gives every version in the batch one outcome" $ do
            let outcomes = unreachedBatch aFault (map version ["1.0.0", "1.1.0", "1.2.0"])
            map (renderVersion . fst) outcomes `shouldBe` ["1.0.0", "1.1.0", "1.2.0"]
            map snd outcomes `shouldBe` replicate 3 (VersionUnreached aFault)

        it "reports nothing for an empty batch" $
            unreachedBatch aFault [] `shouldBe` []

pagingSpec :: Spec
pagingSpec = describe "pageAll" $ do
    it "walks every page in order and returns one listing" $ do
        fetch <- pagesFrom [(Just "p2", ["a"]), (Just "p3", ["b"]), (Nothing, ["c"])]
        pageAll fetch `shouldReturn` Right ["a", "b", "c"]

    it "sends the token the previous page returned" $ do
        seen <- newIORef []
        fetch <- pagesFrom [(Just "p2", ["a"]), (Nothing, ["b"])]
        _ <- pageAll (\token -> modifyIORef' seen (<> [token]) >> fetch token)
        readIORef seen `shouldReturn` [Nothing, Just "p2"]

    it "reports the fault a page returned rather than a short listing" $ do
        fetch <- pagesFrom [(Just "p2", ["a"])]
        outcome <- pageAll (\token -> if isJust token then pure (Left aFault) else fetch token)
        outcome `shouldBe` Left aFault

    it "refuses a store that hands back the token it was just given" $ do
        fetch <- pagesFrom [(Just "p2", ["a"]), (Just "p2", ["b"])]
        outcome <- pageAll fetch
        outcome `shouldSatisfy` isLeft
        either (tfCause . faultTransport) (const TransportTimeout) outcome `shouldBe` TransportProtocol
        either faultRetry (const RetryWorthwhile) outcome `shouldBe` RetryFutile

    it "refuses a cycle through two tokens, which no single-step check would catch" $ do
        fetch <- pagesFrom [(Just "p2", ["a"]), (Just "p3", ["b"]), (Just "p2", ["c"])]
        outcome <- pageAll fetch
        outcome `shouldSatisfy` isLeft

    it "refuses a store that reopens a token several pages later" $ do
        fetch <-
            pagesFrom
                [(Just "p2", ["a"]), (Just "p3", ["b"]), (Just "p4", ["c"]), (Just "p3", ["d"])]
        outcome <- pageAll fetch
        outcome `shouldSatisfy` isLeft

chunkingSpec :: Spec
chunkingSpec = describe "chunksOfCeiling" $ do
    it "splits a batch at the ceiling and keeps the remainder" $ do
        let chunks = chunksOfCeiling (AtMost 100) [1 :: Int .. 250]
        map length chunks `shouldBe` [100, 100, 50]
        concat chunks `shouldBe` [1 .. 250]

    it "leaves a batch inside the ceiling whole, and an empty one empty" $ do
        chunksOfCeiling (AtMost 100) [1 :: Int .. 3] `shouldBe` [[1, 2, 3]]
        chunksOfCeiling (AtMost 100) ([] :: [Int]) `shouldBe` []

    it "takes one item at a time rather than divide forever on a ceiling below one" $
        chunksOfCeiling (AtMost 0) [1 :: Int .. 3] `shouldBe` [[1], [2], [3]]

    it "sends a batch of any size in one call to a backend with no ceiling" $ do
        chunksOfCeiling NoCeiling [1 :: Int .. 250] `shouldBe` [[1 .. 250]]
        chunksOfCeiling NoCeiling ([] :: [Int]) `shouldBe` []

deleteDriveSpec :: Spec
deleteDriveSpec = describe "deleteAll" $ do
    it "sends every chunk and collects the outcomes in order" $ do
        sent <- newIORef []
        outcomes <- deleteAll (recordingSender sent Nothing) chunks
        map (renderVersion . fst) outcomes `shouldBe` ["1.0.0", "1.1.0", "1.2.0"]
        map (map renderVersion) <$> readIORef sent `shouldReturn` [["1.0.0", "1.1.0"], ["1.2.0"]]

    it "stops sending once a chunk faults, because the fault carries the backend's advice" $ do
        sent <- newIORef []
        _ <- deleteAll (recordingSender sent (Just (version "1.0.0"))) chunks
        map (map renderVersion) <$> readIORef sent `shouldReturn` [["1.0.0", "1.1.0"]]

    it "marks the faulted chunk and every chunk it never sent unreached" $ do
        sent <- newIORef []
        outcomes <- deleteAll (recordingSender sent (Just (version "1.0.0"))) chunks
        map (renderVersion . fst) outcomes `shouldBe` ["1.0.0", "1.1.0", "1.2.0"]
        map snd outcomes `shouldBe` replicate 3 (VersionUnreached aFault)

    it "keeps the outcomes of the chunks that landed before the fault" $ do
        sent <- newIORef []
        outcomes <- deleteAll (recordingSender sent (Just (version "1.2.0"))) chunks
        map snd outcomes
            `shouldBe` [VersionRemoved, VersionRemoved, VersionUnreached aFault]

    it "reports nothing when there is no chunk to send" $ do
        sent <- newIORef []
        deleteAll (recordingSender sent Nothing) [] `shouldReturn` []
  where
    chunks = [[version "1.0.0", version "1.1.0"], [version "1.2.0"]]

{- A sender that records the chunks it was handed and faults on the chunk carrying the
named version, so a spec can place the fault at any point in the run. -}
recordingSender ::
    IORef [[Version]] ->
    Maybe Version ->
    [Version] ->
    IO (Either StoreFault [(Version, VersionOutcome)])
recordingSender sent faultOn batch = do
    modifyIORef' sent (<> [batch])
    pure $
        if maybe False (`elem` batch) faultOn
            then Left aFault
            else Right [(v, VersionRemoved) | v <- batch]

-- A fetch that answers from a fixed page sequence, so a listing walk is drivable in IO.
pagesFrom :: [(Maybe Text, [Text])] -> IO (Maybe Text -> IO (Either StoreFault (Maybe Text, [Text])))
pagesFrom pages = do
    remaining <- newIORef pages
    pure $ \_ ->
        atomicModifyIORef' remaining $ \case
            [] -> ([], Right (Nothing, []))
            (page : rest) -> (rest, Right page)

version :: Text -> Version
version = mkVersion Npm

aFault :: StoreFault
aFault =
    StoreFault
        { faultTransport = transportFault TransportTimeout "the store did not answer"
        , faultRetry = RetryWorthwhile
        }
