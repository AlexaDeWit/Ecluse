-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Listing selection preserves bounded corpus order and rejects invalid eviction before boot.
module Ecluse.BenchLoad.ListingSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Harness (LoadKnobs (lkWorkingSet), defaultLoadKnobs)
import Ecluse.BenchLoad.Listing (ListingSetup (..), cacheEvictsLoad, listingWorkingSet)
import Ecluse.BenchLoad.Npm (npmPrivateCopy)
import Ecluse.BenchLoad.PrivateCopy (PrivateCopy (pcPackages))
import Ecluse.BenchLoad.PyPI (pypiPrivateCopy)
import Ecluse.Test.Corpus (CorpusPackage (cpPath), cpName)

spec :: Spec
spec = describe "listing setup" $
    for_ [("npm", npmPrivateCopy), ("pypi", pypiPrivateCopy)] $ \(name, copy) -> do
        let setup = rejectedSetup copy
            names count = map cpName (listingWorkingSet setup defaultLoadKnobs{lkWorkingSet = count})
            captured = map cpName (pcPackages copy)
        it (name <> " clamps the working set without changing corpus order") $ do
            names (-1) `shouldBe` take 1 captured
            names 0 `shouldBe` take 1 captured
            names 2 `shouldBe` take 2 captured
            names maxBound `shouldBe` captured
        it (name <> " rejects the eviction bound before loading captures or calling the target") $
            cacheEvictsLoad setup defaultLoadKnobs (const (expectationFailure "invalid eviction reached the target"))
                `shouldThrow` (\(BenchLoadError message) -> message == "rejected eviction")

rejectedSetup :: PrivateCopy -> ListingSetup
rejectedSetup copy =
    ListingSetup
        { lsCopy = copy
        , lsPrivate = \_ _ _ -> throwIO (BenchLoadError "invalid eviction called a private stub")
        , lsCapturePackages = map (\package -> package{cpPath = "bench/corpus/rejected-eviction-must-not-read.json"})
        , lsPreflight = const (expectationFailure "invalid eviction reached preflight")
        , lsEvictionEntries = \_ _ -> Left "rejected eviction"
        , lsRevalidateError = "unused revalidation"
        }
