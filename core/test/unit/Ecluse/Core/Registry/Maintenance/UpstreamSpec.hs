-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The bounded chain walk every backend that reports its aggregation answers through.
module Ecluse.Core.Registry.Maintenance.UpstreamSpec (spec) where

import Data.List (lookup)
import Test.Hspec

import Ecluse.Core.Registry.Maintenance.Upstream (
    ExternalConnection (ExternalConnection),
    PermissionName (PermissionName),
    RepositoryLinks (RepositoryLinks, rlConnections, rlUpstreams),
    RepositoryName (RepositoryName, repositoryNameText),
    UndecidabilityReason (ChainBoundExceeded, NetworkFailure, NoMechanism),
    UnsafeReason (ConfigurationEvidence, InsufficientPermissions),
    UpstreamSafety (Safe, Undecidable, Unsafe),
    noUpstreamMechanism,
    upstreamCallCeiling,
    upstreamHopCeiling,
    walkUpstreamChain,
 )

spec :: Spec
spec = do
    walkSpec
    mechanismSpec

walkSpec :: Spec
walkSpec = describe "walkUpstreamChain" $ do
    it "reads a repository that aggregates nothing as safe" $
        walkOver [("private", links [] [])] `shouldReturn` (Safe, 1)

    it "reports the repository's own connection to a public registry" $
        fst <$> walkOver [("private", links ["public:npmjs"] [])]
            `shouldReturn` Unsafe (ConfigurationEvidence (RepositoryName "private") (ExternalConnection "public:npmjs"))

    it "reports a connection a repository further along the chain carries" $
        fst <$> walkOver [("private", links [] ["shared"]), ("shared", links ["public:npmjs"] [])]
            `shouldReturn` Unsafe (ConfigurationEvidence (RepositoryName "shared") (ExternalConnection "public:npmjs"))

    it "reads each repository in a cycle once, so a loop terminates" $
        walkOver [("private", links [] ["shared"]), ("shared", links [] ["private"])] `shouldReturn` (Safe, 2)

    it "reads a repository reachable by two paths once" $
        walkOver
            [ ("private", links [] ["left", "right"])
            , ("left", links [] ["shared"])
            , ("right", links [] ["shared"])
            , ("shared", links [] [])
            ]
            `shouldReturn` (Safe, 4)

    it "leaves a chain deeper than the hop ceiling undecided, never safe" $
        fst <$> walkOver (chainOf (upstreamHopCeiling + 2)) `shouldReturn` Undecidable ChainBoundExceeded

    it "leaves a chain wider than the call ceiling undecided, never safe" $ do
        (answer, calls) <- walkOver (fanOf (upstreamCallCeiling + 5))
        answer `shouldBe` Undecidable ChainBoundExceeded
        calls `shouldBe` upstreamCallCeiling

    it "stops at the first hop the reader settled for itself" $ do
        (answer, calls) <- walkOver [("shared", links [] [])]
        answer `shouldBe` Undecidable NetworkFailure
        calls `shouldBe` 1

    it "carries a settled unsafe answer out of the walk" $
        walkUpstreamChain (const (pure (Left refused))) (RepositoryName "private") `shouldReturn` refused

mechanismSpec :: Spec
mechanismSpec =
    describe "noUpstreamMechanism" $
        it "reports a backend that does not report its aggregation as undecided" $
            noUpstreamMechanism `shouldReturn` Undecidable NoMechanism

-- The answer of a backend whose identity may not read the repository at all.
refused :: UpstreamSafety
refused = Unsafe (InsufficientPermissions (PermissionName "codeartifact:DescribeRepository"))

links :: [Text] -> [Text] -> RepositoryLinks
links connections upstreams =
    RepositoryLinks
        { rlConnections = map ExternalConnection connections
        , rlUpstreams = map RepositoryName upstreams
        }

-- A chain of the given length, each repository forwarding to the next and aggregating nothing.
chainOf :: Int -> [(Text, RepositoryLinks)]
chainOf depth = [("private", links [] ["hop1"])] <> [(hop n, links [] [hop (n + 1)]) | n <- [1 .. depth]]
  where
    hop n = "hop" <> show n

-- One repository forwarding to the given number of others, each aggregating nothing.
fanOf :: Int -> [(Text, RepositoryLinks)]
fanOf width = [("private", links [] (map leaf [1 .. width]))] <> [(leaf n, links [] []) | n <- [1 .. width]]
  where
    leaf n = "leaf" <> show n

{- Walk the seeded chain from @private@, counting the reads it made. A repository the case did not
seed is one the backend could not answer for. -}
walkOver :: [(Text, RepositoryLinks)] -> IO (UpstreamSafety, Int)
walkOver chain = do
    calls <- newIORef (0 :: Int)
    answer <- walkUpstreamChain (readLinks calls) (RepositoryName "private")
    (answer,) <$> readIORef calls
  where
    readLinks calls repository = do
        modifyIORef' calls (+ 1)
        pure (maybeToRight (Undecidable NetworkFailure) (lookup (repositoryNameText repository) chain))
