-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Whether public content can reach clients through a private repository, and the bounded walk
of its upstream chain a backend answers with. The answer is a value the boot reads, so the words
an operator sees live in the boot renderers rather than here.
-}
module Ecluse.Core.Registry.Maintenance.Upstream (
    -- * The answer
    UpstreamSafety (..),
    UnsafeReason (..),
    UndecidabilityReason (..),
    noUpstreamMechanism,

    -- * What an answer names
    RepositoryName (..),
    ExternalConnection (..),
    PermissionName (..),

    -- * The chain walk
    RepositoryLinks (..),
    walkUpstreamChain,
    upstreamHopCeiling,
    upstreamCallCeiling,
) where

import Data.Set qualified as Set

-- | Whether public content can reach a client through one repository.
data UpstreamSafety
    = -- | Nothing public reaches a client through it.
      Safe
    | -- | Public content reaches a client, or an identity that cannot ask assumed that it does.
      Unsafe UnsafeReason
    | -- | The question stayed open, so the threat stays the operator's.
      Undecidable UndecidabilityReason
    deriving stock (Eq, Show)

-- | What made a repository unsafe to serve private content from.
data UnsafeReason
    = -- | This repository, or one in its chain, carries the named connection to a public registry.
      ConfigurationEvidence RepositoryName ExternalConnection
    | -- | The role's identity may not read the configuration, which fails closed.
      InsufficientPermissions PermissionName
    deriving stock (Eq, Show)

-- | Why an answer stayed open.
data UndecidabilityReason
    = -- | The backend reports no upstream configuration at all.
      NoMechanism
    | -- | The backend did not answer, or faulted before it did.
      NetworkFailure
    | -- | The walk met a ceiling with part of the chain still unread.
      ChainBoundExceeded
    deriving stock (Eq, Show)

-- | The answer of a backend whose control plane reports no upstream configuration.
noUpstreamMechanism :: (Applicative m) => m UpstreamSafety
noUpstreamMechanism = pure (Undecidable NoMechanism)

-- | A repository, as the backend that holds it names it.
newtype RepositoryName = RepositoryName {repositoryNameText :: Text}
    deriving stock (Eq, Ord, Show)

-- | A backend's own name for a connection that admits content from a public registry.
newtype ExternalConnection = ExternalConnection {externalConnectionText :: Text}
    deriving stock (Eq, Ord, Show)

-- | The grant an identity needs to read a repository's configuration, as the backend spells it.
newtype PermissionName = PermissionName {permissionNameText :: Text}
    deriving stock (Eq, Show)

-- | What one repository reported: what it admits from outside, and where it forwards a miss.
data RepositoryLinks = RepositoryLinks
    { rlConnections :: [ExternalConnection]
    , rlUpstreams :: [RepositoryName]
    }
    deriving stock (Eq, Show)

-- | How many repositories deep the walk follows a chain.
upstreamHopCeiling :: Int
upstreamHopCeiling = 10

-- | How many reads one whole walk makes.
upstreamCallCeiling :: Int
upstreamCallCeiling = 25

{- | Walk a repository's upstream chain breadth-first, stopping at the first external connection.
A hop the reader could not read settles the answer, and a ceiling leaves it undecided, never safe.
-}
walkUpstreamChain ::
    (Monad m) =>
    (RepositoryName -> m (Either UpstreamSafety RepositoryLinks)) ->
    RepositoryName ->
    m UpstreamSafety
walkUpstreamChain readLinks start = go 0 (Set.singleton start) [(start, 0 :: Int)]
  where
    go calls seen = \case
        [] -> pure Safe
        (repository, depth) : rest
            | depth > upstreamHopCeiling || calls >= upstreamCallCeiling ->
                pure (Undecidable ChainBoundExceeded)
            | otherwise ->
                readLinks repository >>= \case
                    Left settled -> pure settled
                    Right links -> case rlConnections links of
                        connection : _ -> pure (Unsafe (ConfigurationEvidence repository connection))
                        [] -> follow (calls + 1) seen rest depth links

    -- The visited set is written when a repository is queued, so a cycle never queues one twice.
    follow calls seen rest depth links =
        let fresh = ordNub [next | next <- rlUpstreams links, not (Set.member next seen)]
         in go calls (foldr Set.insert seen fresh) (rest <> [(next, depth + 1) | next <- fresh])
