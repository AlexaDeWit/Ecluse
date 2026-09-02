-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Endpoint disjointness: which registry roles may share one store, and what each boot role
does when two of them land on the same one.

Each rule names the endpoints it compares and its severity per role, and one 'Vet' pass over all
of them yields both the boot refusals and the boot advisories, so no rule can be fatal on one
path and missing on the other. The vetted 'PublicationTarget' and 'MirrorStore' issue only from
a pass that refused nothing, so a publish relay and a store sweep cannot reach a refused endpoint.
-}
module Ecluse.Composition.Endpoints (
    -- * The endpoint pass
    VettedEndpoints (..),
    vetEndpoints,

    -- * The endpoints it clears
    PublicationTarget,
    publicationTargetUrl,
    MirrorStore,
    mirrorStoreUrl,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.BootError (
    BootError (
        MirrorTargetOnMountEndpoint,
        MirrorTargetOnPublicUpstream,
        PublicationTargetOnMountEndpoint,
        PublicationTargetOnPublicUpstream
    ),
 )
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (
    Severity (Advise, Refuse),
    Vet,
    rule,
    vetRole,
 )
import Ecluse.Config (MountConfig (..), sameRegistry)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)

-- | The endpoints one role's pass cleared it to use, keyed by the mount that declares them.
data VettedEndpoints = VettedEndpoints
    { vePublicationTargets :: Map Ecosystem PublicationTarget
    -- ^ Each mount's cleared publish endpoint.
    , veMirrorStores :: Map Ecosystem MirrorStore
    {- ^ The stores a sweep may delete from. A writing role only advises on the collapses that
    make a delete unsafe, so its pass clears none.
    -}
    }

{- | Vet every mount's declared endpoints against each other: the refusals and advisories this
role earns, and the endpoints a pass that refused nothing clears it to use.
-}
vetEndpoints :: Map Ecosystem MountConfig -> Vet VettedEndpoints
vetEndpoints mounts = clearedFor <$> vetRole <* endpointRules mounts
  where
    clearedFor role =
        VettedEndpoints
            { vePublicationTargets = Map.mapMaybe (fmap PublicationTarget . mntPublicationTarget) mounts
            , veMirrorStores = case role of
                MirrorWriter -> Map.empty
                MirrorPruner -> Map.mapMaybe (fmap MirrorStore . mntMirrorTarget) mounts
            }

{- | A publication target that holds no other registry role. The publish path relays the
publisher's own credential, so the relay takes this vetted value and never a configured URL.
-}
newtype PublicationTarget = PublicationTarget RegistryUrl
    deriving stock (Eq, Show)

-- | The vetted endpoint the publish relay dials.
publicationTargetUrl :: PublicationTarget -> RegistryUrl
publicationTargetUrl (PublicationTarget url) = url

{- | A mirror target no other configured endpoint holds. Deleting from it destroys nothing
another role owns, which is what a store sweep needs before it may delete anything.
-}
newtype MirrorStore = MirrorStore RegistryUrl
    deriving stock (Eq, Show)

-- | The vetted store a sweep may delete from.
mirrorStoreUrl :: MirrorStore -> RegistryUrl
mirrorStoreUrl (MirrorStore url) = url

{- Every endpoint rule, in the order a boot report lists them. A mirror target on another mount's
publication target is 'publicationOffNeighbourEndpoints', read from the publishing side. -}
endpointRules :: Map Ecosystem MountConfig -> Vet ()
endpointRules mounts =
    publicationOffPublicUpstreams mounts
        *> publicationOffNeighbourEndpoints mounts
        *> mirrorOffPublicUpstreams mounts
        *> mirrorOffPrivateUpstreams mounts
        *> mirrorOffOwnPublicationTarget mounts
        *> privateOffPublicUpstream mounts

-- A publish relays the publisher's own credential, which must never reach a public registry.
publicationOffPublicUpstreams :: Map Ecosystem MountConfig -> Vet ()
publicationOffPublicUpstreams =
    vetCollisions (const (Refuse publicationOnPublicUpstream)) $
        EndpointComparison KeyPublicationTarget [KeyPublicUpstream] AnyMount ByHost

-- A publish must not be relayed into a role the operator declared for something else.
publicationOffNeighbourEndpoints :: Map Ecosystem MountConfig -> Vet ()
publicationOffNeighbourEndpoints =
    vetCollisions (const (Refuse publicationOnMountEndpoint)) $
        EndpointComparison
            KeyPublicationTarget
            [KeyPrivateUpstream, KeyMirrorTarget, KeyPublicationTarget]
            OtherMount
            ByRegistry

-- The mirror write carries this proxy's own credential, which must never reach a public registry.
mirrorOffPublicUpstreams :: Map Ecosystem MountConfig -> Vet ()
mirrorOffPublicUpstreams =
    vetCollisions (const (Refuse mirrorOnPublicUpstream)) $
        EndpointComparison KeyMirrorTarget [KeyPublicUpstream] AnyMount ByHost

mirrorOffPrivateUpstreams :: Map Ecosystem MountConfig -> Vet ()
mirrorOffPrivateUpstreams =
    vetCollisions mirrorCollapse $
        EndpointComparison KeyMirrorTarget [KeyPrivateUpstream] AnyMount ByRegistry

mirrorOffOwnPublicationTarget :: Map Ecosystem MountConfig -> Vet ()
mirrorOffOwnPublicationTarget =
    vetCollisions mirrorCollapse $
        EndpointComparison KeyMirrorTarget [KeyPublicationTarget] SameMount ByRegistry

privateOffPublicUpstream :: Map Ecosystem MountConfig -> Vet ()
privateOffPublicUpstream =
    vetCollisions (const (advise mergeTrustsPrivate)) $
        EndpointComparison KeyPrivateUpstream [KeyPublicUpstream] SameMount ByRegistry
  where
    mergeTrustsPrivate =
        "the merge trusts the private leg, so this registry's versions are admitted unfiltered"

-- A mirror target on another declared endpoint: the deleting role refuses, the writing roles warn.
mirrorCollapse :: RegistryRole -> Severity EndpointPair
mirrorCollapse = \case
    MirrorWriter -> advise pruningStaysManual
    MirrorPruner -> Refuse mirrorOnMountEndpoint
  where
    pruningStaysManual =
        "the Dredger refuses this configuration, so pruning this mirror stays manual"

{- The advisory builder every rule here goes through, so each line carries the mount, the keys
and the registry 'advisoryLine' names ahead of its consequence clause. -}
advise :: Text -> Severity EndpointPair
advise = Advise . advisoryLine

publicationOnPublicUpstream :: EndpointPair -> BootError
publicationOnPublicUpstream pair =
    PublicationTargetOnPublicUpstream (epMount pair) (epOtherMount pair) (registryUrlText (epUrl pair))

publicationOnMountEndpoint :: EndpointPair -> BootError
publicationOnMountEndpoint pair =
    PublicationTargetOnMountEndpoint
        (epMount pair)
        (epOtherMount pair)
        (endpointKeyName (epOtherKey pair))

mirrorOnPublicUpstream :: EndpointPair -> BootError
mirrorOnPublicUpstream pair =
    MirrorTargetOnPublicUpstream (epMount pair) (epOtherMount pair) (registryUrlText (epUrl pair))

-- A sweep deletes from the mirror target, so a store another role holds loses that role's data.
mirrorOnMountEndpoint :: EndpointPair -> BootError
mirrorOnMountEndpoint pair =
    MirrorTargetOnMountEndpoint
        (epMount pair)
        (epOtherMount pair)
        (endpointKeyName (epOtherKey pair))
        (registryUrlText (epUrl pair))

-- A registry endpoint of a mount, named by the key the configuration declares it under.
data EndpointKey
    = KeyPublicUpstream
    | KeyPrivateUpstream
    | KeyMirrorTarget
    | KeyPublicationTarget
    deriving stock (Eq, Show)

-- Which mounts a rule compares: one mount's own endpoints, its neighbours', or both.
data MountScope = SameMount | OtherMount | AnyMount
    deriving stock (Eq, Show)

{- How a rule compares two endpoints. Full-URL equality is what store identity needs, because
CodeArtifact repositories share one domain and differ only in path. -}
data RegistryMatch = ByHost | ByRegistry
    deriving stock (Eq, Show)

{- Which endpoints a rule puts against each other: the key whose role is at stake, the keys it
must not land on, the mounts those keys are read from, and how the two are compared. -}
data EndpointComparison = EndpointComparison
    { cmpSubject :: EndpointKey
    , cmpAgainst :: [EndpointKey]
    , cmpScope :: MountScope
    , cmpMatch :: RegistryMatch
    }

-- Two declared endpoints a comparison reads side by side, each named by its mount and its key.
data EndpointPair = EndpointPair
    { epMount :: Ecosystem
    , epKey :: EndpointKey
    , epUrl :: RegistryUrl
    , epOtherMount :: Ecosystem
    , epOtherKey :: EndpointKey
    , epOtherUrl :: RegistryUrl
    }

-- One severity applied to every pair a comparison reads, one 'rule' per pair.
vetCollisions :: (RegistryRole -> Severity EndpointPair) -> EndpointComparison -> Map Ecosystem MountConfig -> Vet ()
vetCollisions severity cmp mounts =
    traverse_ (rule severity (collidingPair (cmpMatch cmp))) (comparedPairs cmp mounts)

-- Every pair of declared endpoints a comparison reads, mounts in ascending key order.
comparedPairs :: EndpointComparison -> Map Ecosystem MountConfig -> [EndpointPair]
comparedPairs cmp mounts =
    [ EndpointPair eco (cmpSubject cmp) url other otherKey otherUrl
    | (eco, mcfg) <- Map.toAscList mounts
    , Just url <- [endpointOf (cmpSubject cmp) mcfg]
    , (other, otherCfg) <- Map.toAscList mounts
    , withinScope (cmpScope cmp) eco other
    , otherKey <- cmpAgainst cmp
    , Just otherUrl <- [endpointOf otherKey otherCfg]
    ]

collidingPair :: RegistryMatch -> EndpointPair -> Maybe EndpointPair
collidingPair match pair
    | sameStore match (epUrl pair) (epOtherUrl pair) = Just pair
    | otherwise = Nothing

-- The URL a mount declares under one key. A mount declares no endpoint it does not hold.
endpointOf :: EndpointKey -> MountConfig -> Maybe RegistryUrl
endpointOf key mcfg = case key of
    KeyPublicUpstream -> Just (mntPublicUpstream mcfg)
    KeyPrivateUpstream -> mntPrivateUpstream mcfg
    KeyMirrorTarget -> mntMirrorTarget mcfg
    KeyPublicationTarget -> mntPublicationTarget mcfg

-- Whether a rule compares this pair of mounts.
withinScope :: MountScope -> Ecosystem -> Ecosystem -> Bool
withinScope scope eco other = case scope of
    SameMount -> eco == other
    OtherMount -> eco /= other
    AnyMount -> True

-- Whether two endpoints name one store under a rule's comparison.
sameStore :: RegistryMatch -> RegistryUrl -> RegistryUrl -> Bool
sameStore = \case
    ByHost -> sameHost
    ByRegistry -> sameRegistry

{- Whether two endpoints dial the same host, compared case-insensitively. An unreadable authority
yields the empty host, which matches no real one, so that endpoint fails later at the relay. -}
sameHost :: RegistryUrl -> RegistryUrl -> Bool
sameHost a b = hostOf a == hostOf b
  where
    hostOf = hostAddress . registryUrlText

-- The configuration key a mount declares an endpoint under.
endpointKeyName :: EndpointKey -> Text
endpointKeyName = \case
    KeyPublicUpstream -> "publicUpstream"
    KeyPrivateUpstream -> "privateUpstream"
    KeyMirrorTarget -> "mirrorTarget"
    KeyPublicationTarget -> "publicationTarget"

-- One advisory line: the collapsed pair, the registry they share, and the consequence.
advisoryLine :: Text -> EndpointPair -> Text
advisoryLine advice pair =
    "mount \""
        <> ecosystemName (epMount pair)
        <> "\": "
        <> endpointKeyName (epKey pair)
        <> " and "
        <> otherRef
        <> " resolve to the same registry ("
        <> registryUrlText (epUrl pair)
        <> "); "
        <> advice
  where
    otherRef
        | epOtherMount pair == epMount pair = endpointKeyName (epOtherKey pair)
        | otherwise =
            "mount \""
                <> ecosystemName (epOtherMount pair)
                <> "\" "
                <> endpointKeyName (epOtherKey pair)
