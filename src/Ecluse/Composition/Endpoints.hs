-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Vet declared read and upload coordinates while preserving each role's store-sharing rules.
module Ecluse.Composition.Endpoints (
    -- * The endpoint pass
    VettedEndpoints (..),
    vetEndpoints,

    -- * The endpoints it clears
    PublicationTarget,
    publicationTargetUrl,
    publicationReadUrl,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.BootError (
    Advisory (MirrorTargetOnOwnPublicationTarget, MirrorTargetOnPrivateUpstream),
    BootError (
        MirrorTargetOnMountEndpoint,
        MirrorTargetOnPublicUpstream,
        PrivateUpstreamOnPublicUpstream,
        PublicationTargetOnMountEndpoint,
        PublicationTargetOnPublicUpstream,
        StoreTagConflict
    ),
 )
import Ecluse.Composition.Types (RegistryRole)
import Ecluse.Composition.Vet (
    Severity (Advise, Ignore, Refuse),
    Vet,
    byStoreRole,
    rule,
 )
import Ecluse.Config (
    MirrorEndpoint (meUploadUrl),
    MountConfig (..),
    PrivateEndpoint (preTarget),
    PublicationEndpoint (peTarget, peUploadUrl),
    StoreTag (TagRegistry),
    Target (Target, tgtTag, tgtUrl),
    meTarget,
    sameRegistry,
    storeTagName,
 )
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)

-- | The endpoints one role's pass cleared it to use, keyed by the mount that declares them.
newtype VettedEndpoints = VettedEndpoints
    { vePublicationTargets :: Map Ecosystem PublicationTarget
    -- ^ Each mount's cleared publish endpoint.
    }

-- | Check every declared read and upload coordinate before clearing publication targets.
vetEndpoints :: Map Ecosystem MountConfig -> Vet VettedEndpoints
vetEndpoints mounts = cleared <$ endpointRules mounts
  where
    cleared = VettedEndpoints (Map.mapMaybe (fmap (\endpoint -> PublicationTarget (tgtUrl (peTarget endpoint)) (fromMaybe (tgtUrl (peTarget endpoint)) (peUploadUrl endpoint))) . mntPublicationTarget) mounts)

-- | A publication target cleared for this role. The relay requires this value, never a configured URL.
data PublicationTarget = PublicationTarget RegistryUrl RegistryUrl
    deriving stock (Eq, Show)

-- | The vetted endpoint the publish relay dials.
publicationTargetUrl :: PublicationTarget -> RegistryUrl
publicationTargetUrl (PublicationTarget _ url) = url

-- | The read root remains distinct from the approved publication API base.
publicationReadUrl :: PublicationTarget -> RegistryUrl
publicationReadUrl (PublicationTarget url _) = url

{- Every endpoint rule, in the order a boot report lists them. A mirror target on another mount's
publication target is 'publicationOffNeighbourEndpoints', read from the publishing side. -}
endpointRules :: Map Ecosystem MountConfig -> Vet ()
endpointRules mounts =
    publicationOffPublicUpstreams mounts
        *> publicationOffNeighbourEndpoints mounts
        *> publicationOffOwnPrivateUpstream mounts
        *> mirrorOffPublicUpstreams mounts
        *> mirrorOffPrivateUpstreams mounts
        *> mirrorOffOwnPublicationTarget mounts
        *> privateOffPublicUpstream mounts
        *> oneTagPerStore mounts

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

-- Dredger requires the private read cache to be separate from user publications.
publicationOffOwnPrivateUpstream :: Map Ecosystem MountConfig -> Vet ()
publicationOffOwnPrivateUpstream =
    vetCollisions severity $
        EndpointComparison KeyPublicationTarget [KeyPrivateUpstream] SameMount ByRegistry
  where
    severity = byStoreRole (Refuse publicationOnMountEndpoint) Ignore

-- The mirror write carries this proxy's own credential, which must never reach a public registry.
mirrorOffPublicUpstreams :: Map Ecosystem MountConfig -> Vet ()
mirrorOffPublicUpstreams =
    vetCollisions (const (Refuse mirrorOnPublicUpstream)) $
        EndpointComparison KeyMirrorTarget [KeyPublicUpstream] AnyMount ByHost

mirrorOffPrivateUpstreams :: Map Ecosystem MountConfig -> Vet ()
mirrorOffPrivateUpstreams =
    vetCollisions (mirrorCollapse mirrorOnPrivateUpstream) $
        EndpointComparison KeyMirrorTarget [KeyPrivateUpstream] AnyMount ByRegistry

mirrorOffOwnPublicationTarget :: Map Ecosystem MountConfig -> Vet ()
mirrorOffOwnPublicationTarget =
    vetCollisions (mirrorCollapse mirrorOnOwnPublicationTarget) $
        EndpointComparison KeyMirrorTarget [KeyPublicationTarget] SameMount ByRegistry

privateOffPublicUpstream :: Map Ecosystem MountConfig -> Vet ()
privateOffPublicUpstream =
    vetCollisions (const (Refuse privateOnPublicUpstream)) $
        EndpointComparison KeyPrivateUpstream [KeyPublicUpstream] SameMount ByRegistry

{- One store cannot be two backends. It sits outside the comparison table because it puts every
declared endpoint against every other, and each unordered pair is read once. -}
oneTagPerStore :: Map Ecosystem MountConfig -> Vet ()
oneTagPerStore = traverse_ (rule (const (Refuse storeTagConflict)) divergentTag) . distinctPairs

divergentTag :: EndpointPair -> Maybe EndpointPair
divergentTag pair
    | sameRegistry (tgtUrl subject) (tgtUrl other), tgtTag subject /= tgtTag other = Just pair
    | otherwise = Nothing
  where
    subject = epTarget pair
    other = epOtherTarget pair

storeTagConflict :: EndpointPair -> BootError
storeTagConflict pair =
    StoreTagConflict
        (epMount pair)
        (taggedKeyName (epKey pair) (epTarget pair))
        (epOtherMount pair)
        (taggedKeyName (epOtherKey pair) (epOtherTarget pair))
        (registryUrlText (tgtUrl (epTarget pair)))

{- A mirror target on another declared endpoint: the deleting role refuses, its preview refuses
under the same reading, and the writing roles warn. -}
mirrorCollapse :: (EndpointPair -> Advisory) -> RegistryRole -> Severity EndpointPair
mirrorCollapse toAdvisory = byStoreRole (Refuse mirrorOnMountEndpoint) (Advise toAdvisory)

{- Each advisory carries the mirror target's own URL, so the warning quotes the spelling the
mount configured rather than the endpoint it collided with. -}
mirrorOnPrivateUpstream :: EndpointPair -> Advisory
mirrorOnPrivateUpstream pair =
    MirrorTargetOnPrivateUpstream (epMount pair) (epOtherMount pair) (tgtUrl (epTarget pair))

mirrorOnOwnPublicationTarget :: EndpointPair -> Advisory
mirrorOnOwnPublicationTarget pair =
    MirrorTargetOnOwnPublicationTarget (epMount pair) (tgtUrl (epTarget pair))

publicationOnPublicUpstream :: EndpointPair -> BootError
publicationOnPublicUpstream pair =
    PublicationTargetOnPublicUpstream (epMount pair) (epOtherMount pair) (pairRegistry pair)

publicationOnMountEndpoint :: EndpointPair -> BootError
publicationOnMountEndpoint pair =
    PublicationTargetOnMountEndpoint
        (epMount pair)
        (epOtherMount pair)
        (endpointKeyName (epOtherKey pair))
        (pairRegistry pair)

mirrorOnPublicUpstream :: EndpointPair -> BootError
mirrorOnPublicUpstream pair =
    MirrorTargetOnPublicUpstream (epMount pair) (epOtherMount pair) (pairRegistry pair)

privateOnPublicUpstream :: EndpointPair -> BootError
privateOnPublicUpstream pair =
    PrivateUpstreamOnPublicUpstream (epMount pair) (pairRegistry pair)

-- A sweep deletes from the mirror target, so a store another role holds loses that role's data.
mirrorOnMountEndpoint :: EndpointPair -> BootError
mirrorOnMountEndpoint pair =
    MirrorTargetOnMountEndpoint
        (epMount pair)
        (epOtherMount pair)
        (endpointKeyName (epOtherKey pair))
        (pairRegistry pair)

pairRegistry :: EndpointPair -> Text
pairRegistry = registryUrlText . tgtUrl . epTarget

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
    , epTarget :: Target
    , epOtherMount :: Ecosystem
    , epOtherKey :: EndpointKey
    , epOtherTarget :: Target
    }

vetCollisions :: (RegistryRole -> Severity EndpointPair) -> EndpointComparison -> Map Ecosystem MountConfig -> Vet ()
vetCollisions severity cmp mounts =
    traverse_ (rule severity (collidingPair (cmpMatch cmp))) (comparedPairs cmp mounts)

comparedPairs :: EndpointComparison -> Map Ecosystem MountConfig -> [EndpointPair]
comparedPairs cmp mounts =
    [ EndpointPair eco (cmpSubject cmp) target other otherKey otherTarget
    | (eco, mcfg) <- Map.toAscList mounts
    , target <- endpointsOf (cmpSubject cmp) mcfg
    , (other, otherCfg) <- Map.toAscList mounts
    , withinScope (cmpScope cmp) eco other
    , otherKey <- cmpAgainst cmp
    , otherTarget <- endpointsOf otherKey otherCfg
    ]

-- Each unordered pair of declared endpoints once, so one collision earns one refusal.
distinctPairs :: Map Ecosystem MountConfig -> [EndpointPair]
distinctPairs mounts =
    [ EndpointPair eco key target other otherKey otherTarget
    | (eco, key, target) : rest <- tails (declaredEndpoints mounts)
    , (other, otherKey, otherTarget) <- rest
    ]

declaredEndpoints :: Map Ecosystem MountConfig -> [(Ecosystem, EndpointKey, Target)]
declaredEndpoints mounts =
    [ (eco, key, target)
    | (eco, mcfg) <- Map.toAscList mounts
    , key <- [KeyPublicUpstream, KeyPrivateUpstream, KeyMirrorTarget, KeyPublicationTarget]
    , target <- endpointsOf key mcfg
    ]

collidingPair :: RegistryMatch -> EndpointPair -> Maybe EndpointPair
collidingPair match pair
    | sameStore match (tgtUrl (epTarget pair)) (tgtUrl (epOtherTarget pair)) = Just pair
    | otherwise = Nothing

endpointsOf :: EndpointKey -> MountConfig -> [Target]
endpointsOf key mount = case key of
    KeyPublicUpstream -> [Target TagRegistry (mntPublicUpstream mount)]
    KeyPrivateUpstream -> maybeToList (preTarget <$> mntPrivateUpstream mount)
    KeyMirrorTarget -> maybe [] (\endpoint -> withUpload (meTarget endpoint) (meUploadUrl endpoint)) (mntMirrorTarget mount)
    KeyPublicationTarget -> maybe [] (\endpoint -> withUpload (peTarget endpoint) (peUploadUrl endpoint)) (mntPublicationTarget mount)

withUpload :: Target -> Maybe RegistryUrl -> [Target]
withUpload target upload = target : [target{tgtUrl = url} | url <- maybeToList upload, not (sameRegistry (tgtUrl target) url)]

withinScope :: MountScope -> Ecosystem -> Ecosystem -> Bool
withinScope scope eco other = case scope of
    SameMount -> eco == other
    OtherMount -> eco /= other
    AnyMount -> True

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

endpointKeyName :: EndpointKey -> Text
endpointKeyName = \case
    KeyPublicUpstream -> "publicUpstream"
    KeyPrivateUpstream -> "privateUpstream"
    KeyMirrorTarget -> "mirrorTarget"
    KeyPublicationTarget -> "publicationTarget"

-- The key path down to the tag, the depth a tag refusal must name to be actionable.
taggedKeyName :: EndpointKey -> Target -> Text
taggedKeyName key target = endpointKeyName key <> "." <> storeTagName (tgtTag target)
