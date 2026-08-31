-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Endpoint disjointness: which registry roles may share one store, and what each boot role
does when two of them land on the same one.

One table holds every rule, and a rule's verdict per role produces both the boot refusals and
the boot advisories, so no rule can be fatal on one path and missing on the other. The vetted
'PublicationTarget' and 'MirrorStore' come only from a passing walk of that table, so a publish
relay and a store sweep cannot reach an endpoint the table refused.
-}
module Ecluse.Composition.Endpoints (
    -- * The rule table
    RegistryRole (..),
    endpointRefusals,
    endpointAdvisories,

    -- * The vetted endpoints
    PublicationTarget,
    publicationTargetUrl,
    vetPublicationTargets,
    MirrorStore,
    mirrorStoreUrl,
    vetMirrorStores,
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
import Ecluse.Config (MountConfig (..), sameRegistry)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)

{- | The endpoint posture a boot role holds. The proxy and the mirror worker write to a mount's
mirror target, and the Dredger deletes from it, which is what makes a shared store unsafe.
-}
data RegistryRole
    = -- | @ecluse proxy@ and @ecluse mirror@: they read and write, and delete nothing.
      MirrorWriter
    | -- | @ecluse dredger@: it permanently deletes from every mount's mirror target.
      MirrorPruner
    deriving stock (Eq, Show)

{- | Every refusal a role earns from the table. 'Ecluse.Composition.validateComposition' folds
the writing roles' refusals into the aggregated boot report, and each role adds its own.
-}
endpointRefusals :: RegistryRole -> Map Ecosystem MountConfig -> [BootError]
endpointRefusals role mounts =
    [ refusal collision
    | (rule, collision) <- matchedRules mounts
    , Refuse refusal <- [erVerdict rule role]
    ]

{- | Every advisory a role logs: one line per collapse it tolerates, naming the two keys, the
registry they share, and what the collapse costs.
-}
endpointAdvisories :: RegistryRole -> Map Ecosystem MountConfig -> [Text]
endpointAdvisories role mounts =
    [ advisoryLine collision advice
    | (rule, collision) <- matchedRules mounts
    , Warn advice <- [erVerdict rule role]
    ]

{- | A publication target that holds no other registry role. The publish path relays the
publisher's own credential, so the relay takes this vetted value and never a configured URL.
-}
newtype PublicationTarget = PublicationTarget RegistryUrl
    deriving stock (Eq, Show)

-- | The vetted endpoint the publish relay dials.
publicationTargetUrl :: PublicationTarget -> RegistryUrl
publicationTargetUrl (PublicationTarget url) = url

{- | Vet every mount's declared publication target, or report every refusal at once. The
composition root builds a publish binding from this result, never from the raw configuration.
-}
vetPublicationTargets :: Map Ecosystem MountConfig -> Either [BootError] (Map Ecosystem PublicationTarget)
vetPublicationTargets mounts = case endpointRefusals MirrorWriter mounts of
    [] -> Right (Map.mapMaybe (fmap PublicationTarget . mntPublicationTarget) mounts)
    errs -> Left errs

{- | A mirror target no other configured endpoint holds. Deleting from it destroys nothing
another role owns, which is what a store sweep needs before it may delete anything.
-}
newtype MirrorStore = MirrorStore RegistryUrl
    deriving stock (Eq, Show)

-- | The vetted store a sweep may delete from.
mirrorStoreUrl :: MirrorStore -> RegistryUrl
mirrorStoreUrl (MirrorStore url) = url

{- | Vet every mount's declared mirror target, or report every refusal at once. The store
maintenance capability is built from this result alone, so an overlap cannot reach a delete.
-}
vetMirrorStores :: Map Ecosystem MountConfig -> Either [BootError] (Map Ecosystem MirrorStore)
vetMirrorStores mounts = case endpointRefusals MirrorPruner mounts of
    [] -> Right (Map.mapMaybe (fmap MirrorStore . mntMirrorTarget) mounts)
    errs -> Left errs

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

-- What one role does about a collision a rule matched.
data RuleVerdict
    = -- The role refuses to boot, reporting this refusal.
      Refuse (EndpointCollision -> BootError)
    | -- The role boots and logs the collapse with this consequence.
      Warn Text

-- Two endpoints of the configured mounts that resolve to one registry.
data EndpointCollision = EndpointCollision
    { ecMount :: Ecosystem
    , ecKey :: EndpointKey
    , ecOtherMount :: Ecosystem
    , ecOtherKey :: EndpointKey
    , -- The declared URL both keys resolve to.
      ecRegistry :: RegistryUrl
    }

{- One rule: the endpoint whose role is at stake, the endpoints it must not land on, how the
two are compared, and what each boot role does when they collide. -}
data EndpointRule = EndpointRule
    { erSubject :: EndpointKey
    , erAgainst :: [EndpointKey]
    , erScope :: MountScope
    , erMatch :: RegistryMatch
    , erVerdict :: RegistryRole -> RuleVerdict
    }

{- The endpoint rules, in the order a boot report lists them. A mirror target on another
mount's publication target is the second rule's business, read from the publishing side. -}
endpointRules :: [EndpointRule]
endpointRules =
    [ EndpointRule
        { erSubject = KeyPublicationTarget
        , erAgainst = [KeyPublicUpstream]
        , erScope = AnyMount
        , erMatch = ByHost
        , erVerdict = const (Refuse publicationOnPublicUpstream)
        }
    , EndpointRule
        { erSubject = KeyPublicationTarget
        , erAgainst = [KeyPrivateUpstream, KeyMirrorTarget, KeyPublicationTarget]
        , erScope = OtherMount
        , erMatch = ByRegistry
        , erVerdict = const (Refuse publicationOnMountEndpoint)
        }
    , EndpointRule
        { erSubject = KeyMirrorTarget
        , erAgainst = [KeyPublicUpstream]
        , erScope = AnyMount
        , erMatch = ByHost
        , erVerdict = const (Refuse mirrorOnPublicUpstream)
        }
    , EndpointRule
        { erSubject = KeyMirrorTarget
        , erAgainst = [KeyPrivateUpstream]
        , erScope = AnyMount
        , erMatch = ByRegistry
        , erVerdict = mirrorCollapseVerdict
        }
    , EndpointRule
        { erSubject = KeyMirrorTarget
        , erAgainst = [KeyPublicationTarget]
        , erScope = SameMount
        , erMatch = ByRegistry
        , erVerdict = mirrorCollapseVerdict
        }
    , EndpointRule
        { erSubject = KeyPrivateUpstream
        , erAgainst = [KeyPublicUpstream]
        , erScope = SameMount
        , erMatch = ByRegistry
        , erVerdict = const (Warn "the merge trusts the private leg, so this registry's versions are admitted unfiltered")
        }
    ]

-- A mirror target on another declared endpoint: the deleting role refuses, the writing roles warn.
mirrorCollapseVerdict :: RegistryRole -> RuleVerdict
mirrorCollapseVerdict = \case
    MirrorWriter -> Warn "the Dredger refuses this configuration, so pruning this mirror stays manual"
    MirrorPruner -> Refuse mirrorOnMountEndpoint

-- A publish relays the publisher's own credential, which must never reach a public registry.
publicationOnPublicUpstream :: EndpointCollision -> BootError
publicationOnPublicUpstream collision =
    PublicationTargetOnPublicUpstream (ecMount collision) (ecOtherMount collision)

-- A publish must not be relayed into a role the operator declared for something else.
publicationOnMountEndpoint :: EndpointCollision -> BootError
publicationOnMountEndpoint collision =
    PublicationTargetOnMountEndpoint
        (ecMount collision)
        (ecOtherMount collision)
        (endpointKeyName (ecOtherKey collision))

-- The mirror write carries this proxy's own credential, which must never reach a public registry.
mirrorOnPublicUpstream :: EndpointCollision -> BootError
mirrorOnPublicUpstream collision =
    MirrorTargetOnPublicUpstream (ecMount collision) (ecOtherMount collision)

-- A sweep deletes from the mirror target, so a store another role holds loses that role's data.
mirrorOnMountEndpoint :: EndpointCollision -> BootError
mirrorOnMountEndpoint collision =
    MirrorTargetOnMountEndpoint
        (ecMount collision)
        (ecOtherMount collision)
        (endpointKeyName (ecOtherKey collision))
        (registryUrlText (ecRegistry collision))

-- Every collision the configured mounts match, paired with the rule that matched it.
matchedRules :: Map Ecosystem MountConfig -> [(EndpointRule, EndpointCollision)]
matchedRules mounts =
    [ (rule, EndpointCollision eco (erSubject rule) other otherKey url)
    | rule <- endpointRules
    , (eco, mcfg) <- Map.toAscList mounts
    , Just url <- [endpointOf (erSubject rule) mcfg]
    , (other, otherCfg) <- Map.toAscList mounts
    , withinScope (erScope rule) eco other
    , otherKey <- erAgainst rule
    , Just otherUrl <- [endpointOf otherKey otherCfg]
    , sameStore (erMatch rule) url otherUrl
    ]

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
advisoryLine :: EndpointCollision -> Text -> Text
advisoryLine collision advice =
    "mount \""
        <> ecosystemName (ecMount collision)
        <> "\": "
        <> endpointKeyName (ecKey collision)
        <> " and "
        <> otherRef
        <> " resolve to the same registry ("
        <> registryUrlText (ecRegistry collision)
        <> "); "
        <> advice
  where
    otherRef
        | ecOtherMount collision == ecMount collision = endpointKeyName (ecOtherKey collision)
        | otherwise =
            "mount \""
                <> ecosystemName (ecOtherMount collision)
                <> "\" "
                <> endpointKeyName (ecOtherKey collision)
