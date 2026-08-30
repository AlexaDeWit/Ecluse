-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The cross-mount registry-role refusals: a publication or mirror target that lands on an
endpoint another role already holds.

The publish path relays the publisher's own credential, so a publication target on a
public-upstream host would carry that credential to the public registry, which
@docs\/architecture\/registry-model.md@ forbids outright. A vetted 'PublicationTarget' is the
only value the publish binding accepts, so an unvetted endpoint cannot reach the relay.
-}
module Ecluse.Composition.Endpoints (
    PublicationTarget,
    publicationTargetUrl,
    vetPublicationTargets,
    endpointCollisions,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.BootError (
    BootError (
        MirrorTargetOnPublicUpstream,
        PublicationTargetOnMountEndpoint,
        PublicationTargetOnPublicUpstream
    ),
 )
import Ecluse.Config (MountConfig (..), sameRegistry)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)

{- | A publication target that holds no other registry role. The constructor stays private, so
'vetPublicationTargets' is the only way to obtain one.
-}
newtype PublicationTarget = PublicationTarget RegistryUrl
    deriving stock (Eq, Show)

-- | The vetted endpoint the publish relay dials.
publicationTargetUrl :: PublicationTarget -> RegistryUrl
publicationTargetUrl (PublicationTarget url) = url

{- | Vet every mount's declared publication target, or report all the collisions at once. The
composition root builds a publish binding from this result, never from the raw configuration.
-}
vetPublicationTargets :: Map Ecosystem MountConfig -> Either [BootError] (Map Ecosystem PublicationTarget)
vetPublicationTargets mounts = case concatMap (publicationCollisions mounts) declared of
    [] -> Right (Map.fromList [(eco, PublicationTarget url) | (eco, url) <- declared])
    errs -> Left errs
  where
    declared =
        [(eco, url) | (eco, mcfg) <- Map.toAscList mounts, Just url <- [mntPublicationTarget mcfg]]

{- | Every cross-mount endpoint refusal: the publication-target rules plus the mirror-target
host rule. 'Ecluse.Composition.validateComposition' folds them into the aggregated report.
-}
endpointCollisions :: Map Ecosystem MountConfig -> [BootError]
endpointCollisions mounts =
    fromLeft [] (vetPublicationTargets mounts)
        <> concatMap (mirrorCollisions mounts) (Map.toAscList mounts)

{- One publication target's collisions: any mount's public upstream by host, and any other
mount's endpoints by URL. Its own mount's endpoints are the documented read-back topology. -}
publicationCollisions :: Map Ecosystem MountConfig -> (Ecosystem, RegistryUrl) -> [BootError]
publicationCollisions mounts (eco, target) =
    [ PublicationTargetOnPublicUpstream eco other
    | (other, mcfg) <- Map.toAscList mounts
    , sameHost target (mntPublicUpstream mcfg)
    ]
        <> [ PublicationTargetOnMountEndpoint eco other key
           | (other, mcfg) <- Map.toAscList mounts
           , other /= eco
           , (key, url) <- declaredEndpoints mcfg
           , sameRegistry target url
           ]

{- One mount's mirror-target collisions. Écluse's own write credential rides that leg, so the
public-upstream rule covers it too, as defence in depth. -}
mirrorCollisions :: Map Ecosystem MountConfig -> (Ecosystem, MountConfig) -> [BootError]
mirrorCollisions mounts (eco, mcfg) =
    [ MirrorTargetOnPublicUpstream eco other
    | Just target <- [mntMirrorTarget mcfg]
    , (other, otherCfg) <- Map.toAscList mounts
    , sameHost target (mntPublicUpstream otherCfg)
    ]

-- The endpoints a mount holds, each under the document key that declares it.
declaredEndpoints :: MountConfig -> [(Text, RegistryUrl)]
declaredEndpoints mcfg =
    [ (key, url)
    | (key, mUrl) <-
        [ ("privateUpstream", mntPrivateUpstream mcfg)
        , ("mirrorTarget", mntMirrorTarget mcfg)
        , ("publicationTarget", mntPublicationTarget mcfg)
        ]
    , Just url <- [mUrl]
    ]

{- Whether two endpoints dial the same host, compared case-insensitively. An unreadable authority
yields the empty host, which matches no real one, so that endpoint fails later at the relay. -}
sameHost :: RegistryUrl -> RegistryUrl -> Bool
sameHost a b = hostOf a == hostOf b
  where
    hostOf = hostAddress . registryUrlText
