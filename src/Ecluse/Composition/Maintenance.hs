-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's store maintenance build: discriminate each vetted mirror store
into the backend that maintains it, then build that backend's handle. The input is the
endpoint-disjointness witness from "Ecluse.Composition.Endpoints" and nothing else, so a
handle that could delete cannot be built for a store another registry role holds. The
discrimination is on the store URL alone, the way the mirror-write credential is derived
("Ecluse.Config.MirrorCredential"), so a store and the backend that sweeps it cannot
diverge. A URL naming no backend this build carries is a refusal rather than a silent
skip, and failures aggregate as 'BootError's so one run reports every store to fix.
-}
module Ecluse.Composition.Maintenance (
    -- * The Dredger's store handles
    resolveStoreMaintenance,

    -- * Internals exported for testing
    StoreBackend (..),
    resolveStoreBackend,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import UnliftIO (tryAny)

import Ecluse.Composition.BootError (BootError (StoreMaintenanceUnavailable))
import Ecluse.Composition.Endpoints (MirrorStore, mirrorStoreUrl)
import Ecluse.Config.MirrorCredential (parseCodeArtifactHost)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Text (displayExceptionT, nonBlank)
import Ecluse.Runtime.Maintenance.CodeArtifact (newCodeArtifactMaintenance)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    codeArtifactFormat,
    formatToken,
 )

{- | Which backend maintains a store, one arm per backend this build carries. A new backend
is one more arm here and one more handle behind it.
-}
newtype StoreBackend = CodeArtifactBackend CodeArtifactStore
    deriving stock (Eq, Show)

{- | Build one handle per vetted mirror store, or the boot errors that block them. Every
store is discriminated before any client is built, so one run reports every unusable URL.
-}
resolveStoreMaintenance :: Map Ecosystem MirrorStore -> IO (Either [BootError] (Map Ecosystem StoreMaintenance))
resolveStoreMaintenance stores = case partitionEithers (map discriminated (Map.toAscList stores)) of
    (errs@(_ : _), _) -> pure (Left errs)
    ([], planned) -> collected <$> traverse built planned
  where
    discriminated (eco, store) = (eco,) <$> resolveStoreBackend eco store

    built (eco, backend) =
        tryAny (buildBackend backend) <&> \case
            Left err ->
                Left
                    ( StoreMaintenanceUnavailable
                        eco
                        ("building its client failed: " <> displayExceptionT err)
                    )
            Right handle -> Right (eco, handle)

    collected results = case partitionEithers results of
        (errs@(_ : _), _) -> Left errs
        ([], handles) -> Right (Map.fromList handles)

{- | Read a vetted store's URL as the backend that maintains it. The host decides which
backend, and only then does that backend read the rest of the URL as its own coordinates.
-}
resolveStoreBackend :: Ecosystem -> MirrorStore -> Either BootError StoreBackend
resolveStoreBackend eco store = case parseCodeArtifactHost (hostAddress raw) of
    Just (domain, owner, region) ->
        CodeArtifactBackend <$> codeArtifactCoordinates eco raw domain owner region
    Nothing ->
        Left (unavailableFor eco "its host names no store maintenance backend this build carries")
  where
    raw = registryUrlText (mirrorStoreUrl store)

-- The live handle for a resolved backend.
buildBackend :: StoreBackend -> IO StoreMaintenance
buildBackend = \case
    CodeArtifactBackend coordinates -> newCodeArtifactMaintenance coordinates

{- The rest of a CodeArtifact endpoint: the format the ecosystem maps to, and the
repository under it. The path's format segment has to be the mount's own, because a
repository's per-format endpoints are separate stores. -}
codeArtifactCoordinates :: Ecosystem -> Text -> Text -> Text -> Text -> Either BootError CodeArtifactStore
codeArtifactCoordinates eco raw domain owner region = do
    format <-
        maybeToRight
            (unavailable ("CodeArtifact has no package format for the " <> ecosystemName eco <> " ecosystem"))
            (codeArtifactFormat eco)
    repository <-
        maybeToRight
            (unavailable ("its path is not a CodeArtifact repository endpoint, /" <> formatToken format <> "/{repository}/"))
            (repositoryOfPath (formatToken format) raw)
    pure
        CodeArtifactStore
            { casDomain = domain
            , casDomainOwner = owner
            , casRegion = region
            , casRepository = repository
            , casFormat = format
            }
  where
    unavailable = unavailableFor eco

unavailableFor :: Ecosystem -> Text -> BootError
unavailableFor = StoreMaintenanceUnavailable

-- The repository a CodeArtifact endpoint path names, under the expected format segment.
repositoryOfPath :: Text -> Text -> Maybe Text
repositoryOfPath format url = case pathSegments url of
    [pathFormat, repository] | pathFormat == format -> nonBlank repository
    _ -> Nothing

-- The non-empty path segments of an absolute URL, which the egress boundary has already vetted.
pathSegments :: Text -> [Text]
pathSegments url = filter (not . T.null) (T.splitOn "/" (T.dropWhile (/= '/') afterAuthority))
  where
    afterAuthority = T.drop 2 (snd (T.breakOn "//" url))
