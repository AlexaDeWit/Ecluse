-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Production full-entry accounting for captured bodies after authority rewriting.
module Ecluse.BenchLoad.CacheWeight (accountedFullBytes) where

import Data.ByteString.Lazy qualified as LBS

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations)
import Ecluse.Core.Registry.CachedDocument (npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (digestOf)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Registry.Npm.Request (npmArtifactHosts)
import Ecluse.Core.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts)
import Ecluse.Core.Security (defaultLimits, ecosystemArtifactAuthorities)
import Ecluse.Core.Server.Cache (CacheEntry (..), weighCacheEntry)

-- | Apply the same location filtering as the public read before weighing the retained value.
accountedFullBytes :: Ecosystem -> Text -> PackageName -> LByteString -> Either Text Int
accountedFullBytes ecosystem upstreamBase package bytes = do
    let raw = LBS.toStrict bytes
    (info, document) <-
        first show $
            if ecosystem == Npm
                then second (fst npmCached) <$> projectNpmManifest defaultLimits package raw
                else second (fst pypiSimpleCached) <$> projectPyPIIndex defaultLimits package raw
    let hosts = if ecosystem == Npm then npmArtifactHosts else pypiArtifactHosts
        located = enforceArtifactLocations (ecosystemArtifactAuthorities hosts) upstreamBase info
    pure (weighCacheEntry (CacheEntry located document (fromIntegral (LBS.length bytes)) (digestOf raw)))
