-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The PyPI registry __wire__ JSON, decoded into a typed model for the version-capture
oracles.

This is __oracle apparatus__. It decodes a live PyPI response as far as the one shape the
capture path reads. That shape is the list of published version strings, from the
@releases@ object of the @\/pypi\/{project}\/json@ document.
"Ecluse.Test.RegistryCapture" dispatches a PyPI capture through 'projectVersions' to feed
the version-ordering differential and to detect protocol drift against the live registry.
This decoder keeps the per-release metadata (file URLs, digests, @requires-python@) as an
opaque 'Value' instead of modelling it.

This decoder serves the test oracles only. It is not the production PyPI wire module,
which belongs to the PyPI adapter's own design.

The decoder is __lenient__: a document with no @releases@ object yields an empty listing,
not a decode failure. A partial or unexpected body therefore parses to "no versions"
instead of throwing.
-}
module Ecluse.Test.Registry.Pypi.Wire (
    ProjectJson (..),
    projectVersions,
) where

import Data.Aeson (FromJSON (parseJSON), Value, withObject, (.!=), (.:?))
import Data.Map.Strict qualified as Map

{- | A PyPI project's @\/pypi\/{project}\/json@ document, modelled only as far as its
@releases@ map. Each key is a published version string. Each value is the unmodelled
array of release files for that version.
-}
newtype ProjectJson = ProjectJson
    { pjReleases :: Map Text Value
    -- ^ The @releases@ object: a published version string to its (opaque) file list.
    }
    deriving stock (Eq, Show)

instance FromJSON ProjectJson where
    parseJSON = withObject "PyPI project JSON" $ \o ->
        ProjectJson <$> o .:? "releases" .!= mempty

{- | The published version strings of a PyPI project: the keys of its @releases@
map, exactly as PyPI lists them.
-}
projectVersions :: ProjectJson -> [Text]
projectVersions = Map.keys . pjReleases
