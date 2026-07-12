-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The PyPI registry __wire__ JSON, decoded into a typed model for the version-capture
oracles.

This is __oracle apparatus__. It decodes a live PyPI response as far as the one shape the
capture path reads: the list of published version strings, taken from the @releases@
object of the @\/pypi\/{project}\/json@ document. "Ecluse.Test.RegistryCapture" dispatches
a PyPI capture through 'projectVersions' to feed the version-ordering differential and to
detect protocol drift against the live registry. The per-release metadata (file URLs,
digests, @requires-python@) is left as an opaque 'Value' rather than modelled.

The PyPI adapter (roadmap #760-766) is born from its own adapter design with its own
production wire module; this decoder serves the test oracles.

The decoder is __lenient__: a document with no @releases@ object yields an empty listing
rather than a decode failure, so a partial or unexpected body still parses to "no
versions" instead of throwing.
-}
module Ecluse.Test.Registry.Pypi.Wire (
    ProjectJson (..),
    projectVersions,
) where

import Data.Aeson (FromJSON (parseJSON), Value, withObject, (.!=), (.:?))
import Data.Map.Strict qualified as Map

{- | A PyPI project's @\/pypi\/{project}\/json@ document, modelled only as far as
its @releases@ map: each key is a published version string, each value the
(unmodelled) array of release files for that version.
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
