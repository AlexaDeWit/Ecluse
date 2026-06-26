{- | The PyPI registry __wire__ JSON, decoded into a typed model.

This is a __placeholder boundary__, the PyPI counterpart to
"Ecluse.Registry.Npm.Wire". Écluse serves only npm, so the PyPI wire model covers
just the one shape another part of the system reads from a PyPI project: the list
of published version strings, taken from the @releases@ object of the
@\/pypi\/{project}\/json@ response. The per-release metadata (file URLs, digests,
@requires-python@) is left as an opaque 'Value' rather than modelled, so a full
PyPI adapter can grow this module the way the npm wire layer models the packument
without disturbing callers that only want the version listing.

Like the npm wire layer, the decoder is __lenient__: a document with no @releases@
object yields an empty listing rather than a decode failure, so a partial or
unexpected body still parses to "no versions" instead of throwing.
-}
module Ecluse.Registry.Pypi.Wire (
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
