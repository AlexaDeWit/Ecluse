-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI request paths, metadata negotiation and credentials over shared HTTP mechanics.
module Ecluse.Core.Registry.PyPI.Request (
    -- * The ecosystem's artifact hosts
    pypiArtifactHosts,

    -- * Request building
    simpleIndexRequest,
    simpleIndexRequestUnder,
    artifactRequestByFile,
    artifactRequestByUrl,

    -- * URL building
    simpleIndexUrl,
    artifactFileUrl,
    artifactPath,
) where

import Network.HTTP.Client (Request (requestHeaders))
import Network.HTTP.Types.Header (hAccept, hAcceptEncoding)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.PyPI.Credential (pypiCredential)
import Ecluse.Core.Registry.PyPI.Project (canonicalName)
import Ecluse.Core.Registry.PyPI.Wire (simpleIndexMediaType)
import Ecluse.Core.Registry.Request (BaseRequest, attachCredential, joinPath, parseRequestEither, requestAt)
import Ecluse.Core.Registry.Request qualified as Request
import Ecluse.Core.Server.Path (encodeComponent)

{- | PyPI's canonical artifact hosts, declared to the artifact-host gate so the same-host default
admits the files host without an operator naming it.
-}
pypiArtifactHosts :: [Text]
pypiArtifactHosts = ["https://files.pythonhosted.org"]

-- | Build the Simple-index @GET@ for a project. It fails only on an empty or unparseable base URL.
simpleIndexRequest ::
    Text ->
    Maybe ClientCredential ->
    PackageName ->
    Either UrlFormationError Request
simpleIndexRequest baseUrl credential name = indexGet credential <$> (simpleIndexUrl baseUrl name >>= parseRequestEither)

-- | 'simpleIndexRequest' under a base parsed once, through 'requestAt'.
simpleIndexRequestUnder ::
    BaseRequest ->
    Maybe ClientCredential ->
    PackageName ->
    Either UrlFormationError Request
simpleIndexRequestUnder base credential name = indexGet credential <$> requestAt base (indexPath name)

indexGet :: Maybe ClientCredential -> Request -> Request
indexGet credential base =
    attachCredential pypiCredential credential $
        base
            { requestHeaders =
                (hAccept, simpleIndexMediaType)
                    : (hAcceptEncoding, "gzip")
                    : requestHeaders base
            }

{- | Build the artifact @GET@ at @{baseUrl}\/simple\/{canonical-name}\/{filename}@, the spelling
this mount serves and a private index addresses its own files under.
-}
artifactRequestByFile ::
    Text ->
    Maybe ClientCredential ->
    PackageName ->
    Text ->
    Either UrlFormationError Request
artifactRequestByFile baseUrl credential name filename = do
    url <- artifactFileUrl baseUrl name filename
    artifactRequestByUrl credential url

{- | Build PyPI's artifact @GET@ for the absolute @url@ the projection preserved from the index's
own @files[].url@, so it names no base URL of its own.
-}
artifactRequestByUrl ::
    Maybe ClientCredential ->
    Text ->
    Either UrlFormationError Request
artifactRequestByUrl = Request.artifactRequestByUrl pypiCredential

-- | The Simple-index URL @{baseUrl}\/simple\/{canonical-name}\/@.
simpleIndexUrl :: Text -> PackageName -> Either UrlFormationError Text
simpleIndexUrl baseUrl name = joinPath baseUrl (indexPath name)

indexPath :: PackageName -> Text
indexPath name = projectPath (canonicalName name) <> "/"

-- | The artifact URL @{baseUrl}\/simple\/{canonical-name}\/{encoded-filename}@.
artifactFileUrl :: Text -> PackageName -> Text -> Either UrlFormationError Text
artifactFileUrl baseUrl name filename = joinPath baseUrl (artifactPath (canonicalName name) filename)

{- | @simple\/{canonical-project}\/{file}@, relative to an index root. Each component is
percent-encoded, so a decoded escape cannot reach upstream raw.
-}
artifactPath :: Text -> Text -> Text
artifactPath project filename = projectPath project <> "/" <> encodeComponent filename

-- The project's index path, @simple\/{canonical-name}@.
projectPath :: Text -> Text
projectPath project = "simple/" <> encodeComponent project
