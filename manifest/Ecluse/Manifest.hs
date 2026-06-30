{-# LANGUAGE DerivingVia #-}

{- | The capability manifest: a pure assembly of Écluse's OpenAPI 3 document from
the closed serve-route enumeration and the configured mounts.

The manifest is a __capability statement__, not a client-integration contract:
registry clients (npm, pnpm, yarn) hardcode the registry protocol and never read
an API description. So this document exists to say, for a human, /which registry
protocols this one server speaks and exactly what is -- and is not -- supported/,
per ecosystem. It is __statically generated__ from a fixed canonical source and
published as static content; it is __not served__, and there is no route or WAI
wiring for it.

The document is __assembled by hand__ (Écluse routes on a raw @wai@ 'Application',
not Servant, so there is no route table to reflect): the operations are folded
from the closed 'Route' sum over the configured mounts, and the owned response
bodies carry code-first schemas. Three kinds of surface are distinguished:

* __Owned / synthesized__ bodies -- the error\/denial envelope ('ErrorEnvelope')
  and the merged-and-filtered packument ('synthesizedPackumentSchema') -- are
  modelled in full, because Écluse authors them.
* __Opaque pass-through__ -- artifact bytes -- are described as a streamed media
  type and link out rather than reproducing the upstream protocol.
* __Unsupported__ -- @search@ and any unrecognised path -- are first-class
  documented boundaries (a @501@ and the deny-by-default @404@), so a reader
  learns the limit from the manifest, not from an error reply.

== Determinism

The rendered bytes must be __byte-stable__ across runs and machines, so the
published artifact yields a meaningful line-level diff on every contract change.
'renderManifest' pins object-key ordering (sorted) and 'buildOpenApi' is a pure
function of an explicit 'ManifestSource' -- generate from 'canonicalManifestSource'
(fixed mounts and base URL), never a live or environment-derived configuration,
or the output would churn on per-deployment values.

== Schema strategy

The owned error envelope is a code-first type: one @autodocodec@ codec backs both
its @aeson@ instances and its OpenAPI schema, so the /documented/ schema is derived
rather than hand-maintained. The codec backs the documented schema only -- the
denial body the server renders is shaped separately in
"Ecluse.Core.Registry.Npm.Serve", and the two are expected to agree on the
@{"error": …}@ shape (a behavioural correspondence, not one this codec enforces).
The synthesized packument is a further exception: it is an /open/ schema (unlisted
fields relayed unchanged from upstream), which has no clean codec representation, so
it carries a hand-written partial schema. npm's /inbound/ wire decoding stays
lenient hand-rolled @aeson@ ("Ecluse.Core.Registry.Npm.Wire") -- @autodocodec@ is for
what Écluse owns and emits, not for tolerantly parsing someone else's loose document.
-}
module Ecluse.Manifest (
    -- * Inputs
    ManifestSource (..),
    canonicalManifestSource,

    -- * Assembly and rendering
    buildOpenApi,
    renderManifest,

    -- * Owned schemas
    ErrorEnvelope (..),
    errorEnvelopeSchemaName,
    SynthesizedPackument,
    synthesizedPackumentSchema,
    synthesizedPackumentSchemaName,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.HashSet.InsOrd qualified as InsOrdSet
import Data.Text qualified as T

import Autodocodec (HasCodec (codec), object, requiredField, (.=))

-- The 'Autodocodec' data constructor must be in scope: deriving the @aeson@
-- instances via it coerces aeson's default methods (@omitField@\/@omittedField@)
-- through the newtype. The 'ToSchema' deriving needs only the type constructor.
import Autodocodec.DerivingVia (Autodocodec (Autodocodec))
import Autodocodec.OpenAPI.DerivingVia (AutodocodecOpenApi)
import Data.OpenApi (
    AdditionalProperties (AdditionalPropertiesAllowed, AdditionalPropertiesSchema),
    Components (_componentsSchemas),
    HttpStatusCode,
    Info (_infoDescription, _infoTitle, _infoVersion),
    MediaTypeObject (_mediaTypeObjectSchema),
    NamedSchema (NamedSchema),
    OpenApi (_openApiComponents, _openApiInfo, _openApiPaths, _openApiServers, _openApiTags),
    OpenApiType (OpenApiObject, OpenApiString),
    Operation (_operationDescription, _operationRequestBody, _operationResponses, _operationSummary, _operationTags),
    Param (_paramDescription, _paramIn, _paramName, _paramRequired, _paramSchema),
    ParamLocation (ParamPath),
    PathItem (_pathItemGet, _pathItemParameters, _pathItemPut),
    Reference (Reference),
    Referenced (Inline, Ref),
    RequestBody (_requestBodyContent, _requestBodyDescription, _requestBodyRequired),
    Response (_responseContent, _responseDescription),
    Responses (_responsesResponses),
    Schema (_schemaAdditionalProperties, _schemaDescription, _schemaFormat, _schemaProperties, _schemaRequired, _schemaTitle, _schemaType),
    Server (Server, _serverDescription, _serverUrl, _serverVariables),
    Tag (Tag),
    ToSchema (declareNamedSchema),
    toSchema,
 )
import Network.HTTP.Media (MediaType)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName, prefixFor)
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Server.Route (Filename (Filename), Route (Packument, Ping, Publish, Search, Tarball, Unsupported))
import Ecluse.Core.Version (Version, mkVersion)

{- | The explicit inputs the manifest is a pure function of: the server's
externally-reachable base URL (the @servers@ entry artifact URLs resolve against)
and the mounted ecosystems (the manifest's tags and per-mount path grammars).

It is deliberately a narrow value -- the base URL and the set of mounts, not the
proxy's credentials, upstreams, or policy -- so the assembly stays a total,
deterministic function of something trivial to fix for the generator.
-}
data ManifestSource = ManifestSource
    { manifestBaseUrl :: Text
    -- ^ The proxy's externally-reachable base URL (the @servers@ entry).
    , manifestEcosystems :: NonEmpty Ecosystem
    -- ^ The mounted ecosystems, each contributing a tag and its route grammar.
    }
    deriving stock (Eq, Show)

{- | The fixed canonical source the build-time generator runs against. Its base
URL is a stable placeholder and its mounts are the single served @npm@ ecosystem,
so the generated artifact is byte-reproducible across machines and captures
/code-level/ changes rather than per-deployment values.
-}
canonicalManifestSource :: ManifestSource
canonicalManifestSource =
    ManifestSource
        { manifestBaseUrl = "https://registry.ecluse.example"
        , manifestEcosystems = Npm :| []
        }

{- | Assemble the OpenAPI 3 document. A __pure__ function of the source: the
operations are folded from the closed 'Route' enumeration over each mount, the
owned bodies carry their schemas in @components@, and operations are tagged by
ecosystem so a renderer groups the document as "one server, these protocols."
-}
buildOpenApi :: ManifestSource -> OpenApi
buildOpenApi src =
    (mempty :: OpenApi)
        { _openApiInfo = manifestInfo
        , _openApiServers = [server]
        , _openApiPaths = pathsFrom (concatMap ecosystemRoutes (toList (manifestEcosystems src)))
        , _openApiComponents = (mempty :: Components){_componentsSchemas = ownedSchemas}
        , _openApiTags = InsOrdSet.fromList (map ecosystemTag (toList (manifestEcosystems src)))
        }
  where
    server =
        Server
            { _serverUrl = manifestBaseUrl src
            , _serverDescription = Just "The proxy's externally-reachable base URL; served artifact URLs resolve against it."
            , _serverVariables = mempty
            }

manifestInfo :: Info
manifestInfo =
    (mempty :: Info)
        { _infoTitle = "Écluse capability manifest"
        , _infoVersion = "0.1.0"
        , _infoDescription =
            Just
                "Which registry protocols this Écluse server speaks, and exactly what is and is not \
                \supported, per ecosystem. A capability manifest for operators and contributors -- \
                \not a client-integration contract: registry clients hardcode the protocol and never \
                \read this document. Generated statically from the closed serve-route enumeration; \
                \it is not served."
        }

ownedSchemas :: InsOrd.InsOrdHashMap Text Schema
ownedSchemas =
    InsOrd.fromList
        [ (errorEnvelopeSchemaName, toSchema (Proxy :: Proxy ErrorEnvelope))
        , (synthesizedPackumentSchemaName, synthesizedPackumentSchema)
        ]

-- | The tag for an ecosystem (the manifest groups operations by mount).
ecosystemTag :: Ecosystem -> Tag
ecosystemTag eco = Tag (ecosystemName eco) (Just (ecosystemName eco <> " registry protocol coverage")) Nothing

{- | One entry in the fold: the path-template key and a 'PathItem' carrying just
this route's operation (and any path parameters). Entries that share a path key
are merged through 'PathItem''s 'Semigroup' in 'pathsFrom'.
-}
data RouteEntry = RouteEntry
    { rePath :: FilePath
    , rePathItem :: PathItem
    }

{- | Merge the route entries into the paths map, combining same-path entries
(e.g. the packument @GET@ and the publish @PUT@ on @\/{pkg}@) through 'PathItem''s
'Semigroup'.
-}
pathsFrom :: [RouteEntry] -> InsOrd.InsOrdHashMap FilePath PathItem
pathsFrom = foldl' insertEntry InsOrd.empty
  where
    insertEntry acc entry = InsOrd.insertWith (<>) (rePath entry) (rePathItem entry) acc

{- | The route entries an ecosystem contributes. The 'Route' sum is the shared
serve vocabulary, but the /path grammar/ for each route is ecosystem-specific, so
this dispatches on the ecosystem and applies that ecosystem's grammar. Only @npm@
has a served route grammar today; the others contribute nothing rather than
documenting routes the server cannot yet serve.
-}
ecosystemRoutes :: Ecosystem -> [RouteEntry]
ecosystemRoutes = \case
    Npm -> map npmRouteEntry npmRoutes
    PyPI -> []
    RubyGems -> []

{- | One representative value per 'Route' constructor -- the iteration the fold
runs over. The constructor payloads are inert (the manifest documents path
/templates/, not concrete coordinates); 'npmRouteEntry' reads the constructor, not
the payload.
-}
npmRoutes :: [Route]
npmRoutes =
    [ Packument placeholderPackage
    , Tarball placeholderPackage placeholderVersion placeholderFilename
    , Publish placeholderPackage
    , Ping
    , Search
    , Unsupported
    ]
  where
    placeholderPackage :: PackageName
    placeholderPackage = mkPackageName Npm Nothing "package"

    placeholderVersion :: Version
    placeholderVersion = mkVersion Npm "1.0.0"

    placeholderFilename :: Filename
    placeholderFilename = Filename "package-1.0.0.tgz"

{- | Map an npm 'Route' to its manifest entry. __Total__ over the closed 'Route'
sum: adding a constructor without a manifest entry is a compile-time gap here, so
the documented surface cannot silently fall behind what the server routes.
-}
npmRouteEntry :: Route -> RouteEntry
npmRouteEntry = \case
    -- The packument @GET@ carries the @{package}@ path parameter for the path it
    -- shares with the publish @PUT@; the publish entry leaves it off so the merge
    -- does not duplicate it.
    Packument{} -> RouteEntry (npmPath "/{package}") (getItem [packageParam] packumentOperation)
    Tarball{} -> RouteEntry (npmPath "/{package}/-/{filename}") (getItem [packageParam, filenameParam] tarballOperation)
    Publish{} -> RouteEntry (npmPath "/{package}") (putItem [] publishOperation)
    Ping -> RouteEntry (npmPath "/-/ping") (getItem [] pingOperation)
    Search -> RouteEntry (npmPath "/-/v1/search") (getItem [] searchOperation)
    Unsupported -> RouteEntry (npmPath "/{unsupportedPath}") (getItem [unsupportedParam] unsupportedOperation)
  where
    getItem params op = (mempty :: PathItem){_pathItemGet = Just op, _pathItemParameters = map Inline params}
    putItem params op = (mempty :: PathItem){_pathItemPut = Just op, _pathItemParameters = map Inline params}

-- | An npm mount path, prefixed by the mount's derived path prefix (e.g. @\/npm@).
npmPath :: Text -> FilePath
npmPath suffix = toString ("/" <> T.intercalate "/" (toList (prefixFor Npm)) <> suffix)

packageParam :: Param
packageParam = pathParam "package" "The package name, URL-encoded; a scoped name is `@scope%2Fname`."

filenameParam :: Param
filenameParam = pathParam "filename" "The artifact's on-the-wire file name, e.g. `lodash-4.17.21.tgz`."

unsupportedParam :: Param
unsupportedParam = pathParam "unsupportedPath" "Any path under this mount matched by none of the routes above."

pathParam :: Text -> Text -> Param
pathParam name description =
    (mempty :: Param)
        { _paramName = name
        , _paramIn = ParamPath
        , _paramRequired = Just True
        , _paramDescription = Just description
        , _paramSchema = Just (Inline (stringSchema Nothing))
        }

packumentOperation :: Operation
packumentOperation =
    operation
        "Fetch a package's metadata (packument)"
        "Returns Écluse's merged-and-filtered packument: versions merged across upstreams and \
        \gated, each `dist.tarball` rewritten to resolve back through this proxy. With no surviving \
        \version the status follows the most recoverable cause."
        Nothing
        [ (200, jsonResponse "The synthesized packument." synthRef)
        , (403, errorResponse "Every version was withheld by policy or admission, and none survived the merge.")
        , (404, errorResponse "No such package upstream (a forwarded miss).")
        , (500, errorResponse "A permanent or internal inability to decide.")
        , (502, errorResponse "A responding upstream returned a packument for a different package.")
        , (503, errorResponse "A transient upstream or advisory condition; retry (see `Retry-After`).")
        ]

tarballOperation :: Operation
tarballOperation =
    operation
        "Stream a package artifact (tarball)"
        "The artifact bytes are streamed verbatim with bounded memory; the manifest documents the \
        \media type and links out rather than re-specifying the upstream artifact protocol. The \
        \client verifies the bytes against the packument's preserved integrity digest."
        Nothing
        [ (200, octetResponse "The artifact bytes.")
        , (403, errorResponse "Refused by policy, or by admission (a missing or below-floor integrity digest).")
        , (404, errorResponse "The upstream did not have the artifact (a forwarded miss).")
        , (500, errorResponse "A permanent or internal inability to serve.")
        , (503, errorResponse "A transient upstream condition; retry (see `Retry-After`).")
        ]

publishOperation :: Operation
publishOperation =
    operation
        "Publish a first-party package"
        "Relays the publish document to the configured publication target after the anti-shadowing \
        \scope guard. Écluse keys the write on the route's package name, never the document's \
        \self-reported name."
        (Just (Inline publishRequestBody))
        [ (201, plainResponse "The publication target accepted the package (its response is relayed).")
        , (403, errorResponse "The package name is outside the configured publish scopes (anti-shadowing), or refused by policy.")
        , (405, errorResponse "Publishing is not configured (no publication target).")
        ]

pingOperation :: Operation
pingOperation =
    operation
        "Liveness probe"
        "Answered locally with `200` and an empty object; `npm ping` checks the endpoint it talks \
        \to is up, so there is no reason to round-trip upstream."
        Nothing
        [(200, jsonResponse "An empty object." (Inline emptyObjectSchema))]

searchOperation :: Operation
searchOperation =
    operation
        "Package search (not supported)"
        "Search is a first-class documented boundary: a discovery convenience, not an install path, \
        \so Écluse returns `501` and points to the public registry's website rather than scope-creeping \
        \a filtered or pass-through search."
        Nothing
        [(501, errorResponse "Not implemented: search is not supported.")]

unsupportedOperation :: Operation
unsupportedOperation =
    operation
        "Deny by default (unsupported path)"
        "Any request under this mount matched by none of the routes above is denied with `404` -- \
        \deny by default at the routing layer."
        Nothing
        [(404, errorResponse "Unrecognised path; deny by default.")]

operation :: Text -> Text -> Maybe (Referenced RequestBody) -> [(HttpStatusCode, Response)] -> Operation
operation summary description requestBody statuses =
    (mempty :: Operation)
        { _operationTags = InsOrdSet.fromList [ecosystemName Npm]
        , _operationSummary = Just summary
        , _operationDescription = Just description
        , _operationRequestBody = requestBody
        , _operationResponses = (mempty :: Responses){_responsesResponses = InsOrd.fromList [(code, Inline resp) | (code, resp) <- statuses]}
        }

jsonResponse :: Text -> Referenced Schema -> Response
jsonResponse description ref =
    (mempty :: Response){_responseDescription = description, _responseContent = jsonContent ref}

errorResponse :: Text -> Response
errorResponse description = jsonResponse description errorRef

octetResponse :: Text -> Response
octetResponse description =
    (mempty :: Response)
        { _responseDescription = description
        , _responseContent = mediaContent "application/octet-stream" (Inline binarySchema)
        }

plainResponse :: Text -> Response
plainResponse description = (mempty :: Response){_responseDescription = description}

publishRequestBody :: RequestBody
publishRequestBody =
    (mempty :: RequestBody)
        { _requestBodyDescription = Just "The npm publish document (the version manifest plus the base64-encoded tarball in `_attachments`)."
        , _requestBodyRequired = Just True
        , _requestBodyContent = jsonContent (Inline publishDocumentSchema)
        }

jsonContent :: Referenced Schema -> InsOrd.InsOrdHashMap MediaType MediaTypeObject
jsonContent = mediaContent "application/json"

mediaContent :: MediaType -> Referenced Schema -> InsOrd.InsOrdHashMap MediaType MediaTypeObject
mediaContent mediaType ref = InsOrd.singleton mediaType ((mempty :: MediaTypeObject){_mediaTypeObjectSchema = Just ref})

errorRef :: Referenced Schema
errorRef = Ref (Reference errorEnvelopeSchemaName)

synthRef :: Referenced Schema
synthRef = Ref (Reference synthesizedPackumentSchemaName)

{- | The owned model of the client-facing error\/denial body: a single @error@
string carrying the human-facing reason. One @autodocodec@ codec backs both this
type's @aeson@ instances and its OpenAPI schema, so the /documented/ schema is
code-first.

This type backs the manifest's documented schema only. The denial body the server
actually emits is shaped independently by each mount's renderer (npm's
@{"error": …}@ object lives in "Ecluse.Core.Registry.Npm.Serve"); that the rendered
body matches this documented shape is a behavioural correspondence, not an invariant
this codec enforces.
-}
newtype ErrorEnvelope = ErrorEnvelope
    { errorEnvelopeError :: Text
    -- ^ The human-facing reason the request was refused.
    }
    deriving stock (Eq, Show)
    deriving (FromJSON, ToJSON) via (Autodocodec ErrorEnvelope)
    deriving (ToSchema) via (AutodocodecOpenApi ErrorEnvelope)

instance HasCodec ErrorEnvelope where
    codec =
        object "ErrorEnvelope" $
            ErrorEnvelope
                <$> requiredField "error" "The human-facing reason the request was refused." .= errorEnvelopeError

-- | The @components.schemas@ name the error envelope is registered under.
errorEnvelopeSchemaName :: Text
errorEnvelopeSchemaName = "ErrorEnvelope"

{- | A type-level handle for the synthesized packument's hand-written schema. It
has no values: the served packument is built by the npm serve path, not decoded
into a single Haskell type, so this exists only to carry 'synthesizedPackumentSchema'
as a 'ToSchema' instance for schema-validation backstops.
-}
data SynthesizedPackument

instance ToSchema SynthesizedPackument where
    declareNamedSchema _ = pure (NamedSchema (Just synthesizedPackumentSchemaName) synthesizedPackumentSchema)

-- | The @components.schemas@ name the synthesized packument is registered under.
synthesizedPackumentSchemaName :: Text
synthesizedPackumentSchemaName = "SynthesizedPackument"

{- | The hand-written __partial__ schema of the served (merged-and-filtered)
packument -- the documented __trust boundary__.

This is the highest-scrutiny piece of the manifest. It models only the fields
Écluse reads and transforms (@versions@, @dist-tags@, @time@, and each version's
@dist@), and carries @additionalProperties: true@ everywhere: every unlisted field
is __relayed unchanged from the contributing upstream, and the private upstream
wins on a collision__. It is therefore a precise statement of what the gate
touches -- and what it does not -- but a valid instance is __not__ a proof that the
filtered document is internally coherent (that every @dist-tags@ target is a
surviving @versions@ key); that cross-field coherence is not schema-expressible.

It is hand-written, not codec-derived, because an open schema has no clean
@autodocodec@ representation.
-}
synthesizedPackumentSchema :: Schema
synthesizedPackumentSchema =
    (mempty :: Schema)
        { _schemaTitle = Just "Synthesized packument"
        , _schemaType = Just OpenApiObject
        , _schemaDescription =
            Just
                "Écluse's merged-and-filtered view of a package's metadata. Versions are merged across \
                \upstreams and gated (private versions trusted, public versions admitted only by policy), \
                \and each version's `dist.tarball` is rewritten to resolve back through this proxy. Only \
                \the fields Écluse reads and transforms are modelled; every other field is relayed unchanged \
                \from the contributing upstream (the private upstream wins on a collision)."
        , _schemaRequired = ["name", "versions"]
        , _schemaProperties =
            InsOrd.fromList
                [ ("name", Inline (stringSchema (Just "The package name.")))
                , ("dist-tags", Inline distTagsSchema)
                , ("versions", Inline versionsSchema)
                , ("time", Inline timeSchema)
                ]
        , _schemaAdditionalProperties = Just (AdditionalPropertiesAllowed True)
        }
  where
    distTagsSchema =
        (mempty :: Schema)
            { _schemaType = Just OpenApiObject
            , _schemaDescription = Just "Tag to version string. `latest` is repointed to the newest surviving version after the gate."
            , _schemaAdditionalProperties = Just (AdditionalPropertiesSchema (Inline (stringSchema Nothing)))
            }
    versionsSchema =
        (mempty :: Schema)
            { _schemaType = Just OpenApiObject
            , _schemaDescription = Just "Surviving versions, keyed by version string."
            , _schemaAdditionalProperties = Just (AdditionalPropertiesSchema (Inline versionManifestSchema))
            }
    versionManifestSchema =
        (mempty :: Schema)
            { _schemaType = Just OpenApiObject
            , _schemaDescription = Just "A single version's manifest. Only the fields Écluse reads or transforms are modelled; the rest are relayed unchanged."
            , _schemaRequired = ["name", "version", "dist"]
            , _schemaProperties =
                InsOrd.fromList
                    [ ("name", Inline (stringSchema Nothing))
                    , ("version", Inline (stringSchema Nothing))
                    , ("dist", Inline distSchema)
                    ]
            , _schemaAdditionalProperties = Just (AdditionalPropertiesAllowed True)
            }
    distSchema =
        (mempty :: Schema)
            { _schemaType = Just OpenApiObject
            , _schemaDescription = Just "The artifact descriptor. `tarball` is rewritten to resolve through the proxy; `integrity`/`shasum` are preserved byte-for-byte so the client's own check still holds."
            , _schemaRequired = ["tarball"]
            , _schemaProperties =
                InsOrd.fromList
                    [ ("tarball", Inline (stringSchema (Just "Rewritten artifact URL, under this mount's prefix.")))
                    , ("integrity", Inline (stringSchema (Just "Subresource-Integrity string, preserved from upstream.")))
                    , ("shasum", Inline (stringSchema (Just "Legacy SHA-1 digest, preserved from upstream.")))
                    ]
            , _schemaAdditionalProperties = Just (AdditionalPropertiesAllowed True)
            }
    timeSchema =
        (mempty :: Schema)
            { _schemaType = Just OpenApiObject
            , _schemaDescription = Just "Publish timestamps: `created`, `modified`, and one entry per version."
            , _schemaAdditionalProperties = Just (AdditionalPropertiesSchema (Inline dateTimeSchema))
            }
    dateTimeSchema = (mempty :: Schema){_schemaType = Just OpenApiString, _schemaFormat = Just "date-time"}

stringSchema :: Maybe Text -> Schema
stringSchema description = (mempty :: Schema){_schemaType = Just OpenApiString, _schemaDescription = description}

binarySchema :: Schema
binarySchema =
    (mempty :: Schema)
        { _schemaType = Just OpenApiString
        , _schemaFormat = Just "binary"
        , _schemaDescription = Just "Opaque artifact bytes, streamed verbatim."
        }

emptyObjectSchema :: Schema
emptyObjectSchema =
    (mempty :: Schema)
        { _schemaType = Just OpenApiObject
        , _schemaAdditionalProperties = Just (AdditionalPropertiesAllowed False)
        , _schemaDescription = Just "An empty object."
        }

publishDocumentSchema :: Schema
publishDocumentSchema =
    (mempty :: Schema)
        { _schemaType = Just OpenApiObject
        , _schemaAdditionalProperties = Just (AdditionalPropertiesAllowed True)
        , _schemaDescription = Just "The npm publish document, relayed to the publication target (its full shape is npm's, not re-specified here)."
        }

{- | Render the document to __byte-stable__ JSON: object keys are sorted, so the
output is independent of insertion order and reproducible across runs and
machines, and the file ends in a trailing newline for a clean diff.
-}
renderManifest :: OpenApi -> LByteString
renderManifest =
    Pretty.encodePretty'
        Pretty.Config
            { Pretty.confIndent = Pretty.Spaces 2
            , Pretty.confCompare = compare
            , Pretty.confNumFormat = Pretty.Generic
            , Pretty.confTrailingNewline = True
            }
