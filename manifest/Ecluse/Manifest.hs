-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
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

The document is __rendered from the mounted adapters' route grammar__: each
configured ecosystem's 'Ecluse.Core.Registry.Adapter.Types.RegistryAdapter' carries
its serve surface as data ('Ecluse.Core.Registry.Adapter.Types.serveRoutes', a
'RouteSpec' per served 'Route'), the same grammar the server's
'Ecluse.Core.Server.Route.Classifier' routes on. This module walks those specs into
OpenAPI paths and marries each to its owned documentation, so the described surface
is a projection of how the server mounts routes, not a hand-kept parallel copy. The
owned response bodies carry code-first schemas. Three kinds of surface are
distinguished:

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
rather than hand-maintained. The denial body the server emits is shaped separately
in "Ecluse.Core.Registry.Npm.Serve" (the doc-generation tree stays out of the proxy),
but the two are held together: the emitted body and this schema share the one JSON key
('Ecluse.Core.Registry.Npm.Serve.npmErrorKey'), and a correspondence test
("Ecluse.ManifestSpec") holds an emitted body against this schema, so a drift fails the
suite rather than shipping. The synthesized packument is a further exception: it is an /open/ schema (unlisted
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
    routePathKey,

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
import Data.List (nubBy)
import Data.OpenApi (
    AdditionalProperties (AdditionalPropertiesAllowed, AdditionalPropertiesSchema),
    Components (_componentsSchemas),
    Info (_infoDescription, _infoTitle, _infoVersion),
    MediaTypeObject (_mediaTypeObjectSchema),
    NamedSchema (NamedSchema),
    OpenApi (_openApiComponents, _openApiInfo, _openApiPaths, _openApiServers, _openApiTags),
    OpenApiType (OpenApiObject, OpenApiString),
    Operation (_operationDescription, _operationOperationId, _operationRequestBody, _operationResponses, _operationSummary, _operationTags),
    Param (_paramDescription, _paramIn, _paramName, _paramRequired, _paramSchema),
    ParamLocation (ParamPath),
    PathItem (
        _pathItemDelete,
        _pathItemGet,
        _pathItemHead,
        _pathItemOptions,
        _pathItemParameters,
        _pathItemPatch,
        _pathItemPost,
        _pathItemPut,
        _pathItemTrace
    ),
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
import Network.HTTP.Types.Method (StdMethod (..))

import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName, prefixFor)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Types (AdapterServe (serveRoutes), RegistryAdapter (adapterServe))
import Ecluse.Core.Registry.Npm.Serve (npmErrorKey)
import Ecluse.Core.Server.Route (RouteName, unRouteName)
import Ecluse.Core.Server.RouteDoc (
    BodyDoc (ArtifactBody, EmptyObjectBody, ErrorEnvelopeBody, NoBody, PackumentBody, PublishDocumentBody),
    RequestDoc (reqBody, reqDescription, reqRequired),
    ResponseDoc (respBody, respDescription, respStatus),
    RouteDoc (rdDescription, rdRequest, rdResponses, rdSummary),
 )
import Ecluse.Core.Server.RouteSpec (
    ParamSpec (psDescription, psName),
    PathSeg (Lit, Param),
    RouteSpec (rsDoc, rsMethod, rsName, rsPattern),
 )

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
        , _openApiPaths = pathsFrom (concatMap ecosystemRouteSpecs (toList (manifestEcosystems src)))
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

{- | The (path key, spec) entries an ecosystem contributes: its mounted adapter's
declarative route grammar ('Ecluse.Core.Registry.Adapter.Types.serveRoutes'), each
keyed by its rendered path template under the ecosystem's mount prefix. An ecosystem
with no adapter contributes nothing (rather than documenting a route the server
cannot serve), so adding a mount is what adds its routes to the manifest.
-}
ecosystemRouteSpecs :: Ecosystem -> [(Ecosystem, FilePath, RouteSpec)]
ecosystemRouteSpecs eco =
    case adapterFor eco of
        Nothing -> []
        Just adapter ->
            [ (eco, toString (routePathKey (prefixFor eco) spec), spec)
            | spec <- toList (serveRoutes (adapterServe adapter))
            ]

{- | Fold the (path key, spec) entries into the paths map. Specs that render to the
same key (the packument @GET@ and the publish @PUT@ on @\/{package}@) merge: their
operations combine through 'PathItem''s 'Semigroup', and the key's path parameters
are the union of the contributing specs' parameters, de-duplicated by name so a
shared parameter is documented once.
-}
pathsFrom :: [(Ecosystem, FilePath, RouteSpec)] -> InsOrd.InsOrdHashMap FilePath PathItem
pathsFrom entries =
    InsOrd.fromList
        [ (key, item{_pathItemParameters = paramsFor key})
        | (key, item) <- InsOrd.toList operations
        ]
  where
    operations = foldl' addOperation InsOrd.empty entries
    addOperation acc (eco, key, spec) =
        InsOrd.insertWith (<>) key (methodItem (rsMethod spec) (operationFrom eco (rsName spec) (rsDoc spec))) acc

    parameters = foldl' addParams InsOrd.empty entries
    -- Accumulate a key's parameters in first-seen order (@old <> new@) before the
    -- by-name de-duplication in 'paramsFor'.
    addParams acc (_eco, key, spec) = InsOrd.insertWith (flip (<>)) key (specParams spec) acc
    paramsFor key =
        map (Inline . toParam) (nubBy sameName (fromMaybe [] (InsOrd.lookup key parameters)))
    sameName a b = psName a == psName b

{- | The full path-template key for a route under a mount prefix, e.g.
@\/npm\/{package}\/-\/{filename}@. A 'Lit' segment renders verbatim and a 'Param' as
the OpenAPI @{name}@ template. Routes that share a key (the packument @GET@ and the
publish @PUT@ on @\/{package}@) render to the same string, so they merge.
-}
routePathKey :: NonEmpty Text -> RouteSpec -> Text
routePathKey prefix spec =
    "/" <> T.intercalate "/" (toList prefix <> map renderSeg (rsPattern spec))
  where
    renderSeg = \case
        Lit s -> s
        Param p -> "{" <> psName p <> "}"

-- | The path parameters a route's template carries, in template order.
specParams :: RouteSpec -> [ParamSpec]
specParams spec = [p | Param p <- rsPattern spec]

{- | Place an operation on the 'PathItem' field its HTTP method names. Total over
'StdMethod'; the final @CONNECT@ branch is genuinely unreachable (see below), so
it is the sanctioned per-declaration @error@ escape hatch (STYLE.md section 10).
-}

{- HLINT ignore methodItem "Avoid restricted function" -}
methodItem :: StdMethod -> Operation -> PathItem
methodItem method op = case method of
    GET -> (mempty :: PathItem){_pathItemGet = Just op}
    PUT -> (mempty :: PathItem){_pathItemPut = Just op}
    POST -> (mempty :: PathItem){_pathItemPost = Just op}
    DELETE -> (mempty :: PathItem){_pathItemDelete = Just op}
    HEAD -> (mempty :: PathItem){_pathItemHead = Just op}
    PATCH -> (mempty :: PathItem){_pathItemPatch = Just op}
    OPTIONS -> (mempty :: PathItem){_pathItemOptions = Just op}
    TRACE -> (mempty :: PathItem){_pathItemTrace = Just op}
    -- CONNECT has no OpenAPI operation slot; no served route uses it.
    CONNECT -> error "Ecluse.Manifest: OpenAPI has no CONNECT operation"

{- | Interpret a route's __documentation__ into an OpenAPI operation.

This is the whole of the manifest's per-route knowledge: there is none. A route's
summary, its request body, and its status set are declared in the core, beside the
pattern that routes it ("Ecluse.Core.Server.RouteDoc"), and this function renders
whatever it is handed. There is no table here to keep in step with the routes, so there
is nothing to drift.

The tag is the ecosystem being walked, so a mount's operations are tagged with the
registry they belong to, and the route's name is qualified by that ecosystem to form
OpenAPI's @operationId@ (which client generators key on, and which must be unique across
the whole document: only here, where every mount is in view, can that be guaranteed).
-}
operationFrom :: Ecosystem -> RouteName -> RouteDoc -> Operation
operationFrom eco name doc =
    (mempty :: Operation)
        { _operationTags = InsOrdSet.fromList [ecosystemName eco]
        , _operationOperationId = Just (operationIdFor eco name)
        , _operationSummary = Just (rdSummary doc)
        , _operationDescription = Just (rdDescription doc)
        , _operationRequestBody = Inline . requestBodyFrom <$> rdRequest doc
        , _operationResponses =
            (mempty :: Responses)
                { _responsesResponses =
                    InsOrd.fromList [(respStatus r, Inline (responseFrom r)) | r <- rdResponses doc]
                }
        }

{- | A route's globally unique @operationId@: its ecosystem-local name, qualified by the
mount it is served under (@packument@ under the npm mount is @npm.packument@).
-}
operationIdFor :: Ecosystem -> RouteName -> Text
operationIdFor eco name = ecosystemName eco <> "." <> unRouteName name

-- | The request body a write route accepts.
requestBodyFrom :: RequestDoc -> RequestBody
requestBodyFrom req =
    (mempty :: RequestBody)
        { _requestBodyDescription = Just (reqDescription req)
        , _requestBodyRequired = Just (reqRequired req)
        , _requestBodyContent = bodyContent (reqBody req)
        }

-- | One documented response: its status's meaning, and the body it carries.
responseFrom :: ResponseDoc -> Response
responseFrom r =
    (mempty :: Response)
        { _responseDescription = respDescription r
        , _responseContent = bodyContent (respBody r)
        }

{- | The schema behind each body shape the core can name. __Total__ over the closed
'BodyDoc' vocabulary, so a new body shape cannot go unrendered: it is a compile error
here until it is given a schema.

This is the one join between the core's OpenAPI-free vocabulary and @openapi3@. The
schemas for the documents Écluse owns are @autodocodec@ codecs that also back their
@aeson@ instances, so the documented schema and the wire format cannot diverge.
-}
bodyContent :: BodyDoc -> InsOrd.InsOrdHashMap MediaType MediaTypeObject
bodyContent = \case
    NoBody -> mempty
    EmptyObjectBody -> jsonContent (Inline emptyObjectSchema)
    ErrorEnvelopeBody -> jsonContent errorRef
    PackumentBody -> jsonContent synthRef
    ArtifactBody -> mediaContent "application/octet-stream" (Inline binarySchema)
    PublishDocumentBody -> jsonContent (Inline publishDocumentSchema)

-- | Render a route's 'ParamSpec' as an OpenAPI path parameter.
toParam :: ParamSpec -> Param
toParam p = pathParam (psName p) (psDescription p)

pathParam :: Text -> Text -> Param
pathParam name description =
    (mempty :: Param)
        { _paramName = name
        , _paramIn = ParamPath
        , _paramRequired = Just True
        , _paramDescription = Just description
        , _paramSchema = Just (Inline (stringSchema Nothing))
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

This is the documented schema of npm's denial body. The body the server emits is
'Ecluse.Core.Registry.Npm.Serve.NpmError', which carries its reason under the same
'Ecluse.Core.Registry.Npm.Serve.npmErrorKey' this codec documents. The doc-generation
tree stays out of the running proxy, so the emitted body (a core @aeson@ encoding) and
this documented schema are separate definitions bound by that shared key and a
correspondence test in "Ecluse.ManifestSpec", not a single codec.
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
                <$> requiredField npmErrorKey "The human-facing reason the request was refused." .= errorEnvelopeError

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
