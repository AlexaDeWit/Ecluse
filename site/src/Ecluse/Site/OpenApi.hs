-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The OpenAPI reference fragment: Écluse's OpenAPI document rendered as static
Markdown, one section per path and method.

The page states, for a human, which registry protocols the server speaks and what
each route answers. It renders the same in-code document @Ecluse.Manifest@
serialises, so the page and the published JSON cannot diverge. Paths, media types,
schemas, and properties are sorted, so the rendered text is byte-stable.
-}
module Ecluse.Site.OpenApi (renderOpenApiPage) where

import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.Text qualified as T

import Data.OpenApi (
    AdditionalProperties (AdditionalPropertiesAllowed, AdditionalPropertiesSchema),
    Components (_componentsSchemas),
    Info (_infoDescription, _infoVersion),
    MediaTypeObject (_mediaTypeObjectSchema),
    OpenApi (_openApiComponents, _openApiInfo, _openApiPaths, _openApiServers),
    OpenApiItems (OpenApiItemsArray, OpenApiItemsObject),
    OpenApiType (
        OpenApiArray,
        OpenApiBoolean,
        OpenApiInteger,
        OpenApiNull,
        OpenApiNumber,
        OpenApiObject,
        OpenApiString
    ),
    Operation (_operationDescription, _operationOperationId, _operationParameters, _operationRequestBody, _operationResponses, _operationSummary),
    Param (_paramDescription, _paramIn, _paramName, _paramRequired, _paramSchema),
    ParamLocation (ParamCookie, ParamHeader, ParamPath, ParamQuery),
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
    Responses (_responsesDefault, _responsesResponses),
    Schema (_schemaAdditionalProperties, _schemaDescription, _schemaFormat, _schemaItems, _schemaProperties, _schemaRequired, _schemaTitle, _schemaType),
    Server (_serverDescription, _serverUrl),
 )
import Network.HTTP.Media (MediaType)

import Ecluse.Site.Markdown (
    Alignment (AlignLeft),
    attributedHeading,
    bold,
    code,
    escapeCell,
    heading,
    link,
    slugify,
    table,
 )

-- | Render the whole reference page.
renderOpenApiPage :: OpenApi -> Text
renderOpenApiPage doc =
    T.unlines
        ( infoLines (_openApiInfo doc)
            <> serverLines (_openApiServers doc)
            <> endpointLines doc
            <> schemaLines doc
        )

infoLines :: Info -> [Text]
infoLines info = prose (_infoDescription info) <> [bold "API version" <> ": " <> code (_infoVersion info), ""]

serverLines :: [Server] -> [Text]
serverLines servers
    | null servers = []
    | otherwise =
        heading 2 "Servers"
            : ""
            : table
                [(AlignLeft, "URL"), (AlignLeft, "Description")]
                [[code (_serverUrl s), escapeCell (fromMaybe "" (_serverDescription s))] | s <- servers]
                <> [""]

endpointLines :: OpenApi -> [Text]
endpointLines doc
    | null paths = []
    | otherwise = heading 2 "Endpoints" : "" : concatMap pathSection paths
  where
    paths = sortOn fst (InsOrd.toList (_openApiPaths doc))

pathSection :: (FilePath, PathItem) -> [Text]
pathSection (path, item) =
    concatMap (operationSection (toText path) (_pathItemParameters item)) (operationsOf item)

-- The OpenAPI method slots, in the document's own field order, so a path's
-- operations always render in the same sequence.
operationsOf :: PathItem -> [(Text, Operation)]
operationsOf item = [(method, op) | (method, slot) <- slots, Just op <- [slot item]]
  where
    slots =
        [ ("GET", _pathItemGet)
        , ("PUT", _pathItemPut)
        , ("POST", _pathItemPost)
        , ("DELETE", _pathItemDelete)
        , ("HEAD", _pathItemHead)
        , ("PATCH", _pathItemPatch)
        , ("OPTIONS", _pathItemOptions)
        , ("TRACE", _pathItemTrace)
        ]

operationSection :: Text -> [Referenced Param] -> (Text, Operation) -> [Text]
operationSection path shared (method, op) =
    [attributedHeading 3 (operationAnchor path method op) [] (method <> " " <> code path), ""]
        <> prose (_operationSummary op)
        <> prose (_operationDescription op)
        <> parameterLines (shared <> _operationParameters op)
        <> requestLines (_operationRequestBody op)
        <> responseLines (_operationResponses op)

operationAnchor :: Text -> Text -> Operation -> Text
operationAnchor path method op =
    "op-" <> slugify (fromMaybe (method <> " " <> path) (_operationOperationId op))

parameterLines :: [Referenced Param] -> [Text]
parameterLines params
    | null params = []
    | otherwise =
        bold "Parameters"
            : ""
            : table
                [ (AlignLeft, "Name")
                , (AlignLeft, "In")
                , (AlignLeft, "Required")
                , (AlignLeft, "Type")
                , (AlignLeft, "Description")
                ]
                (map parameterRow params)
                <> [""]

parameterRow :: Referenced Param -> [Text]
parameterRow = \case
    Ref (Reference name) -> [code name, absentField, absentField, absentField, absentField]
    Inline param ->
        [ code (_paramName param)
        , parameterLocation (_paramIn param)
        , yesNo (fromMaybe False (_paramRequired param))
        , maybe absentField schemaSummary (_paramSchema param)
        , escapeCell (fromMaybe "" (_paramDescription param))
        ]

parameterLocation :: ParamLocation -> Text
parameterLocation = \case
    ParamQuery -> "query"
    ParamHeader -> "header"
    ParamPath -> "path"
    ParamCookie -> "cookie"

requestLines :: Maybe (Referenced RequestBody) -> [Text]
requestLines = \case
    Nothing -> []
    Just (Ref (Reference name)) -> [bold "Request body" <> ": " <> code name, ""]
    Just (Inline body) ->
        [bold "Request body" <> requiredSuffix (_requestBodyRequired body), ""]
            <> prose (_requestBodyDescription body)
            <> contentTable (_requestBodyContent body)

requiredSuffix :: Maybe Bool -> Text
requiredSuffix required
    | fromMaybe False required = " (required)"
    | otherwise = ""

contentTable :: InsOrd.InsOrdHashMap MediaType MediaTypeObject -> [Text]
contentTable content
    | null entries = []
    | otherwise = table [(AlignLeft, "Media type"), (AlignLeft, "Schema")] (map contentRow entries) <> [""]
  where
    entries = sortedContent content
    contentRow (mediaType, object) =
        [code (mediaTypeText mediaType), maybe absentField schemaSummary (_mediaTypeObjectSchema object)]

responseLines :: Responses -> [Text]
responseLines responses
    | null rows = []
    | otherwise =
        bold "Responses"
            : ""
            : table
                [ (AlignLeft, "Status")
                , (AlignLeft, "Description")
                , (AlignLeft, "Media type")
                , (AlignLeft, "Schema")
                ]
                rows
                <> [""]
  where
    rows = concatMap (uncurry responseRows) exact <> maybe [] (responseRows "default") (_responsesDefault responses)
    exact = [(show status, response) | (status, response) <- sortOn fst (InsOrd.toList (_responsesResponses responses))]

responseRows :: Text -> Referenced Response -> [[Text]]
responseRows status = \case
    Ref (Reference name) -> [[code status, code name, absentField, absentField]]
    Inline response -> case sortedContent (_responseContent response) of
        [] -> [[code status, escapeCell (_responseDescription response), absentField, absentField]]
        entries -> map (responseRow status response) entries

responseRow :: Text -> Response -> (MediaType, MediaTypeObject) -> [Text]
responseRow status response (mediaType, object) =
    [ code status
    , escapeCell (_responseDescription response)
    , code (mediaTypeText mediaType)
    , maybe absentField schemaSummary (_mediaTypeObjectSchema object)
    ]

schemaLines :: OpenApi -> [Text]
schemaLines doc
    | null schemas = []
    | otherwise = heading 2 "Schemas" : "" : concatMap (uncurry schemaSection) schemas
  where
    schemas = sortOn fst (InsOrd.toList (_componentsSchemas (_openApiComponents doc)))

schemaSection :: Text -> Schema -> [Text]
schemaSection name schema =
    [attributedHeading 3 (schemaAnchor name) [] name, ""]
        <> prose (_schemaDescription schema)
        <> propertyTable schema
        <> additionalPropertyLines (_schemaAdditionalProperties schema)

propertyTable :: Schema -> [Text]
propertyTable schema
    | null properties = []
    | otherwise =
        table
            [ (AlignLeft, "Property")
            , (AlignLeft, "Type")
            , (AlignLeft, "Required")
            , (AlignLeft, "Description")
            ]
            (map propertyRow properties)
            <> [""]
  where
    properties = sortOn fst (InsOrd.toList (_schemaProperties schema))
    propertyRow (name, referenced) =
        [ code name
        , schemaSummary referenced
        , yesNo (name `elem` _schemaRequired schema)
        , escapeCell (propertyDescription referenced)
        ]

propertyDescription :: Referenced Schema -> Text
propertyDescription = \case
    Ref _ -> ""
    Inline schema -> fromMaybe "" (_schemaDescription schema)

additionalPropertyLines :: Maybe AdditionalProperties -> [Text]
additionalPropertyLines = \case
    Nothing -> []
    Just (AdditionalPropertiesAllowed allowed) -> [bold "Additional properties" <> ": " <> yesNo allowed, ""]
    Just (AdditionalPropertiesSchema referenced) ->
        [bold "Additional properties" <> ": " <> schemaSummary referenced, ""]

-- A referenced schema links to its own section, so the reader lands on the full
-- definition instead of an inlined copy of it.
schemaSummary :: Referenced Schema -> Text
schemaSummary = \case
    Ref (Reference name) -> link name ("#" <> schemaAnchor name)
    Inline schema -> inlineSchemaSummary schema

inlineSchemaSummary :: Schema -> Text
inlineSchemaSummary schema = case _schemaType schema of
    Just OpenApiArray -> "array of " <> itemSummary (_schemaItems schema)
    Just openApiType -> typeName openApiType <> formatSuffix (_schemaFormat schema)
    Nothing -> fromMaybe "any" (_schemaTitle schema)

itemSummary :: Maybe OpenApiItems -> Text
itemSummary = \case
    Nothing -> "any"
    Just (OpenApiItemsObject referenced) -> schemaSummary referenced
    Just (OpenApiItemsArray referenced) -> T.intercalate ", " (map schemaSummary referenced)

typeName :: OpenApiType -> Text
typeName = \case
    OpenApiString -> "string"
    OpenApiNumber -> "number"
    OpenApiInteger -> "integer"
    OpenApiBoolean -> "boolean"
    OpenApiArray -> "array"
    OpenApiNull -> "null"
    OpenApiObject -> "object"

formatSuffix :: Maybe Text -> Text
formatSuffix = maybe "" (\format -> " (" <> format <> ")")

schemaAnchor :: Text -> Text
schemaAnchor name = "schema-" <> slugify name

sortedContent :: InsOrd.InsOrdHashMap MediaType MediaTypeObject -> [(MediaType, MediaTypeObject)]
sortedContent = sortOn (mediaTypeText . fst) . InsOrd.toList

mediaTypeText :: MediaType -> Text
mediaTypeText = show

prose :: Maybe Text -> [Text]
prose = \case
    Just text | not (T.null text) -> [text, ""]
    _ -> []

yesNo :: Bool -> Text
yesNo required = if required then "yes" else "no"

absentField :: Text
absentField = "-"
