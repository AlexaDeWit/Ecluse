-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Site.OpenApiSpec (spec) where

import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.Text qualified as T
import Test.Hspec

import Data.OpenApi (
    AdditionalProperties (AdditionalPropertiesSchema),
    Components (_componentsSchemas),
    Info (_infoDescription, _infoVersion),
    MediaTypeObject (_mediaTypeObjectSchema),
    OpenApi (_openApiComponents, _openApiInfo, _openApiPaths, _openApiServers),
    OpenApiItems (OpenApiItemsObject),
    OpenApiType (OpenApiArray, OpenApiInteger, OpenApiObject, OpenApiString),
    Operation (_operationDescription, _operationOperationId, _operationRequestBody, _operationResponses, _operationSummary),
    Param (_paramDescription, _paramIn, _paramName, _paramRequired, _paramSchema),
    ParamLocation (ParamPath),
    PathItem (_pathItemGet, _pathItemParameters, _pathItemPut),
    Reference (Reference),
    Referenced (Inline, Ref),
    RequestBody (_requestBodyContent, _requestBodyDescription, _requestBodyRequired),
    Response (_responseContent, _responseDescription),
    Responses (_responsesDefault, _responsesResponses),
    Schema (_schemaAdditionalProperties, _schemaDescription, _schemaFormat, _schemaItems, _schemaProperties, _schemaRequired, _schemaType),
    Server (Server, _serverDescription, _serverUrl, _serverVariables),
 )
import Network.HTTP.Media (MediaType)

import Ecluse.Site.OpenApi (renderOpenApiPage)

spec :: Spec
spec = do
    describe "the page lead" $ do
        it "opens with the document's own description" $
            rendered `carries` "What this server speaks."
        it "states the API version" $
            rendered `carries` "**API version**: `9.9.9`"
        it "tables the servers" $
            rendered `carries` "| `https://registry.example` | The externally-reachable base URL. |"

    describe "endpoints" $ do
        it "sections one heading per path and method, anchored on the operation id" $ do
            rendered `carries` "### GET `/npm/-/ping` {#op-npm-ping}"
            rendered `carries` "### GET `/npm/{package}` {#op-npm-packument}"
            rendered `carries` "### PUT `/npm/{package}` {#op-npm-publish}"
        it "orders the paths and, within a path, the methods" $
            sectionOrder rendered
                `shouldBe` [ "### GET `/npm/-/ping` {#op-npm-ping}"
                           , "### GET `/npm/{package}` {#op-npm-packument}"
                           , "### PUT `/npm/{package}` {#op-npm-publish}"
                           ]
        it "carries the summary and the description" $ do
            rendered `carries` "Fetch a packument"
            rendered `carries` "Returns the merged and filtered packument."
        it "tables a path-level parameter under every method of that path" $
            occurrences "| `package` | path | yes | string | The package name. |" rendered
                `shouldBe` 2
        it "marks a required request body and links its schema" $ do
            rendered `carries` "**Request body** (required)"
            rendered `carries` "| `application/json` | [PublishDocument](#schema-publishdocument) |"

    describe "responses" $ do
        it "orders the exact statuses and puts the default last" $
            responseStatuses rendered `shouldBe` ["`200`", "`404`", "`default`"]
        it "links a referenced response schema to its schema section" $
            rendered `carries` "| `200` | The packument. | `application/json` | [Packument](#schema-packument) |"
        it "dashes the media type and schema of a response with no body" $
            rendered `carries` "| `404` | No such package. | - | - |"

    describe "schemas" $ do
        it "anchors each named schema" $
            rendered `carries` "### Packument {#schema-packument}"
        it "tables the properties in name order, marking the required ones" $
            propertyRows rendered
                `shouldBe` [ "| `keywords` | array of string | no | Search keywords. |"
                           , "| `name` | string | yes | The package name. |"
                           , "| `revision` | integer (int32) | no |  |"
                           ]
        it "summarises the additional properties" $
            rendered `carries` "**Additional properties**: object"

    describe "an empty document" $
        it "renders the version line alone" $
            renderOpenApiPage mempty `shouldBe` "**API version**: ``\n\n"

rendered :: Text
rendered = renderOpenApiPage document

carries :: Text -> Text -> Expectation
carries page fragment = (fragment `T.isInfixOf` page) `shouldBe` True

occurrences :: Text -> Text -> Int
occurrences fragment page = length (filter (== fragment) (T.lines page))

sectionOrder :: Text -> [Text]
sectionOrder page = filter (T.isInfixOf "{#op-") (T.lines page)

responseStatuses :: Text -> [Text]
responseStatuses page =
    [ T.takeWhile (/= ' ') (T.drop 2 line)
    | line <- rowsUnder "| Status | Description | Media type | Schema |" page
    ]

propertyRows :: Text -> [Text]
propertyRows = rowsUnder "| Property | Type | Required | Description |"

-- The body rows of the first table under the given header line.
rowsUnder :: Text -> Text -> [Text]
rowsUnder header page = takeWhile (T.isPrefixOf "| ") (drop 2 (dropWhile (/= header) (T.lines page)))

document :: OpenApi
document =
    (mempty :: OpenApi)
        { _openApiInfo =
            (mempty :: Info)
                { _infoVersion = "9.9.9"
                , _infoDescription = Just "What this server speaks."
                }
        , _openApiServers =
            [ Server
                { _serverUrl = "https://registry.example"
                , _serverDescription = Just "The externally-reachable base URL."
                , _serverVariables = mempty
                }
            ]
        , _openApiPaths = InsOrd.fromList [("/npm/{package}", packageItem), ("/npm/-/ping", pingItem)]
        , _openApiComponents = (mempty :: Components){_componentsSchemas = InsOrd.singleton "Packument" packumentSchema}
        }

packageItem :: PathItem
packageItem =
    (mempty :: PathItem)
        { _pathItemParameters = [Inline packageParam]
        , _pathItemGet = Just packumentOperation
        , _pathItemPut = Just publishOperation
        }

pingItem :: PathItem
pingItem = (mempty :: PathItem){_pathItemGet = Just pingOperation}

packageParam :: Param
packageParam =
    (mempty :: Param)
        { _paramName = "package"
        , _paramIn = ParamPath
        , _paramRequired = Just True
        , _paramDescription = Just "The package name."
        , _paramSchema = Just (Inline stringSchema)
        }

packumentOperation :: Operation
packumentOperation =
    (mempty :: Operation)
        { _operationOperationId = Just "npm.packument"
        , _operationSummary = Just "Fetch a packument"
        , _operationDescription = Just "Returns the merged and filtered packument."
        , _operationResponses =
            (mempty :: Responses)
                { _responsesResponses =
                    InsOrd.fromList
                        [ (404, Inline ((mempty :: Response){_responseDescription = "No such package."}))
                        , (200, Inline packumentResponse)
                        ]
                , _responsesDefault = Just (Inline ((mempty :: Response){_responseDescription = "An upstream response."}))
                }
        }

packumentResponse :: Response
packumentResponse =
    (mempty :: Response)
        { _responseDescription = "The packument."
        , _responseContent = jsonContent (Ref (Reference "Packument"))
        }

publishOperation :: Operation
publishOperation =
    (mempty :: Operation)
        { _operationOperationId = Just "npm.publish"
        , _operationSummary = Just "Publish a package"
        , _operationRequestBody =
            Just
                ( Inline
                    (mempty :: RequestBody)
                        { _requestBodyDescription = Just "The npm publish document."
                        , _requestBodyRequired = Just True
                        , _requestBodyContent = jsonContent (Ref (Reference "PublishDocument"))
                        }
                )
        }

pingOperation :: Operation
pingOperation = (mempty :: Operation){_operationOperationId = Just "npm.ping", _operationSummary = Just "Liveness probe"}

packumentSchema :: Schema
packumentSchema =
    (mempty :: Schema)
        { _schemaType = Just OpenApiObject
        , _schemaDescription = Just "The merged and filtered packument."
        , _schemaRequired = ["name"]
        , _schemaProperties =
            InsOrd.fromList
                [ ("name", Inline stringSchema{_schemaDescription = Just "The package name."})
                , ("revision", Inline ((mempty :: Schema){_schemaType = Just OpenApiInteger, _schemaFormat = Just "int32"}))
                , ("keywords", Inline keywordsSchema)
                ]
        , _schemaAdditionalProperties =
            Just (AdditionalPropertiesSchema (Inline ((mempty :: Schema){_schemaType = Just OpenApiObject})))
        }

keywordsSchema :: Schema
keywordsSchema =
    (mempty :: Schema)
        { _schemaType = Just OpenApiArray
        , _schemaDescription = Just "Search keywords."
        , _schemaItems = Just (OpenApiItemsObject (Inline stringSchema))
        }

stringSchema :: Schema
stringSchema = (mempty :: Schema){_schemaType = Just OpenApiString}

jsonContent :: Referenced Schema -> InsOrd.InsOrdHashMap MediaType MediaTypeObject
jsonContent schema = InsOrd.singleton "application/json" ((mempty :: MediaTypeObject){_mediaTypeObjectSchema = Just schema})
