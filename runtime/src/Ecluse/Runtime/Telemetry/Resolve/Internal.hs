-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The environment reads and the projections behind "Ecluse.Runtime.Telemetry.Resolve", which
documents the configuration model and re-exports the curated surface. Importing this module opts
out of that stability promise, the convention @text@ and @bytestring@ use, so production code
imports the public one.
-}
module Ecluse.Runtime.Telemetry.Resolve.Internal (
    -- * The resolved telemetry identity
    ResolvedTelemetry (..),
    TelemetryEndpoint (..),
    EndpointSource (..),
    resolveTelemetry,
    declaredEnv,

    -- * Canonical @OTEL_*@ projection
    otelEnvironmentOverrides,
    ResourceAttributes (..),
    resourceAttributes,

    -- * Boot wiring
    telemetryWarnings,
    prepareTelemetry,
) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Text qualified as T
import GHC.Exts qualified as Exts
import System.Environment (setEnv)

import Katip (LogEnv, Severity (WarningS))
import OpenTelemetry.Baggage (Baggage, Element, Token)
import OpenTelemetry.Baggage qualified as Baggage

import Ecluse.Core.BuildIdentity (productVersion)
import Ecluse.Core.Text (nonBlank)
import Ecluse.Runtime.Log (moduleLog)

{- | Where a resolved OTLP endpoint came from, so the boot path can tell a configured target from
the silent default.
-}
data EndpointSource
    = -- | Derived from @DD_AGENT_HOST@ (as @http:\/\/{host}:4318@).
      FromDdAgentHost
    | -- | Taken verbatim from @OTEL_EXPORTER_OTLP_ENDPOINT@.
      FromOtelEndpoint
    | -- | No endpoint was configured, so the @http:\/\/localhost:4318@ default applies.
      DefaultedEndpoint
    deriving stock (Eq, Show)

-- | A resolved OTLP export endpoint and the source it was resolved from.
data TelemetryEndpoint = TelemetryEndpoint
    { teUrl :: Text
    -- ^ The endpoint URL the exporter targets (always @http\/protobuf@).
    , teSource :: EndpointSource
    -- ^ How the URL was resolved.
    }
    deriving stock (Eq, Show)

{- | The telemetry identity the SDK configuration and the @dd@ log object share. The process
cannot know its own deployment environment, so 'rtEnvironment' stays optional.
-}
data ResolvedTelemetry = ResolvedTelemetry
    { rtServiceName :: Text
    -- ^ @service.name@ \/ @dd.service@ (defaults to @ecluse@).
    , rtEnvironment :: Maybe Text
    -- ^ @deployment.environment.name@ \/ @dd.env@, when configured.
    , rtVersion :: Maybe Text
    -- ^ @service.version@ \/ @dd.version@ (defaults to the build version).
    , rtEndpoint :: TelemetryEndpoint
    -- ^ The resolved OTLP export endpoint.
    }
    deriving stock (Eq, Show)

{- | Resolve the telemetry identity, each field falling __Datadog value, then vanilla
OpenTelemetry, then the default__. The resolver never reads @DD_API_KEY@ or @DD_SITE@.

>>> rtServiceName (resolveTelemetry [("DD_SERVICE", "api"), ("OTEL_SERVICE_NAME", "ignored")])
"api"

>>> teUrl (rtEndpoint (resolveTelemetry []))
"http://localhost:4318"
-}
resolveTelemetry :: [(String, String)] -> ResolvedTelemetry
resolveTelemetry environment =
    ResolvedTelemetry
        { rtServiceName = fromMaybe defaultServiceName serviceName
        , rtEnvironment = deploymentEnvironment
        , rtVersion = declared "DD_VERSION" <|> attr "service.version" <|> Just productVersion
        , rtEndpoint = endpoint
        }
  where
    declared :: String -> Maybe Text
    declared name = declaredEnv name environment

    attributes :: Baggage
    attributes = fromRight Baggage.empty (decodeResourceAttributes environment)

    attr :: Text -> Maybe Text
    attr key = do
        name <- Baggage.mkToken key
        nonBlank =<< Baggage.getValue name attributes

    serviceName :: Maybe Text
    serviceName = declared "DD_SERVICE" <|> declared "OTEL_SERVICE_NAME" <|> attr "service.name"

    -- The SDK deprecates deployment.environment for deployment.environment.name. Both spellings
    -- are read, so an operator on either one resolves, and only the current spelling is emitted.
    deploymentEnvironment :: Maybe Text
    deploymentEnvironment =
        declared "DD_ENV" <|> attr "deployment.environment.name" <|> attr "deployment.environment"

    endpoint :: TelemetryEndpoint
    endpoint = case declared "DD_AGENT_HOST" of
        Just host -> TelemetryEndpoint (agentHostUrl host) FromDdAgentHost
        Nothing -> case declared "OTEL_EXPORTER_OTLP_ENDPOINT" of
            Just url -> TelemetryEndpoint url FromOtelEndpoint
            Nothing -> TelemetryEndpoint defaultEndpointUrl DefaultedEndpoint

-- | Read one environment variable, counting a present but blank value as unset.
declaredEnv :: String -> [(String, String)] -> Maybe Text
declaredEnv name environment = nonBlank . toText =<< lookup name environment

defaultServiceName :: Text
defaultServiceName = "ecluse"

defaultEndpointUrl :: Text
defaultEndpointUrl = "http://localhost:4318"

{- The Datadog Agent's OTLP receiver listens on 4318 for HTTP\/protobuf. A literal IPv6 host is
bracketed so the authority stays well-formed, and a host with a scheme or a port passes unchanged. -}
agentHostUrl :: Text -> Text
agentHostUrl raw
    | "://" `T.isInfixOf` host = host
    | otherwise = "http://" <> authority
  where
    host = T.strip raw
    authority
        | "[" `T.isPrefixOf` host = if "]:" `T.isInfixOf` host then host else host <> ":4318"
        | T.count ":" host >= 2 = "[" <> host <> "]:4318"
        | T.count ":" host == 1 = host
        | otherwise = host <> ":4318"

{- | Project the resolved identity back to the canonical @OTEL_*@ variables the env-driven SDK
reads. The protocol is pinned to @http\/protobuf@ because gRPC sits behind a disabled cabal flag.
-}
otelEnvironmentOverrides :: [(String, String)] -> [(String, String)]
otelEnvironmentOverrides environment =
    [ ("OTEL_SERVICE_NAME", toString (rtServiceName resolved))
    , ("OTEL_EXPORTER_OTLP_ENDPOINT", toString (teUrl (rtEndpoint resolved)))
    , ("OTEL_EXPORTER_OTLP_PROTOCOL", "http/protobuf")
    , ("OTEL_RESOURCE_ATTRIBUTES", renderResourceAttributes (raCarried (resourceAttributes environment)))
    ]
  where
    resolved :: ResolvedTelemetry
    resolved = resolveTelemetry environment

-- Overlay the resolved identity onto the operator's own attributes. An inserted member replaces
-- an inherited one of the same name, so a stale operator value never overrides the resolution.
mergedResourceAttributes :: ResolvedTelemetry -> [(String, String)] -> Baggage
mergedResourceAttributes resolved environment =
    foldr insertAttribute withoutServiceName (resolvedAttributes resolved)
  where
    inherited :: Baggage
    inherited = fromRight Baggage.empty (decodeResourceAttributes environment)

    -- OTEL_SERVICE_NAME carries the service name, and every SDK signal path prefers that
    -- variable, so an inherited copy here spends header budget to fight it and lose.
    withoutServiceName :: Baggage
    withoutServiceName = maybe inherited (`Baggage.delete` inherited) (Baggage.mkToken "service.name")

resolvedAttributes :: ResolvedTelemetry -> [(Text, Text)]
resolvedAttributes resolved =
    [ (key, value)
    | (key, Just value) <-
        [ ("deployment.environment.name", rtEnvironment resolved)
        , ("service.version", rtVersion resolved)
        ]
    ]

-- A key the W3C token grammar cannot express is dropped, because the SDK's decoder rejects a
-- whole header over one such member.
insertAttribute :: (Text, Text) -> Baggage -> Baggage
insertAttribute (key, value) bag =
    maybe bag (\name -> Baggage.insert name (Baggage.element value) bag) (Baggage.mkToken key)

-- | The members the exported header carries, and the keys the W3C baggage limits left out.
data ResourceAttributes = ResourceAttributes
    { raCarried :: Baggage
    -- ^ What @OTEL_RESOURCE_ATTRIBUTES@ exports.
    , raDropped :: [Text]
    -- ^ The keys the limits excluded, in admission order.
    }
    deriving stock (Eq, Show)

{- | Decide what the exported header carries. The SDK's encoder would shed the overflow in hash
order, so the choice is made here: the carried set is stable and every shed key warns at boot.
-}
resourceAttributes :: [(String, String)] -> ResourceAttributes
resourceAttributes environment = carry (admitMembers 0 0 (admissionOrder resolved merged))
  where
    resolved :: ResolvedTelemetry
    resolved = resolveTelemetry environment

    merged :: Baggage
    merged = mergedResourceAttributes resolved environment

    carry :: ([(Token, Element)], [Text]) -> ResourceAttributes
    carry (kept, dropped) = ResourceAttributes (foldr (uncurry Baggage.insert) Baggage.empty kept) dropped

-- The resolved identity is offered first, so the limits shed the operator's extras rather than
-- the keys a dashboard joins on. Everything else follows in key order.
admissionOrder :: ResolvedTelemetry -> Baggage -> [(Token, Element)]
admissionOrder resolved bag = sortOn (rank . memberKey . fst) (Exts.toList (Baggage.values bag))
  where
    identityKeys :: [Text]
    identityKeys = map fst (resolvedAttributes resolved)

    rank :: Text -> (Int, Text)
    rank key = (if key `elem` identityKeys then 0 else 1, key)

{- Take members while the W3C limits allow and name the rest. An excluded member is skipped rather
than ending the scan, so a small attribute still lands after a large one is left out. -}
admitMembers :: Int -> Int -> [(Token, Element)] -> ([(Token, Element)], [Text])
admitMembers _ _ [] = ([], [])
admitMembers usedBytes usedMembers ((tok, el) : rest)
    | admissible = first ((tok, el) :) (admitMembers (usedBytes + separator + size) (usedMembers + 1) rest)
    | otherwise = second (memberKey tok :) (admitMembers usedBytes usedMembers rest)
  where
    size :: Int
    size = encodedMemberBytes tok el

    separator :: Int
    separator = if usedMembers == 0 then 0 else 1

    admissible :: Bool
    admissible =
        size <= Baggage.maxMemberBytes
            && usedMembers < Baggage.maxMembers
            && usedBytes + separator + size <= Baggage.maxBaggageBytes

-- One member's encoded size, measured with the SDK's own encoder. That encoder emits nothing for a
-- member over its per-member limit, so an empty encoding reports as one byte past the limit.
encodedMemberBytes :: Token -> Element -> Int
encodedMemberBytes tok el =
    case BS.length (Baggage.encodeBaggageHeader (Baggage.insert tok el Baggage.empty)) of
        0 -> Baggage.maxMemberBytes + 1
        n -> n

memberKey :: Token -> Text
memberKey = decodeUtf8 . Baggage.tokenValue

{- Decode @OTEL_RESOURCE_ATTRIBUTES@ with the SDK's own W3C baggage parser. Blank members are
dropped first, so a trailing comma parses where the grammar alone would reject the whole value. -}
decodeResourceAttributes :: [(String, String)] -> Either Text Baggage
decodeResourceAttributes environment = case members of
    [] -> Right Baggage.empty
    _ -> first toText (Baggage.decodeBaggageHeader (encodeUtf8 (T.intercalate "," members)))
  where
    members :: [Text]
    members = filter (not . T.null) (map T.strip (T.splitOn "," raw))

    raw :: Text
    raw = maybe "" toText (lookup "OTEL_RESOURCE_ATTRIBUTES" environment)

-- Render with the SDK's own encoder, so the value the SDK decodes is the one this module resolved.
-- 'resourceAttributes' has already brought the bag within the limits, so nothing is shed here.
renderResourceAttributes :: Baggage -> String
renderResourceAttributes = decodeUtf8 . Baggage.encodeBaggageHeader

-- | The boot warnings the environment raises, in the order 'prepareTelemetry' surfaces them.
telemetryWarnings :: [(String, String)] -> [Text]
telemetryWarnings environment = endpointWarning <> attributeWarning <> droppedWarning
  where
    endpoint :: TelemetryEndpoint
    endpoint = rtEndpoint (resolveTelemetry environment)

    endpointWarning :: [Text]
    endpointWarning =
        [defaultedEndpointMessage (teUrl endpoint) | teSource endpoint == DefaultedEndpoint]

    attributeWarning :: [Text]
    attributeWarning =
        either
            (\reason -> [malformedAttributesMessage reason])
            (const [])
            (decodeResourceAttributes environment)

    droppedWarning :: [Text]
    droppedWarning = case raDropped (resourceAttributes environment) of
        [] -> []
        dropped -> [droppedAttributesMessage dropped]

defaultedEndpointMessage :: Text -> Text
defaultedEndpointMessage url =
    "no telemetry export endpoint configured (DD_AGENT_HOST / OTEL_EXPORTER_OTLP_ENDPOINT unset); defaulting to "
        <> url
        <> "."

malformedAttributesMessage :: Text -> Text
malformedAttributesMessage reason =
    "OTEL_RESOURCE_ATTRIBUTES is not valid W3C baggage ("
        <> reason
        <> "). Dropping its attributes and exporting the resolved service identity alone."

droppedAttributesMessage :: [Text] -> Text
droppedAttributesMessage dropped =
    "OTEL_RESOURCE_ATTRIBUTES is over the W3C baggage limits ("
        <> show Baggage.maxBaggageBytes
        <> " bytes total, "
        <> show Baggage.maxMemberBytes
        <> " bytes per member, "
        <> show Baggage.maxMembers
        <> " members). Dropping "
        <> T.intercalate ", " dropped
        <> " from the exported resource attributes."

{- | Surface the boot warnings and set the canonical @OTEL_*@ environment, before the SDK reads it.
A defaulted endpoint is a warning and never a failure: the destination is the operator's to declare.
-}
prepareTelemetry :: LogEnv -> [(String, String)] -> IO ()
prepareTelemetry logEnv environment = do
    mapM_ (moduleLog logEnv resolveModule WarningS) (telemetryWarnings environment)
    mapM_ (uncurry setEnv) (otelEnvironmentOverrides environment)

-- The operator filter key every line raised here is tagged with. It names the public module,
-- not this one, because operators filter on it.
resolveModule :: Text
resolveModule = "Ecluse.Runtime.Telemetry.Resolve"
