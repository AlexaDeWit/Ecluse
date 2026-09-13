-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

-- | Configuration loading, mount resolution, and redacted operator diagnostics.
module Ecluse.Config (
    Config (..),
    AppConfig (..),
    ServerSettings (..),
    QueueSettings (..),
    LimitsSettings (..),
    CacheSettings (..),
    IntegritySettings (..),
    EgressSettings (..),
    AdvisoriesSettings (..),
    RuntimeSettings (..),
    ObservabilitySettings (..),
    DredgerSettings (..),
    MountMap,
    Mount (..),
    MountRegistries (..),
    MountMode (..),
    MirroredLegs (..),
    regPrivateUpstream,
    regMirrorTarget,
    MirrorTarget (..),
    StoreTag (..),
    storeTagName,
    Target (..),
    DeletionConsent (..),
    MirrorWrite (..),
    MirrorEndpoint (..),
    meTarget,
    PublicationEndpoint (..),
    MintPlan (..),
    ControlPlane (..),
    StoreBackend (..),
    sbTag,
    sbMint,
    sbControl,
    FirstParty (..),
    MountIntegrity (..),
    MountConfig (..),
    Url,
    mkUrl,
    unUrl,
    QueueTarget (..),
    QueueUrl,
    queueUrlText,
    queueUrlTarget,
    AdvisoryStoreTarget (..),
    AdvisoryStoreUrl,
    advisoryStoreUrlText,
    advisoryStoreTarget,
    advisoryStoreBucket,
    advisoryObjectKey,
    RulePatch (..),
    RuleEntry (..),
    RulePolicy (..),
    PolicyError (..),
    renderPolicyError,
    emptyPolicy,
    defaultPolicy,
    ConfigError (..),
    renderConfigError,
    loadConfig,
    sameRegistry,
    mountPostureLines,
    mountAdvisoryAge,
    mountEpssRequirement,
    advisoryAgeLines,
    resolvedKeyProvenance,
) where

import Data.Aeson (Value (..), encode, parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, withObject, (.!=), (.:?))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Yaml (decodeEither')

import Ecluse.Config.AdvisoryStore (advisoryObjectKey, advisoryStoreBucket)
import Ecluse.Config.Aeson ()
import Ecluse.Config.DefaultConfig (defaultConfigBytes)
import Ecluse.Config.Resolve (buildEnvAst, deepMerge, secretLeafKeys)
import Ecluse.Config.Rule
import Ecluse.Config.Target (resolveStoreBackend, vetTargetTag)
import Ecluse.Config.Types
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName, parseEcosystem)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional, EpssRequired))
import Ecluse.Core.Rules (renderDuration)
import Ecluse.Core.Rules.Freshness (
    AdvisoryAgeBasis (AgeBeforeQuarantine, AgeConfigured, AgeFloor),
    MaxAdvisoryAge (maxAdvisoryAge, maxAdvisoryAgeBasis),
    maxAdvisoryAgeFor,
 )
import Ecluse.Core.Rules.Types (PrecededRule (prRule), Rule (DenyIfEpss), readsAdvisories)
import Ecluse.Core.Security (HostPort, hostPortAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Text (registryPath, stripTrailingSlash)

-- | The rule policy embedded in the shipped configuration.

{- HLINT ignore defaultPolicy "Avoid restricted function" -}
defaultPolicy :: RulePolicy
defaultPolicy =
    case decodeEither' defaultConfigBytes of
        Right ast -> case parseRulesPatch ast of
            Right globalRules -> either (error . show) id (resolvePolicy emptyPolicy globalRules)
            Left e -> error ("Invalid default policy JSON: " <> T.pack e)
        Left e -> error ("Invalid default policy YAML: " <> show e)

{- | Load the merged configuration: the defaults, the operator document, then the environment
overlay, strongest-last. A mount is __active__ only where that overlay declares a key under it.
-}
loadConfig :: [(String, String)] -> Maybe ByteString -> Either [ConfigError] Config
loadConfig envVars mBytes = do
    defaultAst <- parseDefaultAst
    docAst <- parseDocumentAst mBytes
    let overridesAst = deepMerge docAst (buildEnvAst envVars)
    let merged = deepMerge defaultAst overridesAst
    parsed <- parseAppConfig merged
    active <- declaredMounts overridesAst
    let declared = Map.restrictKeys (cfgMounts parsed) active
        -- enabled: false switches a declared mount off. Anything else declared serves.
        served = Map.filter (\mcfg -> mntEnabled mcfg /= Just False) declared
        appConfig = parsed{cfgMounts = served}
    -- The proxy rewrites served tarball URLs against its own public base URL. Without one, every
    -- install fails client by client instead of loudly at boot.
    let publicUrlErrs = [PublicUrlRequired | not (Map.null served), isNothing (srvPublicUrl (cfgServer appConfig))]
    globalPolicy <- resolveGlobalPolicy overridesAst
    mounts <- case (publicUrlErrs, resolveMounts globalPolicy appConfig) of
        ([], resolved) -> resolved
        (errs, resolved) -> Left (errs <> fromLeft [] resolved)
    Right (Config appConfig mounts)

declaredMounts :: Value -> Either [ConfigError] (Set Ecosystem)
declaredMounts overridesAst = Set.fromList <$> traverse parseKey (mountKeysOf overridesAst)
  where
    parseKey k = case parseEcosystem (Key.toText k) of
        Just eco -> Right eco
        Nothing -> Left [ParseError ("Invalid ecosystem: " <> Key.toText k)]

mountKeysOf :: Value -> [Key.Key]
mountKeysOf (Object o) = case KeyMap.lookup "mounts" o of
    Just (Object mounts) -> KeyMap.keys mounts
    _ -> []
mountKeysOf _ = []

parseDefaultAst :: Either [ConfigError] Value
parseDefaultAst = case decodeEither' defaultConfigBytes of
    Right ast -> Right ast
    Left err -> Left [ParseError ("config/default.yaml is invalid YAML: " <> T.pack (show err))]

parseDocumentAst :: Maybe ByteString -> Either [ConfigError] Value
parseDocumentAst = \case
    Nothing -> Right (Object mempty)
    Just bytes -> case decodeEither' bytes of
        Right ast -> Right ast
        Left err -> Left [ParseError ("the config document is invalid YAML: " <> T.pack (show err))]

parseAppConfig :: Value -> Either [ConfigError] AppConfig
parseAppConfig merged = case parseEither parseJSON merged of
    Right appConfig -> Right appConfig
    Left err -> Left [ParseError ("Configuration parse error: " <> T.pack err)]

parseRulesPatch :: Value -> Either String RulePatch
parseRulesPatch = parseEither (withObject "Config" (\obj -> obj .:? "rules" .!= RulePatch Map.empty))

resolveGlobalPolicy :: Value -> Either [ConfigError] RulePolicy
resolveGlobalPolicy overridesAst = do
    globalRulePatch <- case parseRulesPatch overridesAst of
        Right r -> Right r
        Left err -> Left [ParseError ("Rules parse error: " <> T.pack err)]
    first (pure . PolicyErrors) (resolvePolicy defaultPolicy globalRulePatch)

resolveMounts :: RulePolicy -> AppConfig -> Either [ConfigError] MountMap
resolveMounts globalPolicy appConfig =
    case partitionEithers (map resolveOne (Map.toAscList (cfgMounts appConfig))) of
        ([], mounts) -> Right (Map.fromList mounts)
        (errs, _) -> Left (concat errs)
  where
    resolveOne (eco, mcfg) = case lefts (map (uncurry (vetTargetTag eco)) (readAndPublishTargets mcfg)) of
        [] -> (eco,) <$> resolveMode globalPolicy eco mcfg
        tagErrs -> Left tagErrs

{- Each read or publish endpoint a mount declares, with the key it is written under. The mirror
target is absent because 'resolveStoreBackend' vets it while resolving its backend. -}
readAndPublishTargets :: MountConfig -> [(Text, Target)]
readAndPublishTargets mcfg =
    [("privateUpstream", target) | Just target <- [mntPrivateUpstream mcfg]]
        <> [("publicationTarget", peTarget endpoint) | Just endpoint <- [mntPublicationTarget mcfg]]

-- A declared mirror target makes the mount mirrored, which then needs its private upstream.
resolveMode :: RulePolicy -> Ecosystem -> MountConfig -> Either [ConfigError] Mount
resolveMode globalPolicy eco mcfg = case (mntMirrorTarget mcfg, mntPrivateUpstream mcfg) of
    (Just mirrorTarget, Just privateUpstream) ->
        resolveMirrored globalPolicy eco privateUpstream mirrorTarget mcfg
    (Just _, Nothing) -> Left [MountMissingPrivateUpstream eco]
    (Nothing, mPrivate) -> resolveServeOnly globalPolicy eco mPrivate mcfg

resolveMirrored :: RulePolicy -> Ecosystem -> Target -> MirrorEndpoint -> MountConfig -> Either [ConfigError] Mount
resolveMirrored globalPolicy eco privateUpstream mirrorTarget mcfg = do
    policy <- resolveMountPolicy globalPolicy mcfg
    backend <- first (: []) (resolveStoreBackend eco mirrorTarget)
    Right $
        mountOf eco mcfg policy $
            Mirrored
                MirroredLegs
                    { mlPrivateUpstream = tgtUrl privateUpstream
                    , mlMirrorTarget =
                        MirrorTarget
                            { mtUrl = meUrl mirrorTarget
                            , mtBackend = backend
                            }
                    }

resolveServeOnly :: RulePolicy -> Ecosystem -> Maybe Target -> MountConfig -> Either [ConfigError] Mount
resolveServeOnly globalPolicy eco mPrivate mcfg = do
    policy <- resolveMountPolicy globalPolicy mcfg
    Right (mountOf eco mcfg policy (ServeOnly (tgtUrl <$> mPrivate)))

resolveMountPolicy :: RulePolicy -> MountConfig -> Either [ConfigError] RulePolicy
resolveMountPolicy globalPolicy mcfg =
    first (\errs -> [PolicyErrors errs]) (resolvePolicy globalPolicy (mntAdditionalRules mcfg))

mountOf :: Ecosystem -> MountConfig -> RulePolicy -> MountMode -> Mount
mountOf eco mcfg policy mode =
    Mount
        { mountEcosystem = eco
        , mountRegistries =
            MountRegistries
                { regPublicUpstream = mntPublicUpstream mcfg
                , regMode = mode
                }
        , mountPolicy = rulesOf policy
        }

rulesOf :: RulePolicy -> [PrecededRule]
rulesOf = Map.elems . policyRules

{- | Whether two configured endpoints name the same registry. The authority folds to lower case
with its default port applied, and the path is compared exactly past a trailing slash.
-}
sameRegistry :: RegistryUrl -> RegistryUrl -> Bool
sameRegistry a b = registryKey a == registryKey b

{- The key two endpoints are equal on. A boot refusal gates permanent deletion on it, so the
authority folds the way DNS and TLS resolve it, and two unreadable ones count as one store. -}
registryKey :: RegistryUrl -> (Maybe HostPort, Text)
registryKey url = (hostPortAddress raw, stripTrailingSlash (registryPath raw))
  where
    raw = registryUrlText url

{- | One line per resolved leaf of the merged configuration: the dotted path, the value with
secret-typed keys redacted, and its layer. Empty when a layer fails to parse, and computed keys are absent.
-}
resolvedKeyProvenance :: [(String, String)] -> Maybe ByteString -> [Text]
resolvedKeyProvenance envVars mBytes = fromRight [] $ do
    defaultAst <- parseDefaultAst
    docAst <- parseDocumentAst mBytes
    let envAst = buildEnvAst envVars
        merged = deepMerge defaultAst (deepMerge docAst envAst)
    pure (map (renderResolvedLeaf envAst docAst) (sortOn fst (leafPaths [] merged)))

leafPaths :: [Text] -> Value -> [(Text, Value)]
leafPaths path (Object o) =
    concatMap (\(k, v) -> leafPaths (path <> [Key.toText k]) v) (KeyMap.toList o)
leafPaths path v = [(T.intercalate "." path, v)]

renderResolvedLeaf :: Value -> Value -> (Text, Value) -> Text
renderResolvedLeaf envAst docAst (path, v) =
    "config: " <> path <> " = " <> renderLeafValue path v <> " (" <> source <> ")"
  where
    source
        | pathPresentIn envAst = "environment"
        | pathPresentIn docAst = "document"
        | otherwise = "default"
    pathPresentIn ast = isJust (lookupPath (T.splitOn "." path) ast)
    lookupPath [] ast = Just ast
    lookupPath (k : ks) (Object o) = lookupPath ks =<< KeyMap.lookup (Key.fromText k) o
    lookupPath _ _ = Nothing

-- Secret-typed keys are redacted: the provenance dump must never widen a secret's exposure
-- beyond the layer it arrived on.
renderLeafValue :: Text -> Value -> Text
renderLeafValue path v
    | any (`T.isSuffixOf` path) secretLeafKeys = "<redacted>"
    | otherwise = case v of
        String t -> t
        other -> decodeUtf8 (LBS.toStrict (encode other))

-- | Mount modes followed by the live-environment limits of @check-config@, shared with boot.
mountPostureLines :: Config -> [Text]
mountPostureLines config = map postureLine mounts <> mapMaybe maintenanceClientLine mounts
  where
    mounts = Map.toAscList (configMounts config)

maintenanceClientLine :: (Ecosystem, Mount) -> Maybe Text
maintenanceClientLine (eco, mount) = do
    target <- regMirrorTarget (mountRegistries mount)
    case sbControl (mtBackend target) of
        ControlNone -> Nothing
        ControlCodeArtifact{} -> Just notice
        ControlProtocol{} -> Just notice
  where
    notice =
        "mount \""
            <> ecosystemName eco
            <> "\": the store maintenance client is built at boot against the live environment. check-config does not attempt this build."

{- | One mount's effective maximum advisory push age, derived from that mount's own rules. An
explicit @advisories.maxAgeSeconds@ overrides the derivation on every mount.
-}
mountAdvisoryAge :: AdvisoriesSettings -> Mount -> MaxAdvisoryAge
mountAdvisoryAge advisories = maxAdvisoryAgeFor (advMaxAgeSeconds advisories) . mountRulesOf

-- | Require enrichment when this mount's resolved policy contains an EPSS rule.
mountEpssRequirement :: Mount -> EpssRequirement
mountEpssRequirement = bool EpssOptional EpssRequired . any requiresEpss . mountRulesOf
  where
    requiresEpss DenyIfEpss{} = True
    requiresEpss _ = False

mountRulesOf :: Mount -> [Rule]
mountRulesOf = map prRule . mountPolicy

{- | The effective maximum push age of every mount whose rules read the advisory database, with
the basis that produced it. A mount that reads no advisories has no limit to report.
-}
advisoryAgeLines :: Config -> [Text]
advisoryAgeLines config =
    [ ageLine eco (mountAdvisoryAge advisories mount)
    | (eco, mount) <- Map.toAscList (configMounts config)
    , any readsAdvisories (mountRulesOf mount)
    ]
  where
    advisories = cfgAdvisories (configApp config)

ageLine :: Ecosystem -> MaxAdvisoryAge -> Text
ageLine eco limit =
    "mount \""
        <> ecosystemName eco
        <> "\": CVE-based denial refuses on an advisory push older than "
        <> renderDuration (maxAdvisoryAge limit)
        <> ", "
        <> renderBasis (maxAdvisoryAgeBasis limit)

renderBasis :: AdvisoryAgeBasis -> Text
renderBasis = \case
    AgeConfigured -> "set by advisories.maxAgeSeconds"
    AgeBeforeQuarantine quarantine ->
        "derived a day ahead of this mount's earliest AllowIfOlderThan quarantine of " <> renderDuration quarantine
    AgeFloor -> "the shipped floor, which no derivation goes below"

postureLine :: (Ecosystem, Mount) -> Text
postureLine (eco, mount) = case regMode (mountRegistries mount) of
    Mirrored legs ->
        "mount \""
            <> ecosystemName eco
            <> "\": mirrored; admitted public artifacts back-fill the "
            <> storeTagName (sbTag (mtBackend (mlMirrorTarget legs)))
            <> " store "
            <> registryUrlText (mtUrl (mlMirrorTarget legs))
            <> consentClause (mtBackend (mlMirrorTarget legs))
    ServeOnly (Just private) ->
        "mount \""
            <> ecosystemName eco
            <> "\": serve-only (no mirrorTarget declared): merges the private upstream "
            <> registryUrlText private
            <> " and never mirrors; admitted public artifacts stay on the gated public leg"
    ServeOnly Nothing ->
        "mount \""
            <> ecosystemName eco
            <> "\": serve-only pure public gate (no private upstream, no mirrorTarget): every artifact streams from the gated public leg and is never mirrored"

{- The deletion consent a store carries, which the operator declares on the store itself. Only a
Verdaccio store carries one, so the clause is empty everywhere else. -}
consentClause :: StoreBackend -> Text
consentClause = \case
    BackendVerdaccio _ DeletionPermitted -> ", which permits deletion"
    BackendVerdaccio _ DeletionWithheld -> ", which withholds deletion"
    BackendRegistry{} -> ""
    BackendCodeArtifact{} -> ""
