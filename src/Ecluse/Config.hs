-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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
    QuotaOverride (..),
    MountMap,
    Mount (..),
    RuleInactivity (..),
    MountRegistries (..),
    MountMode (..),
    MirroredLegs (..),
    regPrivateUpstream,
    regMirrorTarget,
    MirrorTarget (..),
    StoreTag (..),
    storeTagName,
    taggedKeyPath,
    Target (..),
    PrivateEndpoint (..),
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
    decodeDefaultConfig,
    sourceDefaultNames,
    advisorySourced,
    ConfigError (..),
    renderConfigError,
    loadConfig,
    sameRegistry,
    mountPostureLines,
    mountAdvisoryAge,
    mountEpssRequirement,
    compileMountEpssRequirement,
    mountAdvisoryDenials,
    mountDatabaseRequirement,
    advisoryAgeLines,
    advisoryEpssLines,
    resolvedKeyProvenance,
) where

import Data.Aeson (Value (..), encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Yaml (decodeEither')

import Ecluse.Config.AdvisoryStore (advisoryObjectKey, advisoryStoreBucket)
import Ecluse.Config.Aeson (parseAppConfig, parseRulesPatch)
import Ecluse.Config.DefaultConfig (defaultConfigBytes)
import Ecluse.Config.Mirror (validateMirrorSettings)
import Ecluse.Config.Resolve (buildEnvAst, deepMerge, secretLeafKeys)
import Ecluse.Config.Rule
import Ecluse.Config.Target (resolveStoreBackend, vetPrivateRepository, vetTargetTag)
import Ecluse.Config.Types
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName, parseEcosystem)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional, EpssRequired))
import Ecluse.Core.Rules.Freshness (
    AdvisoryAgeBasis (AgeBeforeQuarantine, AgeConfigured, AgeFloor),
    MaxAdvisoryAge (maxAdvisoryAge, maxAdvisoryAgeBasis),
    maxAdvisoryAgeFor,
 )
import Ecluse.Core.Rules.Render (renderDuration)
import Ecluse.Core.Rules.Types (PrecededRule (prRule), Rule, deniesOnAdvisories, readsAdvisories, requiresEpss, ruleName)
import Ecluse.Core.Security (HostPort, hostPortAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Readiness (DatabaseRequirement (DatabaseOptional, DatabaseRequired))
import Ecluse.Core.Text (registryPath, stripTrailingSlash)

-- | Decode the default backbone without resolving mounts that need an operator overlay.
decodeDefaultConfig :: ByteString -> Either [ConfigError] (Value, RulePolicy)
decodeDefaultConfig bytes = do
    ast <- parseDefaultAst bytes
    _ <- decodeAppConfig ast
    patch <- decodeRulesPatch ast
    policy <- first (pure . PolicyErrors) (resolvePolicy emptyPolicy patch)
    pure (ast, policy)

{- Each descriptor stays patchable after a disable, until the final mount policy is selected. The
middle field is why a rule left to the source does not run, absent when a source is configured. -}
data PendingPolicy = PendingPolicy RulePolicy (Maybe RuleInactivity) (Map Text Activation)

-- How one scope decides a source-activated rule: left to the source, or enabled or disabled by a patch.
data Activation = SourceDecides | OperatorEnabled | OperatorDisabled
    deriving stock (Eq)

-- | The shipped rules an advisory source activates: neither can decide anything without one.
sourceDefaultNames :: Set Text
sourceDefaultNames = Set.fromList ["deny-known-malicious", "remediation-fast-track"]

pendingPolicy :: RulePolicy -> Bool -> PendingPolicy
pendingPolicy policy sourced =
    PendingPolicy policy (NoAdvisorySource <$ guard (not sourced)) (Map.fromSet (const SourceDecides) sourceDefaultNames)

-- @enabled: false@ deactivates, @enabled: true@ or a restated type activates, and a partial patch keeps the state.
resolvePendingPolicy :: PendingPolicy -> RulePatch -> Either [PolicyError] PendingPolicy
resolvePendingPolicy (PendingPolicy policy unsourced activation) (RulePatch patch) =
    (\resolved -> PendingPolicy resolved unsourced (Map.mapWithKey decide activation)) <$> resolvePolicy policy (RulePatch descriptorPatch)
  where
    decide name current = case Map.lookup name patch of
        Nothing -> current
        Just declared -> case entryEnabled declared of
            Just True -> OperatorEnabled
            Just False -> OperatorDisabled
            Nothing -> if isJust (entryType declared) then OperatorEnabled else current
    descriptorPatch = foldr (Map.adjust (\declared -> declared{entryEnabled = Just True})) patch (Map.keys activation)

-- A scope's selected rules, and the rules it leaves to a source that is not configured, with why.
data SettledPolicy = SettledPolicy RulePolicy (Map Text RuleInactivity)

settlePolicy :: PendingPolicy -> SettledPolicy
settlePolicy (PendingPolicy policy unsourced activation) =
    SettledPolicy (RulePolicy (Map.withoutKeys (policyRules policy) idle)) waiting
  where
    idle = Map.keysSet (Map.filter (not . runs) activation)
    runs = \case
        SourceDecides -> isNothing unsourced
        OperatorEnabled -> True
        OperatorDisabled -> False
    waiting = maybe Map.empty (\reason -> reason <$ Map.filter (== SourceDecides) activation) unsourced

-- The source-activated rules a scope runs.
runningSourceRules :: SettledPolicy -> Set Text
runningSourceRules (SettledPolicy policy _) = Map.keysSet (Map.restrictKeys (policyRules policy) sourceDefaultNames)

-- A shared rule left to the source is marked only where some mount leaves it to the source too and none runs it.
sharedInactivity :: MountMap -> Set Text -> SettledPolicy -> Map Text RuleInactivity
sharedInactivity mounts mountRuns (SettledPolicy _ waiting) =
    Map.restrictKeys waiting (foldMap (Map.keysSet . mountInactiveRules) mounts `Set.difference` mountRuns)

-- | Whether @advisories.url@ is configured, the source the advisory rules read.
advisorySourced :: AppConfig -> Bool
advisorySourced = isJust . advUrl . cfgAdvisories

-- | Load the defaults, the operator document, then the environment overlay, strongest last. A mount is active only where the overlay declares it.
loadConfig :: [(String, String)] -> Maybe ByteString -> Either [ConfigError] Config
loadConfig envVars mBytes = do
    (defaultAst, catalogue) <- decodeDefaultConfig defaultConfigBytes
    docAst <- parseDocumentAst mBytes
    let overridesAst = deepMerge docAst (buildEnvAst envVars)
    let merged = deepMerge defaultAst overridesAst
    parsed <- decodeAppConfig merged
    active <- declaredMounts overridesAst
    let appConfig = parsed{cfgMounts = servedMounts active (cfgMounts parsed)}
    globalPolicy <- resolveGlobalPolicy (pendingPolicy catalogue (advisorySourced appConfig)) overridesAst
    validateMirrorSettings appConfig
    (mounts, mountRuns) <- case (publicUrlErrors appConfig, resolveMounts defaultAst overridesAst globalPolicy appConfig) of
        ([], resolved) -> resolved
        (errs, resolved) -> Left (errs <> fromLeft [] resolved)
    Right (Config appConfig mounts (sharedInactivity mounts mountRuns (settlePolicy globalPolicy)))

-- enabled: false switches a declared mount off. Anything else declared serves.
servedMounts :: Set Ecosystem -> Map Ecosystem MountConfig -> Map Ecosystem MountConfig
servedMounts active declared =
    Map.filter (\mcfg -> mntEnabled mcfg /= Just False) (Map.restrictKeys declared active)

-- The proxy rewrites served tarball URLs against its own public base URL. Without one, every
-- install fails client by client instead of loudly at boot.
publicUrlErrors :: AppConfig -> [ConfigError]
publicUrlErrors appConfig =
    [ PublicUrlRequired
    | not (Map.null (cfgMounts appConfig))
    , isNothing (srvPublicUrl (cfgServer appConfig))
    ]

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

parseDefaultAst :: ByteString -> Either [ConfigError] Value
parseDefaultAst bytes = case decodeEither' bytes of
    Right ast -> Right ast
    Left err -> Left [ParseError ("config/default.yaml is invalid YAML: " <> T.pack (show err))]

parseDocumentAst :: Maybe ByteString -> Either [ConfigError] Value
parseDocumentAst = \case
    Nothing -> Right (Object mempty)
    Just bytes -> case decodeEither' bytes of
        Right ast -> Right ast
        Left err -> Left [ParseError ("the config document is invalid YAML: " <> T.pack (show err))]

decodeAppConfig :: Value -> Either [ConfigError] AppConfig
decodeAppConfig merged = case parseEither parseAppConfig merged of
    Right appConfig -> Right appConfig
    Left err -> Left [ParseError ("Configuration parse error: " <> T.pack err)]

decodeRulesPatch :: Value -> Either [ConfigError] RulePatch
decodeRulesPatch ast = case parseEither parseRulesPatch ast of
    Right patch -> Right patch
    Left err -> Left [ParseError ("Rules parse error: " <> T.pack err)]

resolveGlobalPolicy :: PendingPolicy -> Value -> Either [ConfigError] PendingPolicy
resolveGlobalPolicy shared overridesAst = do
    globalRulePatch <- decodeRulesPatch overridesAst
    first sharedPolicyErrors (resolvePendingPolicy shared globalRulePatch)

sharedPolicyErrors :: [PolicyError] -> [ConfigError]
sharedPolicyErrors errors
    | any ((`elem` errors) . SuppressUnknownRule) npmShippedRules = [SharedPolicyErrors errors]
    | otherwise = [PolicyErrors errors]

-- The resolved mounts, and the source-activated rules any of them runs.
resolveMounts :: Value -> Value -> PendingPolicy -> AppConfig -> Either [ConfigError] (MountMap, Set Text)
resolveMounts defaultAst overridesAst globalPolicy appConfig =
    case partitionEithers (map resolveOne (Map.toAscList (cfgMounts appConfig))) of
        ([], resolved) -> Right (Map.fromList (map fst resolved), foldMap snd resolved)
        (errs, _) -> Left (concat errs)
  where
    resolveOne (eco, mcfg) = case lefts (declarationErrors eco mcfg) of
        [] -> do
            shipped <- mountRulePatch defaultAst eco
            declared <- mountRulePatch overridesAst eco
            settled <- settlePolicy <$> resolveMountPolicy globalPolicy shipped declared
            mount <- resolveMode settled eco mcfg
            pure ((eco, mount), runningSourceRules settled)
        tagErrs -> Left tagErrs

{- Every endpoint checked against the tag it was declared under, and the private upstream checked
once more for the repository the boot addresses its aggregation question to. -}
declarationErrors :: Ecosystem -> MountConfig -> [Either ConfigError ()]
declarationErrors eco mcfg =
    map (uncurry (vetTargetTag eco)) (readAndPublishTargets mcfg)
        <> [vetPrivateRepository eco (preTarget endpoint) | Just endpoint <- [mntPrivateUpstream mcfg]]

{- Each read or publish endpoint a mount declares, with the key it is written under. The mirror
target is absent because 'resolveStoreBackend' vets it while resolving its backend. -}
readAndPublishTargets :: MountConfig -> [(Text, Target)]
readAndPublishTargets mcfg =
    [("privateUpstream", preTarget target) | Just target <- [mntPrivateUpstream mcfg]]
        <> [("publicationTarget", peTarget endpoint) | Just endpoint <- [mntPublicationTarget mcfg]]

-- A declared mirror target makes the mount mirrored, which then needs its private upstream.
resolveMode :: SettledPolicy -> Ecosystem -> MountConfig -> Either [ConfigError] Mount
resolveMode policy eco mcfg = case (mntMirrorTarget mcfg, mntPrivateUpstream mcfg) of
    (Just mirrorTarget, Just privateUpstream) ->
        resolveMirrored policy eco (preTarget privateUpstream) mirrorTarget mcfg
    (Just _, Nothing) -> Left [MountMissingPrivateUpstream eco]
    (Nothing, mPrivate) -> resolveServeOnly policy eco (preTarget <$> mPrivate) mcfg

resolveMirrored :: SettledPolicy -> Ecosystem -> Target -> MirrorEndpoint -> MountConfig -> Either [ConfigError] Mount
resolveMirrored policy eco privateUpstream mirrorTarget mcfg = do
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
                            , mtUploadUrl = fromMaybe (meUrl mirrorTarget) (meUploadUrl mirrorTarget)
                            , mtUsername = meUsername mirrorTarget
                            }
                    }

resolveServeOnly :: SettledPolicy -> Ecosystem -> Maybe Target -> MountConfig -> Either [ConfigError] Mount
resolveServeOnly policy eco mPrivate mcfg =
    Right (mountOf eco mcfg policy (ServeOnly (tgtUrl <$> mPrivate)))

mountRulePatch :: Value -> Ecosystem -> Either [ConfigError] RulePatch
mountRulePatch ast eco =
    first (pure . ParseError . (prefix <>) . T.pack) $
        parseEither parseRulesPatch (fromMaybe (Object mempty) (lookupPath ["mounts", ecosystemName eco] ast))
  where
    prefix = "mounts." <> ecosystemName eco <> ".rules parse error: "

resolveMountPolicy :: PendingPolicy -> RulePatch -> RulePatch -> Either [ConfigError] PendingPolicy
resolveMountPolicy shared shipped declared =
    first (pure . PolicyErrors) (resolvePendingPolicy shared shipped >>= (`resolvePendingPolicy` declared))

mountOf :: Ecosystem -> MountConfig -> SettledPolicy -> MountMode -> Mount
mountOf eco mcfg (SettledPolicy policy inactive) mode =
    Mount
        { mountEcosystem = eco
        , mountRegistries =
            MountRegistries
                { regPublicUpstream = mntPublicUpstream mcfg
                , regMode = mode
                }
        , mountPolicy = rulesOf policy
        , mountInactiveRules = inactive
        }

rulesOf :: RulePolicy -> [PrecededRule]
rulesOf = Map.elems . policyRules

-- | Whether two endpoints name one registry: the authority folds to lower case with its default port, and the path compares past a trailing slash.
sameRegistry :: RegistryUrl -> RegistryUrl -> Bool
sameRegistry a b = registryKey a == registryKey b

{- The key two endpoints are equal on. A boot refusal gates permanent deletion on it, so the
authority folds the way DNS and TLS resolve it, and two unreadable ones count as one store. -}
registryKey :: RegistryUrl -> (Maybe HostPort, Text)
registryKey url = (hostPortAddress raw, stripTrailingSlash (registryPath raw))
  where
    raw = registryUrlText url

-- | One line per resolved leaf of the layers 'Config' loaded from: path, redacted value, layer, and any rule inactivity.
resolvedKeyProvenance :: Config -> [(String, String)] -> Maybe ByteString -> [Text]
resolvedKeyProvenance config envVars mBytes = fromRight [] $ do
    defaultAst <- parseDefaultAst defaultConfigBytes
    docAst <- parseDocumentAst mBytes
    let envAst = buildEnvAst envVars
        merged = deepMerge defaultAst (deepMerge docAst envAst)
    pure (map (renderResolvedLeaf config envAst docAst) (sortOn fst (leafPaths [] merged)))

leafPaths :: [Text] -> Value -> [(Text, Value)]
leafPaths path (Object o) =
    concatMap (\(k, v) -> leafPaths (path <> [Key.toText k]) v) (KeyMap.toList o)
leafPaths path v = [(T.intercalate "." path, v)]

renderResolvedLeaf :: Config -> Value -> Value -> (Text, Value) -> Text
renderResolvedLeaf config envAst docAst (path, v) =
    "config: " <> path <> " = " <> renderLeafValue path v <> " (" <> source <> maybe "" inactiveMarker (leafInactivity config segments) <> ")"
  where
    segments = T.splitOn "." path
    source
        | pathPresentIn envAst = "environment"
        | pathPresentIn docAst = "document"
        | otherwise = "default"
    pathPresentIn ast = isJust (lookupPath segments ast)

-- A shared rule's key reads the shared inactivity, and a mount rule's key that mount's.
leafInactivity :: Config -> [Text] -> Maybe RuleInactivity
leafInactivity config = \case
    "rules" : name : _ -> Map.lookup name (configInactiveRules config)
    "mounts" : eco : "rules" : name : _ -> Map.lookup name . mountInactiveRules =<< (`Map.lookup` configMounts config) =<< parseEcosystem eco
    _ -> Nothing

inactiveMarker :: RuleInactivity -> Text
inactiveMarker = \case
    NoAdvisorySource -> ", inactive: no advisories.url"

lookupPath :: [Text] -> Value -> Maybe Value
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
mountPostureLines config =
    map postureLine mounts <> mapMaybe maintenanceClientLine mounts <> mapMaybe upstreamProbeLine mounts
  where
    mounts = Map.toAscList (configMounts config)

{- Asking a backend what its private upstream aggregates is a call against the live control plane,
which a checker makes none of. -}
upstreamProbeLine :: (Ecosystem, Mount) -> Maybe Text
upstreamProbeLine (eco, mount) =
    notice <$ regPrivateUpstream (mountRegistries mount)
  where
    notice =
        "mount \""
            <> ecosystemName eco
            <> "\": the private upstream is asked at boot whether it, or a repository in its upstream chain, connects to a public registry. check-config does not make that call."

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

-- | One mount's maximum advisory push age, derived from its own rules unless @advisories.maxAgeSeconds@ overrides it.
mountAdvisoryAge :: AdvisoriesSettings -> Mount -> MaxAdvisoryAge
mountAdvisoryAge advisories = maxAdvisoryAgeFor (advMaxAgeSeconds advisories) . mountRulesOf

-- | Require enrichment when this mount's resolved policy holds a rule that reads EPSS scores.
mountEpssRequirement :: Mount -> EpssRequirement
mountEpssRequirement = bool EpssRequired EpssOptional . null . mountEpssRules

mountEpssRules :: Mount -> [Text]
mountEpssRules = ordNub . map ruleName . filter requiresEpss . mountRulesOf

-- | A compile's EPSS requirement for this mount. Without a source it cannot tell which rules its consumers run.
compileMountEpssRequirement :: AdvisoriesSettings -> Mount -> EpssRequirement
compileMountEpssRequirement advisories mount = case compileEpssBasis advisories mount of
    EpssUnread -> EpssOptional
    EpssReadBy _ -> EpssRequired
    EpssUnsourced -> EpssRequired

-- Why a compile for one mount needs EPSS enrichment, or why it does not.
data EpssBasis = EpssReadBy (NonEmpty Text) | EpssUnsourced | EpssUnread

compileEpssBasis :: AdvisoriesSettings -> Mount -> EpssBasis
compileEpssBasis advisories mount = case (nonEmpty (mountEpssRules mount), advUrl advisories) of
    (Just rules, _) -> EpssReadBy rules
    (Nothing, Nothing) -> EpssUnsourced
    (Nothing, Just _) -> EpssUnread

mountRulesOf :: Mount -> [Rule]
mountRulesOf = map prRule . mountPolicy

-- | This mount's rules that refuse without advisory evidence. Any one makes a store mandatory and readiness wait for it.
mountAdvisoryDenials :: Mount -> [Text]
mountAdvisoryDenials = ordNub . map ruleName . filter deniesOnAdvisories . mountRulesOf

-- | Whether this mount must hold an advisory database before it can serve anything.
mountDatabaseRequirement :: Mount -> DatabaseRequirement
mountDatabaseRequirement = bool DatabaseRequired DatabaseOptional . null . mountAdvisoryDenials

-- | The maximum push age of every mount whose rules read advisories, with its basis. Without a store nothing has an age.
advisoryAgeLines :: Config -> [Text]
advisoryAgeLines config =
    [ ageLine eco (mountAdvisoryAge advisories mount)
    | advisorySourced (configApp config)
    , (eco, mount) <- Map.toAscList (configMounts config)
    , any readsAdvisories (mountRulesOf mount)
    ]
  where
    advisories = cfgAdvisories (configApp config)

ageLine :: Ecosystem -> MaxAdvisoryAge -> Text
ageLine eco limit =
    "mount \""
        <> ecosystemName eco
        <> "\": advisory denial refuses on an advisory push older than "
        <> renderDuration (maxAdvisoryAge limit)
        <> ", "
        <> renderBasis (maxAdvisoryAgeBasis limit)

-- | Each mount's EPSS requirement, as 'compileMountEpssRequirement' decides it, with or without a store.
advisoryEpssLines :: Config -> [Text]
advisoryEpssLines config =
    [epssLine eco (compileEpssBasis (cfgAdvisories (configApp config)) mount) | (eco, mount) <- Map.toAscList (configMounts config)]

epssLine :: Ecosystem -> EpssBasis -> Text
epssLine eco = \case
    EpssUnread -> prefix <> "EPSS enrichment is optional, because no active rule reads EPSS scores. A failed EPSS feed publishes the OSV data with epss_status=unavailable"
    EpssReadBy rules -> prefix <> "EPSS enrichment is required, because these active rules read EPSS scores: " <> T.intercalate ", " (toList rules) <> ". A failed EPSS feed publishes no artifact, and consumers refuse one without enrichment"
    EpssUnsourced -> prefix <> "EPSS enrichment is required for a compile. With no advisories.url, no source activates the shipped remediation rule, so a compile cannot tell which rules its consumers run. A failed EPSS feed publishes no artifact"
  where
    prefix = "mount \"" <> ecosystemName eco <> "\": "

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
