-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

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
    MountMap,
    Mount (..),
    MountRegistries (..),
    MountMode (..),
    MirroredLegs (..),
    regPrivateUpstream,
    regMirrorTarget,
    MirrorTarget (..),
    MirrorCredential (..),
    PublishAllow (..),
    MountConfig (..),
    Url,
    mkUrl,
    unUrl,
    QueueTarget (..),
    QueueUrl (..),
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
    mountCollisionWarnings,
    mountPostureLines,
    resolvedKeyProvenance,
) where

import Data.Aeson (Result (..), Value (..), encode, fromJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, withObject, (.!=), (.:?))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Yaml (decodeEither')

import Ecluse.Config.Aeson ()
import Ecluse.Config.DefaultConfig (defaultConfigBytes)
import Ecluse.Config.MirrorCredential (resolveMirrorCredential)
import Ecluse.Config.Resolve (buildEnvAst, deepMerge, secretLeafKeys)
import Ecluse.Config.Rule
import Ecluse.Config.Types
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName, parseEcosystem)
import Ecluse.Core.Rules.Types (PrecededRule)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Text (stripTrailingSlash)

{- HLINT ignore defaultPolicy "Avoid restricted function" -}
defaultPolicy :: RulePolicy
defaultPolicy =
    case decodeEither' defaultConfigBytes of
        Right ast -> case parseRulesPatch ast of
            Right globalRules -> either (error . show) id (resolvePolicy emptyPolicy globalRules)
            Left e -> error ("Invalid default policy JSON: " <> T.pack e)
        Left e -> error ("Invalid default policy YAML: " <> show e)

{- | Load the merged configuration: the defaults, the optional operator document, then the
environment overlay, strongest-last.

A mount is __active__ only when the operator overlay declares a key under
@mounts.\<ecosystem\>@, in the document or in @ECLUSE_MOUNTS__*@. Until then the mounts in
@config\/default.yaml@ stay dormant per-ecosystem templates. @enabled: false@ switches a
declared mount off.
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

{- | The ecosystems the operator overlay declares under @mounts@: the activation set. The
merged defaults never activate a mount. An unknown ecosystem key cannot reach here, because
parsing already rejected it, and this refuses it anyway rather than assuming it away.
-}
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
parseAppConfig merged = case fromJSON merged of
    Success appConfig -> Right appConfig
    Error err -> Left [ParseError ("Configuration parse error: " <> T.pack err)]

parseRulesPatch :: Value -> Either String RulePatch
parseRulesPatch = parseEither (withObject "Config" (\obj -> obj .:? "rules" .!= RulePatch Map.empty))

resolveGlobalPolicy :: Value -> Either [ConfigError] RulePolicy
resolveGlobalPolicy overridesAst = do
    globalRulePatch <- case parseRulesPatch overridesAst of
        Right r -> Right r
        Left err -> Left [ParseError ("Rules parse error: " <> T.pack err)]
    first (pure . PolicyErrors) (resolvePolicy defaultPolicy globalRulePatch)

{- | Resolve every active mount into its served 'Mount', aggregating failures so one load
reports every incomplete mount. A declared @mirrorTarget@ makes the mount mirrored and
requires a private upstream. An absent one makes it serve-only.
-}
resolveMounts :: RulePolicy -> AppConfig -> Either [ConfigError] MountMap
resolveMounts globalPolicy appConfig =
    case partitionEithers (map resolveOne (Map.toAscList (cfgMounts appConfig))) of
        ([], mounts) -> Right (Map.fromList mounts)
        (errs, _) -> Left (concat errs)
  where
    resolveOne (eco, mcfg) = case (mntMirrorTarget mcfg, mntPrivateUpstream mcfg) of
        (Just mirrorTarget, Just privateUpstream) ->
            (eco,) <$> resolveMirrored globalPolicy eco privateUpstream mirrorTarget mcfg
        -- A mirrored mount must be able to read its mirror back.
        (Just _, Nothing) -> Left [MountMissingPrivateUpstream eco]
        (Nothing, mPrivate) -> case writeOnlySettings mcfg of
            [] -> (eco,) <$> resolveServeOnly globalPolicy eco mPrivate mcfg
            -- A write credential or token duration on a mount that never writes signals a
            -- misunderstanding. Refuse each offending key rather than ignoring it.
            offending -> Left (map (MirrorSettingWithoutWrite eco) offending)

    writeOnlySettings mcfg =
        ["mirrorTargetToken" | isJust (mntMirrorTargetToken mcfg)]
            <> ["mirrorCodeArtifactTokenDuration" | isJust (mntMirrorCodeArtifactTokenDuration mcfg)]

{- | Project a mirrored mount onto its served form. 'resolveMirrorCredential' derives the
mirror-write credential from the mirror-target URL, so the resolved 'MirrorTarget' never pairs
an endpoint with a credential meant for another.
-}
resolveMirrored :: RulePolicy -> Ecosystem -> RegistryUrl -> RegistryUrl -> MountConfig -> Either [ConfigError] Mount
resolveMirrored globalPolicy eco privateUpstream mirrorTarget mcfg = do
    policy <- resolveMountPolicy globalPolicy mcfg
    credential <-
        first (: []) $
            resolveMirrorCredential eco mirrorTarget (mntMirrorTargetToken mcfg) (mntMirrorCodeArtifactTokenDuration mcfg)
    Right $
        mountOf eco mcfg policy $
            Mirrored
                MirroredLegs
                    { mlPrivateUpstream = privateUpstream
                    , mlMirrorTarget =
                        MirrorTarget
                            { mtUrl = mirrorTarget
                            , mtCredential = credential
                            }
                    }

{- | Project a serve-only mount onto its served form. It makes no mirror write, and
its private upstream is optional, absent on the pure public gate.
-}
resolveServeOnly :: RulePolicy -> Ecosystem -> Maybe RegistryUrl -> MountConfig -> Either [ConfigError] Mount
resolveServeOnly globalPolicy eco mPrivate mcfg = do
    policy <- resolveMountPolicy globalPolicy mcfg
    Right (mountOf eco mcfg policy (ServeOnly mPrivate))

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

{- | Boot-time advisory: one warning per pair of a mount's resolved endpoints that point at
the same registry. Each collapse is supported, and a distinct registry per endpoint stays the
recommended posture. A publication target equal to the private upstream is the documented
publish arrangement, so that pair raises no warning.
-}
mountCollisionWarnings :: Config -> [Text]
mountCollisionWarnings config =
    concatMap (mountCollisions (configApp config)) (Map.toAscList (configMounts config))

mountCollisions :: AppConfig -> (Ecosystem, Mount) -> [Text]
mountCollisions app (eco, mount) = mapMaybe (collisionWarning eco) pairs
  where
    regs = mountRegistries mount
    mirror = mtUrl <$> regMirrorTarget regs
    private = regPrivateUpstream regs
    publication = Map.lookup eco (cfgMounts app) >>= mntPublicationTarget
    -- A serve-only mount has no mirror rows (and the pure gate no private row):
    -- absent endpoints cannot collide.
    pairs =
        [("mirrorTarget", m, "privateUpstream", private) | Just m <- [mirror]]
            <> [("mirrorTarget", m, "publicUpstream", Just (regPublicUpstream regs)) | Just m <- [mirror]]
            <> [("mirrorTarget", m, "publicationTarget", publication) | Just m <- [mirror]]
            <> [("privateUpstream", p, "publicUpstream", Just (regPublicUpstream regs)) | Just p <- [private]]

collisionWarning :: Ecosystem -> (Text, RegistryUrl, Text, Maybe RegistryUrl) -> Maybe Text
collisionWarning eco (aName, a, bName, mb) = do
    b <- mb
    guard (sameRegistry a b)
    pure
        ( "mount \""
            <> ecosystemName eco
            <> "\": "
            <> aName
            <> " and "
            <> bName
            <> " resolve to the same registry ("
            <> registryUrlText a
            <> "); a distinct registry per endpoint is strongly recommended"
        )

sameRegistry :: RegistryUrl -> RegistryUrl -> Bool
sameRegistry a b = strip a == strip b
  where
    strip = stripTrailingSlash . registryUrlText

{- | One line per resolved leaf of the merged configuration: the dotted path, the rendered
value with secret-typed keys redacted, and the layer that supplied it. That layer is
environment, document, or default, in that order of precedence. Derived and computed values
are absent, because their own resolvers log them. Renders nothing when a layer fails to parse.
-}
resolvedKeyProvenance :: [(String, String)] -> Maybe ByteString -> [Text]
resolvedKeyProvenance envVars mBytes = fromRight [] $ do
    defaultAst <- parseDefaultAst
    docAst <- parseDocumentAst mBytes
    let envAst = buildEnvAst envVars
        merged = deepMerge defaultAst (deepMerge docAst envAst)
    pure (map (renderResolvedLeaf envAst docAst) (sortOn fst (leafPaths [] merged)))

-- Every leaf of a config AST with its dotted path (objects recurse, and anything
-- else, arrays included, is a leaf).
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

{- | Boot-time posture: one line per served mount naming its derived mode and its consequence.
An unintentionally dropped @mirrorTarget@ then shows up as "serve-only" at the next boot,
rather than silently un-mirroring.
-}
mountPostureLines :: Config -> [Text]
mountPostureLines config = map postureLine (Map.toAscList (configMounts config))

postureLine :: (Ecosystem, Mount) -> Text
postureLine (eco, mount) = case regMode (mountRegistries mount) of
    Mirrored legs ->
        "mount \""
            <> ecosystemName eco
            <> "\": mirrored; admitted public artifacts back-fill "
            <> registryUrlText (mtUrl (mlMirrorTarget legs))
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
