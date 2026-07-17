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
    MountConfig (..),
    Url (..),
    mkUrl,
    unUrl,
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
) where

import Data.Aeson (Result (..), Value (..), fromJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, withObject, (.!=), (.:?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Yaml (decodeEither')

import Ecluse.Config.Aeson ()
import Ecluse.Config.DefaultConfig (defaultConfigBytes)
import Ecluse.Config.MirrorCredential (resolveMirrorCredential)
import Ecluse.Config.Resolve (buildEnvAst, deepMerge)
import Ecluse.Config.Rule
import Ecluse.Config.Types
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName, parseEcosystem)
import Ecluse.Core.Rules.Types (PrecededRule)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)

{- HLINT ignore defaultPolicy "Avoid restricted function" -}
defaultPolicy :: RulePolicy
defaultPolicy =
    case decodeEither' defaultConfigBytes of
        Right ast -> case parseRulesPatch ast of
            Right globalRules -> either (error . show) id (resolvePolicy emptyPolicy globalRules)
            Left e -> error ("Invalid default policy JSON: " <> T.pack e)
        Left e -> error ("Invalid default policy YAML: " <> show e)

{- | Load the full configuration: defaults, the optional operator document, and the
environment overlay, merged strongest-last, then parsed, activated, and resolved.

A mount is __active__ when the operator overlay (the document or the
@ECLUSE_MOUNTS__*@ environment variables) declares any key under
@mounts.\<ecosystem\>@; the mounts shipped in @config\/default.yaml@ are dormant
per-ecosystem templates until then. The @enabled@ key is itself a declaration, so
@enabled: true@ alone activates a mount against its template public upstream (the
serve-only pure public gate), and @enabled: false@ switches a mount off without
removing its other keys.

Whether an active mount __mirrors__ is derived from its declared endpoints: a
@mirrorTarget@ makes it mirrored (its private upstream is then required, so the
mirror can be read back: 'MountMissingPrivateUpstream'), and an absent one makes it
serve-only (never writing anywhere; a mirror-write setting left behind is refused
per key as 'MirrorSettingWithoutWrite' rather than silently ignored). The boot log
names each mount's resolved posture, so an unintentionally dropped @mirrorTarget@
is visible at start-up.
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
        -- enabled: false switches a declared mount off; anything else declared serves.
        served = Map.filter (\mcfg -> mntEnabled mcfg /= Just False) declared
        appConfig = parsed{cfgMounts = served}
    -- Any served mount needs the proxy's own client-facing base URL: served
    -- tarball URLs are rewritten against it, and without one every real install
    -- fails client by client instead of loudly here. Aggregated with the mount
    -- resolution so one load reports both classes at once.
    let publicUrlErrs = [PublicUrlRequired | not (Map.null served), isNothing (srvPublicUrl (cfgServer appConfig))]
    globalPolicy <- resolveGlobalPolicy overridesAst
    mounts <- case (publicUrlErrs, resolveMounts globalPolicy appConfig) of
        ([], resolved) -> resolved
        (errs, resolved) -> Left (errs <> fromLeft [] resolved)
    Right (Config appConfig mounts)

{- | The ecosystems the operator overlay declares under @mounts@: the activation
set. Only keys the operator wrote count; the merged defaults never activate a
mount. An unknown ecosystem key is unreachable here (parsing the merged document
has already rejected it) but is still refused totally rather than assumed away.
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

{- | Resolve every active mount into its served 'Mount', aggregating failures so
one load reports each incomplete mount rather than only the first. The mode is
derived from the declared endpoints: a @mirrorTarget@ makes the mount mirrored
(private upstream required), an absent one makes it serve-only.
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
            -- A write credential or token duration on a mount that never writes
            -- signals a misunderstanding; refuse each offending key rather than
            -- silently ignoring it.
            offending -> Left (map (MirrorSettingWithoutWrite eco) offending)

    writeOnlySettings mcfg =
        ["mirrorTargetToken" | isJust (mntMirrorTargetToken mcfg)]
            <> ["mirrorCodeArtifactTokenDuration" | isJust (mntMirrorCodeArtifactTokenDuration mcfg)]

{- | Project a mirrored mount, whose private upstream and mirror target the caller
has already established (see 'resolveMounts'), onto its served form. The
mirror-write credential is derived from the mirror-target URL here
('resolveMirrorCredential'), so the resolved 'MirrorTarget' pairs an endpoint only
with the credential that endpoint dictates.
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

{- | Project a serve-only mount (no mirror write; the private upstream optional,
absent on the pure public gate) onto its served form.
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

{- | Boot-time advisory: one warning per pair of an active mount's resolved
registry endpoints that point at the same registry. Each collapse is supported by
the proxy (declaring the mirror target equal to the private upstream is a valid
arrangement), but a distinct registry per endpoint is the recommended posture, so
every collision is surfaced once at boot. A publication target equal to the private
upstream is the documented publish arrangement and is not warned. Comparison is
textual on the validated URL, insensitive to trailing slashes.
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
    strip = T.dropWhileEnd (== '/') . registryUrlText

{- | Boot-time posture: one line per served mount naming its derived mode and its
consequence. The mode is derived from the declared endpoints (see 'loadConfig'), so
this is the loud counterpart of that inference: an unintentionally dropped
@mirrorTarget@ shows up here as "serve-only" at the very next boot rather than
silently un-mirroring.
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
