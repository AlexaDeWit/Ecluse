{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ecluse.Config (
    Config (..),
    AppConfig (..),
    MountMap,
    Mount (..),
    MountRegistries (..),
    MirrorTarget (..),
    MountConfig (..),
    QueueBackend (..),
    CredentialBackend (..),
    MirrorCredentialProvider (..),
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
    validateDefaultConfig,
) where

import Data.Aeson (Result (..), Value (..), fromJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, withObject, (.!=), (.:?))
import Data.FileEmbed (embedFile)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Yaml (decodeEither')

import Ecluse.Config.Aeson ()
import Ecluse.Config.Resolve (buildEnvAst, deepMerge)
import Ecluse.Config.Rule
import Ecluse.Config.Types
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName, parseEcosystem)
import Ecluse.Core.Rules.Types (PrecededRule)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)

{- HLINT ignore defaultPolicy "Avoid restricted function" -}
defaultPolicy :: RulePolicy
defaultPolicy =
    let defaultBytes = $(embedFile "config/default.yaml")
     in case decodeEither' defaultBytes of
            Right ast -> case parseRulesPatch ast of
                Right globalRules -> either (error . show) id (resolvePolicy emptyPolicy globalRules)
                Left e -> error ("Invalid default policy JSON: " <> T.pack e)
            Left e -> error ("Invalid default policy YAML: " <> show e)

{- | Validate the embedded default configuration as a self-contained backbone: that
it decodes as YAML, parses into an 'AppConfig', and resolves its rule policy without
'error'. This is the __default-alone-safe subset__ of 'loadConfig'; it deliberately
omits mount resolution, which needs each mount's @privateUpstream@ from the operator
overlay and so is not a property of the shipped baseline. Exported so a test pins the
shipped default as a valid backbone rather than a malformed one surfacing only at
process start.
-}
validateDefaultConfig :: Either [ConfigError] (AppConfig, RulePolicy)
validateDefaultConfig = do
    ast <- parseDefaultAst
    appConfig <- parseAppConfig ast
    patch <- first (\e -> [ParseError ("config/default.yaml has invalid rules: " <> T.pack e)]) (parseRulesPatch ast)
    policy <- first (pure . PolicyErrors) (resolvePolicy emptyPolicy patch)
    Right (appConfig, policy)

{- | Load the full configuration: defaults, the optional operator document, and the
environment overlay, merged strongest-last, then parsed, activated, and resolved.

A mount is __active__ when the operator overlay (the document or the
@ECLUSE_MOUNTS__*@ environment variables) declares any key under
@mounts.\<ecosystem\>@; the mounts shipped in @config\/default.yaml@ are dormant
per-ecosystem templates until then. Every active mount must define its private
upstream or the load fails with 'MountMissingPrivateUpstream' (one error per
incomplete mount), so a declared mount can never silently vanish from service.
-}
loadConfig :: [(String, String)] -> Maybe ByteString -> Either [ConfigError] Config
loadConfig envVars mBytes = do
    defaultAst <- parseDefaultAst
    docAst <- parseDocumentAst mBytes
    let overridesAst = deepMerge docAst (buildEnvAst envVars)
    let merged = deepMerge defaultAst overridesAst
    parsed <- parseAppConfig merged
    active <- declaredMounts overridesAst
    let appConfig = parsed{cfgMounts = Map.restrictKeys (cfgMounts parsed) active}
    globalPolicy <- resolveGlobalPolicy overridesAst
    mounts <- resolveMounts globalPolicy appConfig
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
parseDefaultAst = case decodeEither' $(embedFile "config/default.yaml") of
    Right ast -> Right ast
    Left err -> Left [ParseError ("config/default.yaml is invalid YAML: " <> T.pack (show err))]

parseDocumentAst :: Maybe ByteString -> Either [ConfigError] Value
parseDocumentAst = \case
    Nothing -> Right (Object mempty)
    Just bytes -> case decodeEither' bytes of
        Right ast -> Right ast
        Left err -> Left [ParseError ("/etc/ecluse/config.yaml is invalid YAML: " <> T.pack (show err))]

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
one load reports each incomplete mount rather than only the first.
-}
resolveMounts :: RulePolicy -> AppConfig -> Either [ConfigError] MountMap
resolveMounts globalPolicy appConfig =
    case partitionEithers (map resolveOne (Map.toAscList (cfgMounts appConfig))) of
        ([], mounts) -> Right (Map.fromList mounts)
        (errs, _) -> Left (concat errs)
  where
    resolveOne (eco, mcfg) = case mntPrivateUpstream mcfg of
        Nothing -> Left [MountMissingPrivateUpstream eco]
        Just privateUpstream -> case resolveMount globalPolicy eco privateUpstream mcfg appConfig of
            Left errs -> Left [PolicyErrors errs]
            Right mount -> Right (eco, mount)

{- | Project an active mount, whose private upstream the caller has already
established (see 'resolveMounts'), onto its served form.
-}
resolveMount :: RulePolicy -> Ecosystem -> RegistryUrl -> MountConfig -> AppConfig -> Either [PolicyError] Mount
resolveMount globalPolicy eco privateUpstream mcfg app = do
    policy <- resolvePolicy globalPolicy (mntAdditionalRules mcfg)
    Right $
        Mount
            { mountEcosystem = eco
            , mountRegistries =
                MountRegistries
                    { regPrivateUpstream = privateUpstream
                    , regPublicUpstream = mntPublicUpstream mcfg
                    , regMirrorTarget =
                        MirrorTarget
                            { mtUrl = fromMaybe privateUpstream (mntMirrorTarget mcfg)
                            , mtCredential = mntCredentialProvider mcfg
                            , mtQueue = cfgQueueBackend app
                            }
                    }
            , mountPolicy = rulesOf policy
            }

rulesOf :: RulePolicy -> [PrecededRule]
rulesOf = Map.elems . policyRules

{- | Boot-time advisory: one warning per pair of an active mount's resolved
registry endpoints that point at the same registry. Each collapse is supported by
the proxy (the mirror target folds onto the private upstream when unset), but a
distinct registry per endpoint is the recommended posture, so every collision is
surfaced once at boot. A publication target equal to the private upstream is the
documented publish arrangement and is not warned. Comparison is textual on the
validated URL, insensitive to trailing slashes.
-}
mountCollisionWarnings :: Config -> [Text]
mountCollisionWarnings config =
    concatMap (mountCollisions (configApp config)) (Map.toAscList (configMounts config))

mountCollisions :: AppConfig -> (Ecosystem, Mount) -> [Text]
mountCollisions app (eco, mount) = mapMaybe (collisionWarning eco) pairs
  where
    regs = mountRegistries mount
    mirror = mtUrl (regMirrorTarget regs)
    publication = Map.lookup eco (cfgMounts app) >>= mntPublicationTarget
    pairs =
        [ ("mirrorTarget", mirror, "privateUpstream", Just (regPrivateUpstream regs))
        , ("mirrorTarget", mirror, "publicUpstream", Just (regPublicUpstream regs))
        , ("mirrorTarget", mirror, "publicationTarget", publication)
        , ("privateUpstream", regPrivateUpstream regs, "publicUpstream", Just (regPublicUpstream regs))
        ]

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
