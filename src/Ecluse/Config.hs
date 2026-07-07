{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Avoid restricted function" -}
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
    validateDefaultConfig,
) where

import Data.Aeson (Result (..), Value (..), fromJSON)
import Data.Aeson.Types (parseEither, withObject, (.!=), (.:?))
import Data.FileEmbed (embedFile)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Yaml (decodeEither')

import Ecluse.Config.Aeson ()
import Ecluse.Config.Resolve (buildEnvAst, deepMerge)
import Ecluse.Config.Rule
import Ecluse.Config.Types
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Rules.Types (PrecededRule)

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

loadConfig :: [(String, String)] -> Maybe ByteString -> Either [ConfigError] Config
loadConfig envVars mBytes = do
    defaultAst <- parseDefaultAst
    docAst <- parseDocumentAst mBytes
    let overridesAst = deepMerge docAst (buildEnvAst envVars)
    let merged = deepMerge defaultAst overridesAst
    appConfig <- parseAppConfig merged
    globalPolicy <- resolveGlobalPolicy overridesAst
    mounts <- resolveMounts globalPolicy appConfig
    Right (Config appConfig mounts)

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

resolveMounts :: RulePolicy -> AppConfig -> Either [ConfigError] MountMap
resolveMounts globalPolicy appConfig =
    Map.traverseWithKey resolveOne (cfgMounts appConfig)
  where
    resolveOne eco mcfg =
        first (pure . PolicyErrors) (resolveMount globalPolicy eco mcfg appConfig)

{- HLINT ignore resolveMount "Avoid restricted function" -}
resolveMount :: RulePolicy -> Ecosystem -> MountConfig -> AppConfig -> Either [PolicyError] Mount
resolveMount globalPolicy eco mcfg app = do
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
  where
    privateUpstream = fromMaybe (error "privateUpstream filtered out") (mntPrivateUpstream mcfg)

rulesOf :: RulePolicy -> [PrecededRule]
rulesOf = Map.elems . policyRules
