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
            Right ast -> case parseEither (withObject "Config" (\obj -> obj .:? "rules" .!= RulePatch Map.empty)) ast of
                Right globalRules -> either (error . show) id (resolvePolicy emptyPolicy globalRules)
                Left e -> error ("Invalid default policy JSON: " <> T.pack e)
            Left e -> error ("Invalid default policy YAML: " <> show e)

loadConfig :: [(String, String)] -> Maybe ByteString -> Either [ConfigError] Config
loadConfig envVars mBytes = do
    let envAst = buildEnvAst envVars

    let defaultBytes = $(embedFile "config/default.yaml")

    defaultAst <- case decodeEither' defaultBytes of
        Right ast -> Right ast
        Left err -> Left [ParseError ("config/default.yaml is invalid YAML: " <> T.pack (show err))]

    docAst <- case mBytes of
        Nothing -> Right (Object mempty)
        Just bytes -> case decodeEither' bytes of
            Right ast -> Right ast
            Left err -> Left [ParseError ("/etc/ecluse/config.yaml is invalid YAML: " <> T.pack (show err))]

    let overridesAst = deepMerge docAst envAst
    let merged = deepMerge defaultAst overridesAst

    appConfig <- case fromJSON merged of
        Success e -> Right e
        Error err -> Left [ParseError ("Configuration parse error: " <> T.pack err)]

    globalRulePatch <- case parseEither (withObject "Config" (\obj -> obj .:? "rules" .!= RulePatch Map.empty)) overridesAst of
        Right r -> Right r
        Left err -> Left [ParseError ("Rules parse error: " <> T.pack err)]

    globalPolicy <- first (pure . PolicyErrors) (resolvePolicy defaultPolicy globalRulePatch)

    mountsList <- traverse (\(eco, mcfg) -> either (Left . pure . PolicyErrors) Right (resolveMount globalPolicy eco mcfg appConfig)) (Map.toList (cfgMounts appConfig))
    let mountsMap = Map.fromList (map (\m -> (mountEcosystem m, m)) mountsList)

    Right (Config appConfig mountsMap)
  where
    {- HLINT ignore resolveMount "Avoid restricted function" -}
    resolveMount :: RulePolicy -> Ecosystem -> MountConfig -> AppConfig -> Either [PolicyError] Mount
    resolveMount globalPolicy eco mcfg app = do
        policy <- resolvePolicy globalPolicy (mntAdditionalRules mcfg)
        Right $
            Mount
                { mountEcosystem = eco
                , mountRegistries =
                    MountRegistries
                        { regPrivateUpstream = fromMaybe (error "privateUpstream filtered out") (mntPrivateUpstream mcfg)
                        , regPublicUpstream = mntPublicUpstream mcfg
                        , regMirrorTarget =
                            MirrorTarget
                                { mtUrl = fromMaybe (fromMaybe (error "privateUpstream filtered out") (mntPrivateUpstream mcfg)) (mntMirrorTarget mcfg)
                                , mtCredential = mntCredentialProvider mcfg
                                , mtQueue = cfgQueueBackend app
                                }
                        }
                , mountPolicy = rulesOf policy
                }

rulesOf :: RulePolicy -> [PrecededRule]
rulesOf = Map.elems . policyRules
