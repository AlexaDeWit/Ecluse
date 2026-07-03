{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ecluse.Config.Aeson () where

import Data.Aeson (FromJSON (..), Value (..), withObject, withText, (.!=), (.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)

import Ecluse.Config.Parser
import Ecluse.Config.Rule
import Ecluse.Config.Types

import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem, parseEcosystem)
import Ecluse.Core.Package (Scope, mkScope)
import Ecluse.Core.Package.Integrity (parseMinIntegrity, parseMinTrustedIntegrity)
import Ecluse.Log (parseLogFormat)
import Ecluse.Telemetry (parseTelemetrySwitch)

instance FromJSON MountConfig where
    parseJSON = withObject "MountConfig" $ \o -> do
        rejectUnknownKeys "mount" ["privateUpstream", "publicUpstream", "mirrorTarget", "mirrorTargetToken", "credentialProvider", "respectUpstreamTarballHost", "mirrorCodeArtifactDomain", "mirrorCodeArtifactDomainOwner", "mirrorCodeArtifactRegion", "mirrorCodeArtifactTokenDuration", "publicationTarget", "publicationTargetToken", "publishScopes", "rules"] o
        MountConfig
            <$> (o .:? "privateUpstream" >>= traverse parseRegistryUrl)
            <*> (o .: "publicUpstream" >>= parseRegistryUrl)
            <*> (o .:? "mirrorTarget" >>= traverse parseRegistryUrl)
            <*> (o .:? "mirrorTargetToken" >>= traverse parseSecret)
            <*> (o .: "credentialProvider" >>= parseEnum parseCredentialBackend "credentialProvider")
            <*> o .: "respectUpstreamTarballHost"
            <*> o .:? "mirrorCodeArtifactDomain"
            <*> (o .:? "mirrorCodeArtifactDomainOwner" >>= traverse parseTextOrNumber)
            <*> o .:? "mirrorCodeArtifactRegion"
            <*> (o .:? "mirrorCodeArtifactTokenDuration" >>= traverse parseDuration)
            <*> (o .:? "publicationTarget" >>= traverse parseRegistryUrl)
            <*> (o .:? "publicationTargetToken" >>= traverse parseSecret)
            <*> (o .:? "publishScopes" .!= String "" >>= parseScopes)
            <*> o .:? "rules" .!= RulePatch Map.empty
      where
        parseDuration :: Value -> Parser Natural
        parseDuration (String t) = case readMaybe (T.unpack t) :: Maybe Natural of
            Just n -> pure n
            Nothing -> fail ("invalid duration: " <> T.unpack t)
        parseDuration v = parseJSON v

        parseTextOrNumber :: Value -> Parser Text
        parseTextOrNumber (String t) = pure t
        parseTextOrNumber v = T.pack . show <$> (parseJSON v :: Parser Integer)

        parseSecret :: Value -> Parser Secret
        parseSecret = withText "Secret" (pure . mkSecret)

        parseScopes :: Value -> Parser [Scope]
        parseScopes = withText "Scopes" $ \t ->
            if T.null (T.strip t)
                then pure []
                else pure (map (mkScope . T.strip) (T.splitOn "," t))

instance FromJSON AppConfig where
    parseJSON = withObject "AppConfig" $ \o -> do
        rejectUnknownKeys "document" ["port", "mounts", "queueBackend", "queueUrl", "queueMemoryMaxDepth", "awsRegion", "awsEndpointUrlSqs", "awsEndpointUrl", "awsAccessKeyId", "awsSecretAccessKey", "googleProject", "authToken", "helpMessage", "cveSyncInterval", "shutdownDrainTimeout", "cores", "maxHeapBytes", "serveMaxInFlight", "publicConnectionsPerHost", "privateConnectionsPerHost", "cacheTtl", "cacheMaxEntries", "cacheMaxBytes", "maxResponseBytes", "maxVersionCount", "maxNestingDepth", "logFormat", "telemetry", "publicUrl", "minPublicIntegrity", "minTrustedIntegrity", "rules", "osvDataDir", "vulnerabilityDatabaseBucket"] o
        AppConfig
            <$> o .: "port"
            <*> (o .:? "mounts" .!= mempty >>= parseMounts)
            <*> (o .: "queueBackend" >>= parseEnum parseQueueBackend "queueBackend")
            <*> (o .:? "queueUrl" >>= traverse parseUrl)
            <*> o .: "queueMemoryMaxDepth"
            <*> o .:? "awsRegion"
            <*> o .:? "awsEndpointUrlSqs"
            <*> o .:? "awsEndpointUrl"
            <*> o .:? "googleProject"
            <*> (o .:? "authToken" >>= traverse parseSecret)
            <*> o .:? "helpMessage"
            <*> (o .: "cveSyncInterval" >>= parseSeconds)
            <*> o .: "shutdownDrainTimeout"
            <*> (o .:? "cores" >>= traverse (parsePositiveInt "cores"))
            <*> (o .:? "maxHeapBytes" >>= traverse (parsePositiveInt "maxHeapBytes"))
            <*> (o .:? "serveMaxInFlight" >>= traverse (parsePositiveInt "serveMaxInFlight"))
            <*> (o .: "publicConnectionsPerHost" >>= parsePositiveInt "publicConnectionsPerHost")
            <*> (o .:? "privateConnectionsPerHost" >>= traverse (parsePositiveInt "privateConnectionsPerHost"))
            <*> (o .: "cacheTtl" >>= parseSeconds)
            <*> o .: "cacheMaxEntries"
            <*> o .: "cacheMaxBytes"
            <*> o .: "maxResponseBytes"
            <*> o .: "maxVersionCount"
            <*> o .: "maxNestingDepth"
            <*> (o .: "logFormat" >>= parseEnum parseLogFormat "logFormat")
            <*> (o .: "telemetry" >>= parseEnum parseTelemetrySwitch "telemetry")
            <*> (o .:? "publicUrl" >>= traverse parseUrl)
            <*> (o .: "minPublicIntegrity" >>= parseEnum parseMinIntegrity "minPublicIntegrity")
            <*> (o .: "minTrustedIntegrity" >>= parseEnum parseMinTrustedIntegrity "minTrustedIntegrity")
            <*> (o .:? "osvDataDir" .!= "data/osv")
            <*> o .:? "vulnerabilityDatabaseBucket"
      where
        parseMounts :: KeyMap.KeyMap Value -> Parser (Map.Map Ecosystem MountConfig)
        parseMounts km = do
            pairs <-
                traverse
                    ( \(k, v) -> do
                        eco <- case parseEcosystem (Key.toText k) of
                            Just e -> pure e
                            Nothing -> fail ("Invalid ecosystem: " <> T.unpack (Key.toText k))
                        mcfg <- parseJSON v
                        pure (eco, mcfg)
                    )
                    (KeyMap.toList km)
            let mounts = Map.fromList pairs
            pure (Map.filter (isJust . mntPrivateUpstream) mounts)

        parseSecret :: Value -> Parser Secret
        parseSecret = withText "Secret" (pure . mkSecret)

        parseSeconds :: Value -> Parser NominalDiffTime
        parseSeconds (String t) = case readMaybe (T.unpack t) :: Maybe Integer of
            Just n | n >= 0 -> pure (fromInteger n)
            _ -> fail ("expected a non-negative integer count of seconds, got " <> show t)
        parseSeconds (Number n) =
            let val = truncate n :: Integer
             in if val >= 0 then pure (fromInteger val) else fail "expected a non-negative integer count of seconds"
        parseSeconds _ = fail "expected a String or Number for Seconds"

        parsePositiveInt :: String -> Int -> Parser Int
        parsePositiveInt field value
            | value > 0 = pure value
            | otherwise = fail (field <> " must be a positive integer")

instance FromJSON RulePatch where
    parseJSON = withObject "rules" $ \o ->
        RulePatch . Map.fromList <$> traverse decodeEntry (KeyMap.toList o)
      where
        decodeEntry (k, v) = (Key.toText k,) <$> parseJSON v

instance FromJSON RuleEntry where
    parseJSON = withObject "rule" $ \o -> do
        rejectSecretKeys o
        rejectUnknownKeys "rule" ["type", "precedence", "enabled", "ageSeconds", "scope", "identity"] o
        RuleEntry
            <$> o .:? "type"
            <*> o .:? "precedence"
            <*> o .:? "enabled"
            <*> o .:? "ageSeconds"
            <*> o .:? "scope"
            <*> o .:? "identity"
