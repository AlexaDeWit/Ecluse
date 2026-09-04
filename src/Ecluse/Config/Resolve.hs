-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

{- |
Hierarchical configuration resolution (Viper-style).
Unifies defaults, configuration files, and environment variables into a single
resolution tree with strict precedence: Defaults < File < Env.
-}
module Ecluse.Config.Resolve (
    deepMerge,
    buildEnvAst,
    secretLeafKeys,
    secretEnvSpellings,
    envSpellingOf,
    mountKeyRef,
    mountDocRef,
) where

import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isUpper)

import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)

{- | Right-biased deep merge of two Aeson Values. Objects merge recursively.
The right side overwrites any other type, arrays and strings included.
-}
deepMerge :: Value -> Value -> Value
deepMerge (Object l) (Object r) = Object $ KeyMap.unionWith deepMerge l r
deepMerge _ r = r

{- | Convert @ECLUSE_@-prefixed environment variables into a nested JSON Object, prefix stripped:
@__@ descends into an object and @_@ joins a camelCase word ('mountKeyRef' inverts it).
-}
buildEnvAst :: [(String, String)] -> Value
buildEnvAst env =
    foldl' deepMerge (Object KeyMap.empty) (map envVarValue configVars)
  where
    configVars = [(key, v) | (name, v) <- env, Just key <- [configEnvKey (T.pack name)]]

configEnvKey :: Text -> Maybe Text
configEnvKey name
    | name `elem` reservedProcessKeys = Nothing
    | otherwise = T.stripPrefix "ECLUSE_" name

{- | @ECLUSE_@-prefixed variables that address the boot process, not the config document.
"Ecluse.Boot" consumes them before resolution, so they never become document keys.
-}
reservedProcessKeys :: [Text]
reservedProcessKeys = ["ECLUSE_CONFIG"]

{- | The secret-typed leaf keys, in their document spelling: @token@ is the one under every store
tag. Verbatim env values, redacted provenance, and the @*_FILE@ indirection all read them here.
-}
secretLeafKeys :: [Text]
secretLeafKeys = ["authToken", "token"]

-- | 'secretLeafKeys' in their environment spelling (@authToken@ -> @AUTH_TOKEN@).
secretEnvSpellings :: [Text]
secretEnvSpellings = map envSpellingOf secretLeafKeys

{- | The environment spelling of a camelCase document key (@authToken@ -> @AUTH_TOKEN@).
It inverts 'buildEnvAst', so a boot error names exactly the key the resolver reads.
-}
envSpellingOf :: Text -> Text
envSpellingOf = T.toUpper . T.concatMap underscoreUpper
  where
    underscoreUpper c
        | isUpper c = "_" <> T.singleton c
        | otherwise = T.singleton c

{- | The environment key a mount-scoped document path resolves from: @mirrorTarget.codeArtifact.url@
on npm gives @ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__URL@. Every refusal builds it here.
-}
mountKeyRef :: Ecosystem -> Text -> Text
mountKeyRef eco path =
    "ECLUSE_MOUNTS__"
        <> T.toUpper (ecosystemName eco)
        <> "__"
        <> T.intercalate "__" (map envSpellingOf (T.splitOn "." path))

-- | 'mountKeyRef' in the document spelling: @mounts.npm.mirrorTarget.codeArtifact.url@.
mountDocRef :: Ecosystem -> Text -> Text
mountDocRef eco path = "mounts." <> ecosystemName eco <> "." <> path

envVarValue :: (Text, String) -> Value
envVarValue (key, value) =
    nest segments (leafValue (T.pack value))
  where
    segments = map toCamelCase (T.splitOn "__" key)
    -- A secret is taken verbatim: JSON-parsing it would coerce a value like
    -- 12345 or true into a non-string the secret parser rightly refuses.
    leafValue
        | maybe False ((`elem` secretLeafKeys) . Key.toText) (listToMaybe (reverse segments)) = String
        | otherwise = parseEnvValue

toCamelCase :: Text -> Key.Key
toCamelCase t =
    let words' = filter (not . T.null) (T.splitOn "_" t)
     in Key.fromText $ case words' of
            [] -> ""
            (w : ws) -> T.toLower w <> T.concat (map T.toTitle ws)

nest :: [Key.Key] -> Value -> Value
nest [] v = v
nest (p : ps) v = Object $ KeyMap.singleton p (nest ps v)

parseEnvValue :: Text -> Value
parseEnvValue txt = case eitherDecodeStrict (encodeUtf8 txt) of
    Right v -> v
    Left _ -> String txt
