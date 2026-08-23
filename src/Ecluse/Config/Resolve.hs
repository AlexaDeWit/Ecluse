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

{- | Convert environment variables into a nested JSON Object. It keeps @ECLUSE_@-prefixed keys
and strips the prefix. Double underscores nest, single underscores become camelCase, so
@ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM@ becomes @{"mounts": {"npm": {"privateUpstream": ...}}}@.
A value that parses as JSON decodes, anything else stays a String. Anything outside the prefix
is never config: the composition root reads the ambient SDK environment, @AWS_*@ included,
directly (see "Ecluse.Config.Ambient").
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

{- | The secret-typed leaf keys of the config schema, in their document spelling. The one source
for every site that treats a secret specially: verbatim env values, redacted provenance, and the
@*_FILE@ indirection ("Ecluse.Boot").
-}
secretLeafKeys :: [Text]
secretLeafKeys = ["authToken", "mirrorTargetToken", "publicationTargetToken"]

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

{- | The environment key a mount-scoped document key resolves from, @mirrorTargetToken@ on npm
giving @ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN@. Every refusal that names one builds it here.
-}
mountKeyRef :: Ecosystem -> Text -> Text
mountKeyRef eco key =
    "ECLUSE_MOUNTS__" <> T.toUpper (ecosystemName eco) <> "__" <> envSpellingOf key

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
