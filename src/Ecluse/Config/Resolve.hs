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
    mountEnvKey,
) where

import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isUpper)

import Data.Text qualified as T

{- | Right-biased deep merge of two Aeson Values.
Objects merge recursively. The right side (the higher precedence value) overwrites
any other type, Arrays and Strings included.
-}
deepMerge :: Value -> Value -> Value
deepMerge (Object l) (Object r) = Object $ KeyMap.unionWith deepMerge l r
deepMerge _ r = r

{- | Convert a list of environment variables into a nested JSON Object.
It keeps only keys starting with @ECLUSE_@ and strips the prefix.
Double underscores (@__@) represent nested object paths.
Single underscores (@_@) become camelCase for Aeson key matching.
For example, @ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM@ becomes
@{"mounts": {"npm": {"privateUpstream": ...}}}@.
It decodes a value that parses as valid JSON, a number or a boolean say.
Anything else stays a String.

Two kinds of variable never enter the AST. The first is anything outside the
@ECLUSE_@ prefix: the composition root reads the ambient SDK environment, @AWS_*@
included, directly (see "Ecluse.Config.Ambient"). The second is the reserved
process-level @ECLUSE_CONFIG@, the config-document path override. "Ecluse.Boot"
consumes it before it reads the document, so it must not double as a document key.
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

{- | @ECLUSE_@-prefixed variables that address the boot process, not the config
document. The boot consumes them before resolution, and they never become document
keys.
-}
reservedProcessKeys :: [Text]
reservedProcessKeys = ["ECLUSE_CONFIG"]

{- | The secret-typed leaf keys of the config schema, in their document spelling.
The one source for every site that treats a secret specially. The environment layer
takes their values verbatim ('buildEnvAst'). The provenance dump renders these leaves
redacted (see "Ecluse.Config"). Their env spellings are the only file-shaped side
door for the @*_FILE@ indirection (see "Ecluse.Boot").
-}
secretLeafKeys :: [Text]
secretLeafKeys = ["authToken", "mirrorTargetToken", "publicationTargetToken"]

-- | 'secretLeafKeys' in their environment spelling (@authToken@ -> @AUTH_TOKEN@).
secretEnvSpellings :: [Text]
secretEnvSpellings = map envSpellingOf secretLeafKeys

{- | The environment spelling of a camelCase document key (@authToken@ ->
@AUTH_TOKEN@). It inverts the camelCase reconstruction 'buildEnvAst' applies, so the
key a boot error tells an operator to set is exactly the key the resolver reads.
-}
envSpellingOf :: Text -> Text
envSpellingOf = T.toUpper . T.concatMap underscoreUpper
  where
    underscoreUpper c
        | isUpper c = "_" <> T.singleton c
        | otherwise = T.singleton c

{- | The full environment key of a mount-scoped setting
(@ECLUSE_MOUNTS__{ECOSYSTEM}__{KEY}@), assembled from the mount's ecosystem name and
the setting's already env-spelled suffix. The one home shared by the config-load
errors ("Ecluse.Config.Types") and the boot-error renderings
("Ecluse.Composition.BootError").
-}
mountEnvKey :: Text -> Text -> Text
mountEnvKey ecosystem key = "ECLUSE_MOUNTS__" <> T.toUpper ecosystem <> "__" <> key

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
