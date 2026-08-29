-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.DocCoverageSpec (spec) where

import Data.Char (isAsciiLower)
import Data.Text qualified as T
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Hspec

import Ecluse.Config (loadConfig)

{- | Every operator-facing @ECLUSE_*@ spelling, paired with a value the loader must accept.
Each must have its document key in @config\/default.yaml@ (active or commented) and must load,
so listing a new key here is part of adding it.
-}
documentedEnvVars :: [(String, String)]
documentedEnvVars =
    [ ("ECLUSE_SERVER__PORT", "8081")
    , ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")
    , ("ECLUSE_SERVER__AUTH_TOKEN", "edge-token")
    , ("ECLUSE_SERVER__HELP_MESSAGE", "ask platform engineering")
    , ("ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT", "5")
    , ("ECLUSE_QUEUE__URL", "https://sqs.us-east-1.amazonaws.com/123456789012/mirror")
    , ("ECLUSE_QUEUE__MEMORY_MAX_DEPTH", "100")
    , ("ECLUSE_QUEUE__MAX_RECEIVE_COUNT", "8")
    , ("ECLUSE_LIMITS__MAX_RESPONSE_BYTES", "1048576")
    , ("ECLUSE_LIMITS__MAX_REQUEST_BYTES", "1048576")
    , ("ECLUSE_LIMITS__MAX_VERSION_COUNT", "100")
    , ("ECLUSE_LIMITS__MAX_NESTING_DEPTH", "16")
    , ("ECLUSE_CACHE__TTL", "30")
    , ("ECLUSE_CACHE__MAX_ENTRIES", "64")
    , ("ECLUSE_CACHE__MAX_BYTES", "1048576")
    , ("ECLUSE_INTEGRITY__MIN_PUBLIC", "sha256")
    , ("ECLUSE_INTEGRITY__MIN_TRUSTED", "sha256")
    , ("ECLUSE_INTEGRITY__DIVERGENCE_POLICY", "warn")
    , ("ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES", "198.18.0.0/15")
    , ("ECLUSE_ADVISORIES__BUCKET", "advisories")
    , ("ECLUSE_ADVISORIES__POLL_INTERVAL", "60")
    , ("ECLUSE_ADVISORIES__COMPILE_INTERVAL", "3600")
    , ("ECLUSE_ADVISORIES__DATA_DIR", "data/osv")
    , ("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", "https://osv.example.test")
    , ("ECLUSE_ADVISORIES__MAX_DATABASE_BYTES", "1048576")
    , ("ECLUSE_RUNTIME__CORES", "2")
    , ("ECLUSE_RUNTIME__MAX_HEAP_BYTES", "268435456")
    , ("ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT", "8")
    , ("ECLUSE_RUNTIME__PUBLIC_CONNECTIONS_PER_HOST", "4")
    , ("ECLUSE_RUNTIME__PRIVATE_CONNECTIONS_PER_HOST", "4")
    , ("ECLUSE_OBSERVABILITY__LOG_FORMAT", "json")
    , ("ECLUSE_OBSERVABILITY__LOG_LEVEL", "info")
    , ("ECLUSE_OBSERVABILITY__TELEMETRY", "off")
    , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM", "https://registry.npmjs.org")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "mirror-write-token")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION", "3600")
    , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test")
    , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN", "publish-token")
    , ("ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW", "@acme")
    , ("ECLUSE_MOUNTS__NPM__MIN_TRUSTED_INTEGRITY", "sha256")
    , ("ECLUSE_MOUNTS__NPM__DIVERGENCE_POLICY", "warn")
    ]

{- | Process-level and indirection spellings: documented in the operator manual's prose,
because they are consumed before (or beside) config resolution and have no document key.
-}
documentedProcessVars :: [String]
documentedProcessVars =
    [ "ECLUSE_CONFIG"
    , "ECLUSE_RULES"
    , "ECLUSE_SERVER__AUTH_TOKEN_FILE"
    , "ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN_FILE"
    , "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN_FILE"
    ]

-- | Every Markdown page of the operator manual, read as one text.
readOperatorManual :: IO Text
readOperatorManual = do
    names <- listDirectory manualDir
    pages <- traverse (readFileBS . (manualDir </>)) (filter isMarkdown names)
    pure (foldMap decodeUtf8 pages)
  where
    manualDir = "web/content/docs"
    isMarkdown = (== ".md") . takeExtension

{- | The document key an @ECLUSE_*@ spelling resolves to: its top-level section and its
leaf, camel-cased the way the resolver reads them (@ECLUSE_CACHE__MAX_BYTES@ is
@cache.maxBytes@). The mount segment in between is irrelevant to the lookup. 'Nothing'
for a string that is not an @ECLUSE_@ spelling, which the test then reports as missing.
-}
documentKey :: String -> Maybe (Text, Text)
documentKey var = do
    body <- T.stripPrefix "ECLUSE_" (T.pack var)
    top :| rest <- nonEmpty (T.splitOn "__" body)
    pure (T.toLower top, camel (lastOf top rest))
  where
    lastOf x [] = x
    lastOf _ (y : ys) = lastOf y ys
    camel segment = case T.splitOn "_" (T.toLower segment) of
        (w : ws) -> w <> T.concat (map T.toTitle ws)
        [] -> ""

{- | Whether @config/default.yaml@ documents a leaf key under a top-level section. A
commented key (@# key:@) counts, section and leaf alike: it is how the file documents a
computed default and a dormant section.
-}
documentsKey :: Text -> (Text, Text) -> Bool
documentsKey yaml (section, leaf) = any keyLine sectionLines
  where
    sectionLines =
        takeWhile (not . isSection)
            . drop 1
            . dropWhile (not . isHeader)
            $ T.lines yaml
    uncomment = T.stripStart . T.dropWhile (== '#') . T.stripStart
    topLevel l = not (T.isPrefixOf " " l)
    isHeader l = topLevel l && uncomment l == section <> ":"
    isSection l = topLevel l && maybe False (T.all isAsciiLower) (T.stripSuffix ":" (uncomment l))
    keyLine l = T.isPrefixOf (leaf <> ":") (uncomment l)

spec :: Spec
spec = describe "the configuration reference covers the accepted variables" $ do
    it "config/default.yaml documents every golden-list key" $ do
        yaml <- decodeUtf8 <$> readFileBS "config/default.yaml"
        let missing =
                [ var
                | var <- map fst documentedEnvVars
                , maybe True (not . documentsKey yaml) (documentKey var)
                ]
        missing `shouldBe` []

    it "the operator manual mentions every process-level spelling" $ do
        manual <- readOperatorManual
        let missing =
                [ var
                | var <- documentedProcessVars
                , not (T.pack var `T.isInfixOf` manual)
                ]
        missing `shouldBe` []

    it "keeps the golden list honest: every listed variable loads together" $
        loadConfig documentedEnvVars Nothing `shouldSatisfy` isRight
