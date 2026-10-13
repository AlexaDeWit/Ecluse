-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Generate exact native merge inputs without running a measurement.
module Main (main) where

import Ecluse.Core.Server.MemoryModel.Probe (packages, writeMergeDocuments)
import Ecluse.Test.Corpus (CorpusPackage (cpPath))
import Ecluse.Test.Corpus.Merge (MergeShape (HeavyBase, PublishOrder))

-- | This entrypoint belongs only to a private helper checkout.
main :: IO ()
main =
    getArgs >>= \case
        ["HeavyBase", "bench/corpus/npm/aws-sdk-client-s3.full.json", directory] -> generate HeavyBase "bench/corpus/npm/aws-sdk-client-s3.full.json" directory
        ["PublishOrder", "bench/corpus/npm/lodash.full.json", directory] -> generate PublishOrder "bench/corpus/npm/lodash.full.json" directory
        _ -> fail ("unknown fixed merge input request" :: String)
  where
    generate shape path directory = do
        package <- maybe (fail ("unknown committed corpus path" :: String)) pure (find ((== path) . cpPath) packages)
        (private, public) <- writeMergeDocuments shape directory package
        putStrLn private
        putStrLn public
