-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the resource labels and exemplars the product's exporter includes in counter scrapes.
module Ecluse.Test.ExpositionSpec (spec) where

import Test.Hspec

import Ecluse.Test.Exposition (Sample (..), parseExposition, parseExpositionStrict)

spec :: Spec
spec = do
    describe "parseExposition" $ do
        it "keeps the report parser's prefix reads and skips unreadable lines" $
            parseExposition
                ( unlines
                    [ "# TYPE metric counter"
                    , "metric{note=\"a\\q\",note=\"second\"} 2 ignored tail"
                    , "metric{left=\"x\"right=\"y\"} 3"
                    , "1metric 4"
                    , "not a sample"
                    , ""
                    ]
                )
                `shouldBe` [ Sample "metric" [("note", "aq"), ("note", "second")] 2
                           , Sample "metric" [("left", "x"), ("right", "y")] 3
                           , Sample "1metric" [] 4
                           ]
    describe "parseExpositionStrict" $ do
        it "reads the pinned exporter's resource labels without splitting their spaces" $
            parseExpositionStrict
                ( unlines
                    [ "# TYPE ecluse_rule_eval_duration histogram"
                    , "ecluse_rule_eval_duration_count{job=\"ecluse\",process_runtime_description=\"ghc 9.10.3\",ecosystem=\"pypi\"} 7"
                    , "ecluse_mirror_enqueued{process_runtime_description=\"ghc 9.10.3\"} 2 # {trace_id=\"0123456789abcdef0123456789abcdef\",span_id=\"0123456789abcdef\"} 1"
                    , ""
                    ]
                )
                `shouldBe` Right
                    [ Sample "ecluse_rule_eval_duration_count" [("job", "ecluse"), ("process_runtime_description", "ghc 9.10.3"), ("ecosystem", "pypi")] 7
                    , Sample "ecluse_mirror_enqueued" [("process_runtime_description", "ghc 9.10.3")] 2
                    ]
        it "reads escaped quotes, backslashes, commas, and newlines within labels" $
            parseExpositionStrict "metric{note=\"a \\\"quoted\\\", value\\\\end\\n\"} 3"
                `shouldBe` Right [Sample "metric" [("note", "a \"quoted\", value\\end\n")] 3]
        it "reads histogram buckets and keeps the bucket value apart from its exemplar" $
            parseExpositionStrict "metric_bucket{le=\"+Inf\"} 9 # {trace_id=\"trace\",span_id=\"span\"} 0.125"
                `shouldBe` Right [Sample "metric_bucket" [("le", "+Inf")] 9]
        it "reads scientific values, tabs, empty label sets, and optional timestamps" $
            parseExpositionStrict "metric{}\t1.25e-3 -123 # {trace_id=\"trace\"} 0.5 123.25"
                `shouldBe` Right [Sample "metric" [] 0.00125]
        it "reads both infinite sample values" $ do
            fmap (map (isInfinite . sampleValue)) (parseExpositionStrict "positive +Inf\nnegative -Inf")
                `shouldBe` Right [True, True]
        it "reads the exporter's NaN spelling" $
            fmap (map (isNaN . sampleValue)) (parseExpositionStrict "metric NaN")
                `shouldBe` Right [True]
        it "skips only blank lines and comment lines" $
            parseExpositionStrict "\n  # HELP metric a description\n\t\n"
                `shouldBe` Right []
        for_ malformedSamples $ \line ->
            it ("rejects the malformed sample " <> toString line) $
                parseExpositionStrict line `shouldSatisfy` isLeft
        it "reports the first malformed line without dropping an earlier sample" $
            parseExpositionStrict "# TYPE metric gauge\nmetric 1\nbroken\nmetric 2"
                `shouldBe` Left "metrics scrape contains a malformed sample at line 3: broken"

malformedSamples :: [Text]
malformedSamples =
    [ "broken"
    , "metric not-a-number"
    , "metric (1)"
    , "metric +-1"
    , "metric 1 extra"
    , "metric 1 123 extra"
    , "metric 1 1.5"
    , "metric 1 (123)"
    , "metric{note=\"unterminated} 1"
    , "metric{note=\"bad\\q\"} 1"
    , "metric{left=\"x\"right=\"y\"} 1"
    , "metric{note=\"x\",note=\"y\"} 1"
    , "metric{note=\"x\"}1"
    , "metric{,note=\"x\"} 1"
    , "1metric 1"
    , "metric 1 #"
    , "metric 1 # {} 2"
    , "metric 1 # {trace_id=\"unterminated} 2"
    , "metric 1 # {trace_id=\"trace\"} not-a-number"
    , "metric 1 # {trace_id=\"trace\"} 2 bad-timestamp"
    , "metric 1 # {trace_id=\"trace\"} 2 NaN"
    , "metric 1 # {trace_id=\"trace\"} 2 +Inf"
    , "metric 1 # {trace_id=\"trace\"} 2 123 extra"
    ]
