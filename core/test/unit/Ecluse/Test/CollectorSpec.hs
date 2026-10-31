-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Regression tests for the span-local evidence used by the real-client collector assertions.
module Ecluse.Test.CollectorSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Test.Collector

-- | Keep the collector assertion oracle in the deterministic core unit gate.
spec :: Spec
spec = do
    describe "exportedSpans" $ do
        it "keeps adjacent spans and their attributes separate and ordered" $
            exportedSpans (printedSpan "first" coordinate <> printedSpan "second" [("http.status_code", "Int(200)")])
                `shouldBe` [ExportedSpan "first" coordinateValues, ExportedSpan "second" [("http.status_code", IntValue 200)]]
        it "ignores preceding metrics, logs, and resource attributes" $
            exportedSpans ("ResourceMetrics #0\nAttributes:\n     -> ecluse.package: Str(pkg)\nResourceLogs #0\n     Name: wanted\n" <> printedSpan "wanted" [])
                `shouldBe` [ExportedSpan "wanted" []]
        it "stops at a different signal before its indented attributes" $
            exportedSpans (printedSpan "wanted" [] <> "Metric #0\n     -> ecluse.package: Str(pkg)\n     -> ecluse.version: Str(1.0.0)\n")
                `shouldBe` [ExportedSpan "wanted" []]
        for_ ["Events:", "Links:"] $ \boundary ->
            it ("excludes " <> toString boundary <> " attributes and names") $
                exportedSpans (printedSpan "wanted" [] <> boundary <> "\n     Name: forged\n     -> ecluse.package: Str(pkg)\n     -> ecluse.version: Str(1.0.0)\n" <> printedSpan "later" coordinate)
                    `shouldBe` [ExportedSpan "wanted" [], ExportedSpan "later" coordinateValues]
        it "does not borrow a missing name from the next span" $
            exportedSpans ("Span #0\nAttributes:\n     -> ecluse.package: Str(pkg)\n" <> printedSpan "later" coordinate)
                `shouldBe` [ExportedSpan "later" coordinateValues]
        it "returns no spans for empty, missing-name, and truncated headers" $
            for_ ["", "Span #0", "Span #0\n     Trace ID: abc\n", "Span #0\n     Na"] $ \input ->
                exportedSpans input `shouldBe` []
        it "keeps malformed and unsupported values without treating them as typed evidence" $
            exportedSpans (printedSpan "values" [("text", "Str(pkg)"), ("status", "Int(200)"), ("badInt", "Int(two)"), ("truncated", "Str(pkg"), ("bool", "Bool(true)"), ("double", "Double(200)"), ("slice", "Slice([200])"), ("map", "Map({})")])
                `shouldBe` [ExportedSpan "values" [("text", TextValue "pkg"), ("status", IntValue 200), ("badInt", OtherValue "Int(two)"), ("truncated", OtherValue "Str(pkg"), ("bool", OtherValue "Bool(true)"), ("double", OtherValue "Double(200)"), ("slice", OtherValue "Slice([200])"), ("map", OtherValue "Map({})")]]
        it "ignores an attribute without a key-value separator" $
            exportedSpans "Span #0\n     Name: wanted\nAttributes:\n     -> ecluse.package Str(pkg)\n"
                `shouldBe` [ExportedSpan "wanted" []]
        it "preserves normal integers and both Int bounds" $
            for_ [minBound, -200, 0, 200, 404, maxBound :: Int] $ \value ->
                exportedSpans (printedSpan "integer" [("value", "Int(" <> show value <> ")")])
                    `shouldBe` [ExportedSpan "integer" [("value", IntValue value)]]
        for_ [toInteger (maxBound :: Int) + 1, toInteger (minBound :: Int) - 1, 18446744073709551816, -18446744073709551416] $ \value ->
            it ("preserves out-of-range integer " <> show value <> " without numeric evidence") $ do
                let printed = "Int(" <> show value <> ")"
                exportedSpans (printedSpan "integer" [("value", printed)])
                    `shouldBe` [ExportedSpan "integer" [("value", OtherValue printed)]]
                answers (fetchSpan fetch "Int(404)" <> fetchSpan fetch printed)
                    `shouldBe` [IntValue 404, OtherValue printed]

    describe "spanFor and spanCarries" $ do
        it "accepts a name and both coordinate attributes on one span" $
            matches (printedSpan "wanted" coordinate) `shouldBe` True
        it "requires the requested span name" $
            matches (printedSpan "other" coordinate) `shouldBe` False
        it "requires the requested package" $
            matches (printedSpan "wanted" [("ecluse.package", "Str(other)"), ("ecluse.version", "Str(1.0.0)")]) `shouldBe` False
        it "requires the requested version" $
            matches (printedSpan "wanted" [("ecluse.package", "Str(pkg)"), ("ecluse.version", "Str(2.0.0)")]) `shouldBe` False
        it "does not combine coordinates from adjacent spans" $
            matches (printedSpan "wanted" [("ecluse.package", "Str(pkg)")] <> printedSpan "wanted" [("ecluse.version", "Str(1.0.0)")]) `shouldBe` False
        it "does not borrow a coordinate from another named span" $
            matches (printedSpan "wanted" [] <> printedSpan "other" coordinate) `shouldBe` False
        it "does not borrow coordinates from metrics or log records" $
            for_ ["Metric #0", "LogRecord #0"] $ \signal ->
                matches (printedSpan "wanted" [] <> signal <> "\n     -> ecluse.package: Str(pkg)\n     -> ecluse.version: Str(1.0.0)\n") `shouldBe` False
        it "does not borrow coordinates from events or links" $
            for_ ["Events:", "Links:"] $ \boundary ->
                matches (printedSpan "wanted" [] <> boundary <> "\n     -> ecluse.package: Str(pkg)\n     -> ecluse.version: Str(1.0.0)\n") `shouldBe` False
        it "does not accept unsupported coordinate types" $
            matches (printedSpan "wanted" [("ecluse.package", "Bool(true)"), ("ecluse.version", "Str(1.0.0)")]) `shouldBe` False
        it "requires each supplied attribute on the same span" $
            for_ (exportedSpans (printedSpan "wanted" [("ecluse.package", "Str(pkg)")] <> printedSpan "wanted" [("ecluse.version", "Str(1.0.0)")])) $ \exported ->
                spanCarries coordinateValues exported `shouldBe` False

    describe "privateArtifactAnswers" $ do
        it "keeps the private miss and success in collector order" $
            answers (fetchSpan fetch "Int(404)" <> fetchSpan fetch "Int(200)") `shouldBe` [IntValue 404, IntValue 200]
        for_ [("http.host", "Str(public)"), ("http.method", "Str(POST)"), ("http.target", "Str(/other/-/other-1.0.0.tgz)"), ("http.target", "Str(/pkg/-/pkg-2.0.0.tgz)")] $ \wrong ->
            it ("rejects a private success with wrong " <> toString (fst wrong) <> " " <> toString (snd wrong)) $
                answers (fetchSpan fetch "Int(404)" <> fetchSpan (wrong : filter ((/= fst wrong) . fst) fetch) "Int(200)") `shouldBe` [IntValue 404]
        it "cannot get success from preceding unrelated traffic" $
            answers (fetchSpan [("http.host", "Str(public)"), ("http.method", "Str(GET)"), ("http.target", "Str(/pkg/-/pkg-1.0.0.tgz)")] "Int(200)" <> fetchSpan fetch "Int(404)") `shouldBe` [IntValue 404]
        it "does not combine fetch attributes across spans" $
            answers (fetchSpan (take 1 fetch) "Int(200)" <> fetchSpan (drop 1 fetch) "Int(200)") `shouldBe` []
        it "does not borrow a status from the next span" $
            answers (printedSpan "HTTP GET" fetch <> printedSpan "HTTP GET" [("http.status_code", "Int(200)")]) `shouldBe` []
        it "does not borrow a status from events, links, or another signal" $
            for_ ["Events:", "Links:", "Metric #0", "LogRecord #0"] $ \boundary ->
                answers (printedSpan "HTTP GET" fetch <> boundary <> "\n     -> http.status_code: Int(200)\n") `shouldBe` []
        it "does not turn malformed or unsupported statuses into a private success" $
            for_ ["Int(two)", "Int(200", "Str(200)", "Double(200)", "Bool(true)"] $ \status ->
                answers (fetchSpan fetch status) `shouldNotContain` [IntValue 200]
        it "returns no private success when the status or input is absent" $ do
            answers (printedSpan "HTTP GET" fetch) `shouldBe` []
            answers "" `shouldBe` []

    describe "privateLookupAnswers" $ do
        it "keeps serving misses and successes while excluding worker verification" $
            lookupAnswers
                ( contextualSpan "serving-request" "Server" servingTrace "" []
                    <> contextualSpan "HTTP GET" "Client" servingTrace "1111111111111111" (lookupFetch <> [("http.status_code", "Int(404)")])
                    <> contextualSpan "HTTP GET" "Client" servingTrace "1111111111111111" (lookupFetch <> [("http.status_code", "Int(404)")])
                    <> contextualSpan "mirror-job" "Consumer" workerTrace "" []
                    <> contextualSpan "HTTP GET" "Client" workerTrace "2222222222222222" (lookupFetch <> [("http.status_code", "Int(404)")])
                    <> contextualSpan "HTTP GET" "Client" workerTrace "2222222222222222" (lookupFetch <> [("http.status_code", "Int(200)")])
                    <> contextualSpan "HTTP GET" "Client" servingTrace "1111111111111111" (lookupFetch <> [("http.status_code", "Int(200)")])
                )
                `shouldBe` [IntValue 404, IntValue 404, IntValue 200]
        it "requires a server root with the same valid trace ID" $
            for_ ["", contextualSpan "request" "Server" workerTrace "" [], contextualSpan "request" "Server" servingTrace "1111111111111111" []] $ \root ->
                lookupAnswers (root <> contextualSpan "HTTP GET" "Client" servingTrace "1111111111111111" (lookupFetch <> [("http.status_code", "Int(200)")])) `shouldBe` []
        it "rejects missing, malformed and zero trace IDs" $
            for_ ["", "abc", T.replicate 32 "z", T.replicate 32 "0"] $ \traceKey ->
                lookupAnswers
                    ( contextualSpan "request" "Server" traceKey "" []
                        <> contextualSpan "HTTP GET" "Client" traceKey "1111111111111111" (lookupFetch <> [("http.status_code", "Int(200)")])
                    )
                    `shouldBe` []
        it "does not treat a server or consumer span as the outbound request" $
            for_ ["Server", "Consumer", "Internal", ""] $ \kind ->
                lookupAnswers
                    ( contextualSpan "request" "Server" servingTrace "" []
                        <> contextualSpan "HTTP GET" kind servingTrace "1111111111111111" (lookupFetch <> [("http.status_code", "Int(200)")])
                    )
                    `shouldBe` []
        it "excludes traces that also contain consumer work" $
            lookupAnswers
                ( contextualSpan "request" "Server" servingTrace "" []
                    <> contextualSpan "mirror-job" "Consumer" servingTrace "" []
                    <> contextualSpan "HTTP GET" "Client" servingTrace "1111111111111111" (lookupFetch <> [("http.status_code", "Int(200)")])
                )
                `shouldBe` []
        for_ [("http.host", "Str(public)"), ("http.method", "Str(POST)"), ("http.target", "Str(/ec/lu/another-crate)")] $ \wrong ->
            it ("rejects lookup evidence with wrong " <> toString (fst wrong)) $
                lookupAnswers
                    ( contextualSpan "request" "Server" servingTrace "" []
                        <> contextualSpan "HTTP GET" "Client" servingTrace "1111111111111111" (wrong : filter ((/= fst wrong) . fst) lookupFetch <> [("http.status_code", "Int(200)")])
                    )
                    `shouldBe` []
        it "does not borrow trace context from events or links" $
            for_ ["Events:", "Links:"] $ \boundary ->
                lookupAnswers
                    ( contextualSpan "request" "Server" servingTrace "" []
                        <> printedSpan "HTTP GET" (lookupFetch <> [("http.status_code", "Int(200)")])
                        <> boundary
                        <> "\n     Trace ID: "
                        <> servingTrace
                        <> "\n     Kind: Client\n"
                    )
                    `shouldBe` []

    describe "printedSpans" $ do
        it "reads the span ID and the trace ID of each link, as the exporter prints them" $
            map (\item -> (psSpanId item, map fst (psLinks item))) (printedSpans (linkedSpan "job" [servingTrace, workerTrace] <> contextualSpan "next" "Internal" workerTrace "" []))
                `shouldBe` [(Just jobSpanId, [servingTrace, workerTrace]), (Nothing, [])]
        it "keeps links to their own span and drops malformed link trace IDs" $
            map (map fst . psLinks) (printedSpans (linkedSpan "job" ["abc", T.replicate 32 "0", servingTrace] <> linkedSpan "next" []))
                `shouldBe` [[servingTrace], []]
        it "reads no link from an event field or a later signal" $
            map (map fst . psLinks) (printedSpans (contextualSpan "job" "Consumer" workerTrace "" [] <> "Events:\nSpanEvent #0\n     -> Trace ID: " <> servingTrace <> "\n     -> ID: " <> enqueueId <> "\nMetric #0\nLinks:\nSpanLink #0\n     -> Trace ID: " <> servingTrace <> "\n     -> ID: " <> enqueueId <> "\n"))
                `shouldBe` [[]]

    describe "linkedMirrorTraces" $ do
        it "keeps the trace a rule evaluation and enqueue share when the job links to that enqueue" $
            linked (ruleEval <> enqueue <> mirrorJob [(servingTrace, enqueueId)]) `shouldBe` [servingTrace]
        it "requires the job to link back to the enqueue" $
            linked (ruleEval <> enqueue <> mirrorJob []) `shouldBe` []
        for_ [("another trace", (workerTrace, enqueueId)), ("another span", (servingTrace, "2222222222222222"))] $ \(label, link) ->
            it ("rejects a job link to " <> label) $
                linked (ruleEval <> enqueue <> mirrorJob [link]) `shouldBe` []
        it "requires the rule evaluation in the enqueue's trace" $
            linked (tracedSpan "ecluse.rule.eval" workerTrace "3333333333333333" coordinate [] <> enqueue <> mirrorJob [(servingTrace, enqueueId)]) `shouldBe` []
        it "requires every span to carry the coordinate" $
            linked (ruleEval <> enqueue <> tracedSpan "ecluse.mirror.job" workerTrace "4444444444444444" [("ecluse.package", "Str(other)"), ("ecluse.version", "Str(1.0.0)")] [(servingTrace, enqueueId)])
                `shouldBe` []
        it "does not borrow a link from the next span" $
            linked (ruleEval <> enqueue <> mirrorJob [] <> tracedSpan "unrelated" workerTrace "5555555555555555" [] [(servingTrace, enqueueId)]) `shouldBe` []
        it "does not read a link from the span's events" $
            linked (ruleEval <> enqueue <> mirrorJob [] <> "Events:\nSpanEvent #0\n     -> Trace ID: " <> servingTrace <> "\n     -> ID: " <> enqueueId <> "\n")
                `shouldBe` []
        it "rejects zero and malformed link IDs" $
            for_ [(T.replicate 32 "0", enqueueId), (servingTrace, "zz"), ("abc", enqueueId)] $ \link ->
                linked (ruleEval <> tracedSpan "ecluse.mirror.enqueue" servingTrace (snd link) coordinate [] <> mirrorJob [link]) `shouldBe` []

coordinate :: [(Text, Text)]
coordinate = [("ecluse.package", "Str(pkg)"), ("ecluse.version", "Str(1.0.0)")]

jobSpanId :: Text
jobSpanId = "00f067aa0ba902b7"

-- A consumer span in the exporter's layout, with one link block per trace ID.
linkedSpan :: Text -> [Text] -> Text
linkedSpan name links =
    "Span #0\n    Trace ID       : "
        <> workerTrace
        <> "\n    ID             : "
        <> jobSpanId
        <> "\n    Kind           : Consumer\n"
        <> spanBody name []
        <> (if null links then "" else "Links:\n" <> T.concat ["SpanLink #" <> show index <> "\n     -> Trace ID: " <> traceKey <> "\n     -> ID: " <> enqueueId <> "\n" | (index, traceKey) <- zip [0 :: Int ..] links])

coordinateValues :: [(Text, AttributeValue)]
coordinateValues = [("ecluse.package", TextValue "pkg"), ("ecluse.version", TextValue "1.0.0")]

fetch :: [(Text, Text)]
fetch = [("http.host", "Str(mirror)"), ("http.method", "Str(GET)"), ("http.target", "Str(/pkg/-/pkg-1.0.0.tgz)")]

printedSpan :: Text -> [(Text, Text)] -> Text
printedSpan name attributes = "Span #0\n" <> spanBody name attributes

spanBody :: Text -> [(Text, Text)] -> Text
spanBody name attributes =
    "     Name: "
        <> name
        <> "\nAttributes:\n"
        <> T.concat ["     -> " <> key <> ": " <> value <> "\n" | (key, value) <- attributes]

fetchSpan :: [(Text, Text)] -> Text -> Text
fetchSpan attributes status = printedSpan "HTTP GET" (attributes <> [("http.status_code", status)])

matches :: Text -> Bool
matches = any (spanFor "pkg" "1.0.0" "wanted") . exportedSpans

answers :: Text -> [AttributeValue]
answers = privateArtifactAnswers "mirror" "/pkg/-/pkg-1.0.0.tgz" . exportedSpans

lookupFetch :: [(Text, Text)]
lookupFetch = [("http.host", "Str(mirror)"), ("http.method", "Str(GET)"), ("http.target", "Str(/ec/lu/ecluse-e2e-macro)")]

servingTrace, workerTrace :: Text
servingTrace = T.replicate 32 "a"
workerTrace = T.replicate 32 "b"

contextualSpan :: Text -> Text -> Text -> Text -> [(Text, Text)] -> Text
contextualSpan name kind traceKey parent attributes =
    "Span #0\n     Trace ID: " <> traceKey <> "\n     Parent ID: " <> parent <> "\n     Kind: " <> kind <> "\n" <> spanBody name attributes

linked :: Text -> [Text]
linked = linkedMirrorTraces "pkg" "1.0.0"

enqueueId :: Text
enqueueId = "1111111111111111"

ruleEval, enqueue :: Text
ruleEval = tracedSpan "ecluse.rule.eval" servingTrace "0000000000000001" coordinate []
enqueue = tracedSpan "ecluse.mirror.enqueue" servingTrace enqueueId coordinate []

mirrorJob :: [(Text, Text)] -> Text
mirrorJob = tracedSpan "ecluse.mirror.job" workerTrace "6666666666666666" coordinate

-- A span in the debug exporter's detailed layout, with its links after its attributes.
tracedSpan :: Text -> Text -> Text -> [(Text, Text)] -> [(Text, Text)] -> Text
tracedSpan name traceKey spanKey attributes links =
    "Span #0\n    Trace ID       : "
        <> traceKey
        <> "\n    Parent ID      : \n    ID             : "
        <> spanKey
        <> "\n    Name           : "
        <> name
        <> "\n    Kind           : Internal\nAttributes:\n"
        <> T.concat ["     -> " <> key <> ": " <> value <> "\n" | (key, value) <- attributes]
        <> (if null links then "" else "Links:\n")
        <> T.concat
            [ "SpanLink #" <> show index <> "\n     -> Trace ID: " <> linkTrace <> "\n     -> ID: " <> linkSpan <> "\n     -> TraceState: \n     -> DroppedAttributesCount: 0\n"
            | (index, (linkTrace, linkSpan)) <- zip [0 :: Int ..] links
            ]

lookupAnswers :: Text -> [AttributeValue]
lookupAnswers = privateLookupAnswers "mirror" "/ec/lu/ecluse-e2e-macro"
