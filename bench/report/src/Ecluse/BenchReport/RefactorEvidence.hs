-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Per-pass bindings keep archived observations tied to their source, build and execution.
module Ecluse.BenchReport.RefactorEvidence (
    Binding (..),
    ComparisonScope (..),
    passSchedule,
    scopeSchedule,
    scopeName,
    scopeProblems,
    writePassConditions,
    recordBinding,
    verifyEvidence,
    bindingFaults,
    receiptArtifactFaults,
    nativeReceiptPaths,
    orderProblems,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.FilePath (isAbsolute, splitDirectories, takeFileName, (</>))

import Ecluse.BenchReport.RefactorFiles (captureProblems, readComparisonText)
import Ecluse.BenchReport.RefactorInputAudit (auditInputs, expectedProbeInputs)
import Ecluse.BenchReport.RefactorInputs (Input (..))
import Ecluse.Test.Contract.Manifest (digest, digestFile, jsonLine, readJsonFile, writeJsonFile)
import Ecluse.Test.Contract.Performance (PerformanceReceipt (prArtifacts, prSubject))
import Ecluse.Test.Contract.Result (auditExactIds)
import Ecluse.Test.Corpus (CorpusPackage (cpPath), corpusPackages, pypiCorpusPackages)

-- | Paths are relative to the uploaded evidence root. Every required artifact carries a SHA-256 digest.
data Binding = Binding
    { bindingSchema :: Int
    , bindingContext :: Map Text Text
    , bindingArtifacts :: Map FilePath Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Binding
instance FromJSON Binding

-- | Fixed entrypoints select either the complete comparison or the four missing listing passes.
data ComparisonScope = FullComparison | ListingComparison
    deriving stock (Eq, Show)

-- | Scope labels remain part of the new run's identity.
scopeName :: ComparisonScope -> Text
scopeName FullComparison = "full"
scopeName ListingComparison = "listing"

-- | The serial order is fixed independently of available results.
passSchedule :: [(Text, Text)]
passSchedule =
    [("benchmark", name) | name <- ["A1", "A2", "B1", "A3", "B2", "B3"]]
        <> [("allocation", name) | name <- ["A1", "B1", "A2", "B2"]]
        <> [("residency", name) | name <- ["A1", "B1", "A2", "B2"]]
        <> [(kind, name) | kind <- ["versions", "listing"], name <- ["A1", "B1", "A2", "B2"]]

-- | A listing recovery cannot select fewer cases or borrow passes from another run.
scopeSchedule :: ComparisonScope -> [(Text, Text)]
scopeSchedule FullComparison = passSchedule
scopeSchedule ListingComparison = [("listing", name) | name <- ["A1", "B1", "A2", "B2"]]

-- | Older full runs have no scope field. A recovery always requires an explicit listing scope.
scopeProblems :: ComparisonScope -> Map Text Text -> [Text]
scopeProblems scope recorded = ["declared comparison scope differs" | not accepted]
  where
    accepted = case Map.lookup "scope" recorded of
        Nothing -> scope == FullComparison
        Just value -> value == scopeName scope

-- | Task writes these declarations before starting the measured process.
writePassConditions :: FilePath -> Text -> Text -> IO ()
writePassConditions root kind name = contextFor root kind name >>= writeJsonFile (root </> passPath kind name </> "conditions.json")

contextFor :: FilePath -> Text -> Text -> IO (Map Text Text)
contextFor root kind name = do
    run <- readJsonFile (root </> "run.json")
    let side = sideOf name
    source <- required ("source" <> side) run
    tree <- required ("tree" <> side) run
    let build = (if kind == "listing" then "listing-build-" else "build-") <> toString side
        target = case kind of
            "benchmark" -> "ecluse-bench"
            "allocation" -> "perf-acceptance"
            _ -> "ecluse-residency"
    binaries <- executableDigests (root </> build </> if kind == "listing" then "executable.sha256" else "executables.sha256")
    binary <- maybe (fail "measured executable is missing") pure (Map.lookup (toString target) binaries)
    plan <- digestFile (root </> build </> "plan.json")
    overlay <- if kind == "listing" then digestFile (root </> build </> "overlay.patch") else pure "none"
    worker <- digestFile (root </> "worker.txt")
    inputs <- traverse (digestFile . (root </>) . ("inputs" </>)) ["versions.json", "listings.json", "capture-sizes.json"]
    let rts = case kind of
            "benchmark" -> "-T"
            "allocation" -> "shipped-rts, checked against native capabilities and allocationAreaBytes"
            "residency" -> "suite -T, children -T -N1, listing and merge -F1 -A128k"
            _ -> "-N1 -A4m -T"
    pure (Map.union (Map.fromList [("source", source), ("tree", tree), ("kind", kind), ("pass", name), ("rts", rts), ("executable", target), ("executableSha256", binary), ("buildPlanSha256", plan), ("overlaySha256", overlay), ("workerSha256", worker), ("inputsSha256", digest (jsonLine inputs))]) run)

-- | Seal a completed pass after its status and final build snapshot exist.
recordBinding :: FilePath -> Text -> Text -> IO ()
recordBinding root kind name = do
    context <- contextFor root kind name
    paths <- evidencePaths root kind name
    artifacts <- Map.fromList <$> traverse (\path -> (path,) <$> digestFile (root </> path)) paths
    writeJsonFile (root </> passPath kind name </> "binding.json") (Binding 1 context artifacts)

-- | Rehash artifacts, compare snapshots and validate the entire recorded execution before reading measurements.
verifyEvidence :: ComparisonScope -> FilePath -> Text -> Text -> Text -> Text -> Text -> Text -> IO [Text]
verifyEvidence scope root runId attempt baseline baselineTree candidate candidateTree = do
    inputFaults <- auditInputs root
    runFaults <- captureProblems $ do
        run <- readJsonFile (root </> "run.json")
        let expected = Map.fromList [("run", runId), ("attempt", attempt), ("sourceA", baseline), ("treeA", baselineTree), ("sourceB", candidate), ("treeB", candidateTree), ("architecture", "aarch64")]
        worker <- readComparisonText (root </> "worker.txt")
        controller <- T.lines <$> readComparisonText (root </> "controller.txt")
        declaredController <- traverse (`required` run) ["controllerSource", "controllerTree"]
        runner <- required "runner" run
        image <- required "image" run
        pure
            ( scopeProblems scope run
                <> ["run identity mismatch: " <> key | (key, value) <- Map.toList expected, Map.lookup key run /= Just value]
                <> ["worker run or attempt mismatch" | not (all (`elem` T.lines worker) ["run=" <> runId, "attempt=" <> attempt])]
                <> ["worker architecture missing" | not ("aarch64" `T.isInfixOf` worker)]
                <> ["worker identity mismatch" | T.null runner || T.null image || not (all (`elem` T.lines worker) ["runner=" <> runner, "image=" <> image])]
                <> ["invalid controller source identity" | controller /= declaredController || not (all (validHex 40) controller)]
            )
    faults <- forM (scopeSchedule scope) $ \(kind, name) -> captureProblems $ do
        context <- contextFor root kind name
        paths <- evidencePaths root kind name
        binding <- readJsonFile (root </> passPath kind name </> "binding.json")
        bound <- bindingFaults root context paths binding
        snapshots <- snapshotFaults root kind name context
        pure (map ((kind <> "-" <> name <> ": ") <>) (bound <> snapshots))
    orderFaults <- captureProblems (checkOrder scope root)
    preparationFaults <- if scope == ListingComparison then captureProblems (zeroStatus root "input-audit.status") else pure []
    sharedFaults <- captureProblems $ concat <$> traverse (equalFile root "build-A" "build-B") ["locks.sha256", "corpus.sha256", "toolchain.txt"]
    pure (inputFaults <> runFaults <> concat faults <> orderFaults <> preparationFaults <> sharedFaults)

-- | Reject changed, missing or unbound files and stale execution declarations.
bindingFaults :: FilePath -> Map Text Text -> [FilePath] -> Binding -> IO [Text]
bindingFaults root expected paths binding = do
    digests <- artifactFaults root (bindingArtifacts binding)
    pure
        ( ["unsupported binding schema" | bindingSchema binding /= 1]
            <> ["pass binding identity differs" | bindingContext binding /= expected]
            <> auditExactIds "bound artifact" (map toText paths) (map toText (Map.keys (bindingArtifacts binding)))
            <> digests
        )

artifactFaults :: FilePath -> Map FilePath Text -> IO [Text]
artifactFaults root artifacts =
    concat
        <$> forM
            (Map.toList artifacts)
            ( \(path, expectedDigest) -> captureProblems $ do
                unless (safePath path) (fail "artifact path escapes the evidence root")
                actual <- digestFile (root </> path)
                pure ["artifact digest mismatch: " <> toText path | not (validHex 64 expectedDigest) || expectedDigest /= actual]
            )

evidencePaths :: FilePath -> Text -> Text -> IO [FilePath]
evidencePaths root kind name = do
    run <- readJsonFile (root </> "run.json") :: IO (Map Text Text)
    let side = toString (sideOf name)
        build = "build-" <> side
        directory = passPath kind name
    corpus <- checksums (root </> build </> "corpus.sha256")
    probeFiles <-
        if kind `elem` ["versions", "listing"]
            then do
                inputs <- expectedProbeInputs root kind
                pure [directory </> toString (inputId input) <> extension | input <- inputs, extension <- [".json", ".rss", ".status"]]
            else pure []
    let common =
            ["controller.txt", "worker.txt", "run.json", "validator.sha256", "controller-build.status", "controls.status"]
                <> [path | Map.lookup "scope" run == Just ("listing" :: Text), path <- ["input-audit.status", "input-audit.log"]]
                <> map ("inputs" </>) ["versions.json", "versions.tsv", "listings.json", "capture-sizes.json", "small.json", "large.json", "small.expected", "large.expected"]
                <> map (build </>) ("status" : "output.txt" : identityFiles)
                <> [build </> "config" </> path | path <- ["ecluse.cabal", "cabal.project", "cabal.project.freeze", "flake.lock"]]
                <> [directory </> phase </> path | phase <- ["before", "after"], path <- identityFiles]
                <> map (directory </>) ["conditions.json", "conditions.status", "conditions.log", "events.txt", "pass.status", "pass.log", "measure.status", "measure.log", "audit.status"]
                <> ["source-inputs-" <> side </> path | path <- "acceptance/criteria.json" : Map.keys corpus]
        native =
            if kind `elem` ["benchmark", "allocation"]
                then
                    map (directory </>) (nativeArtifacts kind <> ["audit.log", "receipts/conditions.json", "receipts/native-status"])
                        <> [directory </> "receipts" </> subject </> file | subject <- ["ecosystem-npm", "ecosystem-pypi"], file <- ["obligation.json", "request.json", "receipt.json"]]
                else []
        overlay =
            if kind == "listing"
                then
                    map (("listing-build-" <> side) </>) ["status", "output.txt", "source.txt", "plan.json", "executable.sha256", "overlay.patch", "overlay.sha256", "ListingAdapter.hs", "Main.hs"]
                        <> map (directory </>) ["overlay.patch", "overlay-after.patch", "listing-before.sha256", "listing-after.sha256"]
                else []
    pure (common <> native <> probeFiles <> overlay)

identityFiles :: [FilePath]
identityFiles = ["source.txt", "corpus.sha256", "locks.sha256", "plan.json", "plan.sha256", "executables.sha256", "toolchain.txt"]

snapshotFaults :: FilePath -> Text -> Text -> Map Text Text -> IO [Text]
snapshotFaults root kind name context = do
    let build = "build-" <> toString (sideOf name)
        directory = passPath kind name
    source <- required "source" context
    tree <- required "tree" context
    conditions <- readJsonFile (root </> directory </> "conditions.json")
    identities <- forM [build, directory </> "before", directory </> "after"] $ \snapshot -> do
        sourceLines <- T.lines <$> readComparisonText (root </> snapshot </> "source.txt")
        plan <- digestFile (root </> snapshot </> "plan.json")
        planRecord <- checksums (root </> snapshot </> "plan.sha256")
        binaries <- executableDigests (root </> snapshot </> "executables.sha256")
        pure
            ( ["source or tree mismatch: " <> toText snapshot | sourceLines /= [source, tree] || not (all (validHex 40) sourceLines)]
                <> ["build plan digest mismatch: " <> toText snapshot | planRecord /= Map.singleton "dist-newstyle/cache/plan.json" plan]
                <> auditExactIds "binary" ["bench-report", "ecluse-bench", "perf-acceptance", "ecluse-residency"] (map toText (Map.keys binaries))
            )
    unchanged <- concat <$> sequence [equalFile root build (directory </> phase) path | phase <- ["before", "after"], path <- identityFiles]
    corpus <- checksums (root </> build </> "corpus.sha256")
    corpusFaults <- artifactFaults root (Map.mapKeys (("source-inputs-" <> toString (sideOf name)) </>) corpus)
    locks <- checksums (root </> build </> "locks.sha256")
    lockFaults <- artifactFaults root (Map.mapKeys ((build </> "config") </>) locks)
    statusFaults <- concat <$> traverse (zeroStatus root) (["controller-build.status", "controls.status", build </> "status"] <> map (directory </>) ["binding.status", "conditions.status", "pass.status", "measure.status", "audit.status"])
    overlay <- if kind == "listing" then listingSnapshot root name [source, tree] else pure []
    pure
        ( ["recorded pass conditions differ" | conditions /= context]
            <> concat identities
            <> unchanged
            <> corpusFaults
            <> lockFaults
            <> statusFaults
            <> overlay
            <> auditExactIds "dependency lock" ["cabal.project", "cabal.project.freeze", "flake.lock"] (map toText (Map.keys locks))
        )

listingSnapshot :: FilePath -> Text -> [Text] -> IO [Text]
listingSnapshot root name source = do
    let build = "listing-build-" <> toString (sideOf name)
        directory = passPath "listing" name
    recorded <- T.lines <$> readComparisonText (root </> build </> "source.txt")
    binary <- executableDigests (root </> build </> "executable.sha256")
    before <- executableDigests (root </> directory </> "listing-before.sha256")
    after <- executableDigests (root </> directory </> "listing-after.sha256")
    overlay <- executableDigests (root </> build </> "overlay.sha256")
    mainDigest <- digestFile (root </> build </> "Main.hs")
    adapterDigest <- digestFile (root </> build </> "ListingAdapter.hs")
    patchFaults <- concat <$> traverse (\path -> equalPath root (build </> "overlay.patch") (directory </> path)) ["overlay.patch", "overlay-after.patch"]
    statuses <- zeroStatus root (build </> "status")
    pure
        ( ["listing source or tree mismatch" | recorded /= source]
            <> ["listing binary changed or missing" | Map.keys binary /= ["ecluse-residency"] || binary /= before || binary /= after]
            <> ["listing overlay source digest mismatch" | overlay /= Map.fromList [("Main.hs", mainDigest), ("ListingAdapter.hs", adapterDigest)]]
            <> patchFaults
            <> statuses
        )

-- | Rehash the copied native result and archived source inputs against each native receipt.
receiptArtifactFaults :: FilePath -> Text -> Text -> PerformanceReceipt -> IO [Text]
receiptArtifactFaults root kind name receipt = do
    let archived path = if path `elem` nativeArtifacts kind then passPath kind name </> path else "source-inputs-" <> toString (sideOf name) </> path
    faults <- artifactFaults root (Map.mapKeys archived (prArtifacts receipt))
    pure (auditExactIds "native receipt artifact" (map toText (nativeReceiptPaths kind (prSubject receipt))) (map toText (Map.keys (prArtifacts receipt))) <> faults)

-- | Mandatory native outputs and corpus inputs come from the fixed comparison contract.
nativeReceiptPaths :: Text -> Text -> [FilePath]
nativeReceiptPaths kind subject = nativeArtifacts kind <> ["bench/corpus/pins.json"] <> ["acceptance/criteria.json" | kind == "allocation"] <> map cpPath packages
  where
    packages = case subject of
        "ecosystem-npm" -> corpusPackages
        "ecosystem-pypi" -> pypiCorpusPackages
        _ -> []

nativeArtifacts :: Text -> [FilePath]
nativeArtifacts "benchmark" = ["bench-results.csv", "bench-output.txt"]
nativeArtifacts _ = ["perf-allocation-observations.json", "perf-allocation-report.md"]

checksums :: FilePath -> IO (Map FilePath Text)
checksums path = do
    entries <- traverse entry . T.lines =<< readComparisonText path
    unless (not (null entries) && Map.size (Map.fromList entries) == length entries) (fail "empty or duplicate checksum record")
    pure (Map.fromList entries)
  where
    entry line = case T.splitAt 64 line of
        (hash, rest) | validHex 64 hash, Just name <- T.stripPrefix "  " rest, not (T.null name) -> pure (toString name, hash)
        _ -> fail "invalid checksum record"

executableDigests :: FilePath -> IO (Map FilePath Text)
executableDigests path = do
    hashes <- checksums path
    let named = Map.fromList [(takeFileName name, hash) | (name, hash) <- Map.toList hashes]
    unless (Map.size named == Map.size hashes) (fail "duplicate executable basename")
    pure named

checkOrder :: ComparisonScope -> FilePath -> IO [Text]
checkOrder scope root = do
    actual <- T.lines <$> readComparisonText (root </> "order.txt")
    perPass <- forM (scopeSchedule scope) $ \(kind, name) -> T.lines <$> readComparisonText (root </> passPath kind name </> "events.txt")
    pure (orderProblems scope actual perPass)

-- | Match every start and end to the fixed serial schedule and reject overlapping recorded times.
orderProblems :: ComparisonScope -> [Text] -> [[Text]] -> [Text]
orderProblems scope actual perPass =
    ["serial pass order differs" | actual /= concat perPass || length perPass /= length expected || not (and (zipWith (<=) timestamps (drop 1 timestamps)))]
        <> ["invalid pass events" | not (and (zipWith validEvents expected perPass))]
  where
    expected = scopeSchedule scope
    validEvents (kind, name) events = case map T.words events of
        [[started, "start", startKind, startName], [ended, "end", endKind, endName, "status=0"]] -> started <= ended && (startKind, startName) == (kind, name) && (endKind, endName) == (kind, name)
        _ -> False
    timestamps = mapMaybe (listToMaybe . T.words) actual

equalFile :: FilePath -> FilePath -> FilePath -> FilePath -> IO [Text]
equalFile root left right path = equalPath root (left </> path) (right </> path)

equalPath :: FilePath -> FilePath -> FilePath -> IO [Text]
equalPath root left right = do
    before <- digestFile (root </> left)
    after <- digestFile (root </> right)
    pure ["recorded snapshot differs: " <> toText right | before /= after]

zeroStatus :: FilePath -> FilePath -> IO [Text]
zeroStatus root path = do
    value <- T.strip <$> readComparisonText (root </> path)
    pure ["recorded command failed: " <> toText path <> "=" <> value | value /= "0"]

required :: Text -> Map Text Text -> IO Text
required key = maybe (fail ("missing run field " <> toString key)) pure . Map.lookup key

validHex :: Int -> Text -> Bool
validHex size value = T.length value == size && T.all (`elem` ("0123456789abcdef" :: String)) value

safePath :: FilePath -> Bool
safePath path = not (null path || isAbsolute path) && all (`notElem` ["..", "."]) (splitDirectories path)

sideOf :: Text -> Text
sideOf = T.take 1

passPath :: Text -> Text -> FilePath
passPath kind name = toString (kind <> "-" <> name)
