-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Render benchmark CSV rows as grouped Markdown with separate ecosystem sections.
Console output carries generator and complexity verdicts omitted from the CSV.
-}
module Ecluse.BenchReport (
    -- * The parsed CSV
    BenchRow (..),
    parseCsv,
    splitName,
    groupRows,

    -- * Rendering
    ReportInput (..),
    renderReport,

    -- * Formatting
    formatPs,
    formatBytes,
    stripAnsi,
) where

import Data.Char (isAlphaNum)
import Data.Foldable1 qualified as Foldable1
import Data.Text qualified as T
import Numeric (showFFloat)

-- | A CSV row with optional GC statistics when the run enabled RTS statistics.
data BenchRow = BenchRow
    { rowGroup :: Text
    -- ^ The tasty path above the bench: the group heading it appears under.
    , rowBench :: Text
    -- ^ The bench's own name: the path's last dot segment.
    , rowMeanPs :: Integer
    -- ^ Mean time per iteration, in picoseconds.
    , rowStdev2Ps :: Integer
    -- ^ Twice the standard deviation, in picoseconds: the achieved precision bound.
    , rowAllocatedBytes :: Maybe Integer
    -- ^ Bytes allocated per iteration, from GC stats.
    , rowCopiedBytes :: Maybe Integer
    -- ^ Bytes copied during GC per iteration, from GC stats.
    , rowPeakBytes :: Maybe Integer
    -- ^ The process-wide peak memory high-water mark, in bytes (megabyte-granular).
    }
    deriving stock (Eq, Show)

-- The tasty path prefix every bench shares: the tasty-bench root inserts plus the one
-- top-level group Main declares. Stripping it leaves only the distinguishing path.
tierPrefix :: Text
tierPrefix = "All.ecluse-core (work-per-request)."

-- | Split the last dot-delimited component as the leaf, retaining dotted group names.
splitName :: Text -> (Text, Text)
splitName name =
    case T.breakOnEnd "." stripped of
        ("", leaf) -> ("(ungrouped)", leaf)
        (grouped, leaf) -> (T.dropEnd 1 grouped, leaf)
  where
    stripped = fromMaybe name (T.stripPrefix tierPrefix name <|> T.stripPrefix "All." name)

-- | Accept GC-statistics or time-only CSV, rejecting unknown headers and malformed rows.
parseCsv :: Text -> Either Text [BenchRow]
parseCsv raw =
    case filter (not . T.null) (map (T.dropWhileEnd (== '\r')) (T.lines raw)) of
        [] -> Left "the CSV is empty"
        (header : rows) -> do
            gcStats <- parseHeader header
            traverse (parseRow gcStats) rows

parseHeader :: Text -> Either Text Bool
parseHeader header = do
    fields <- splitRecord header
    case fields of
        ["Name", "Mean (ps)", "2*Stdev (ps)", "Allocated", "Copied", "Peak Memory"] -> Right True
        ["Name", "Mean (ps)", "2*Stdev (ps)"] -> Right False
        _ -> Left ("unrecognised CSV header: " <> header)

parseRow :: Bool -> Text -> Either Text BenchRow
parseRow gcStats line = do
    fields <- splitRecord line
    case (gcStats, fields) of
        (True, [name, mean, stdev, alloc, copied, peak]) ->
            build name mean stdev (Just alloc) (Just copied) (Just peak)
        (False, [name, mean, stdev]) ->
            build name mean stdev Nothing Nothing Nothing
        _ -> Left ("row arity does not match the header: " <> line)
  where
    build :: Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Either Text BenchRow
    build name mean stdev alloc copied peak = do
        let (grp, leaf) = splitName name
        BenchRow grp leaf
            <$> int "mean" mean
            <*> int "2*stdev" stdev
            <*> traverse (int "allocated") alloc
            <*> traverse (int "copied") copied
            <*> traverse (int "peak memory") peak
    int :: Text -> Text -> Either Text Integer
    int label t =
        maybeToRight
            ("could not read the " <> label <> " column of: " <> line)
            (readMaybe (toString t))

-- RFC 4180 quoting, where a doubled quote inside a quoted field is a literal quote.
-- Names never contain newlines, so one record is always one line.
splitRecord :: Text -> Either Text [Text]
splitRecord line = go line
  where
    go t = case T.uncons t of
        Nothing -> Right [""]
        Just ('"', rest) -> do
            (field, rest') <- quoted rest
            continue field rest'
        Just _ ->
            let (field, rest) = T.break (== ',') t
             in continue field rest
    continue field rest = case T.uncons rest of
        Nothing -> Right [field]
        Just (',', rest') -> (field :) <$> go rest'
        Just _ -> Left ("malformed quoting in CSV record: " <> line)
    quoted t = case T.breakOn "\"" t of
        (_, "") -> Left ("unterminated quote in CSV record: " <> line)
        (chunk, rest) -> case T.stripPrefix "\"\"" rest of
            Just rest' -> first ((chunk <> "\"") <>) <$> quoted rest'
            Nothing -> Right (chunk, T.drop 1 rest)

-- | Group rows by heading, preserving first appearance and row order.
groupRows :: [BenchRow] -> [(Text, NonEmpty BenchRow)]
groupRows rows =
    [ (grp, grouped)
    | grp <- ordNub (map rowGroup rows)
    , grouped <- maybeToList (nonEmpty (filter ((== grp) . rowGroup) rows))
    ]

-- | CSV results and the console verdicts that benchmark rows omit.
data ReportInput = ReportInput
    { riCsv :: Either Text [BenchRow]
    , riConsoleLog :: Maybe Text
    }
    deriving stock (Eq, Show)

-- | Render measurements or an explicit missing-results note when CSV parsing failed.
renderReport :: ReportInput -> Text
renderReport input =
    T.unlines (preamble <> body <> consoleSection (riConsoleLog input) <> readingNotes)
  where
    body = case riCsv input of
        Left err -> noResults err
        Right [] -> noResults "the CSV carried no benchmark rows"
        Right rows ->
            let groups = groupRows rows
             in operatingPoint (length rows) (length groups)
                    <> atAGlance groups
                    <> ecosystemSections groups

ecosystemSections :: [(Text, NonEmpty BenchRow)] -> [Text]
ecosystemSections groups = concatMap section (ordNub (map (ecosystemSection . fst) groups))
  where
    section ecosystem =
        maybe [] (\name -> ["### " <> name, ""]) ecosystem
            <> concatMap
                (groupSectionAt (if isJust ecosystem then "#### " else "### "))
                (filter ((== ecosystem) . ecosystemSection . fst) groups)

ecosystemSection :: Text -> Maybe Text
ecosystemSection groupName = fst . T.breakOn "." <$> T.stripPrefix "ecosystem: " groupName

preamble :: [Text]
preamble =
    [ "## Benchmarks -- work-per-request over ecluse-core"
    , ""
    , "Inform-only: time and allocations are reported for a human to read and trend, never"
        <> " compared to a threshold. Allocations (from GC stats, +RTS -T) are the"
        <> " machine-independent signal to track across commits. Time varies with the runner."
        <> " The run's only red state is a literal benchmark failure: a build error, a crashed"
        <> " harness, or a tripped complexity assertion."
    , ""
    ]

noResults :: Text -> [Text]
noResults err =
    [ "**No benchmark results to render** -- " <> err <> "."
    , ""
    , "This note only means the summary has no table to show. A benchmark"
        <> " failure reds the run on its own. The run's artifact and the raw console"
        <> " output carry whatever the run produced."
    , ""
    ]

operatingPoint :: Int -> Int -> [Text]
operatingPoint benches groups =
    [ "**Operating point**"
    , ""
    , "| knob | value |"
    , "| --- | --- |"
    , opRow "benches measured" (show benches <> " benches in " <> show groups <> " groups")
    , opRow "corpus" "frozen ecosystem captures (bench/corpus) plus synthetic scaled inputs"
    , opRow "optimisation" "-O1, the shipped build posture"
    , opRow
        "precision"
        ( "each bench iterates until its relative stdev meets the run's --stdev target."
            <> " the 2*stdev column is the achieved bound"
        )
    , opRow
        "correctness guards"
        ( "generator tests and complexity assertions run in the same tree (raw output"
            <> " below). A trip is this run's one red state"
        )
    , ""
    ]
  where
    opRow k v = "| " <> k <> " | " <> v <> " |"

atAGlance :: [(Text, NonEmpty BenchRow)] -> [Text]
atAGlance groups =
    [ "### At a glance"
    , ""
    , "| group | benches | slowest | mean | alloc/iter |"
    , "| --- | --: | --- | --: | --: |"
    ]
        <> map glanceRow groups
        <> [""]

glanceRow :: (Text, NonEmpty BenchRow) -> Text
glanceRow (grp, rows) =
    cells
        [ "[" <> grp <> "](#" <> anchor grp <> ")"
        , show (length rows)
        , rowBench slowest
        , formatPs (rowMeanPs slowest)
        , maybe "n/a" formatBytes (rowAllocatedBytes slowest)
        ]
  where
    slowest = Foldable1.maximumBy (compare `on` rowMeanPs) rows

groupSectionAt :: Text -> (Text, NonEmpty BenchRow) -> [Text]
groupSectionAt heading (grp, rows) =
    [ heading <> grp
    , ""
    , "| bench | mean | 2*stdev | allocated | copied | peak |"
    , "| --- | --: | --: | --: | --: | --: |"
    ]
        <> map detailRow (toList rows)
        <> [""]
  where
    detailRow r =
        cells
            [ rowBench r
            , formatPs (rowMeanPs r)
            , formatPs (rowStdev2Ps r)
            , bytesCell (rowAllocatedBytes r)
            , bytesCell (rowCopiedBytes r)
            , bytesCell (rowPeakBytes r)
            ]
    bytesCell = maybe "n/a" formatBytes

consoleSection :: Maybe Text -> [Text]
consoleSection = \case
    Nothing ->
        [ "_The console log was not captured. The generator-test and complexity-assertion"
            <> " verdicts are only in the job log._"
        , ""
        ]
    Just raw ->
        [ "<details>"
        , "<summary>Raw console output (carries the generator tests and complexity"
            <> " assertions, which the CSV does not)</summary>"
        , ""
        , "```text"
        , T.stripEnd (stripAnsi raw)
        , "```"
        , ""
        , "</details>"
        , ""
        ]

readingNotes :: [Text]
readingNotes =
    [ "### Reading the numbers"
    , ""
    , "- **Inform-only.** Time is runner-dependent. Nothing here gates, and there is no"
        <> " cross-run baseline."
    , "- **Allocated and copied are per-iteration GC-stats deltas** -- the"
        <> " machine-independent signal to trend."
    , "- **Peak memory is a process-wide high-water mark** at megabyte granularity: it"
        <> " only ever rises down the table, so read it as the run's footprint, never as"
        <> " one bench's cost."
    , "- **The generator tests and complexity assertions are not in the CSV**. Their"
        <> " verdicts live in the raw console output, and a trip is the run's one red state."
    ]

cells :: [Text] -> Text
cells xs = "| " <> T.intercalate " | " xs <> " |"

-- A heading's GitHub anchor slug: lowercase, punctuation dropped, spaces to hyphens
-- (hyphens and underscores survive), matching how the run summary renders heading ids.
anchor :: Text -> Text
anchor = T.map dashify . T.toLower . T.filter keep
  where
    keep c = isAlphaNum c || c == ' ' || c == '-' || c == '_'
    dashify ' ' = '-'
    dashify c = c

-- | Format picoseconds at three significant figures using the appropriate time unit.
formatPs :: Integer -> Text
formatPs = scaled 1000 ("ps" :| ["ns", "us", "ms", "s"])

-- | Format bytes at three significant figures using binary units.
formatBytes :: Integer -> Text
formatBytes = scaled 1024 ("B" :| ["KiB", "MiB", "GiB"])

scaled :: Double -> NonEmpty Text -> Integer -> Text
scaled step units n = pick (fromIntegral n) units
  where
    pick v (unit :| rest) = case rest of
        (next : more) | v >= step -> pick (v / step) (next :| more)
        _ -> sig3 v <> " " <> unit
    sig3 v
        | v == 0 = "0"
        | v >= 100 = fmt 0 v
        | v >= 10 = fmt 1 v
        | otherwise = fmt 2 v
    fmt d v = toText (showFFloat (Just d) v "")

-- | Remove console colour and cursor sequences before embedding Markdown output.
stripAnsi :: Text -> Text
stripAnsi t = case T.breakOn "\ESC[" t of
    (before, "") -> before
    (before, rest) -> before <> stripAnsi (dropSequence (T.drop 2 rest))
  where
    -- A CSI sequence ends at its first final byte (the @ to ~ range). Everything
    -- before it is parameter and intermediate bytes.
    dropSequence = T.drop 1 . T.dropWhile (\c -> c < '@' || c > '~')
