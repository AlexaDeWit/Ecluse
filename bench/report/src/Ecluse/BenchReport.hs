{- | The pure rendering core behind the @bench-report@ executable: it turns the
work-per-request benchmark CSV (@tasty-bench --csv@) into the structured Markdown
report the CI run summary shows, kept apart from the file-reading shell so the parse,
the grouping, and the rendering are exercised deterministically.

The CSV is @tasty-bench@'s own format: a @Name,Mean (ps),2*Stdev (ps)@ header
(extended with @Allocated,Copied,Peak Memory@ when the RTS runs with @-T@, as the
benchmark component bakes in), then one row per bench. The @Name@ column is the
dot-joined tasty path, quoted RFC-4180 style only when it carries a comma or a
quote. Two of its quirks shape this module:

  * __The dot-joined path is ambiguous.__ Group names contain dots
    (@rules.evalRules@) but no current leaf bench name does, so 'splitName' takes the
    last dot segment as the bench and everything before it as the group. A future
    bench whose own name contains a dot would fold its head into the group heading --
    a cosmetic misfiling, never lost data.

  * __Peak Memory is not a per-bench figure.__ It is the RTS's process-wide
    high-water mark at megabyte granularity, so it only ever rises down the file; the
    report's reading notes say so, because a per-bench reading would mislead.

The generator tests and complexity assertions that share the benchmark tree are
HUnit cases, which @tasty-bench@ omits from the CSV; their verdicts survive only in
the raw console output, which the report carries in a collapsed section.
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

{- | One bench's CSV row: its group and bench name (see 'splitName'), the measured
mean and the achieved two-standard-deviation bound (both picoseconds per iteration),
and the GC-stats columns -- absent when the run lacked @+RTS -T@.
-}
data BenchRow = BenchRow
    { rowGroup :: Text
    -- ^ The tasty path above the bench: the group heading it is reported under.
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

-- The tasty path prefix shared by every bench in the tree: the root tasty-bench
-- inserts plus the single top-level group Main declares. Stripped from every name so
-- group headings carry only the distinguishing path.
tierPrefix :: Text
tierPrefix = "All.ecluse-core (work-per-request)."

{- | Split a CSV @Name@ into its group heading and bench name. The shared tier
prefix (or, failing that, the bare @All.@ root) is stripped, then the last dot
segment is the bench and the rest is the group -- correct for every current bench
name, since groups contain dots but no leaf does (the module header has the caveat).
A name with no dot at all lands under an explicit ungrouped heading.
-}
splitName :: Text -> (Text, Text)
splitName name =
    case T.breakOnEnd "." stripped of
        ("", leaf) -> ("(ungrouped)", leaf)
        (grouped, leaf) -> (T.dropEnd 1 grouped, leaf)
  where
    stripped = fromMaybe name (T.stripPrefix tierPrefix name <|> T.stripPrefix "All." name)

{- | Parse the @tasty-bench --csv@ output. Accepts the six-column GC-stats shape
(@+RTS -T@, the CI posture) and the plain three-column shape; anything else -- a
missing header, a row of the wrong arity, an unreadable number -- is a @Left@ with
the offending content, which the renderer surfaces as a loud note.
-}
parseCsv :: Text -> Either Text [BenchRow]
parseCsv raw =
    case filter (not . T.null) (map (T.dropWhileEnd (== '\r')) (T.lines raw)) of
        [] -> Left "the CSV is empty"
        (header : rows) -> do
            gcStats <- parseHeader header
            traverse (parseRow gcStats) rows

-- Whether the header is the GC-stats shape (True) or the plain time-only shape
-- (False); any other header is malformed.
parseHeader :: Text -> Either Text Bool
parseHeader header = do
    fields <- splitRecord header
    case fields of
        ["Name", "Mean (ps)", "2*Stdev (ps)", "Allocated", "Copied", "Peak Memory"] -> Right True
        ["Name", "Mean (ps)", "2*Stdev (ps)"] -> Right False
        _ -> Left ("unrecognised CSV header: " <> header)

-- One data row, at the arity the header promised.
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

-- Split one CSV record into its fields: bare fields up to a comma, quoted fields
-- RFC-4180 style with doubled quotes as a literal quote. Names never contain
-- newlines (tasty-bench's own comparison tooling assumes the same), so a record is
-- always one line.
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

{- | Group parsed rows under their group headings, preserving both the groups'
first-appearance order and the row order within each -- the tree order the bench run
reported in.
-}
groupRows :: [BenchRow] -> [(Text, NonEmpty BenchRow)]
groupRows rows =
    [ (grp, grouped)
    | grp <- ordNub (map rowGroup rows)
    , grouped <- maybeToList (nonEmpty (filter ((== grp) . rowGroup) rows))
    ]

{- | What the renderer works from: the CSV parse outcome (a @Left@ is rendered as a
loud note, never dropped) and the raw console output when it was captured (the only
carrier of the generator-test and complexity-assertion verdicts).
-}
data ReportInput = ReportInput
    { riCsv :: Either Text [BenchRow]
    , riConsoleLog :: Maybe Text
    }
    deriving stock (Eq, Show)

{- | Render the full Markdown report: the inform-only preamble, an operating-point
table, an at-a-glance table (one row per group, anchor-linked to its section), a
detail table per group, the raw console output in a collapsed section, and closing
reading notes. A failed or empty CSV renders the loud note in place of the tables so
the summary never silently shows nothing.
-}
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
                    <> concatMap groupSection groups

preamble :: [Text]
preamble =
    [ "## Benchmarks -- work-per-request over ecluse-core"
    , ""
    , "Inform-only: time and allocations are reported for a human to read and trend, never"
        <> " compared to a threshold. Allocations (from GC stats, +RTS -T) are the"
        <> " machine-independent signal to track across commits; time varies with the runner."
        <> " The run's only red state is a literal benchmark failure: a build error, a crashed"
        <> " harness, or a tripped complexity assertion."
    , ""
    ]

noResults :: Text -> [Text]
noResults err =
    [ "**No benchmark results to render** -- " <> err <> "."
    , ""
    , "This note only means the summary has no table to show; a genuine benchmark"
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
    , opRow "corpus" "real-world packument captures (bench/corpus/npm) plus synthetic scaled inputs"
    , opRow "optimisation" "-O1, the shipped build posture; no benchmark-only flags"
    , opRow
        "precision"
        ( "each bench iterates until its relative stdev meets the run's --stdev target;"
            <> " the 2*stdev column is the achieved bound"
        )
    , opRow
        "correctness guards"
        ( "generator tests and complexity assertions run in the same tree (raw output"
            <> " below); a trip is this run's one red state"
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

-- One at-a-glance row per group, linked to its detail section and headlined by its
-- slowest bench.
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

groupSection :: (Text, NonEmpty BenchRow) -> [Text]
groupSection (grp, rows) =
    [ "### " <> grp
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
        [ "_The console log was not captured; the generator-test and complexity-assertion"
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
    , "- **Inform-only.** Time is runner-dependent; nothing here gates, and there is no"
        <> " cross-run baseline."
    , "- **Allocated and copied are per-iteration GC-stats deltas** -- the"
        <> " machine-independent signal to trend."
    , "- **Peak memory is a process-wide high-water mark** at megabyte granularity: it"
        <> " only ever rises down the table, so read it as the run's footprint, never as"
        <> " one bench's cost."
    , "- **The generator tests and complexity assertions are not in the CSV**; their"
        <> " verdicts live in the raw console output, and a trip is the run's one red state."
    ]

-- A Markdown table row from its cells.
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

{- | A picosecond duration scaled to a readable unit (ps, ns, us, ms, s) at three
significant figures.
-}
formatPs :: Integer -> Text
formatPs = scaled 1000 ("ps" :| ["ns", "us", "ms", "s"])

{- | A byte count scaled to a readable unit (B, KiB, MiB, GiB) at three significant
figures.
-}
formatBytes :: Integer -> Text
formatBytes = scaled 1024 ("B" :| ["KiB", "MiB", "GiB"])

-- Scale a quantity through successive units (one division by the step each), then
-- render it at three significant figures against the unit it settled on.
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

{- | Drop ANSI CSI escape sequences (colours, cursor moves) from console output, so
the raw log embeds cleanly in Markdown.
-}
stripAnsi :: Text -> Text
stripAnsi t = case T.breakOn "\ESC[" t of
    (before, "") -> before
    (before, rest) -> before <> stripAnsi (dropSequence (T.drop 2 rest))
  where
    -- A CSI sequence ends at its first final byte (the @ to ~ range); everything
    -- before it is parameter and intermediate bytes.
    dropSequence = T.drop 1 . T.dropWhile (\c -> c < '@' || c > '~')
