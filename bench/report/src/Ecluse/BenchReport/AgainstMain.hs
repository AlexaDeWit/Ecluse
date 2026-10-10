-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What every comparison of a performance report with the latest run on @main@ shares: the
run it compares with, the change in one figure, how a set of changes spreads, and the
section around them. One run stands on each side, so a comparison carries no verdict and a
missing baseline is a note. "Ecluse.BenchReport.AgainstMain.Bench", @.Acceptance@, and
@.Load@ each read one kind of report.
-}
module Ecluse.BenchReport.AgainstMain (
    -- * The run on main
    Origin (..),
    parseOrigin,
    Baseline (..),
    Missing (..),
    missingCodes,

    -- * Changes
    percentChange,
    signedPercent,
    Spread (..),
    spreadOf,
    spreadLine,

    -- * Pairing two reports
    Paired (..),
    pairBy,
    groupedBy,
    unpairedLines,

    -- * The section
    Report (..),
    renderAgainstMain,
    sharedBaselineNote,
) where

import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.BenchReport.Markdown (fixed)

-- | The run on @main@ whose results a report is compared with.
data Origin = Origin
    { originCommit :: Text
    , originRun :: Text
    -- ^ The URL of the run's page.
    , originCreated :: Text
    -- ^ When the run was created, as GitHub prints it.
    }
    deriving stock (Eq, Show)

{- | Read the record @scripts/perf-baseline.sh@ writes: @key=value@ lines that name the commit, the
run, and its creation time, or one @unavailable@ line that holds a code of 'missingCodes'.
-}
parseOrigin :: Text -> Either Missing Origin
parseOrigin raw = case Map.lookup "unavailable" fields of
    Just code -> Left (fromMaybe (UnknownReason code) (lookup code missingCodes))
    Nothing -> Origin <$> field "commit" <*> field "run" <*> field "created"
  where
    fields = Map.fromList [(key, T.drop 1 value) | (key, value) <- map (T.breakOn "=") (T.lines raw), not (T.null value)]
    field key = maybeToRight (RecordLacks key) (mfilter (not . T.null) (Map.lookup key fields))

-- | The results of the run on @main@, or why there are none.
data Baseline a
    = Baseline Origin a
    | NoBaseline Missing
    deriving stock (Eq, Show)

-- | Why a comparison has no baseline.
data Missing
    = -- | The fetch could not list the runs on @main@.
      RunsNotListed
    | -- | The list of the runs did not parse.
      RunListNotParsed
    | -- | The list holds no successful run that this repository started on @main@.
      NoSuccessfulRun
    | -- | No listed run still holds the report's artifact.
      NoRunWithArtifact
    | -- | The artifact of the chosen run could not be downloaded.
      DownloadFailed
    | -- | A file the fetch leaves could not be read: the problem, as the read gave it.
      FileMissing Text
    | -- | The record lacks this key.
      RecordLacks Text
    | -- | The record holds a code that 'missingCodes' does not.
      UnknownReason Text
    | -- | The report of the run on @main@ did not parse: the run, and the problem.
      ReportUnread Origin Text
    deriving stock (Eq, Show)

-- | The codes @scripts/perf-baseline.sh@ writes for a fetch that found no baseline.
missingCodes :: [(Text, Missing)]
missingCodes =
    [ ("runs-not-listed", RunsNotListed)
    , ("run-list-not-parsed", RunListNotParsed)
    , ("no-successful-run", NoSuccessfulRun)
    , ("no-run-with-artifact", NoRunWithArtifact)
    , ("download-failed", DownloadFailed)
    ]

renderMissing :: Missing -> Text
renderMissing = \case
    RunsNotListed -> "The fetch could not list the runs on `main`."
    RunListNotParsed -> "The list of the runs on `main` did not parse."
    NoSuccessfulRun -> "The fetch found no successful run on `main`."
    NoRunWithArtifact -> "No successful run on `main` that the fetch listed still holds this report's artifact."
    DownloadFailed -> "The fetch could not download the artifact of the run on `main`."
    FileMissing problem -> "A file of the baseline is missing: " <> problem <> "."
    RecordLacks key -> "The baseline record names no " <> key <> "."
    UnknownReason code -> "The fetch gave a reason this tool does not know: `" <> code <> "`."
    ReportUnread origin problem -> "The report of " <> originLink origin <> " could not be read. " <> problem

-- | The change from the figure on @main@ to this run's, in percent of the figure on @main@.
percentChange :: Double -> Double -> Maybe Double
percentChange onMain here
    | onMain > 0 = Just ((here - onMain) / onMain * 100)
    | otherwise = Nothing

-- | A change to one decimal place with its sign. A change that rounds to zero carries none.
signedPercent :: Double -> Text
signedPercent percent
    | magnitude == fixed 1 0 = magnitude <> "%"
    | percent > 0 = "+" <> magnitude <> "%"
    | otherwise = "-" <> magnitude <> "%"
  where
    magnitude = fixed 1 (abs percent)

-- | How the changes of one set of rows spread.
data Spread = Spread
    { spreadRose :: Int
    , spreadFell :: Int
    , spreadHeld :: Int
    , spreadMedian :: Double
    , spreadLowest :: Double
    , spreadHighest :: Double
    }
    deriving stock (Eq, Show)

-- | The spread of a set of changes in percent. The median of an even count is the mean of the middle two.
spreadOf :: NonEmpty Double -> Spread
spreadOf changes =
    Spread
        { spreadRose = count (> 0)
        , spreadFell = count (< 0)
        , spreadHeld = count (== 0)
        , spreadMedian = (middle (total `div` 2) + middle ((total - 1) `div` 2)) / 2
        , spreadLowest = NE.head sorted
        , spreadHighest = NE.last sorted
        }
  where
    sorted = NE.sort changes
    total = length sorted
    count p = length (NE.filter p changes)
    middle i = fromMaybe (NE.head sorted) (toList sorted !!? i)

{- | How a set of rows moved, from the capitalised noun and figure. One runner measures every row
of a job, so the line says so when no row moved against the others.
-}
spreadLine :: Text -> Text -> [Double] -> Text
spreadLine noun figure changes = case nonEmpty changes of
    Nothing -> noun <> " compared: 0. No row has a figure on both sides."
    Just present ->
        let spread = spreadOf present
         in T.unwords $
                [ noun <> " compared: " <> show (length present) <> "."
                , figure <> " rose in " <> show (spreadRose spread) <> ", fell in " <> show (spreadFell spread) <> ", and held in " <> show (spreadHeld spread) <> "."
                , "The median change is " <> signedPercent (spreadMedian spread) <> ","
                , "and the changes run from " <> signedPercent (spreadLowest spread) <> " to " <> signedPercent (spreadHighest spread) <> "."
                ]
                    <> ["None moved the other way. One runner measured them all, so the runner can be the cause." | movedTogether spread]

-- At least two rows moved, and all of them in one direction.
movedTogether :: Spread -> Bool
movedTogether spread =
    (spreadRose spread == 0 && spreadFell spread >= 2) || (spreadFell spread == 0 && spreadRose spread >= 2)

-- | The rows of two reports matched by key.
data Paired k a = Paired
    { pairedBoth :: [(a, a)]
    -- ^ The row on @main@ and this run's row, in this run's order.
    , pairedOnlyHere :: [k]
    , pairedOnlyOnMain :: [k]
    }
    deriving stock (Eq, Show)

-- | Match this run's rows with the rows on @main@ that carry the same key.
pairBy :: (Ord k) => (a -> k) -> [a] -> [a] -> Paired k a
pairBy key onMain here =
    Paired
        { pairedBoth = [(base, row) | row <- here, Just base <- [Map.lookup (key row) baseByKey]]
        , pairedOnlyHere = [key row | row <- here, Map.notMember (key row) baseByKey]
        , pairedOnlyOnMain = [key row | row <- onMain, Map.notMember (key row) hereByKey]
        }
  where
    baseByKey = Map.fromList [(key row, row) | row <- onMain]
    hereByKey = Map.fromList [(key row, row) | row <- here]

-- | The matched rows under each group of this run's rows, the groups in order of first appearance.
groupedBy :: (Ord g) => (a -> g) -> [(a, a)] -> [(g, [(a, a)])]
groupedBy groupOf pairs = [(grp, filter ((== grp) . groupOf . snd) pairs) | grp <- ordNub (map (groupOf . snd) pairs)]

-- | The rows only one side holds, in a line for each side that has any.
unpairedLines :: (k -> Text) -> Paired k a -> [Text]
unpairedLines name paired =
    side "Only in this run" (pairedOnlyHere paired) <> side "Only on `main`" (pairedOnlyOnMain paired)
  where
    side _ [] = []
    side label keys = [label <> ": " <> T.intercalate ", " (map (code . name) keys) <> ".", ""]
    code text = "`" <> text <> "`"

-- | One kind of report: how to read it, and how to set the reading on @main@ beside this run's.
data Report a = Report
    { reportTitle :: Text
    , reportRead :: Text -> Either Text a
    , reportBody :: a -> a -> [Text]
    -- ^ The reading on @main@, then this run's.
    , reportNotes :: [Text]
    -- ^ What the reader must know to use the body, printed under it.
    }

{- | Render one comparison. An unreadable report of this run, a missing baseline, and a baseline
that does not parse each become a note, so the caller has nothing to fail on.
-}
renderAgainstMain :: Report a -> Baseline Text -> Either Text Text -> Text
renderAgainstMain report baseline current =
    T.unlines $
        ["## " <> reportTitle report, ""] <> case (current >>= reportRead report, baseline) of
            (Left problem, _) -> ["**Nothing to compare.** This run's report could not be read. " <> problem, ""]
            (Right _, NoBaseline missing) -> noBaseline missing
            (Right here, Baseline origin raw) -> case reportRead report raw of
                Left problem -> noBaseline (ReportUnread origin problem)
                Right onMain -> ["Compared with " <> originLink origin <> ".", ""] <> reportBody report onMain here <> reportNotes report

noBaseline :: Missing -> [Text]
noBaseline missing = ["**No baseline.** " <> renderMissing missing, "", "Nothing is compared, and the job does not fail on it.", ""]

-- | The note every comparison carries: a repeat of the run keeps the same run on @main@.
sharedBaselineNote :: Text
sharedBaselineNote =
    "- **One baseline for every run.** Every run off `main` compares with this same run on `main` until a newer one succeeds there, so a row that it drew slow or fast shows the same difference in every pull request and in every repeat."

originLink :: Origin -> Text
originLink origin =
    "`main` at `" <> T.take 9 (originCommit origin) <> "` ([run](" <> originRun origin <> ") created " <> originCreated origin <> ")"
