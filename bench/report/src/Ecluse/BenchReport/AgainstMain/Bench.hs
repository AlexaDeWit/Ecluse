-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The work-per-request benchmark times of a run beside those of the latest run on @main@,
in a section for each ecosystem. It compares time alone: the allocation budgets hold
allocation. One runner measures every bench of a run, so each section leads with how its
benches moved together before it lists them.
-}
module Ecluse.BenchReport.AgainstMain.Bench (
    againstMain,
) where

import Data.Text qualified as T
import Numeric (showFFloat)

import Ecluse.BenchReport (BenchRow (..), formatPs, parseCsv, splitEcosystem)
import Ecluse.BenchReport.AgainstMain (
    Baseline,
    Paired (pairedBoth),
    Report (..),
    Spread (..),
    groupedBy,
    pairBy,
    percentChange,
    renderAgainstMain,
    signedPercent,
    spreadLine,
    spreadOf,
    unpairedLines,
 )
import Ecluse.BenchReport.Markdown (cells)

-- | Render the comparison from the two @bench-results.csv@ bodies: the one on @main@, then this run's.
againstMain :: Baseline Text -> Either Text Text -> Text
againstMain =
    renderAgainstMain
        Report
            { reportTitle = "Time against main: work-per-request benchmarks"
            , reportRead = readRows
            , reportBody = body
            , reportNotes = notes
            }

readRows :: Text -> Either Text [BenchRow]
readRows raw = case parseCsv raw of
    Left problem -> Left ("The CSV did not parse: " <> problem <> ".")
    Right [] -> Left "The CSV holds no benchmark row."
    Right rows -> Right rows

body :: [BenchRow] -> [BenchRow] -> [Text]
body onMain here =
    ["**Whole run.** " <> spreadLine "Benches" "Time" (mapMaybe timeChange pairs), ""]
        <> concatMap (uncurry section) (groupedBy ecosystemOf pairs)
        <> unpairedLines (\(grp, bench) -> grp <> "." <> bench) paired
  where
    paired = pairBy (\row -> (rowGroup row, rowBench row)) onMain here
    pairs = pairedBoth paired

section :: Maybe Text -> [(BenchRow, BenchRow)] -> [Text]
section ecosystem pairs =
    [ "### " <> name
    , ""
    , spreadLine "Benches" "Time" (mapMaybe timeChange pairs)
    , ""
    , "| top group | benches | median change | lowest | highest |"
    , "| --- | --: | --: | --: | --: |"
    ]
        <> mapMaybe (uncurry topGroupRow) (groupedBy topGroupOf pairs)
        <> [ ""
           , "<details>"
           , "<summary>Every bench: " <> name <> "</summary>"
           , ""
           , "| group | bench | main | this run | change | 2*stdev on main | 2*stdev in this run |"
           , "| --- | --- | --: | --: | --: | --: | --: |"
           ]
        <> map benchRow pairs
        <> ["", "</details>", ""]
  where
    name = fromMaybe "no ecosystem" ecosystem

topGroupRow :: Text -> [(BenchRow, BenchRow)] -> Maybe Text
topGroupRow top pairs = do
    changes <- nonEmpty (mapMaybe timeChange pairs)
    let spread = spreadOf changes
    pure (cells (top : show (length changes) : map signedPercent [spreadMedian spread, spreadLowest spread, spreadHighest spread]))

benchRow :: (BenchRow, BenchRow) -> Text
benchRow pair@(onMain, here) =
    cells
        [ snd (splitEcosystem (rowGroup here))
        , rowBench here
        , formatPs (rowMeanPs onMain)
        , formatPs (rowMeanPs here)
        , maybe "n/a" signedPercent (timeChange pair)
        , precision onMain
        , precision here
        ]

timeChange :: (BenchRow, BenchRow) -> Maybe Double
timeChange (onMain, here) = percentChange (fromIntegral (rowMeanPs onMain)) (fromIntegral (rowMeanPs here))

-- Twice the standard deviation as a share of the mean. A single-iteration row has none.
precision :: BenchRow -> Text
precision row
    | rowMeanPs row > 0 && rowStdev2Ps row > 0 =
        toText (showFFloat (Just 1) (fromIntegral (rowStdev2Ps row) / fromIntegral (rowMeanPs row) * 100 :: Double) "") <> "%"
    | otherwise = "n/a"

ecosystemOf :: BenchRow -> Maybe Text
ecosystemOf = fst . splitEcosystem . rowGroup

-- The first segment of the group path under the ecosystem.
topGroupOf :: BenchRow -> Text
topGroupOf = fst . T.breakOn "." . snd . splitEcosystem . rowGroup

notes :: [Text]
notes =
    [ "### Reading the comparison"
    , ""
    , "- **One run stands on each side.** One runner measures every bench of a run, and the run on `main` had another runner. A whole run can move together, so read a bench beside the benches the change does not touch."
    , "- **`2*stdev`** is the precision tasty-bench reached for a bench in one run, as a share of its mean. One run on each side does not resolve a change smaller than it, and a larger one can still be the runner."
    , "- **Time only.** The allocation budgets hold allocation, so this section leaves it out."
    , "- **No verdict.** No row is marked, and nothing here fails the job."
    ]
