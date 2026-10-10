-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The leg times of a live performance-acceptance run beside those of the latest run on
@main@, in a section for each ecosystem. It reads both from the report the harness prints
(@renderLiveReport@ in "Ecluse.Acceptance"), and compares time alone: the allocation budgets
hold allocation. The documents are live, so each row also shows a version count that moved.
-}
module Ecluse.BenchReport.AgainstMain.Acceptance (
    againstMain,
) where

import Data.Map.Strict qualified as Map
import Numeric (showFFloat)

import Ecluse.BenchReport.AgainstMain (
    Baseline,
    Paired (pairedBoth),
    Report (..),
    groupedBy,
    pairBy,
    percentChange,
    renderAgainstMain,
    signedPercent,
    spreadLine,
    unpairedLines,
 )
import Ecluse.BenchReport.Markdown (Table (tableHeading), cells, hasColumns, records, tables)

-- | Render the comparison from the two @perf-acceptance-report.md@ bodies: the one on @main@, then this run's.
againstMain :: Baseline Text -> Either Text Text -> Text
againstMain =
    renderAgainstMain
        Report
            { reportTitle = "Time against main: live registry documents"
            , reportRead = readLegs
            , reportBody = body
            , reportNotes = notes
            }

-- One measured leg of one package.
data Leg = Leg
    { legEcosystem :: Text
    , legPackage :: Text
    , legName :: Text
    , legVersions :: Maybe Int
    , legMs :: Maybe Double
    }

readLegs :: Text -> Either Text [Leg]
readLegs raw = case concatMap legsOf (filter (hasColumns ["package", "leg", "time (ms)"]) (tables raw)) of
    [] -> Left "It holds no measured leg."
    legs -> Right legs
  where
    legsOf table = mapMaybe (legOf (fromMaybe "no ecosystem" (tableHeading table))) (records table)

-- A package the run did not measure has one row with no leg, which is not a leg.
legOf :: Text -> Map Text Text -> Maybe Leg
legOf ecosystem record = do
    package <- Map.lookup "package" record
    name <- mfilter (/= "--") (Map.lookup "leg" record)
    pure
        Leg
            { legEcosystem = ecosystem
            , legPackage = package
            , legName = name
            , legVersions = number "versions"
            , legMs = number "time (ms)"
            }
  where
    number :: (Read a) => Text -> Maybe a
    number column = Map.lookup column record >>= readMaybe . toString

body :: [Leg] -> [Leg] -> [Text]
body onMain here =
    ["**Whole run.** " <> spreadLine "Legs" "Time" (mapMaybe timeChange pairs), ""]
        <> concatMap (uncurry section) (groupedBy legEcosystem pairs)
        <> unpairedLines (\(ecosystem, package, name) -> ecosystem <> " " <> package <> " " <> name) paired
  where
    paired = pairBy (\leg -> (legEcosystem leg, legPackage leg, legName leg)) onMain here
    pairs = pairedBoth paired

section :: Text -> [(Leg, Leg)] -> [Text]
section ecosystem pairs =
    [ "### " <> ecosystem
    , ""
    , spreadLine "Legs" "Time" (mapMaybe timeChange pairs)
    , ""
    , "| package | versions | leg | main (ms) | this run (ms) | change |"
    , "| --- | --: | --- | --: | --: | --: |"
    ]
        <> map legRow pairs
        <> [""]

legRow :: (Leg, Leg) -> Text
legRow pair@(onMain, here) =
    cells
        [ legPackage here
        , versions (legVersions onMain) (legVersions here)
        , legName here
        , maybe "n/a" milliseconds (legMs onMain)
        , maybe "n/a" milliseconds (legMs here)
        , maybe "n/a" signedPercent (timeChange pair)
        ]
  where
    milliseconds ms = toText (showFFloat (Just 3) ms "")

-- A live document grows as its package publishes, so a count that moved shows both sides.
versions :: Maybe Int -> Maybe Int -> Text
versions onMain here = case (onMain, here) of
    (Just before, Just now)
        | before == now -> show now
        | otherwise -> show before <> " on main, " <> show now <> " here"
    (_, Just now) -> show now
    _ -> "n/a"

timeChange :: (Leg, Leg) -> Maybe Double
timeChange (onMain, here) = join (percentChange <$> legMs onMain <*> legMs here)

notes :: [Text]
notes =
    [ "### Reading the comparison"
    , ""
    , "- **One run stands on each side.** Each time is the median of one run's passes on one runner, and the run on `main` had another runner. A whole run can move together, so read a leg beside the legs the change does not touch."
    , "- **The documents are live.** A package that published between the two runs has more versions to read. The versions column then shows both counts."
    , "- **Time only.** The allocation budgets hold allocation, so this section leaves it out."
    , "- **No verdict.** No row is marked, and nothing here fails the job."
    ]
