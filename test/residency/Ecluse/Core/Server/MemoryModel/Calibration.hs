-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Exact observations that derive the residency limits and check production admission factors.
module Ecluse.Core.Server.MemoryModel.Calibration (
    Calibration (..),
    marginCharge,
    fullReadCharge,
    regressionLimit,
    npmCalibration,
    pypiCalibration,
    pypiRawLimit,
) where

import Data.Ratio ((%))

-- | Historical maxima remain independent when a later measurement lowers a retained envelope.
data Calibration = Calibration
    { realisticReadMaximum :: Rational
    , allReadMaximum :: Rational
    , singleReadMaximum :: Rational
    , outputMaximum :: Rational
    , wireMaximum :: Rational
    , rawMaximum :: Rational
    , typedMaximum :: Rational
    , sharedMaximum :: Rational
    }

-- | A 25% margin rounded up to a tenth, in permille.
marginCharge :: Rational -> Integer
marginCharge maximumRatio = 100 * ceiling (maximumRatio * 25 / 2)

-- | Cover both realistic reads with margin and every measured shape, in permille.
fullReadCharge :: Calibration -> Integer
fullReadCharge calibration = max (marginCharge (realisticReadMaximum calibration)) (100 * ceiling (10 * allReadMaximum calibration))

-- | An 8% margin rounded up to a quarter step, in permille.
regressionLimit :: Rational -> Integer
regressionLimit maximumRatio = 250 * ceiling (maximumRatio * 108 / 25)

-- | npm's native parser, retained projection and packed-entry calibrations.
npmCalibration :: Calibration
npmCalibration =
    -- Listing counts: https://github.com/AlexaDeWit/Ecluse/actions/runs/38107103485/job/114374944192
    Calibration
        { realisticReadMaximum = 5856896 % 6997055
        , allReadMaximum = 51887 % 54134
        , singleReadMaximum = 5856896 % 6997055
        , outputMaximum = 2112758 % 1386415
        , -- Retained counts: https://github.com/AlexaDeWit/Ecluse/actions/runs/36392431999/job/108832038539
          wireMaximum = 15693000 % 15693959
        , rawMaximum = 2620196 % 404491
        , typedMaximum = 239024 % 538235
        , -- Packed-entry counts: https://github.com/AlexaDeWit/Ecluse/actions/runs/38059331291/job/114237799183
          sharedMaximum = 4873008 % 6997055
        }

-- | PyPI's sidecar listing calibration and unchanged retained calibration.
pypiCalibration :: Calibration
pypiCalibration =
    -- Listing counts: https://github.com/AlexaDeWit/Ecluse/actions/runs/38180562637/job/114592183296
    Calibration
        { realisticReadMaximum = 513112 % 183837
        , allReadMaximum = 513112 % 183837
        , singleReadMaximum = 513112 % 183837
        , outputMaximum = 816606 % 553037
        , -- Retained counts: https://github.com/AlexaDeWit/Ecluse/actions/runs/36392431999/job/108832038539
          wireMaximum = 694786 % 695037
        , rawMaximum = 2272328 % 551511
        , typedMaximum = 12219 % 7694
        , sharedMaximum = 48165 % 15388
        }

-- | PyPI's raw tree keeps the original default instead of a calibrated regression limit.
pypiRawLimit :: Integer
pypiRawLimit = 7000
