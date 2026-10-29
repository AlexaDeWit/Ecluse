-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Measured figures behind the residency regression limits, and the maxima behind production admission charges.
module Ecluse.Core.Server.MemoryModel.Calibration (
    -- * Admission charges
    Calibration (..),
    marginCharge,
    fullReadCharge,
    keptCharge,
    npmCalibration,
    pypiCalibration,
    cargoCalibration,
    rubyGemsCalibration,

    -- * Regression limits
    Figures (..),
    heldLimit,
    readPeakLimit,
    npmFigures,
    pypiFigures,
    cargoFigures,
    rubyGemsFigures,
    SyntheticListingFigures (..),
    rubyGemsSyntheticFigures,
) where

import Data.Map.Strict qualified as Map
import Data.Ratio ((%))

import Ecluse.Test.Corpus.Merge (MergeShape (Disjoint, Identical, Overlapping, PublishOrder))

-- | The listing maxima that set an ecosystem's admission charges.
data Calibration = Calibration
    { realisticReadMaximum :: Rational
    , allReadMaximum :: Rational
    , outputMaximum :: Rational
    }

-- | A 25% margin rounded up to a tenth, in permille.
marginCharge :: Rational -> Integer
marginCharge maximumRatio = 100 * ceiling (maximumRatio * 25 / 2)

-- | Cover both realistic reads with margin and every measured shape, in permille.
fullReadCharge :: Calibration -> Integer
fullReadCharge calibration = max (marginCharge (realisticReadMaximum calibration)) (100 * ceiling (10 * allReadMaximum calibration))

-- | Cover the largest shared cache entry with margin, in permille: what a completed read keeps.
keptCharge :: Map Text Figures -> Integer
keptCharge = marginCharge . foldl' max 0 . fmap retainedShared

-- | npm's listing maxima.
npmCalibration :: Calibration
npmCalibration =
    -- Listing counts: https://github.com/AlexaDeWit/Ecluse/actions/runs/38107103485/job/114374944192
    Calibration
        { realisticReadMaximum = 5856896 % 6997055
        , allReadMaximum = 51887 % 54134
        , outputMaximum = 2112758 % 1386415
        }

-- | PyPI's listing maxima with canonical sidecars retained.
pypiCalibration :: Calibration
pypiCalibration =
    -- Listing counts: https://github.com/AlexaDeWit/Ecluse/actions/runs/38180562637/job/114592183296
    Calibration
        { realisticReadMaximum = 513112 % 183837
        , allReadMaximum = 513112 % 183837
        , outputMaximum = 816606 % 553037
        }

-- | Cargo's native listing maxima.
cargoCalibration :: Calibration
cargoCalibration =
    -- https://github.com/AlexaDeWit/Ecluse/actions/runs/40249393996/job/121337825625
    Calibration
        { realisticReadMaximum = 1493744 % 1166746
        , allReadMaximum = 1493744 % 1166746
        , outputMaximum = 2806578 % 1373462
        }

-- | ARM64 maxima combine public captures with labelled synthetic listings.
rubyGemsCalibration :: Calibration
rubyGemsCalibration =
    -- Public captures: https://github.com/AlexaDeWit/Ecluse/actions/runs/40559003401/job/122382799126
    -- Synthetic: https://github.com/AlexaDeWit/Ecluse/actions/runs/40575218918/job/122439883465
    Calibration
        { realisticReadMaximum = 1621168 % 112670
        , allReadMaximum = 1621168 % 112670
        , outputMaximum = 8371680 % 2315422
        }

-- | One capture's live bytes per source byte, and per basis byte for an output working set.
data Figures = Figures
    { retainedWire :: Rational
    , retainedRaw :: Rational
    , retainedTyped :: Rational
    , retainedShared :: Rational
    , readPeak :: Maybe Rational
    -- ^ Measured single-listing read peak. Its per-byte guard applies from one meter step.
    , singleOutput :: Maybe Rational
    -- ^ Measured single-listing output. Its per-byte guard applies from one meter step.
    , mergedOutputs :: Map MergeShape Rational
    -- ^ Measured realistic-merge outputs. Their per-byte guards apply from one meter step.
    }

-- | Add the existing 3% held-figure margin, rounded up to a permille.
heldLimit :: Rational -> Integer
heldLimit ratio = ceiling (ratio * 1030)

-- | Add the existing 4% read-peak margin, rounded up to a permille.
readPeakLimit :: Rational -> Integer
readPeakLimit ratio = ceiling (ratio * 1040)

-- | npm figures by capture name. Each read peak is the largest of the ten main runs up to the job cited.
npmFigures :: Map Text Figures
npmFigures =
    -- https://github.com/AlexaDeWit/Ecluse/actions/runs/39085443993/job/117558292470
    Map.fromList
        [
            ( "@types/node"
            , Figures
                { retainedWire = 11148200 % 11150508
                , retainedRaw = 64051032 % 11150508
                , retainedTyped = 2208328 % 11150508
                , retainedShared = 3413528 % 11150508
                , readPeak = Just (4238952 % 11150508)
                , singleOutput = Just (3903736 % 11150508)
                , mergedOutputs = Map.fromList [(Identical, 3903736 % 11150508), (Overlapping, 3903736 % 11152697), (Disjoint, 3903736 % 11153414), (PublishOrder, 3903736 % 11150508)]
                }
            )
        ,
            ( "webpack"
            , Figures
                { retainedWire = 5197936 % 5200717
                , retainedRaw = 24358328 % 5200717
                , retainedTyped = 836640 % 5200717
                , retainedShared = 1695400 % 5200717
                , readPeak = Just (2431232 % 5200717)
                , singleOutput = Just (7403116 % 5200717)
                , mergedOutputs = Map.fromList [(Identical, 7403116 % 5200717), (Overlapping, 7403116 % 5247473), (Disjoint, 7403116 % 5278738), (PublishOrder, 7403116 % 6285681)]
                }
            )
        ,
            ( "@aws-sdk/client-s3"
            , Figures
                { retainedWire = 4155592 % 4159245
                , retainedRaw = 20228728 % 4159245
                , retainedTyped = 729584 % 4159245
                , retainedShared = 1462984 % 4159245
                , readPeak = Just (2050016 % 4159245)
                , singleOutput = Just (6338274 % 4159245)
                , mergedOutputs = Map.fromList [(Identical, 6338274 % 4159245), (Overlapping, 6338274 % 4190408), (Disjoint, 6338274 % 4221118), (PublishOrder, 6338274 % 4159245)]
                }
            )
        ,
            ( "express"
            , Figures
                { retainedWire = 805024 % 808982
                , retainedRaw = 5237312 % 808982
                , retainedTyped = 251928 % 808982
                , retainedShared = 449776 % 808982
                , readPeak = Nothing
                , singleOutput = Nothing
                , mergedOutputs = mempty
                }
            )
        ,
            ( "typescript"
            , Figures
                { retainedWire = 15689256 % 15693959
                , retainedRaw = 92046584 % 15693959
                , retainedTyped = 4129440 % 15693959
                , retainedShared = 7343736 % 15693959
                , readPeak = Just (8822304 % 15693959)
                , singleOutput = Just (20694364 % 15693959)
                , mergedOutputs = Map.fromList [(Identical, 20694364 % 15693959), (Overlapping, 20694364 % 15695441), (Disjoint, 20694364 % 15699578), (PublishOrder, 20694364 % 15693959)]
                }
            )
        ,
            ( "@babel/core"
            , Figures
                { retainedWire = 792000 % 797193
                , retainedRaw = 3986128 % 797193
                , retainedTyped = 210600 % 797193
                , retainedShared = 370680 % 797193
                , readPeak = Nothing
                , singleOutput = Nothing
                , mergedOutputs = mempty
                }
            )
        ,
            ( "react"
            , Figures
                { retainedWire = 6991432 % 6997055
                , retainedRaw = 35633200 % 6997055
                , retainedTyped = 3124552 % 6997055
                , retainedShared = 4893040 % 6997055
                , readPeak = Just (5926736 % 6997055)
                , singleOutput = Just (6925034 % 6997055)
                , mergedOutputs = Map.fromList [(Identical, 6925034 % 6997055), (Overlapping, 6925034 % 7016607), (Disjoint, 6925034 % 7012325), (PublishOrder, 6925034 % 6997055)]
                }
            )
        ,
            ( "request"
            , Figures
                { retainedWire = 300984 % 307243
                , retainedRaw = 1660064 % 307243
                , retainedTyped = 98280 % 307243
                , retainedShared = 176440 % 307243
                , readPeak = Nothing
                , singleOutput = Nothing
                , mergedOutputs = mempty
                }
            )
        ,
            ( "lodash"
            , Figures
                { retainedWire = 240944 % 247678
                , retainedRaw = 1454272 % 247678
                , retainedTyped = 90640 % 247678
                , retainedShared = 148392 % 247678
                , readPeak = Nothing
                , singleOutput = Nothing
                , mergedOutputs = mempty
                }
            )
        ]

-- | PyPI figures by capture name, from the same runs as 'npmFigures'.
pypiFigures :: Map Text Figures
pypiFigures =
    Map.fromList
        [
            ( "boto3"
            , Figures
                { retainedWire = 2198000 % 2206044
                , retainedRaw = 9082272 % 2206044
                , retainedTyped = 3775632 % 2206044
                , retainedShared = 5642064 % 2206044
                , readPeak = Just (6161208 % 2206044)
                , singleOutput = Just (3232642 % 2206044)
                , mergedOutputs = Map.fromList [(Identical, 3232642 % 2206044), (Overlapping, 3232642 % 2206155), (Disjoint, 3232642 % 2206172), (PublishOrder, 3232642 % 2206044)]
                }
            )
        ,
            ( "numpy"
            , Figures
                { retainedWire = 2771472 % 2780148
                , retainedRaw = 10327440 % 2780148
                , retainedTyped = 3340592 % 2780148
                , retainedShared = 5667808 % 2780148
                , readPeak = Just (6340096 % 2780148)
                , singleOutput = Just (4083030 % 2780148)
                , mergedOutputs = Map.fromList [(Identical, 4083030 % 2780148), (Overlapping, 4083030 % 2774356), (Disjoint, 4083030 % 2765185), (PublishOrder, 4083030 % 3467301)]
                }
            )
        ,
            ( "requests"
            , Figures
                { retainedWire = 113728 % 123104
                , retainedRaw = 494440 % 123104
                , retainedTyped = 213320 % 123104
                , retainedShared = 320272 % 123104
                , readPeak = Nothing
                , singleOutput = Nothing
                , mergedOutputs = mempty
                }
            )
        ]

-- | Cargo figures from the native arm64 corpus measurements.
cargoFigures :: Map Text Figures
cargoFigures =
    -- https://github.com/AlexaDeWit/Ecluse/actions/runs/40249393996/job/121337825625
    Map.fromList
        [
            ( "aws-sdk-ec2"
            , Figures
                { retainedWire = 1410144 % 1427352
                , retainedRaw = 11740848 % 1427352
                , retainedTyped = 282496 % 1427352
                , retainedShared = 1648392 % 1427352
                , readPeak = Just (1778064 % 1427352)
                , singleOutput = Just (2854704 % 1427352)
                , mergedOutputs = Map.fromList [(Identical, 2854704 % 1427352), (Overlapping, 2854704 % 1426377), (Disjoint, 2854704 % 1427352), (PublishOrder, 2854704 % 1471794)]
                }
            )
        ,
            ( "windows"
            , Figures
                { retainedWire = 1386192 % 1403289
                , retainedRaw = 9259592 % 1403289
                , retainedTyped = 51008 % 1403289
                , retainedShared = 1487152 % 1403289
                , readPeak = Just (1792456 % 1403289)
                , singleOutput = Just (2806578 % 1403289)
                , mergedOutputs = Map.fromList [(Identical, 2806578 % 1403289), (Overlapping, 2806578 % 1373462), (Disjoint, 2806578 % 1403289), (PublishOrder, 2806578 % 1690498)]
                }
            )
        ,
            ( "tokio"
            , Figures
                { retainedWire = 853616 % 870666
                , retainedRaw = 7347792 % 870666
                , retainedTyped = 159424 % 870666
                , retainedShared = 1018880 % 870666
                , readPeak = Nothing
                , singleOutput = Nothing
                , mergedOutputs = mempty
                }
            )
        ]

-- | Initial native figures include fixed overhead below one step, not above-step listing qualification.
rubyGemsFigures :: Map Text Figures
rubyGemsFigures =
    -- https://github.com/AlexaDeWit/Ecluse/actions/runs/40559003401/job/122382799126
    Map.fromList
        [
            ( "rails"
            , Figures
                { retainedWire = 171256 % 186441
                , retainedRaw = 473888 % 186441
                , retainedTyped = 458648 % 186441
                , retainedShared = 2565264 % 186441
                , readPeak = Just (2632952 % 186441)
                , singleOutput = Just (330860 % 186441)
                , mergedOutputs = Map.fromList [(Identical, 330860 % 186441), (Overlapping, 330860 % 186554), (Disjoint, 330860 % 186445), (PublishOrder, 330860 % 196174)]
                }
            )
        ,
            ( "nokogiri"
            , Figures
                { retainedWire = 170120 % 186107
                , retainedRaw = 618728 % 186107
                , retainedTyped = 600416 % 186107
                , retainedShared = 2504480 % 186107
                , readPeak = Just (2581376 % 186107)
                , singleOutput = Just (353864 % 186107)
                , mergedOutputs = Map.fromList [(Identical, 342400 % 186107), (Overlapping, 267992 % 185570), (Disjoint, 259776 % 186111), (PublishOrder, 332112 % 227164)]
                }
            )
        ,
            ( "aws-sdk-ec2"
            , Figures
                { retainedWire = 95872 % 112670
                , retainedRaw = 357176 % 112670
                , retainedTyped = 545256 % 112670
                , retainedShared = 1554928 % 112670
                , readPeak = Just (1621168 % 112670)
                , singleOutput = Just (269160 % 112670)
                , mergedOutputs = Map.fromList [(Identical, 322072 % 112670), (Overlapping, 255872 % 112683), (Disjoint, 214000 % 112674), (PublishOrder, 269800 % 113622)]
                }
            )
        ]

-- | Listing-only ratios keep labelled synthetic evidence separate from captured retained figures.
data SyntheticListingFigures = SyntheticListingFigures
    { syntheticReadPeak :: Rational
    , syntheticSingleOutput :: Rational
    , syntheticMergedOutputs :: Map MergeShape Rational
    }

-- | ARM64 listing ratios for syntheticInfoBytes v1 with 16,384 versions, separate from captures.
rubyGemsSyntheticFigures :: Maybe SyntheticListingFigures
rubyGemsSyntheticFigures =
    -- https://github.com/AlexaDeWit/Ecluse/actions/runs/40575218918/job/122439883465
    Just
        SyntheticListingFigures
            { syntheticReadPeak = 30435552 % 2315422
            , syntheticSingleOutput = 6554160 % 2315422
            , syntheticMergedOutputs = Map.fromList [(Identical, 8371680 % 2315422), (Overlapping, 6627408 % 2315417), (Disjoint, 5757712 % 2315426), (PublishOrder, 6654600 % 2315422)]
            }
