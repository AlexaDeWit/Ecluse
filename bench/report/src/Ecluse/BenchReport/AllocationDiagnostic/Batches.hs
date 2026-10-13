-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Validate raw batch arithmetic without changing native estimates.
module Ecluse.BenchReport.AllocationDiagnostic.Batches (auditBatches, batchControls, auditFullBatch, batchFits) where

import Data.Text qualified as T
import System.FilePath ((</>))
import UnliftIO.Exception (tryAny)

import Ecluse.BenchReport (BenchRow (rowAllocatedBytes, rowMeanPs, rowStdev2Ps))
import Ecluse.BenchReport.AllocationDiagnostic.Inventory (requireDiagnostic)
import Ecluse.BenchReport.RefactorFiles (readComparisonText)

data Batch = Batch Integer Integer Integer Integer Integer Integer Integer Integer Text

readBatch :: Text -> Either Text Batch
readBatch line = case T.splitOn "," line of
    [sequenceNumber, n, startTime, startGlobal, endTime, endGlobal, startThread, endThread, thread] ->
        Batch <$> unsigned sequenceNumber <*> unsigned n <*> unsigned startTime <*> unsigned startGlobal <*> unsigned endTime <*> unsigned endGlobal <*> signed startThread <*> signed endThread <*> pure thread
    _ -> Left "malformed batch record"
  where
    unsigned = bounded 0 (toInteger (maxBound :: Word64))
    signed = bounded (toInteger (minBound :: Int64)) (toInteger (maxBound :: Int64))
    bounded lower upper field = case readMaybe (toString field) of
        Just value | value >= lower && value <= upper -> Right value
        _ -> Left "batch integer exceeds its native bounds"

-- | Require one complete doubling sequence and reproduce the native allocation estimate.
auditBatches :: FilePath -> FilePath -> BenchRow -> IO ()
auditBatches root directory row = do
    contents <- T.lines <$> readComparisonText (root </> directory </> "batches.csv")
    batches <- case contents of
        header : rest | header == "sequence,n,startTimePs,startGlobalBytes,endTimePs,endGlobalBytes,startThreadBytes,endThreadBytes,thread" -> either (fail . toString) pure (traverse readBatch rest)
        _ -> fail "batch header differs"
    validateBatches (rowAllocatedBytes row) batches

validateBatches :: Maybe Integer -> [Batch] -> IO ()
validateBatches allocated batches = do
    requireDiagnostic (length batches >= 2 && length batches <= 256) "invalid batch record count"
    forM_ (zip [0 :: Integer ..] batches) $ \(index, Batch sequenceNumber n startTime startGlobal endTime endGlobal startThread endThread thread) -> do
        requireDiagnostic (sequenceNumber == index && n == 2 ^ index) "batch sequence differs or duplicates exist"
        requireDiagnostic (endTime >= startTime && endGlobal >= startGlobal && startThread >= endThread) "batch counters move in the wrong direction"
        requireDiagnostic (not (T.null thread)) "missing measurement thread"
    requireDiagnostic (length (ordNub [thread | Batch _ _ _ _ _ _ _ _ thread <- batches]) == 1) "measurement thread changed"
    case reverse batches of
        Batch _ _ _ endStart _ endFinish _ _ _ : Batch _ n _ previousStart _ previousFinish _ _ _ : _ ->
            requireDiagnostic (allocated == Just (((previousFinish - previousStart) `quot` 5 + 2 * ((endFinish - endStart) `quot` 5)) `quot` n)) "native allocation estimator differs from raw batches"
        _ -> fail "missing final batch pair"

-- | Reconstruct native timing and a separate signed thread fit for full-history records.
batchFits :: FilePath -> FilePath -> Integer -> IO (Integer, Integer, Integer, Integer)
batchFits root directory precision = do
    contents <- T.lines <$> readComparisonText (root </> directory </> "batches.csv")
    batches <- either (fail . toString) pure (traverse readBatch (drop 1 contents))
    case reverse batches of
        Batch _ _ start2 global2 end2 finish2 thread2 threadFinish2 _ : Batch _ n start1 global1 end1 finish1 thread1 threadFinish1 _ : _ -> do
            let time1 = end1 - start1
                time2 = end2 - start2
                fit firstValue secondValue = (firstValue `quot` 5 + 2 * (secondValue `quot` 5)) `quot` n
                perturb firstValue secondValue =
                    let fitted = firstValue `quot` 5 + 2 * (secondValue `quot` 5)
                        firstResidual = fromInteger firstValue - fromInteger fitted :: Double
                        secondResidual = fromInteger secondValue - 2 * fromInteger fitted :: Double
                     in truncate (sqrt (firstResidual * firstResidual + secondResidual * secondResidual)) :: Integer
            requireDiagnostic (precision >= 1000000000 && time1 >= precision && time2 >= precision) "invalid native timing precision or short batch"
            pure (fit (finish1 - global1) (finish2 - global2), fit (thread1 - threadFinish1) (thread2 - threadFinish2), fit time1 time2, 2 * (max (perturb (time1 - precision) (time2 + precision)) (perturb (time1 + precision) (time2 - precision)) `quot` n))
        _ -> fail "missing final batch pair"

-- | The archived precision comes from the same native CPU precision floor.
auditFullBatch :: FilePath -> FilePath -> BenchRow -> IO ()
auditFullBatch root directory row = do
    precisionText <- T.strip <$> readComparisonText (root </> directory </> "precision.txt")
    precision <- maybe (fail "invalid native timing precision") pure (readMaybe (toString precisionText))
    (_, _, meanTime, deviation) <- batchFits root directory precision
    requireDiagnostic (rowMeanPs row == meanTime && rowStdev2Ps row == deviation) "native timing estimator differs from raw batches"

-- | Exercise malformed counters, missing batches and estimator mismatches before remote builds.
batchControls :: IO ()
batchControls = do
    let firstBatch = Batch 0 1 0 0 10 100 1000 900 "ThreadId 1"
        secondBatch = Batch 1 2 10 100 30 300 900 700 "ThreadId 1"
        good = [firstBatch, secondBatch]
    validateBatches (Just 100) good
    forM_ [[], [firstBatch], [firstBatch, firstBatch], [secondBatch, firstBatch], replicate 257 firstBatch] $ \bad -> reject (validateBatches (Just 100) bad)
    reject (validateBatches (Just 101) good)
    reject (validateBatches Nothing good)
    reject (validateBatches (Just 100) [firstBatch, Batch 1 2 10 100 30 300 900 700 "ThreadId 2"])
    reject (validateBatches (Just 100) [firstBatch, Batch 1 2 10 100 30 300 900 1000 "ThreadId 1"])
    reject (validateBatches (Just 100) [firstBatch, Batch 1 2 30 100 10 300 900 700 "ThreadId 1"])
    reject (validateBatches (Just 100) [firstBatch, Batch 1 2 10 300 30 100 900 700 "ThreadId 1"])
    forM_ ["0,18446744073709551616,0,0,0,0,0,0,thread", "0,-1,0,0,0,0,0,0,thread", "0,1,0,0,0,0,9223372036854775808,0,thread", "0,1,0,0,0,0,-9223372036854775809,0,thread"] $ \bad ->
        requireDiagnostic (isLeft (readBatch bad)) "out-of-range counter accepted"
  where
    reject action = tryAny action >>= \result -> requireDiagnostic (isLeft result) "diagnostic negative control accepted"
