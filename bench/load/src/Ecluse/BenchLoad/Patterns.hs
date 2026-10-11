-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Finite request families over real captured identities, without replacement within installs.
module Ecluse.BenchLoad.Patterns (
    Pattern (..),
    PatternKnobs (..),
    defaultPatternKnobs,
    ClientTrace (..),
    RequestTrace (..),
    patternName,
    patternDescription,
    makeTrace,
    workingBytes,
) where

import Data.Map.Strict qualified as Map

-- | Each constructor names a comparison axis, not a representative deployment.
data Pattern = HotSet | ColdInstall | CiFleet | Heterogeneous | Zipf | Restart | Scan
    deriving stock (Eq, Show, Enum, Bounded)

-- | Replay parameters. Invalid or unsupported corpus sizes fail instead of inventing names.
data PatternKnobs = PatternKnobs
    { pkNames :: Int
    , pkClients :: Int
    , pkSkewMicros :: Int
    , pkRounds :: Int
    , pkOverlap :: Double
    , pkExponent :: Double
    , pkArrivalMicros :: Int
    , pkSeed :: Word64
    }
    deriving stock (Eq, Show)

-- | A small matrix point. Sweeps change one or more axes explicitly.
defaultPatternKnobs :: PatternKnobs
defaultPatternKnobs = PatternKnobs 3 4 100_000 4 0.5 1.1 100_000 42

-- | One sequential client, starting at an offset from the shared replay clock.
data ClientTrace = ClientTrace
    { ctStartMicros :: Int
    , ctIntervalMicros :: Int
    , ctNames :: [Text]
    }
    deriving stock (Eq, Show)

-- | The exact finite schedule and its distinct measured identities.
data RequestTrace = RequestTrace
    { rtClients :: [ClientTrace]
    , rtNames :: [Text]
    }
    deriving stock (Eq, Show)

-- | Stable scenario identifiers shared by command selection and reports.
patternName :: Pattern -> Text
patternName = \case
    HotSet -> "pattern-hot-set-control"
    ColdInstall -> "pattern-cold-install"
    CiFleet -> "pattern-ci-fleet"
    Heterogeneous -> "pattern-heterogeneous"
    Zipf -> "pattern-zipf"
    Restart -> "pattern-restart"
    Scan -> "pattern-scan"

-- | The client schedule and package overlap each replay family exercises.
patternDescription :: Pattern -> Text
patternDescription = \case
    HotSet -> "Staggered clients repeatedly request the same package set. The run includes the first cold requests and later reuse."
    ColdInstall -> "One client requests each selected package once, without repeats."
    CiFleet -> "Staggered clients each request the same package sequence once, modelling CI builds with matching dependency lists."
    Heterogeneous -> "Staggered clients each request a partly shared package list once. The overlap fraction sets the common packages. Each client's remaining packages are disjoint."
    Zipf -> "Staggered clients sample packages with a Zipf distribution: higher-ranked packages receive more requests. The exponent controls skew, and the seed fixes the finite draws."
    Restart -> "Clients arrive at fixed intervals into a fresh proxy with empty caches, each requesting the same package sequence once. No running proxy is restarted."
    Scan -> "One client repeatedly scans the entire selected package set in order."

-- | Reject impossible distinct-name counts and overlap before allocating the finite schedule.
makeTrace :: Pattern -> PatternKnobs -> [Text] -> Either Text RequestTrace
makeTrace patternKind knobs names
    | count < 1 || count > length space = Left "distinct names must fit the captured identity space"
    | clients < 1 || pkRounds knobs < 1 = Left "clients and rounds must be positive"
    | pkSkewMicros knobs < 0 || pkArrivalMicros knobs < 0 = Left "schedule offsets must be non-negative"
    | not (pkOverlap knobs >= 0 && pkOverlap knobs <= 1) = Left "overlap must be between zero and one"
    | (pkExponent knobs <= 0 || isNaN (pkExponent knobs)) || isInfinite (pkExponent knobs) = Left "Zipf exponent must be finite and positive"
    | patternKind == Heterogeneous && required > length space = Left "heterogeneous private names exceed the captured identity space"
    | otherwise = Right (RequestTrace traces (ordNub (concatMap ctNames traces)))
  where
    space = ordNub names
    count = pkNames knobs
    clients = pkClients knobs
    ordered = map snd (sortOn fst (zip (randomWords (pkSeed knobs)) space))
    selected = take count ordered
    sharedCount = floor (fromIntegral count * pkOverlap knobs)
    privateCount = count - sharedCount
    required = sharedCount + clients * privateCount
    client i = ClientTrace (i * pkSkewMicros knobs) 0
    repeats = concat (replicate (pkRounds knobs) selected)
    traces = case patternKind of
        HotSet -> [client i repeats | i <- [0 .. clients - 1]]
        ColdInstall -> [client 0 selected]
        CiFleet -> [client i selected | i <- [0 .. clients - 1]]
        Heterogeneous -> [client i (take sharedCount ordered <> take privateCount (drop (sharedCount + i * privateCount) ordered)) | i <- [0 .. clients - 1]]
        Zipf -> [client i (zipfDraws (pkSeed knobs + fromIntegral i) (pkExponent knobs) (count * pkRounds knobs) selected) | i <- [0 .. clients - 1]]
        Restart -> [ClientTrace (i * pkArrivalMicros knobs) 0 selected | i <- [0 .. clients - 1]]
        Scan -> [client 0 repeats]

randomWords :: Word64 -> [Word64]
randomWords seed = drop 1 (iterate (\x -> x * 6364136223846793005 + 1442695040888963407) seed)

zipfDraws :: Word64 -> Double -> Int -> [Text] -> [Text]
zipfDraws seed exponent count names = mapMaybe pick (take count (randomWords seed))
  where
    weights = zipWith (\rank name -> (1 / fromIntegral rank ** exponent, name)) [1 :: Int ..] names
    total = sum (map fst weights)
    pick word = choose (fromIntegral word / (fromIntegral (maxBound :: Word64) + 1) * total) weights
    choose _ [] = Nothing
    choose _ [(_, name)] = Just name
    choose target ((weight, name) : rest)
        | target < weight = Just name
        | otherwise = choose (target - weight) rest

-- | Count each measured identity once, including the fat tail and excluding request multiplicity.
workingBytes :: Map Text Int -> RequestTrace -> Either Text Int
workingBytes sizes requestTrace = sum <$> traverse size (rtNames requestTrace)
  where
    size name = maybe (Left ("missing capture size: " <> name)) Right (Map.lookup name sizes)
