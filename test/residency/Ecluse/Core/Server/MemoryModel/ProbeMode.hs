-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Metadata child command lines keep raw arguments until each existing probe validates them.
module Ecluse.Core.Server.MemoryModel.ProbeMode (ProbeMode (..), parseProbeMode, renderProbeMode, probeArguments) where

-- | Arguments retain their spelling and validation order at the child boundary.
data ProbeMode
    = Retained String
    | Evaluation
    | Listing
    | ListingFor String String
    | MergeFor String String FilePath
    | ListingCensus
    | Merge FilePath FilePath
    | Source String String String String String
    | SelectedRetention String String String String
    | SelectedControl String String String String
    deriving stock (Eq, Show)

-- | Match metadata modes by exact arity after the RTS removes its arguments.
parseProbeMode :: [String] -> Maybe (ProbeMode, FilePath)
parseProbeMode = \case
    ["--metadata-probe", shape, path] -> Just (Retained shape, path)
    ["--metadata-evaluation-probe", path] -> Just (Evaluation, path)
    ["--metadata-listing-probe", path] -> Just (Listing, path)
    ["--metadata-listing-for", ecosystem, name, path] -> Just (ListingFor ecosystem name, path)
    ["--metadata-merge-for", ecosystem, name, private, public] -> Just (MergeFor ecosystem name private, public)
    ["--metadata-listing-census-probe", path] -> Just (ListingCensus, path)
    ["--metadata-merge-probe", private, public, path] -> Just (Merge private public, path)
    ["--metadata-source-probe", ecosystem, mode, name, version, limit, path] -> Just (Source ecosystem mode name version limit, path)
    ["--metadata-selected-retention-probe", ecosystem, name, version, limit, path] -> Just (SelectedRetention ecosystem name version limit, path)
    ["--metadata-selected-control-probe", ecosystem, name, version, limit, path] -> Just (SelectedControl ecosystem name version limit, path)
    _ -> Nothing

-- | Render only metadata arguments, leaving the corpus path and RTS switches to the caller.
renderProbeMode :: ProbeMode -> [String]
renderProbeMode = \case
    Retained shape -> ["--metadata-probe", shape]
    Evaluation -> ["--metadata-evaluation-probe"]
    Listing -> ["--metadata-listing-probe"]
    ListingFor ecosystem name -> ["--metadata-listing-for", ecosystem, name]
    MergeFor ecosystem name private -> ["--metadata-merge-for", ecosystem, name, private]
    ListingCensus -> ["--metadata-listing-census-probe"]
    Merge private public -> ["--metadata-merge-probe", private, public]
    Source ecosystem mode name version limit -> ["--metadata-source-probe", ecosystem, mode, name, version, limit]
    SelectedRetention ecosystem name version limit -> ["--metadata-selected-retention-probe", ecosystem, name, version, limit]
    SelectedControl ecosystem name version limit -> ["--metadata-selected-control-probe", ecosystem, name, version, limit]

-- | Preserve the child argv, including both RTS groups and the path between them.
probeArguments :: ProbeMode -> FilePath -> [String]
probeArguments mode path = renderProbeMode mode <> sampling <> [path, "+RTS", "-T", "-N1", "-RTS"]
  where
    sampling = case mode of
        Listing -> majorSampling
        ListingFor _ _ -> majorSampling
        MergeFor{} -> majorSampling
        Merge _ _ -> majorSampling
        _ -> []
    -- A fixed old generation and 128 KiB nursery sample live data at nearly every collection.
    majorSampling = ["+RTS", "-F1", "-A128k", "-RTS"]
