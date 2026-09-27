-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | An in-memory 'CveLookup' for pure-tier tests, and a per-row reference for range matching.

Rule-evaluation specs use this fake instead of SQLite. "Ecluse.Core.CveSpec" runs the same
behavioural cases against this fake and the real handle, so the two cannot drift apart.
-}
module Ecluse.Test.Cve (
    fakeCveLookup,
    fakeCveDb,
    namesFix,
    unscoredEpssCases,
    referenceInside,
) where

import Ecluse.Core.Cve (AdvisoryRange (..), CveDb (..), CveLookup (..))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Osv.Epss (epssForIds, mkEpssScores, parseEpssLine)
import Ecluse.Core.Osv.Provenance (noProvenance)
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore, LastAffected, Unbounded))
import Ecluse.Core.Version (compareVersions, mkVersion, parseVersionKey)

-- | Build the fake from (package name, range) rows.
fakeCveLookup :: [(Text, AdvisoryRange)] -> CveLookup
fakeCveLookup rows =
    CveLookup
        { cveAdvisoriesFor = \name -> pure [ar | (n, ar) <- rows, n == name]
        , cveCoveredNames = pure (ordNub (map fst rows))
        }

{- | An owning handle over the fake lookup, closing to nothing. A spec that pins when the slot
retires a displaced generation builds its own recording handle instead.
-}
fakeCveDb :: [(Text, AdvisoryRange)] -> CveDb
fakeCveDb rows = CveDb{cveDbLookup = fakeCveLookup rows, cveDbClose = pass, cveDbMeta = [], cveDbProvenance = noProvenance}

-- | Whether a package row names this exact version as its fix, which tells generations apart.
namesFix :: CveLookup -> Text -> Text -> IO Bool
namesFix cve name version = any ((== FixedBefore version) . arUpperBound) <$> cveAdvisoriesFor cve name

-- | Individual gaps in a nonempty feed, joined through the production score parser.
unscoredEpssCases :: [(String, Maybe Double)]
unscoredEpssCases =
    [ ("malware without a CVE alias", score ["MAL-2026-1"] [])
    , ("a CVE absent from a valid feed", score ["CVE-2026-10001"] [])
    , ("a malformed score row in an accepted feed", score ["CVE-2026-10001"] ["CVE-2026-10001,not-a-number,0.5"])
    ]
  where
    score ids rows =
        epssForIds
            (mkEpssScores (mapMaybe parseEpssLine ("CVE-2026-10002,0.75,0.9" : rows)))
            ids

{- | The per-row reference for 'Ecluse.Core.Cve.affecting': it parses the version and both bounds on
every call, so a spec can hold the matcher that parses them once per package to it.
-}
referenceInside :: Ecosystem -> Text -> AdvisoryRange -> Bool
referenceInside eco versionText ar = case referencePoint of
    Just only -> versionText == only
    Nothing -> atOrAboveIntroduced && withinUpperBound
  where
    v = mkVersion eco versionText
    referencePoint = case (arIntroduced ar, arUpperBound ar) of
        (Just introduced, LastAffected lastAffected)
            | introduced == lastAffected
            , isLeft (parseVersionKey eco introduced) ->
                Just introduced
        _ -> Nothing
    atOrAboveIntroduced = case arIntroduced ar of
        Nothing -> True
        Just i -> compareVersions v (mkVersion eco i) /= Just LT
    withinUpperBound = case arUpperBound ar of
        FixedBefore f -> case compareVersions v (mkVersion eco f) of
            Just LT -> True
            Just _ -> False
            Nothing -> True
        LastAffected la -> compareVersions v (mkVersion eco la) /= Just GT
        Unbounded -> True
