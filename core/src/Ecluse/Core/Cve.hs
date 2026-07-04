{- | The advisory lookup capability: answer CVE questions about a package
version from a local, already-synced @osv.db@ artifact, never the network.

The handle is deliberately dumb data access over one artifact file. Rule
semantics live in pure predicates over what it returns
('insideAffectedRange'), because SQLite's text collation cannot order
versions; only 'Ecluse.Core.Version.compareVersions' can. The one deliberate
exception is 'cveRemediationProbe': a fixed bound in the artifact is a single
canonical version string, so exact-fix matching is plain string equality and
rides the @(package_name, fixed_version)@ index in one traversal. A fix
published under a non-canonical version string misses the probe and simply
waits out the ordinary quarantine; the operator workaround is an explicit
allow-by-identity rule.

An artifact is accepted or rejected at 'openCveDb' (epoch stamp, table shape,
ecosystem), with rejection as a value: the caller keeps its last known-good
handle and alarms. See "Ecluse.Core.Cve.Internal" for the hardening detail.
-}
module Ecluse.Core.Cve (
    -- * The handle
    CveLookup (..),
    openCveDb,

    -- * What a lookup returns
    AdvisoryRange (..),

    -- * Rejection
    CveDbRejected (..),

    -- * Pure range matching
    insideAffectedRange,
) where

import Ecluse.Core.Cve.Internal (AdvisoryRange (..), CveDbRejected (..), advisoriesQuery, openHardenedConnection, probeQuery)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Version (compareVersions, mkVersion)

import Database.SQLite.Simple (close)

{- | Advisory questions about one ecosystem's artifact.

Names and versions are the artifact's own vocabulary: the OSV wire package
name (scope inline, e.g. @\@scope\/name@) and verbatim version text. Callers
render their domain values to that form at the boundary.
-}
data CveLookup = CveLookup
    { cveRemediationProbe :: Text -> Text -> IO Bool
    {- ^ Does any advisory for this package name this exact version string as
    a fixed bound? One indexed B-tree traversal.
    -}
    , cveAdvisoriesFor :: Text -> IO [AdvisoryRange]
    {- ^ Every advisory range recorded against a package name; rule predicates
    interpret them.
    -}
    , cveClose :: IO ()
    -- ^ Release the artifact's connection. The handle must not be used after.
    }

{- | Open an @osv.db@ artifact and build the handle over it, or reject the
artifact ('CveDbRejected') with its connection already closed.
-}
openCveDb :: Ecosystem -> FilePath -> IO (Either CveDbRejected CveLookup)
openCveDb eco dbFile = do
    opened <- openHardenedConnection eco dbFile
    pure $ case opened of
        Left rejection -> Left rejection
        Right conn ->
            Right
                CveLookup
                    { cveRemediationProbe = probeQuery conn
                    , cveAdvisoriesFor = advisoriesQuery conn
                    , cveClose = close conn
                    }

{- | Is this version inside the advisory range's affected interval,
@introduced <= v < fixed@, under the ecosystem's version ordering?

__Fail-closed for the allow direction.__ This predicate guards the
remediation fast lane (a fixed version must not fast-track while it sits
inside another advisory's affected range), so every unprovable comparison,
an unparseable bound, an unparseable version, counts as __inside__: trust is
only ever granted on evidence. A future deny-direction consumer wants the
same polarity for its own reason (cannot prove safe, assume affected), but
must not reuse this documentation's rationale blindly if its needs diverge.
-}
insideAffectedRange :: Ecosystem -> Text -> AdvisoryRange -> Bool
insideAffectedRange eco versionText ar = atOrAboveIntroduced && belowFixed
  where
    v = mkVersion eco versionText

    atOrAboveIntroduced = case arIntroduced ar of
        -- No introduced bound: the range starts at the beginning.
        Nothing -> True
        Just i -> case compareVersions v (mkVersion eco i) of
            Just LT -> False
            Just _ -> True
            Nothing -> True

    belowFixed = case arFixed ar of
        -- No fix known: the range never ends.
        Nothing -> True
        Just f -> case compareVersions v (mkVersion eco f) of
            Just LT -> True
            Just _ -> False
            Nothing -> True
