{- | Internal guts of the serve pipeline ("Ecluse.Server.Pipeline"), exposed for tests
without widening that module's two-handler public API — the @.Internal@ convention, as
"Ecluse.Credential.Refresh.Internal" uses. Importing it opts out of the public
module's stability promise.

It holds the degrade signalling for two bad-upstream conditions the response-bound
guards leave silent: 'PackumentUndecodable' (the upstream answered, but its body did
not decode into a usable packument) and 'PackumentNameMismatch' (the upstream
answered with a packument whose self-reported name is for a /different/ package).
Each is a typed throw raised at the fetch and caught by the origin fetcher's
@tryAny@, with a paired @log*@ surfacing it at a 'WarningS' before the contribution
degrades.
-}
module Ecluse.Server.Pipeline.Internal (
    PackumentUndecodable (..),
    PackumentNameMismatch (..),
    logDecodeFailure,
    logNameMismatch,
) where

import Katip (LogEnv, Severity (WarningS), logFM, ls, sl)
import Katip.Monadic (runKatipContextT)

import Ecluse.Log (moduleField)
import Ecluse.Package (PackageName, renderPackageName)

{- | Raised when an upstream packument does not decode into both the typed view and the
raw document the serve path needs. A (typed) throw, not a stringly one, caught by the
origin fetcher's @tryAny@ and degraded to a missing contribution like a bound breach.
-}
data PackumentUndecodable = PackumentUndecodable
    deriving stock (Eq, Show)

instance Exception PackumentUndecodable

{- | Raised when an upstream answered with a packument whose self-reported top-level
@name@ is for a /different/ package than the one requested. The route name is the
validation authority, so a misreporting origin is untrusted for this request: its
contribution is dropped from the merge. A (typed) throw, not a stringly one, caught
by the origin fetcher's @tryAny@ and degraded like 'PackumentUndecodable' — but kept
a distinct type so the serve layer can render the terminal no-valid-origin status as
a @502@ (an upstream returned an invalid response), distinct from a genuine absence.
-}
data PackumentNameMismatch = PackumentNameMismatch
    deriving stock (Eq, Show)

instance Exception PackumentNameMismatch

{- | Log a parse failure at 'WarningS' — the one bad-upstream condition the
response-bound guards leave silent: the upstream answered, but its body did not decode
into the typed view and raw document the serve path needs. Same fail-closed degrade and
the same @module@\/@package@ payload convention as the breach log in
"Ecluse.Server.Pipeline", so an operator sees an undecodable upstream distinctly rather
than as silence. The @module@ tag names this module's own path.
-}
logDecodeFailure :: LogEnv -> PackageName -> IO ()
logDecodeFailure logEnv name =
    runKatipContextT logEnv payload mempty $
        logFM WarningS (ls message)
  where
    payload = moduleField "Ecluse.Server.Pipeline.Internal" <> sl "package" (renderPackageName name)
    message :: Text
    message = "refused an upstream metadata document: it did not decode into a usable packument"

{- | Log an upstream name mismatch at 'WarningS' before the contribution degrades: the
origin answered, but its packument self-reported a name for a different package than
the one requested, so it is dropped as untrusted for this request. The structured
payload carries both names and the origin (its base URL) — the high-cardinality
identifiers that belong on the log line — so an operator can tell a misconfigured or
hostile upstream from an ordinary outage. Same fail-closed degrade and payload
convention as 'logDecodeFailure'.
-}
logNameMismatch :: LogEnv -> PackageName -> Text -> Text -> IO ()
logNameMismatch logEnv requested origin reported =
    runKatipContextT logEnv payload mempty $
        logFM WarningS (ls message)
  where
    payload =
        moduleField "Ecluse.Server.Pipeline.Internal"
            <> sl "package" (renderPackageName requested)
            <> sl "origin" origin
            <> sl "upstreamName" reported
    message :: Text
    message = "dropped an upstream contribution: its packument self-reported a name for a different package"
