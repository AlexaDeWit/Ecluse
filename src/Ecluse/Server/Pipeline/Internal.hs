{- | Internal guts of the serve pipeline ("Ecluse.Server.Pipeline"), exposed for tests
without widening that module's two-handler public API — the @.Internal@ convention, as
"Ecluse.Credential.Refresh.Internal" uses. Importing it opts out of the public
module's stability promise.

It holds the decode-failure signalling: the typed 'PackumentUndecodable' raised when an
upstream packument does not parse, and 'logDecodeFailure', which surfaces that case (the
one bad-upstream condition the response-bound guards leave silent) at a 'WarningS'
before the fetch degrades to a missing contribution.
-}
module Ecluse.Server.Pipeline.Internal (
    PackumentUndecodable (..),
    logDecodeFailure,
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
