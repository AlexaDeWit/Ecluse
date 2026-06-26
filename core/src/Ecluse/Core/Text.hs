{- | Small pure text helpers shared across the codebase, so the blank-value and
URL-path-join idioms have a single definition rather than several near-identical
re-spellings. This module depends on nothing else in @Ecluse@, so any module may
import it without risking an import cycle.
-}
module Ecluse.Core.Text (
    nonBlank,
    stripTrailingSlash,
    joinUrlPath,
) where

import Data.Text qualified as T

{- | The text trimmed of surrounding whitespace, or 'Nothing' when nothing remains.
A value that is empty or all-whitespace is treated as absent — the idiom an
environment lookup or an optional configuration field wants for "present but blank
means unset". The surviving text is returned __trimmed__, so a caller never has to
strip it a second time.
-}
nonBlank :: Text -> Maybe Text
nonBlank t =
    let trimmed = T.strip t
     in if T.null trimmed then Nothing else Just trimmed

{- | Drop a single trailing @\'\/\'@ from a URL base when present, leaving any other
base untouched. At most one slash is removed, and the function is idempotent on a
base that already carries none.
-}
stripTrailingSlash :: Text -> Text
stripTrailingSlash b = fromMaybe b (T.stripSuffix "/" b)

{- | Join a URL base and an already-encoded path with exactly one @\'\/\'@, tolerating
one trailing slash on the base so the join never doubles it. The path is appended
verbatim — this neither encodes nor validates it.
-}
joinUrlPath :: Text -> Text -> Text
joinUrlPath b path = stripTrailingSlash b <> "/" <> path
