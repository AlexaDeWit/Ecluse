-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The private construction boundary for 'AdvisoryStoreUrl', on the same terms as
"Ecluse.Config.Queue.Internal". The @ecluse@ library does not expose this module, so
'Ecluse.Config.AdvisoryStore.mkAdvisoryStoreUrl' is the only builder and a value whose target
disagrees with its text is unrepresentable outside this library.
-}
module Ecluse.Config.Advisory.Internal (
    AdvisoryStoreTarget (..),
    AdvisoryStoreUrl (..),
    advisoryStoreUrlText,
    advisoryStoreTarget,
) where

-- The sum is closed and grows one arm per object-store provider, so it stays a data
-- declaration rather than collapsing to the single arm that ships today.
{- HLINT ignore AdvisoryStoreTarget "Use newtype instead of data" -}

{- | A recognised advisory-database store, parsed from the URL's scheme. It carries the bucket
and the optional key prefix under which the compiled artifacts live.
-}
data AdvisoryStoreTarget
    = -- | An @s3:\/\/bucket[\/prefix]@ store.
      S3Store Text (Maybe Text)
    deriving stock (Eq, Show)

{- | @advisories.url@ as parsed at load ('Ecluse.Config.AdvisoryStore.mkAdvisoryStoreUrl'): the
value as written, with the store its scheme names.
-}
data AdvisoryStoreUrl = AdvisoryStoreUrl Text AdvisoryStoreTarget
    deriving stock (Eq, Show)

-- The halves are positional and these accessors hand-written, so record-update syntax cannot
-- rebuild a value past the smart constructor (as in "Ecluse.Config.Queue.Internal").

-- | The value as written, trimmed.
advisoryStoreUrlText :: AdvisoryStoreUrl -> Text
advisoryStoreUrlText (AdvisoryStoreUrl value _) = value

-- | The store the value's scheme names.
advisoryStoreTarget :: AdvisoryStoreUrl -> AdvisoryStoreTarget
advisoryStoreTarget (AdvisoryStoreUrl _ target) = target
