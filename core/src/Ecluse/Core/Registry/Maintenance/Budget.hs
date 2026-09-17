-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The request capacity a maintenance store runs under: the pool a backend declares, what one
cycle spent against it, and the gate each request passes through.
"Ecluse.Core.Registry.Sweep.Pacing" decides the rate. Nothing here decides one.
-}
module Ecluse.Core.Registry.Maintenance.Budget (
    -- * What a backend declares
    QuotaScope,
    mkQuotaScope,
    renderQuotaScope,
    QuotaDimension (..),
    quotaDimensions,
    quotaDimensionName,
    parseQuotaDimension,
    QuotaOrigin (..),
    StoreBudget (..),
    undeclaredBudget,
    budgetDeclared,
    narrowestBudget,
    smallestQuota,
    renderStoreBudget,
    toHundredths,

    -- * What a cycle spends
    RequestKind (..),
    requestKinds,
    requestKindName,
    parseRequestKind,
    RequestTally,
    oneRequest,
    tallyCounts,
    renderRequestTally,

    -- * The rate one scope runs at
    CyclePace,
    freePace,
    paceOf,
    paceSeconds,

    -- * The gate and the meter behind it
    RequestGate (..),
    CycleCost (..),
    BudgetPort (..),
    newBudgetMeter,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)

import Ecluse.Core.Clock (monoSecondsBetween, monotonicNow)

-- | The capacity pool a store's requests debit, which several stores can share.
newtype QuotaScope = QuotaScope Text
    deriving stock (Eq, Ord, Show)

-- | Build a scope, folding case and surrounding space so two spellings of one pool are one pool.
mkQuotaScope :: Text -> QuotaScope
mkQuotaScope = QuotaScope . T.toLower . T.strip

-- | The scope as an audit line names it.
renderQuotaScope :: QuotaScope -> Text
renderQuotaScope (QuotaScope raw) = raw

{- | One pool a backend meters its callers by. The arms are pool shapes rather than one vendor's
API names, so each backend maps its own calls onto them.
-}
data QuotaDimension
    = -- | Calls that list a store's package names.
      NameListing
    | -- | Calls that list one package's versions.
      VersionListing
    | -- | Requests the backend counts as reads of the account holding the store.
      AccountReads
    | -- | Requests the backend counts as writes to that account.
      AccountWrites
    | -- | Requests sharing the ceiling of one authentication token.
      TokenReads
    | -- | The single undivided request capacity of a backend that publishes no other pool.
      StoreRequests
    deriving stock (Eq, Ord, Show)

{- | Every pool this build meters under. 'quotaDimensionName' carries no wildcard and the spec
pins this list, so a new arm fails both until it is named in each.
-}
quotaDimensions :: [QuotaDimension]
quotaDimensions = [NameListing, VersionListing, AccountReads, AccountWrites, TokenReads, StoreRequests]

-- | The dimension as a configuration key spells it.
quotaDimensionName :: QuotaDimension -> Text
quotaDimensionName = \case
    NameListing -> "nameListing"
    VersionListing -> "versionListing"
    AccountReads -> "accountReads"
    AccountWrites -> "accountWrites"
    TokenReads -> "tokenReads"
    StoreRequests -> "storeRequests"

-- | Read a configured dimension, refusing a spelling this build meters nothing under.
parseQuotaDimension :: Text -> Maybe QuotaDimension
parseQuotaDimension raw = find ((== raw) . quotaDimensionName) quotaDimensions

-- | Where a scope's quota numbers came from, which the boot line reports.
data QuotaOrigin
    = -- | The backend's published defaults, which no call discovered.
      QuotaDocumented
    | -- | The operator declared them, for a backend that publishes none.
      QuotaDeclared
    | -- | Derived from the sweep's own package pace, for a backend that publishes none.
      QuotaDerived
    | -- | Neither, as the backend leaf hands the budget over before the boot resolves it.
      QuotaUndeclared
    deriving stock (Eq, Show)

-- | One store's capacity: the pool it shares, its per-second quotas, and what each request costs.
data StoreBudget = StoreBudget
    { bgScope :: QuotaScope
    , bgQuotas :: Map QuotaDimension Rational
    -- ^ Requests per second the pool admits, per dimension. Empty where none is declared.
    , bgOrigin :: QuotaOrigin
    , bgCosts :: Map RequestKind (Map QuotaDimension Rational)
    -- ^ What one request of each kind debits. A kind absent here debits nothing.
    }
    deriving stock (Eq, Show)

{- | A store whose backend publishes no capacity and whose operator declared none. Its cycles are
paced by the sweep's own pauses alone.
-}
undeclaredBudget :: StoreBudget
undeclaredBudget =
    StoreBudget
        { bgScope = mkQuotaScope ""
        , bgQuotas = Map.empty
        , bgOrigin = QuotaUndeclared
        , bgCosts = Map.empty
        }

-- | Whether anything bounds this store's request rate.
budgetDeclared :: StoreBudget -> Bool
budgetDeclared = not . Map.null . bgQuotas

{- | Combine two descriptions of one pool: the tightest quota per dimension and the dearest cost
per request kind, so two stores sharing a pool are paced by the narrower of what each claims.
-}
narrowestBudget :: StoreBudget -> StoreBudget -> StoreBudget
narrowestBudget left right =
    left
        { bgQuotas = Map.unionWith min (bgQuotas left) (bgQuotas right)
        , bgCosts = Map.unionWith (Map.unionWith max) (bgCosts left) (bgCosts right)
        , bgOrigin = if originRank (bgOrigin left) >= originRank (bgOrigin right) then bgOrigin left else bgOrigin right
        }

-- How well a description accounts for a pool, so the better-founded of two labels the line.
originRank :: QuotaOrigin -> Int
originRank = \case
    QuotaDeclared -> 3
    QuotaDocumented -> 2
    QuotaDerived -> 1
    QuotaUndeclared -> 0

-- | The tightest quota in the pool, which the default budget fraction is derived from.
smallestQuota :: StoreBudget -> Maybe Rational
smallestQuota = foldr (\rate held -> Just (maybe rate (min rate) held)) Nothing . Map.elems . bgQuotas

-- | The resolved capacity as the boot line records it, naming where each number came from.
renderStoreBudget :: StoreBudget -> Text
renderStoreBudget budget = origin <> " (" <> quotas <> ")"
  where
    origin = case bgOrigin budget of
        QuotaDocumented -> "the backend's documented quotas"
        QuotaDeclared -> "the capacity you declared"
        QuotaDerived -> "capacity derived from the sweep's own package pace"
        QuotaUndeclared -> "no capacity at all"
    quotas
        | Map.null (bgQuotas budget) = "none"
        | otherwise =
            T.intercalate
                ", "
                [ quotaDimensionName dimension <> " " <> renderRate rate
                | (dimension, rate) <- Map.toAscList (bgQuotas budget)
                ]

-- | One request a cycle makes against the store being dredged.
data RequestKind
    = -- | One page of the store's package-name listing.
      ListingPage
    | -- | One enumeration of a package's versions.
      VersionPage
    | -- | One read of a package's metadata back from the store.
      ManifestRead
    | -- | One destructive call, whatever number of versions the backend takes in it.
      DeleteBatch
    | -- | One read of a standing permission, including a reassessment before a delete.
      PermissionRead
    | -- | One read of the walk's resumption marker.
      CursorRead
    | -- | One write or clearing of that marker.
      CursorWrite
    deriving stock (Eq, Ord, Show)

-- | Every request a cycle can make, held to 'requestKindName' the way 'quotaDimensions' is.
requestKinds :: [RequestKind]
requestKinds = [ListingPage, VersionPage, ManifestRead, DeleteBatch, PermissionRead, CursorRead, CursorWrite]

-- | The kind as a configuration weight spells it.
requestKindName :: RequestKind -> Text
requestKindName = \case
    ListingPage -> "listingPage"
    VersionPage -> "versionPage"
    ManifestRead -> "manifestRead"
    DeleteBatch -> "deleteBatch"
    PermissionRead -> "permissionRead"
    CursorRead -> "cursorRead"
    CursorWrite -> "cursorWrite"

-- | Read a configured request kind, refusing a spelling no cycle makes.
parseRequestKind :: Text -> Maybe RequestKind
parseRequestKind raw = find ((== raw) . requestKindName) requestKinds

-- | What one cycle attempted, counted per request kind.
newtype RequestTally = RequestTally (Map RequestKind Int)
    deriving stock (Eq, Show)

instance Semigroup RequestTally where
    RequestTally left <> RequestTally right = RequestTally (Map.unionWith (+) left right)

instance Monoid RequestTally where
    mempty = RequestTally Map.empty

-- | The tally of a single attempt.
oneRequest :: RequestKind -> RequestTally
oneRequest kind = RequestTally (Map.singleton kind 1)

-- | Every kind the tally counted, beside its count.
tallyCounts :: RequestTally -> [(RequestKind, Int)]
tallyCounts (RequestTally counts) = Map.toAscList counts

-- | The counts as an audit line reads them.
renderRequestTally :: RequestTally -> Text
renderRequestTally tally
    | null counted = "no requests"
    | otherwise = T.intercalate ", " [requestKindName kind <> " " <> show n | (kind, n) <- counted]
  where
    counted = filter ((> 0) . snd) (tallyCounts tally)

-- | What one request of each kind costs its scope in seconds, at the rate a cycle runs.
newtype CyclePace = CyclePace (Map RequestKind Rational)
    deriving stock (Eq, Show)

-- | The pace that imposes no wait, which a scope with no declared capacity runs at.
freePace :: CyclePace
freePace = CyclePace Map.empty

-- | Build a pace from the seconds each kind is held to.
paceOf :: Map RequestKind Rational -> CyclePace
paceOf = CyclePace

-- | The wait one request of this kind takes, which is none where the pace names no cost.
paceSeconds :: CyclePace -> RequestKind -> NominalDiffTime
paceSeconds (CyclePace costs) kind = maybe 0 fromRational (Map.lookup kind costs)

-- | The gate one store's requests pass through: it counts each one and waits its scope's pace.
newtype RequestGate = RequestGate
    { gateSpend :: RequestKind -> IO ()
    }

-- | What one cycle cost: each scope's own attempts, and the time it spent outside budget waits.
data CycleCost = CycleCost
    { ccRequests :: Map QuotaScope RequestTally
    , ccWorkSeconds :: Rational
    }
    deriving stock (Eq, Show)

{- | The cycle's own end of the budget: it opens a measurement, reads what the cycle cost, and
installs the rate the next cycle's requests wait at.
-}
data BudgetPort = BudgetPort
    { budgetOpen :: IO ()
    , budgetClose :: IO CycleCost
    , budgetPaced :: Map QuotaScope CyclePace -> IO ()
    }

{- | One meter shared by every store a cycle touches, beside the gate each scope's requests pass
through. The wait is injected, so a spec reads the pacing without serving it.
-}
newBudgetMeter :: (NominalDiffTime -> IO ()) -> IO (BudgetPort, QuotaScope -> RequestGate)
newBudgetMeter wait = do
    tallies <- newIORef Map.empty
    waited <- newIORef 0
    paces <- newIORef Map.empty
    opened <- newIORef =<< monotonicNow
    let port =
            BudgetPort
                { budgetOpen = do
                    writeIORef tallies Map.empty
                    writeIORef waited 0
                    monotonicNow >>= writeIORef opened
                , budgetClose = do
                    elapsed <- monoSecondsBetween <$> readIORef opened <*> monotonicNow
                    spent <- readIORef waited
                    counted <- readIORef tallies
                    pure CycleCost{ccRequests = counted, ccWorkSeconds = max 0 (toRational elapsed - spent)}
                , budgetPaced = writeIORef paces
                }
        gateFor scope =
            RequestGate
                { gateSpend = \kind -> do
                    atomicModifyIORef' tallies (\held -> (Map.insertWith (<>) scope (oneRequest kind) held, ()))
                    pace <- Map.findWithDefault freePace scope <$> readIORef paces
                    let seconds = paceSeconds pace kind
                    when (seconds > 0) $ do
                        -- The wait served, not the wait asked for, so the work time it comes out
                        -- of stays right however the injected wait behaves.
                        before <- monotonicNow
                        wait seconds
                        served <- monoSecondsBetween before <$> monotonicNow
                        atomicModifyIORef' waited (\held -> (held + toRational (max 0 served), ()))
                }
    pure (port, gateFor)

-- A rate as a boot or audit line spells it.
renderRate :: Rational -> Text
renderRate rate = show (toHundredths rate) <> "/s"

{- | A rational as a line spells it, rounded to whole hundredths. A budget fraction and a rate are
both reported this way, so a line never carries an unbounded expansion.
-}
toHundredths :: Rational -> Double
toHundredths value = fromInteger (round (value * 100)) / 100
