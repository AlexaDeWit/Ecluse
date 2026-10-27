-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Ruby observations use the existing native TLS edge and actual product forwarding inventory.
module Ecluse.E2E.Harness.RubyGemsGateway (
    ObservedRubyProxy,
    withObservedRubyProxy,
    rubyProxy,
    RubyRequest (..),
    RubyTraffic (..),
    rubyTraffic,
    rubyTrafficSince,
) where

import Ecluse.E2E.Harness.Cargo (CargoRequest (..), CargoTraffic (..))
import Ecluse.E2E.Harness.CargoGateway (ObservedCargoProxy, cargoTraffic, observedProxy, withObservedCargoProxy)
import Ecluse.E2E.Harness.Types (E2E, E2EConfig, GlobalDataPlane)

-- | The shared edge transports arbitrary native paths without interpreting ecosystem metadata.
type ObservedRubyProxy = ObservedCargoProxy

-- | Product requests traverse TLS, the recording edge, and the real image listener.
withObservedRubyProxy :: E2EConfig -> GlobalDataPlane -> (ObservedRubyProxy -> IO a) -> IO a
withObservedRubyProxy = withObservedCargoProxy

-- | This endpoint belongs to the product rather than the public fixture.
rubyProxy :: ObservedRubyProxy -> E2E
rubyProxy = observedProxy

-- | Each answered native request retains its method, full query, authority and status.
data RubyRequest = RubyRequest
    { rubyRequestScheme :: Text
    , rubyRequestHost :: Text
    , rubyRequestMethod :: Text
    , rubyRequestPath :: Text
    , rubyRequestStatus :: Int
    }
    deriving stock (Eq, Show)

-- | Client, forwarding and public inventories remain separately observable.
data RubyTraffic = RubyTraffic
    { rubyClientRequests :: [RubyRequest]
    , rubyProxyRequests :: [RubyRequest]
    , rubyUpstreamRequests :: [RubyRequest]
    }
    deriving stock (Eq, Show)

-- | Reusing the edge preserves its independent listener and upstream access-log checks.
rubyTraffic :: ObservedRubyProxy -> IO RubyTraffic
rubyTraffic observed = do
    traffic <- cargoTraffic observed
    pure (RubyTraffic (map request (cargoClientRequests traffic)) (map request (cargoProxyRequests traffic)) (map request (cargoUpstreamRequests traffic)))
  where
    request raw = RubyRequest (cargoRequestScheme raw) (cargoRequestHost raw) (cargoRequestMethod raw) (cargoRequestPath raw) (cargoRequestStatus raw)

-- | Each operation compares only requests emitted after its own starting observation.
rubyTrafficSince :: RubyTraffic -> RubyTraffic -> RubyTraffic
rubyTrafficSince before after =
    RubyTraffic
        (drop (length (rubyClientRequests before)) (rubyClientRequests after))
        (drop (length (rubyProxyRequests before)) (rubyProxyRequests after))
        (drop (length (rubyUpstreamRequests before)) (rubyUpstreamRequests after))
