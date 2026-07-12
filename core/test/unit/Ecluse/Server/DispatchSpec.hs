-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.DispatchSpec (spec) where

import Data.ByteString.Builder (toLazyByteString)
import Data.Text qualified as T
import Network.HTTP.Types (Method, methodDelete, methodGet, methodHead, methodPut, statusCode)
import Network.Wai (Response, responseStatus, responseToStream)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry.Npm.Serve (npmRenderer)
import Ecluse.Core.Server.Dispatch (RouteAction (AnswerLocally, RunPipeline), routeAction)
import Ecluse.Core.Server.Route (Filename (Filename), Route (..))
import Ecluse.Core.Version (Version, mkVersion)

{- | Which of the two kinds of action a route dispatched to, without inspecting the
function inside it (a handler is not comparable, but the __choice__ is what matters:
an 'AnswerLocally' route must never reach the data plane, and a 'RunPipeline' route
must never be answered without it).
-}
data ActionKind = Local | Pipeline
    deriving stock (Eq, Show)

kindOf :: RouteAction -> ActionKind
kindOf = \case
    AnswerLocally _ -> Local
    RunPipeline _ -> Pipeline

{- | Answer a locally-answered route through the npm mount's renderer, so the assertions
see exactly the bytes a client would. Fails the example on a route that dispatched to
the data plane instead: this helper is only meaningful for the pure half.
-}
answer :: Method -> Route -> IO Response
answer method route = case routeAction method route of
    AnswerLocally toResponse -> pure (toResponse npmRenderer)
    RunPipeline _ -> fail "expected a locally-answered route, but the route dispatched to the data-plane pipeline"

-- | A response's body bytes, drained from its stream.
bodyOf :: Response -> IO LByteString
bodyOf response = do
    let (_, _, withBody) = responseToStream response
    withBody $ \streamingBody -> do
        chunks <- newIORef mempty
        streamingBody (\chunk -> modifyIORef' chunks (<> chunk)) pass
        toLazyByteString <$> readIORef chunks

-- | A response's body as text, for the denial surfaces whose message is asserted.
bodyText :: Response -> IO Text
bodyText response = decodeUtf8 <$> bodyOf response

unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

npmVersion :: Text -> Version
npmVersion = mkVersion Npm

{- | The one interpretation of a classified 'Route': which routes the proxy answers
itself, which reach the data plane, and what the locally-answered ones say.

'routeAction' is total over the 'Route' sum, so these specs pin the whole serve
vocabulary. They deliberately assert the __kind__ of action for the data-plane routes
rather than the handler inside it: that a 'Packument' is never answered from the router
(which would fabricate a response without consulting the rules) is the property worth
holding, and the handlers' own behaviour is pinned by the pipeline specs.
-}
spec :: Spec
spec = do
    describe "routeAction -- which routes the proxy answers itself" $ do
        it "answers /-/ping locally, with the empty JSON object an npm client expects" $ do
            response <- answer methodGet Ping
            statusCode (responseStatus response) `shouldBe` 200
            bodyOf response `shouldReturn` "{}"

        it "answers search with a 501 pointing at the public registry, in the mount's surface" $ do
            response <- answer methodGet Search
            statusCode (responseStatus response) `shouldBe` 501
            body <- bodyText response
            body `shouldSatisfy` T.isInfixOf "search is not supported"

        it "answers an unrecognised in-mount path with a 404, the routing layer's deny-by-default" $ do
            response <- answer methodGet Unsupported
            statusCode (responseStatus response) `shouldBe` 404
            body <- bodyText response
            body `shouldSatisfy` T.isInfixOf "not found"

    describe "routeAction -- which routes reach the data plane" $ do
        -- A route the rules engine must see is never answered by the router: were one
        -- of these to dispatch locally, the proxy would answer a package request
        -- without ever consulting policy.
        it "sends a packument read to the pipeline" $
            kindOf (routeAction methodGet (Packument (unscoped "lodash"))) `shouldBe` Pipeline

        it "sends an artifact read to the pipeline" $
            kindOf (routeAction methodGet (Tarball (unscoped "is-odd") (npmVersion "3.0.1") (Filename "is-odd-3.0.1.tgz")))
                `shouldBe` Pipeline

        it "sends a publish to the pipeline" $
            kindOf (routeAction methodPut (Publish (unscoped "lodash"))) `shouldBe` Pipeline

        -- A HEAD is a bodiless variation of its GET, not a distinct action, so it takes
        -- the same pipeline branch. Which of the two handlers it selects (the head-mode
        -- one, so a bodiless HEAD never pumps a whole artifact) is pinned where the
        -- handlers run, in the tarball and packument specs.
        it "sends a HEAD on the read routes to the pipeline, like its GET" $ do
            kindOf (routeAction methodHead (Packument (unscoped "lodash"))) `shouldBe` Pipeline
            kindOf (routeAction methodHead (Tarball (unscoped "is-odd") (npmVersion "3.0.1") (Filename "is-odd-3.0.1.tgz")))
                `shouldBe` Pipeline

        -- The method never changes a route's kind: the classifier has already decided
        -- what the request names (a method the proxy does not serve never produces a
        -- servable route in the first place), so dispatch reads the method only to pick
        -- the bodiless HEAD mode.
        it "keeps a route's kind under a method the proxy does not serve" $
            kindOf (routeAction methodDelete (Packument (unscoped "lodash"))) `shouldBe` Pipeline
