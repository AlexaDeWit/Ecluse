-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A store-declared presentation reaches a prepared reader without changing its preparation or its checks.
module Ecluse.Core.Registry.Metadata.Fetch.PreparationSpec (spec) where

import Data.List (lookup)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client qualified as Client
import Test.Hspec

import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Registry (UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (PrepareRead), bindToOrigin, canChargeFullReadWorkspace, prepareRead, presentedRead, staticRead)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (erRequest))
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Registry.Request (AuthMechanism (AuthRawKey), CredentialMapping, StoreOperation (StoreRead), declaredPresentation, mkBaseRequest, presentedMapping)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Registry.Npm (isOdd)
import Ecluse.Test.Registry.Npm.Metadata (npmRead)

spec :: Spec
spec = describe "presentedRead" $ do
    it "keeps a static reader static and a prepared reader prepared" $ do
        canChargeFullReadWorkspace (presentedRead rawKey (staticRead npmRead)) `shouldBe` False
        canChargeFullReadWorkspace (presentedRead rawKey (PrepareRead (\_ -> pure (Right npmRead)))) `shouldBe` True

    it "presents the reader's credential under the given mapping" $ do
        presented <- prepared (presentedRead rawKey (staticRead npmRead))
        request <- either (fail . show) pure (erRequest presented (mkBaseRequest root) (Just (bareCredential (mkSecret "tok"))) isOdd)
        lookup "Authorization" (Client.requestHeaders request) `shouldBe` Just "tok"

    it "keeps the reader's own refusal of a request under another base" $ do
        presented <- prepared (presentedRead rawKey (staticRead (bindToOrigin refusal (registryUrlText root) npmRead)))
        void (erRequest presented (mkBaseRequest root) Nothing isOdd) `shouldSatisfy` isRight
        void (erRequest presented (mkBaseRequest elsewhere) Nothing isOdd) `shouldBe` Left refusal

-- npm's mapping under a store that asks for the raw key, which npm's own Bearer never renders.
rawKey :: CredentialMapping
rawKey = presentedMapping (declaredPresentation (const AuthRawKey)) StoreRead npmCredential

-- Prepare a reader under an origin at 'root'.
prepared :: PrepareRead -> IO EcosystemRead
prepared factory =
    withExchangeClient (newManager defaultManagerSettings) $ \client ->
        prepareRead factory (originClient defaultLimits client root Nothing) >>= either (fail . show) pure

root :: RegistryUrl
root = loopbackRegistryUrl "http://127.0.0.1:1/bound"

elsewhere :: RegistryUrl
elsewhere = loopbackRegistryUrl "http://127.0.0.1:1/elsewhere"

refusal :: UrlFormationError
refusal = UnparseableUrl "the reader belongs to another origin"
