-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Ordered install evidence preserves overrides, structured failures and the protected retry pass.
module Ecluse.Core.Rules.InstallSpec (spec) where

import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (CodeExecSignal (..))
import Ecluse.Core.Rules (PreparedRule (PreparedRule), RuleEval (PerVersion), evalRulesWithInstallRead, newEvaluatorWithInstallRead, prepare)
import Ecluse.Core.Rules.Effectful (ReadFault (ReadFault))
import Ecluse.Core.Rules.Install
import Ecluse.Core.Rules.Types
import Ecluse.Core.Version (mkVersion)
import Ecluse.Rules.Support (ctx, pkg)
import Ecluse.Test.Rules (atPrecedence, inertRuleDeps, isApproved, isUndecidable)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = describe "ordered install evidence" $ do
    it "does not read an archive for a higher-ranked identity allow or deny" $
        for_ [AllowByIdentity "thing", DenyByIdentity "thing"] $ \override -> do
            count <- newIORef (0 :: Int)
            rules <- prepare inertRuleDeps [atPrecedence 500 override, atPrecedence 300 DenyInstallTimeExecution]
            decision <- evalRulesWithInstallRead RequireInstallResolution ctx rules unknown (Just (counted count (InstallReadFailed timeoutFault)))
            decision `shouldSatisfy` decided
            readIORef count `shouldReturn` 0
    it "does not read an archive when the default age policy has no execution rule" $ do
        count <- newIORef (0 :: Int)
        rules <- prepare inertRuleDeps [atPrecedence 100 (AllowIfOlderThan 0)]
        decision <- evalRulesWithInstallRead RequireInstallResolution ctx rules unknown (Just (counted count (InstallReadFailed timeoutFault)))
        decision `shouldSatisfy` isApproved
        readIORef count `shouldReturn` 0
    it "uses already determined signals without reading" $
        for_ [NoCodeOnInstall, RunsCodeOnInstall "declared build"] $ \signal -> do
            count <- newIORef (0 :: Int)
            rules <- executionThenAge
            decision <- evalRulesWithInstallRead RequireInstallResolution ctx rules unknown{evInstallCode = Known signal} (Just (counted count (InstallReadFailed timeoutFault)))
            decision `shouldSatisfy` decided
            readIORef count `shouldReturn` 0
    it "refuses a missing resolver or a resolver that returns unknown before a lower age allow" $ do
        rules <- executionThenAge
        for_ [Nothing, Just (InstallEvidenceRead (pure (InstallDetermined CodeExecUnknown))), Just (InstallEvidenceRead (pure (InstallUndetermined InstallSignalUnread)))] $
            evalRulesWithInstallRead RequireInstallResolution ctx rules unknown >=> (`shouldSatisfy` isUndecidable)
    it "retains read-fault transience instead of falling through to age" $ do
        rules <- executionThenAge
        for_ [WillResolve Nothing, WontResolve] $ \transience -> do
            let fault = ReadFault transience EvaluationFailed (InstallEvidenceFailed "unavailable archive")
            evalRulesWithInstallRead RequireInstallResolution ctx rules unknown (Just (InstallEvidenceRead (pure (InstallReadFailed fault))))
                `shouldReturn` Undecidable transience (RuleUnable (ruleName DenyInstallTimeExecution) EvaluationFailed)
    it "reads one immutable subject once when a later rule forces the protected pass" $ do
        count <- newIORef (0 :: Int)
        rules <- executionThenAge
        let poison = PreparedRule "later-fault" 200 (PerVersion (\_ _ -> throwIO (TestContractEscape "after install evidence")))
        decision <- evalRulesWithInstallRead RequireInstallResolution ctx (poison : rules) unknown (Just (counted count (InstallDetermined NoCodeOnInstall)))
        decision `shouldSatisfy` isUndecidable
        readIORef count `shouldReturn` 1
    it "does not share one package's resolver result between version subjects" $ do
        count <- newIORef (0 :: Int)
        rules <- executionThenAge
        decide <- newEvaluatorWithInstallRead RequireInstallResolution ctx rules
        a <- decide unknown (Just (counted count (InstallDetermined NoCodeOnInstall)))
        b <- decide unknown{evVersion = mkVersion Npm "2.0.0"} (Just (counted count (InstallDetermined (RunsCodeOnInstall "other digest"))))
        a `shouldSatisfy` isApproved
        b `shouldSatisfy` blocked
        readIORef count `shouldReturn` 2
    it "preserves projected unknown abstention for existing callers" $ do
        rules <- executionThenAge
        evalRulesWithInstallRead ProjectedInstallFacts ctx rules unknown Nothing >>= (`shouldSatisfy` isApproved)

unknown :: RuleEvidence
unknown = (pkg Nothing 30){evInstallCode = Known CodeExecUnknown}

executionThenAge :: IO [PreparedRule]
executionThenAge = prepare inertRuleDeps [atPrecedence 300 DenyInstallTimeExecution, atPrecedence 100 (AllowIfOlderThan 0)]

counted :: IORef Int -> InstallEvidenceResult -> InstallEvidenceRead
counted count result = InstallEvidenceRead (modifyIORef' count (+ 1) $> result)

timeoutFault :: ReadFault
timeoutFault = ReadFault (WillResolve Nothing) EvaluationFailed AttemptTimedOut

decided :: Decision -> Bool
decided decision = isApproved decision || blocked decision

blocked :: Decision -> Bool
blocked = \case
    Blocked{} -> True
    _ -> False
