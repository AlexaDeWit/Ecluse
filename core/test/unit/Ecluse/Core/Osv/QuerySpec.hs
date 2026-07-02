{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Core.Osv.QuerySpec (spec) where

import Database.SQLite.Simple
import Ecluse.Core.Osv.Query
import Test.Hspec
import UnliftIO (bracket)

spec :: Spec
spec = describe "Osv.Query unit tests" $ do
    it "queries the package_vulnerability_ranges table correctly" $ do
        bracket (open ":memory:") close $ \conn -> do
            execute_ conn "CREATE TABLE package_vulnerability_ranges (package_name TEXT NOT NULL, cve_id TEXT NOT NULL, introduced_version TEXT, fixed_version TEXT, severity TEXT, epss_score REAL, PRIMARY KEY (package_name, cve_id, introduced_version, fixed_version))"
            execute_ conn "INSERT INTO package_vulnerability_ranges (package_name, cve_id, introduced_version, fixed_version, severity, epss_score) VALUES ('pkg', 'CVE-123', '0', '1.0.0', 'HIGH', 0.9)"

            res <- queryPackageVulnerabilities conn "pkg"
            res `shouldBe` [OsvRange "pkg" "CVE-123" (Just "0") (Just "1.0.0") (Just "HIGH") (Just 0.9)]

            resEmpty <- queryPackageVulnerabilities conn "other"
            resEmpty `shouldBe` []
