{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Core.Osv.Query (
    OsvRange (..),
    queryPackageVulnerabilities,
) where

import Database.SQLite.Simple

data OsvRange = OsvRange
    { osvPackage :: Text
    , osvCveId :: Text
    , osvIntroduced :: Maybe Text
    , osvFixed :: Maybe Text
    , osvSeverity :: Maybe Text
    , osvEpss :: Maybe Double
    }
    deriving stock (Show, Eq)

instance FromRow OsvRange where
    fromRow = OsvRange <$> field <*> field <*> field <*> field <*> field <*> field

queryPackageVulnerabilities :: (MonadIO m) => Connection -> Text -> m [OsvRange]
queryPackageVulnerabilities conn pkgName =
    liftIO $
        query
            conn
            "SELECT package_name, cve_id, introduced_version, fixed_version, severity, epss_score FROM package_vulnerability_ranges WHERE package_name = ?"
            (Only pkgName)
