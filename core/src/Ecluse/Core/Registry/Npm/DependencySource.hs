-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm dependency specs a client fetches from outside the registry: git sources and remote URLs.
Aliases and local specs stay exempt on npm-package-arg's terms. Detection follows hosted-git-info,
widened to what another client or URL parsing could fetch, so it refuses beyond npm only a spec
npm cannot install either.
-}
module Ecluse.Core.Registry.Npm.DependencySource (
    -- * One spec
    ExternalSource (..),
    externalSource,

    -- * A release's installable dependencies
    InstallableField (..),
    installableField,
    installableFieldName,
    ExternalDependency (..),
    externalDependencyOf,
    earlierDependency,
    renderExternalDependency,
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Text qualified as T

-- | Where a client fetches a dependency from instead of the registry.
data ExternalSource
    = -- | A git repository, by URL, hosted shortcut, GitHub shorthand or scp-like address.
      GitSource
    | -- | A tarball at an @http@ or @https@ URL.
      RemoteUrl
    deriving stock (Eq, Show)

-- | The dependency maps a consumer's install reads, in the order a refusal names them.
data InstallableField
    = Dependencies
    | OptionalDependencies
    | PeerDependencies
    deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | The field a release member names, or 'Nothing' for any other member, @devDependencies@ included.
installableField :: Text -> Maybe InstallableField
installableField = \case
    "dependencies" -> Just Dependencies
    "optionalDependencies" -> Just OptionalDependencies
    "peerDependencies" -> Just PeerDependencies
    _ -> Nothing

-- | The member name of the field in a release object.
installableFieldName :: InstallableField -> Text
installableFieldName = \case
    Dependencies -> "dependencies"
    OptionalDependencies -> "optionalDependencies"
    PeerDependencies -> "peerDependencies"

-- | One installable dependency whose spec fetches from outside the registry.
data ExternalDependency = ExternalDependency
    { edField :: InstallableField
    , edName :: Text
    -- ^ The dependency's name as the map keys it. The spec itself is never kept, since it can carry a credential.
    , edSource :: ExternalSource
    }
    deriving stock (Eq, Show)

{- | Classify one spec. A spec with neither a colon nor a slash is a registry range, a tag or a file
name, so the common case reads the text once and allocates nothing.
-}
externalSource :: Text -> Maybe ExternalSource
externalSource spec
    | not (T.any (\c -> c == ':' || c == '/') spec) = Nothing
    | aliasOrLocal spec = Nothing
    | otherwise = detect (urlInput spec)

-- npm-package-arg tests these on the spec as written, before any URL parsing trims it.
aliasOrLocal :: Text -> Bool
aliasOrLocal spec = hasPrefixFolded "npm:" spec || hasPrefixFolded "file:" spec || pathLike
  where
    pathLike = case T.unpack (T.take 2 spec) of
        '.' : _ -> True
        '/' : _ -> True
        '\\' : _ -> True
        ['~', '/'] -> True
        [letter, ':'] -> isAsciiLetter letter
        _ -> False

-- URL parsing drops tabs and newlines anywhere, and C0 controls and spaces at either end.
urlInput :: Text -> Text
urlInput spec = T.dropAround (<= ' ') (if T.any tabOrNewline spec then T.filter (not . tabOrNewline) spec else spec)
  where
    tabOrNewline c = c == '\t' || c == '\n' || c == '\r'

detect :: Text -> Maybe ExternalSource
detect spec = case scheme spec of
    Just named
        | named == "http" || named == "https" -> Just RemoteUrl
        | named `elem` gitSchemes || "git+" `T.isPrefixOf` named -> Just GitSource
        -- hosted-git-info reads an unknown scheme before an @ as the user of an ssh URL.
        | userAtGitHost spec -> Just GitSource
        | otherwise -> Nothing
    Nothing
        | gitHubShorthand spec -> Just GitSource
        -- A user@host address is an scp-like or ssh git URL. No registry or local spec holds an @ here.
        | T.any (== '@') (T.takeWhile (/= '#') spec) -> Just GitSource
        | otherwise -> Nothing

gitSchemes :: [Text]
gitSchemes = ["git", "ssh", "github", "gitlab", "bitbucket", "gist", "sourcehut"]

-- The URL scheme, folded to lower case, when the spec starts with one.
scheme :: Text -> Maybe Text
scheme spec = case T.uncons spec of
    Just (lead, _)
        | isAsciiLetter lead
        , (named, rest) <- T.span schemeChar spec
        , ":" `T.isPrefixOf` rest ->
            Just (T.toLower named)
    _ -> Nothing
  where
    schemeChar c = isAsciiLetter c || isDigit c || c == '+' || c == '-' || c == '.'

-- hosted-git-info's isGitHubShorthand, which reads user/repo as github:user/repo.
gitHubShorthand :: Text -> Bool
gitHubShorthand spec =
    not (T.any (\c -> isSpace c || c == '@' || c == ':') beforeHash)
        && T.count "/" beforeHash <= 1
        && maybe False (> 0) (T.findIndex (== '/') spec)
        && not ("/" `T.isSuffixOf` beforeHash)
        && not ("." `T.isPrefixOf` spec)
  where
    beforeHash = T.takeWhile (/= '#') spec

userAtGitHost :: Text -> Bool
userAtGitHost spec = any startsAtGitHost (drop 1 (T.splitOn "@" (T.toLower spec)))
  where
    startsAtGitHost after = any (\host -> leads host after || leads ("www." <> host) after) gitHosts
    leads host after = case T.stripPrefix host after of
        Just rest -> maybe True (\(next, _) -> next == ':' || next == '/' || next == '#') (T.uncons rest)
        Nothing -> False

-- The hosts hosted-git-info resolves from a URL's host name.
gitHosts :: [Text]
gitHosts = ["github.com", "gist.github.com", "gitlab.com", "bitbucket.org", "git.sr.ht"]

hasPrefixFolded :: Text -> Text -> Bool
hasPrefixFolded prefix spec = T.toLower (T.take (T.length prefix) spec) == prefix

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAsciiLower c || isAsciiUpper c

{- | A release object's external dependency, reading only maps that are objects and specs that are
strings. Several pick the first field, then the smallest name, so every read form reports one.
-}
externalDependencyOf :: KeyMap Value -> Maybe ExternalDependency
externalDependencyOf release = asum (map inField [minBound .. maxBound])
  where
    inField field = case KeyMap.lookup (Key.fromText (installableFieldName field)) release of
        Just (Object specs) -> asum [ExternalDependency field (Key.toText name) <$> externalSource spec | (name, String spec) <- KeyMap.toAscList specs]
        _ -> Nothing

-- | The dependency 'externalDependencyOf' would report of the two: the earlier field, then the smaller name.
earlierDependency :: ExternalDependency -> ExternalDependency -> ExternalDependency
earlierDependency held next
    | (edField next, edName next) < (edField held, edName held) = next
    | otherwise = held

-- | Name the dependency, its field and the kind of source, without the spec.
renderExternalDependency :: ExternalDependency -> Text
renderExternalDependency dependency =
    "dependency '" <> edName dependency <> "' in " <> installableFieldName (edField dependency) <> " names " <> kind
  where
    kind = case edSource dependency of
        GitSource -> "a git source"
        RemoteUrl -> "a remote URL"
