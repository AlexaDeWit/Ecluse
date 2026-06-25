{- | Fixture generation for the end-to-end suite: the static file tree the nginx
public-upstream stub serves.

Each fixture package is written as an npm-format packument plus a real gzipped tar
artifact whose __sha-512 SRI is computed from the bytes actually written__, so the
proxy's serve path, the worker's integrity gate, and npm's own SRI check all see a
consistent (or, for the tamper case, deliberately inconsistent) digest. Versions are
backdated well past the default @min-age@ quarantine so the allow path is not gated
shut by the age rule.

The tree mirrors the npm registry layout the stub serves over HTTP:

> \<name\>            — the packument JSON
> \<name\>/-/\<name\>-\<version\>.tgz — the artifact

so a packument's @dist.tarball@ (@http:\/\/upstream\/\<name\>\/-\/…@) resolves on the
same stub.
-}
module Ecluse.E2E.Fixtures (
    PkgSpec (..),
    defaultPkgSpec,
    allowPkg,
    denyPkg,
    mirrorPkg,
    tamperPkg,
    headPkg,
    fixturePackages,
    buildFixtures,
) where

import Crypto.Hash (Digest, SHA512, hash)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString qualified as BS
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process.Typed (proc, runProcess_)

{- | One fixture package: its identity plus the two behaviours the scenarios turn
on — whether it declares an install script (so the @DenyInstallTimeExecution@ rule
blocks it) and whether its served bytes are corrupted after the SRI is fixed (so the
integrity gate must reject it).
-}
data PkgSpec = PkgSpec
    { psName :: Text
    -- ^ The package name (also the mount-relative path the stub serves it at).
    , psVersion :: Text
    -- ^ The single published version.
    , psInstallScript :: Bool
    -- ^ Declare an install script — the @DenyInstallTimeExecution@ trigger.
    , psTamper :: Bool
    {- ^ Corrupt the artifact bytes after the packument's SRI is computed, so the
    served bytes no longer match the declared integrity.
    -}
    }
    deriving stock (Eq, Show)

-- | A benign package: one backdated version, no install script, untampered bytes.
defaultPkgSpec :: Text -> PkgSpec
defaultPkgSpec name =
    PkgSpec{psName = name, psVersion = "1.0.0", psInstallScript = False, psTamper = False}

-- | An allow-listed package: installs cleanly end to end.
allowPkg :: PkgSpec
allowPkg = defaultPkgSpec "e2e-allow"

-- | A package with an install script: denied at the public surface.
denyPkg :: PkgSpec
denyPkg = (defaultPkgSpec "e2e-deny"){psInstallScript = True}

-- | A package used to exercise the mirror round-trip (served, then mirrored).
mirrorPkg :: PkgSpec
mirrorPkg = defaultPkgSpec "e2e-mirror"

-- | A package whose artifact bytes are tampered: the worker must refuse to mirror it.
tamperPkg :: PkgSpec
tamperPkg = (defaultPkgSpec "e2e-tamper"){psTamper = True}

{- | A package used only for @HEAD@ probes. A @HEAD@ on a tarball must report the
artifact size without streaming the body and without enqueueing a mirror, so this
package is never installed or @GET@ (either would itself mirror it) — leaving an empty
mirror attributable to the @HEAD@ alone.
-}
headPkg :: PkgSpec
headPkg = defaultPkgSpec "e2e-head"

-- | The full fixture set the stub serves.
fixturePackages :: [PkgSpec]
fixturePackages = [allowPkg, denyPkg, mirrorPkg, tamperPkg, headPkg]

{- | Write every fixture package under @root@ (the directory bind-mounted into the
nginx stub as its document root). Creates the packument and the gzipped artifact and
fixes the packument's @dist.integrity@ to the artifact's real sha-512, then — for a
tamper spec — corrupts the artifact so the served bytes diverge from that digest.
-}
buildFixtures :: FilePath -> [PkgSpec] -> IO ()
buildFixtures root = traverse_ (buildOne root)

buildOne :: FilePath -> PkgSpec -> IO ()
buildOne root spec = do
    let name = toString (psName spec)
        ver = toString (psVersion spec)
        pkgDir = root </> name
        tarDir = pkgDir </> "-"
        tgzPath = tarDir </> (name <> "-" <> ver <> ".tgz")
        -- A scratch directory holding the package tree `tar` archives.
        workPkg = root </> (".work-" <> name) </> "package"
    createDirectoryIfMissing True tarDir
    createDirectoryIfMissing True workPkg
    -- The artifact's package.json (npm tarballs root everything under `package/`).
    writeFileLBS (workPkg </> "package.json") (Aeson.encode (tarballPackageJson spec))
    writeFileLBS (workPkg </> "index.js") "module.exports = {};\n"
    -- Deterministic gzip (fixed mtime) so a rebuild yields identical bytes.
    runProcess_ $
        proc
            "tar"
            [ "--sort=name"
            , "--mtime=2020-01-01 00:00:00Z"
            , "--owner=0"
            , "--group=0"
            , "--numeric-owner"
            , "-czf"
            , tgzPath
            , "-C"
            , root </> (".work-" <> name)
            , "package"
            ]
    bytes <- BS.readFile tgzPath
    let sri = sha512Sri bytes
    -- The packument is served at @/\<name\>@ but the tarball lives under
    -- @/\<name\>/-/@, so @\<name\>@ cannot be both a file and a directory. The
    -- packument is therefore stored *inside* the package directory and the nginx
    -- stub config (see "Ecluse.E2E.Harness") maps @/\<name\>@ to it.
    writeFileLBS (pkgDir </> "packument.json") (Aeson.encode (packument spec sri))
    when (psTamper spec) $
        -- Corrupt the served artifact after the SRI is fixed: the integrity gate
        -- (worker) and npm's own check must now reject these bytes.
        BS.appendFile tgzPath "tampered"

-- | @sha512-<base64>@ Subresource-Integrity string over the given bytes.
sha512Sri :: ByteString -> Text
sha512Sri bytes =
    let digest = hash bytes :: Digest SHA512
        b64 = convertToBase Base64 digest :: ByteString
     in "sha512-" <> decodeUtf8 b64

-- | The artifact's @package.json@: identity plus, for the deny case, an install script.
tarballPackageJson :: PkgSpec -> Value
tarballPackageJson spec =
    object $
        [ "name" .= psName spec
        , "version" .= psVersion spec
        ]
            <> ["scripts" .= object ["install" .= ("node -e \"\"" :: Text)] | psInstallScript spec]

{- | The npm packument the stub serves for a package: a single backdated version
pointing its artifact at the stub, with the integrity fixed to the real digest and
(for the deny case) @hasInstallScript@ + a declared install script so the rule fires.
-}
packument :: PkgSpec -> Text -> Value
packument spec sri =
    object
        [ "name" .= psName spec
        , "dist-tags" .= object ["latest" .= psVersion spec]
        , "versions" .= object [fromString (toString (psVersion spec)) .= versionMeta]
        , "time"
            .= object
                [ "created" .= backdated
                , "modified" .= backdated
                , fromString (toString (psVersion spec)) .= backdated
                ]
        ]
  where
    backdated :: Text
    backdated = "2020-01-01T00:00:00.000Z"

    versionMeta :: Value
    versionMeta =
        object $
            [ "name" .= psName spec
            , "version" .= psVersion spec
            , "dist"
                .= object
                    [ "tarball" .= tarballUrl
                    , "integrity" .= sri
                    ]
            ]
                <> installScriptFields

    tarballUrl :: Text
    tarballUrl =
        "http://upstream/"
            <> psName spec
            <> "/-/"
            <> psName spec
            <> "-"
            <> psVersion spec
            <> ".tgz"

    installScriptFields :: [Pair]
    installScriptFields
        | psInstallScript spec =
            [ "hasInstallScript" .= True
            , "scripts" .= object ["install" .= ("node -e \"\"" :: Text)]
            ]
        | otherwise = []
