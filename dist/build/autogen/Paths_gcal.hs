module Paths_gcal (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/b/.cabal/bin"
libdir     = "/Users/b/.cabal/lib/x86_64-osx-ghc-7.8.4/gcal-0.1.0.0"
datadir    = "/Users/b/.cabal/share/x86_64-osx-ghc-7.8.4/gcal-0.1.0.0"
libexecdir = "/Users/b/.cabal/libexec"
sysconfdir = "/Users/b/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gcal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gcal_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "gcal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gcal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gcal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
