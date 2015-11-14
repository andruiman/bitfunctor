module Paths_bitfunctor (
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

bindir     = "/home/andruiman/.cabal/bin"
libdir     = "/home/andruiman/.cabal/lib/i386-linux-ghc-7.6.3/bitfunctor-0.1.0.0"
datadir    = "/home/andruiman/.cabal/share/i386-linux-ghc-7.6.3/bitfunctor-0.1.0.0"
libexecdir = "/home/andruiman/.cabal/libexec"
sysconfdir = "/home/andruiman/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bitfunctor_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bitfunctor_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bitfunctor_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bitfunctor_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bitfunctor_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
