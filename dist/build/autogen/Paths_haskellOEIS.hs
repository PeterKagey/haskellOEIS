module Paths_haskellOEIS (
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
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/pkagey/.cabal/bin"
libdir     = "/Users/pkagey/.cabal/lib/x86_64-osx-ghc-7.10.3/haskellOEIS-1.0-8Mq6CWS9yYwC6ngJqDwWtI"
datadir    = "/Users/pkagey/.cabal/share/x86_64-osx-ghc-7.10.3/haskellOEIS-1.0"
libexecdir = "/Users/pkagey/.cabal/libexec"
sysconfdir = "/Users/pkagey/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskellOEIS_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskellOEIS_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskellOEIS_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskellOEIS_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskellOEIS_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
