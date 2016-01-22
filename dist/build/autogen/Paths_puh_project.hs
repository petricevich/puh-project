module Paths_puh_project (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/cromulen/.cabal/bin"
libdir     = "/home/cromulen/.cabal/lib/x86_64-linux-ghc-7.8.4/puh-project-0.1.0.0"
datadir    = "/home/cromulen/.cabal/share/x86_64-linux-ghc-7.8.4/puh-project-0.1.0.0"
libexecdir = "/home/cromulen/.cabal/libexec"
sysconfdir = "/home/cromulen/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "puh_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "puh_project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "puh_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "puh_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "puh_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
