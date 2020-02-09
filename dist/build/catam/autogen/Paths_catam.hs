{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_catam (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jdelouche/.cabal/bin"
libdir     = "/Users/jdelouche/.cabal/lib/x86_64-osx-ghc-8.6.3/catam-0.1.0.0-BR8nGibjMEpHUe8YpKklJ3-catam"
dynlibdir  = "/Users/jdelouche/.cabal/lib/x86_64-osx-ghc-8.6.3"
datadir    = "/Users/jdelouche/.cabal/share/x86_64-osx-ghc-8.6.3/catam-0.1.0.0"
libexecdir = "/Users/jdelouche/.cabal/libexec/x86_64-osx-ghc-8.6.3/catam-0.1.0.0"
sysconfdir = "/Users/jdelouche/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "catam_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "catam_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "catam_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "catam_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "catam_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "catam_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
