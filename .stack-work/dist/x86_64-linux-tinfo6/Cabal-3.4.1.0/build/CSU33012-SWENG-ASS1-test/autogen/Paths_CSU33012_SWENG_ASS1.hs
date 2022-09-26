{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_CSU33012_SWENG_ASS1 (
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

bindir     = "/home/sasha/Documents/gits/CSU33012-SWENG-ASS1/.stack-work/install/x86_64-linux-tinfo6/365d2b08c6aaccaab72876593060ef8dc26e60a174d024f1fa631b2ce59ac9b7/9.0.2/bin"
libdir     = "/home/sasha/Documents/gits/CSU33012-SWENG-ASS1/.stack-work/install/x86_64-linux-tinfo6/365d2b08c6aaccaab72876593060ef8dc26e60a174d024f1fa631b2ce59ac9b7/9.0.2/lib/x86_64-linux-ghc-9.0.2/CSU33012-SWENG-ASS1-0.1.0.0-Is70vHVSMA94j5fSngsttS-CSU33012-SWENG-ASS1-test"
dynlibdir  = "/home/sasha/Documents/gits/CSU33012-SWENG-ASS1/.stack-work/install/x86_64-linux-tinfo6/365d2b08c6aaccaab72876593060ef8dc26e60a174d024f1fa631b2ce59ac9b7/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/sasha/Documents/gits/CSU33012-SWENG-ASS1/.stack-work/install/x86_64-linux-tinfo6/365d2b08c6aaccaab72876593060ef8dc26e60a174d024f1fa631b2ce59ac9b7/9.0.2/share/x86_64-linux-ghc-9.0.2/CSU33012-SWENG-ASS1-0.1.0.0"
libexecdir = "/home/sasha/Documents/gits/CSU33012-SWENG-ASS1/.stack-work/install/x86_64-linux-tinfo6/365d2b08c6aaccaab72876593060ef8dc26e60a174d024f1fa631b2ce59ac9b7/9.0.2/libexec/x86_64-linux-ghc-9.0.2/CSU33012-SWENG-ASS1-0.1.0.0"
sysconfdir = "/home/sasha/Documents/gits/CSU33012-SWENG-ASS1/.stack-work/install/x86_64-linux-tinfo6/365d2b08c6aaccaab72876593060ef8dc26e60a174d024f1fa631b2ce59ac9b7/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CSU33012_SWENG_ASS1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CSU33012_SWENG_ASS1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "CSU33012_SWENG_ASS1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "CSU33012_SWENG_ASS1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CSU33012_SWENG_ASS1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CSU33012_SWENG_ASS1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
