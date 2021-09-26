{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_ValveValueKeyvalue (
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

bindir     = "C:\\Users\\berna\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\berna\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.5\\ValveValueKeyvalue-0.1.0.0-inplace"
dynlibdir  = "C:\\Users\\berna\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.5"
datadir    = "C:\\Users\\berna\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.5\\ValveValueKeyvalue-0.1.0.0"
libexecdir = "C:\\Users\\berna\\AppData\\Roaming\\cabal\\ValveValueKeyvalue-0.1.0.0-inplace\\x86_64-windows-ghc-8.10.5\\ValveValueKeyvalue-0.1.0.0"
sysconfdir = "C:\\Users\\berna\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ValveValueKeyvalue_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ValveValueKeyvalue_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ValveValueKeyvalue_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ValveValueKeyvalue_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ValveValueKeyvalue_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ValveValueKeyvalue_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
