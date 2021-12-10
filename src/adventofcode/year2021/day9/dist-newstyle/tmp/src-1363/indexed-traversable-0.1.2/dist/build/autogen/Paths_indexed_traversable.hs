{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_indexed_traversable (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,1,2] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/jlalwani/.cabal/store/ghc-8.10.7/ndxd-trvrsbl-0.1.2-ec41ea5b/bin"
libdir     = "/Users/jlalwani/.cabal/store/ghc-8.10.7/ndxd-trvrsbl-0.1.2-ec41ea5b/lib"
dynlibdir  = "/Users/jlalwani/.cabal/store/ghc-8.10.7/lib"
datadir    = "/Users/jlalwani/.cabal/store/ghc-8.10.7/ndxd-trvrsbl-0.1.2-ec41ea5b/share"
libexecdir = "/Users/jlalwani/.cabal/store/ghc-8.10.7/ndxd-trvrsbl-0.1.2-ec41ea5b/libexec"
sysconfdir = "/Users/jlalwani/.cabal/store/ghc-8.10.7/ndxd-trvrsbl-0.1.2-ec41ea5b/etc"

getBinDir     = catchIO (getEnv "indexed_traversable_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "indexed_traversable_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "indexed_traversable_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "indexed_traversable_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "indexed_traversable_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "indexed_traversable_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
