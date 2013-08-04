module Paths_hmsshell (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\dvig\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\dvig\\AppData\\Roaming\\cabal\\hmsshell-1.0\\ghc-7.4.2"
datadir    = "C:\\Users\\dvig\\AppData\\Roaming\\cabal\\hmsshell-1.0"
libexecdir = "C:\\Users\\dvig\\AppData\\Roaming\\cabal\\hmsshell-1.0"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hmsshell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hmsshell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hmsshell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hmsshell_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
