module Paths_git_prompt (
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

bindir     = "/home/jassob/Projects/zsh-git-prompt/.cabal-sandbox/bin"
libdir     = "/home/jassob/Projects/zsh-git-prompt/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.1/gitpr_LnUBkCpMwWR2oAXpqtdfWQ"
datadir    = "/home/jassob/Projects/zsh-git-prompt/.cabal-sandbox/share/x86_64-linux-ghc-7.10.1/git-prompt-0.1.0.0"
libexecdir = "/home/jassob/Projects/zsh-git-prompt/.cabal-sandbox/libexec"
sysconfdir = "/home/jassob/Projects/zsh-git-prompt/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "git_prompt_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "git_prompt_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "git_prompt_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "git_prompt_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "git_prompt_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
