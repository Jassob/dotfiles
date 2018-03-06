#! env nix-shell
#! nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (pkgs: [])" rofi

{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception (SomeException, handle)
import           Data.Function (on)
import           Data.List (sortBy, intercalate, elemIndex)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (maybe)
import           System.Directory (getCurrentDirectory)
import           System.Process (spawnProcess, readProcess)
import           System.Environment (getEnv, getArgs)
import           System.Exit (exitSuccess)

data Options = Opts
  { browser :: String
  , file    :: FilePath
  }

def :: Options
def = Opts { browser = "firefox", file = "$HOME/.cache/browse" }

main :: IO ()
main = getArgs >>= bootstrap

bootstrap :: [String] -> IO ()
bootstrap args = maybe printUsage run . handleArgs args $ def

handleArgs :: [String] -> Options -> Maybe Options
handleArgs ("-f":file:args)    os = handleArgs args $ os { file = file }
handleArgs ("-b":browser:args) os = handleArgs args $ os { browser = browser }
handleArgs []                  os = pure os
handleArgs _                   _  = Nothing

run :: Options -> IO ()
run Opts{browser = browser, file = fp} = handle handler $ do
  contentMap <- contentOrEmpty fp
  url <- readProcess "rofi" rofiOptions . unwords . getFrequentItems $ contentMap
  spawnProcess browser [schemized url]
  updateUrlStore fp (init url) contentMap

    where handler :: SomeException -> IO ()
          handler = const exitSuccess

          rofiOptions :: [String]
          rofiOptions = ["-dmenu", "-sep", " ", "-i", "-p", "Open URL: http://"]

          schemized :: String -> String
          schemized url@('h':'t':'t':'p':_) = url
          schemized url                     = "http://" ++ url

browseFile :: IO FilePath
browseFile = (++ "/.cache/browse") <$> getEnv "HOME"

contentOrEmpty :: FilePath -> IO (Map String Int)
contentOrEmpty fp = handle handler $ read <$> (readFile =<< absolutize fp)

  where handler :: SomeException -> IO (Map String Int)
        handler = const . return $ M.empty

        absolutize :: FilePath -> IO String
        absolutize fp | head fp == '/' = pure fp
                      | head fp == '$' = case elemIndex '/' fp of
                          Just idx -> (++ drop idx fp) <$> getEnv (tail . take idx $ fp)
                          Nothing  -> getEnv (tail fp)
                      | otherwise = (++ fp) <$> getCurrentDirectory

getFrequentItems :: Map String Int -> [String]
getFrequentItems = map fst . sortBy (flip compare `on` snd) . M.toList

updateUrlStore :: FilePath -> String -> Map String Int -> IO ()
updateUrlStore fp url = writeFile fp . show . M.alter act url
  where act :: Maybe Int -> Maybe Int
        act Nothing  = pure 1
        act (Just i) = pure $ i + 1

printUsage :: IO ()
printUsage = putStrLn . concat $
  [ "Usage: prompt-open-url [OPTIONS]", "\n", "\n"
  , "OPTIONS:", "\n"
  , "-b", "\t", "Specify browser. A string value that must resolve to a binary in $PATH", "\n", "\t", "Default: \"firefox\""
  , "-f", "\t", "Specify history file. An absolute path to a history file.", "\n", "\t", "Default: \"$HOME/.cache/browse\""
  ]
