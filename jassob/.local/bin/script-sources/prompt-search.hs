#! env nix-shell
#! nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (pkgs: [])" rofi

{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception (SomeException, handle)
import           Data.Function (on)
import           Data.List (sortBy, intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (maybe)
import           System.Process (spawnProcess, readProcess)
import           System.Environment (getEnv, getArgs)
import           System.Exit (exitSuccess)
import           Text.Read (readMaybe)

data Options = Opts
  { browser :: FilePath
  , file    :: FilePath
  , engine  :: SearchEngine
  } deriving Show

data SearchEngine = DUCKDUCKGO | GOOGLE

instance Show SearchEngine where
  show DUCKDUCKGO = "duckduckgo"
  show GOOGLE     = "google"

instance Read SearchEngine where
  readsPrec _ "duckduckgo" = [(DUCKDUCKGO, "")]
  readsPrec _ "google"     = [(GOOGLE, "")]

queryToUrl :: String -> SearchEngine -> String
queryToUrl query DUCKDUCKGO = "https://duckduckgo.com/?q=" ++ format query
  where format = id
queryToUrl query GOOGLE = "https://google.com/search?q=" ++ format query
  where format = id

def :: IO Options
def = searchFile >>= \file -> pure Opts
  { browser = "firefox"
  , file = file
  , engine = DUCKDUCKGO
  }

main :: IO ()
main = getArgs >>= bootstrap

bootstrap :: [String] -> IO ()
bootstrap args = maybe printUsage (withOpts run) . handleArgs args =<< def

handleArgs :: [String] -> Options -> Maybe Options
handleArgs ("-f":file:args)    os = handleArgs args $ os { file = file }
handleArgs ("-b":browser:args) os = handleArgs args $ os { browser = browser }
handleArgs ("-s":engine:args)  os = readMaybe engine >>=
  \e -> handleArgs args $ os { engine = e }
handleArgs []                  os = pure os
handleArgs _                   _  = Nothing

withOpts :: (FilePath -> FilePath -> SearchEngine -> IO ()) -> Options -> IO ()
withOpts f os = f (file os) (browser os) (engine os)

run :: FilePath -> FilePath -> SearchEngine -> IO ()
run fp browser se = handle handler $ do
  contentMap <- contentOrEmpty fp
  query <- readProcess "rofi" rofiOptions . unlines . getFrequentItems $ contentMap
  spawnProcess browser [queryToUrl query se]
  updateQueryStore fp (init query) contentMap

    where handler :: SomeException -> IO ()
          handler _ = exitSuccess

          rofiOptions :: [String]
          rofiOptions = ["-dmenu", "-i", "-p", "Search: "]

          schemized :: String -> String
          schemized url@('h':'t':'t':'p':_) = url
          schemized url                     = "http://" ++ url

searchFile :: IO FilePath
searchFile = (++ "/.cache/search") <$> getEnv "HOME"

contentOrEmpty :: FilePath -> IO (Map String Int)
contentOrEmpty fp = handle handler $ read <$> readFile fp

  where handler :: SomeException -> IO (Map String Int)
        handler = const . return $ M.empty

getFrequentItems :: Map String Int -> [String]
getFrequentItems = map fst . sortBy (flip compare `on` snd) . M.toList

updateQueryStore :: FilePath -> String -> Map String Int -> IO ()
updateQueryStore fp url = writeFile fp . show . M.alter act url
  where act :: Maybe Int -> Maybe Int
        act Nothing  = pure 1
        act (Just i) = pure $ i + 1

printUsage :: IO ()
printUsage = putStrLn . concat $
  [ "Usage: prompt-search [OPTIONS]", "\n", "\n"
  , "OPTIONS:", "\n"
  , "-b", "\t", "Specify browser. A string value that must resolve to a binary in $PATH", "\n", "\t", "Default: \"firefox\""
  , "-f", "\t", "Specify history file. An absolute path to a history file.", "\n", "\t", "Default: \"~/.cache/browse\"", "\n"
  , "-s", "\t", "Specify search engine. Can be one of \"duckduckgo\" or \"google\".", "\n", "\t", "Default: \"duckduckgo\""
  ]
