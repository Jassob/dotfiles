#! env nix-shell
#! nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (pkgs: [])" rofi

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Default
import           Data.Maybe (maybe)
import           System.Environment (getArgs)
import           Text.Read (readMaybe)

import           Lib (run)

data Options = Opts
  { browser :: FilePath
  , file    :: FilePath
  , engine  :: SearchEngine
  } deriving Show

instance Default Options where
  def = Opts { browser = "firefox"
             , file    = "$HOME/.cache/search"
             , engine  = DUCKDUCKGO
             }

data SearchEngine = DUCKDUCKGO | GOOGLE | HACKAGE | HOOGLE

instance Show SearchEngine where
  show DUCKDUCKGO = "duckduckgo"
  show GOOGLE     = "google"
  show HACKAGE    = "hackage"
  show HOOGLE     = "hoogle"

instance Read SearchEngine where
  readsPrec _ "duckduckgo" = [(DUCKDUCKGO, "")]
  readsPrec _ "google"     = [(GOOGLE, "")]
  readsPrec _ "hackage"    = [(HACKAGE, "")]
  readsPrec _ "hoogle"     = [(HOOGLE, "")]

queryToUrl :: SearchEngine -> String -> String
queryToUrl engine query = case engine of
  DUCKDUCKGO -> "https://duckduckgo.com/?q=" ++ format query
  GOOGLE     -> "https://google.com/search?q=" ++ format query
  HOOGLE     -> "https://hoogle.haskell.org/?hoogle=" ++ format query
  HACKAGE    -> "https://hackage.haskell.org/packages/search?terms=" ++ format query
  where format = id

main :: IO ()
main = getArgs >>= bootstrap

bootstrap :: [String] -> IO ()
bootstrap args = maybe printUsage wrapper (handleArgs args def)
  where wrapper :: Options -> IO ()
        wrapper Opts{browser = b, file = f, engine = e} =
          run (engineToFile e f) b (queryToUrl e) $ "Search [" ++ show e ++ "]: "

        engineToFile :: SearchEngine -> FilePath -> FilePath
        engineToFile se f = f ++ "." ++ show se

handleArgs :: [String] -> Options -> Maybe Options
handleArgs ("-f":file:args)    os = handleArgs args $ os { file = file }
handleArgs ("-b":browser:args) os = handleArgs args $ os { browser = browser }
handleArgs ("-s":engine:args)  os = readMaybe engine >>=
  \e -> handleArgs args $ os { engine = e }
handleArgs []                  os = pure os
handleArgs _                   _  = Nothing

printUsage :: IO ()
printUsage = putStrLn . concat $
  [ "Usage: prompt-search [OPTIONS]", "\n", "\n"
  , "OPTIONS:", "\n"
  , "-b", "\t", "specify browser. A string value that must resolve to a binary in $PATH", "\n", "\t", "Default: \"firefox\"", "\n"
  , "-f", "\t", "Specify history file. An absolute path to a history file.", "\n", "\t", "Default: \"~/.cache/browse\"", "\n"
  , "-s", "\t", "Specify search engine. Can be one of \"duckduckgo\" or \"google\".", "\n", "\t", "Default: \"duckduckgo\""
  ]
