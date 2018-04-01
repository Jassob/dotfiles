#! env nix-shell
#! nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (pkgs: [])" rofi

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Default
import           Data.Maybe (maybe)
import           System.Environment (getArgs)

import           Lib

data Options = Opts
  { browser :: String
  , file    :: FilePath
  }

instance Default Options where
  def = Opts { browser = "firefox"
             , file = "$HOME/.cache/browse" }

main :: IO ()
main = getArgs >>= bootstrap

bootstrap :: [String] -> IO ()
bootstrap args = maybe printUsage wrapper (handleArgs args def)
  where wrapper :: Options -> IO ()
        wrapper Opts {browser = b, file = f} = run f b schemized "Open URL: http://"

        schemized :: String -> String
        schemized url@('h':'t':'t':'p':_) = url
        schemized url                     = "http://" ++ url


handleArgs :: [String] -> Options -> Maybe Options
handleArgs ("-f":file:args)    os = handleArgs args $ os { file = file }
handleArgs ("-b":browser:args) os = handleArgs args $ os { browser = browser }
handleArgs []                  os = pure os
handleArgs _                   _  = Nothing

printUsage :: IO ()
printUsage = putStrLn . concat $
  [ "Usage: prompt-open-url [OPTIONS]", "\n", "\n"
  , "OPTIONS:", "\n"
  , "-b", "\t", "Specify browser. A string value that must resolve to a binary in $PATH", "\n", "\t", "Default: \"firefox\"", "\n"
  , "-f", "\t", "Specify history file. An absolute path to a history file.", "\n", "\t", "Default: \"$HOME/.cache/browse\""
  ]
