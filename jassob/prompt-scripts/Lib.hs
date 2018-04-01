-- | Shared code for prompt scripts

module Lib where

import           Control.Exception (handleJust)
import           Control.Monad (guard, unless)
import           Data.Function (on)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.List (elemIndex, sortBy)
import           System.Directory (getCurrentDirectory)
import           System.Environment (getEnv)
import           System.IO.Error (isDoesNotExistError)
import           System.Process (readProcessWithExitCode, spawnProcess)

run :: FilePath -> String -> (String -> String) -> String -> IO ()
run fp browser toOption prompt = do
  contentMap <- contentOrEmpty fp
  (_,str,_)  <- readProcess_ "rofi" rofiOptions . unlines . getItems $ contentMap
  unless (null str) $ do
    _ <- spawnProcess browser [toOption str]
    updateContentStore fp (init str) contentMap

    where rofiOptions :: [String]
          rofiOptions = ["-dmenu", "-i", "-p", prompt]

          readProcess_ = readProcessWithExitCode

absolutize :: FilePath -> IO FilePath
absolutize fp
  | head fp == '/' = pure fp
  | head fp == '$' = case elemIndex '/' fp of
      Just idx -> (++ drop idx fp) <$> getEnv (tail . take idx $ fp)
      Nothing  -> getEnv (tail fp)
  | otherwise = (++ fp) <$> getCurrentDirectory

contentOrEmpty :: FilePath -> IO (Map String Int)
contentOrEmpty fp = handleNotExist handler $ read <$> (readFile =<< absolutize fp)

  where handleNotExist :: (() -> IO a) -> IO a -> IO a
        handleNotExist = handleJust (guard . isDoesNotExistError)

        handler :: () -> IO (Map String Int)
        handler () = absolutize fp >>=
          \ fp' -> writeFile fp' (show emptyMap) >> return emptyMap

        emptyMap :: Map String Int
        emptyMap = M.empty

getItems :: Map String Int -> [String]
getItems = map fst . sortBy (flip compare `on` snd) . M.toList

updateContentStore :: FilePath -> String -> Map String Int -> IO ()
updateContentStore fp url map = absolutize fp >>=
  flip writeFile (show . M.alter act url $ map)
  where act :: Maybe Int -> Maybe Int
        act Nothing  = pure 1
        act (Just i) = pure $ i + 1
