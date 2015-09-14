{-# LANGUAGE RecordWildCards #-}

module Main where

import CC.Analyse       (analyse)
import Data.Monoid      ((<>))
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  issues <- analyse $ currentDir <> "/test/example.sh"
  putStrLn (show issues)
