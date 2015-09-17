{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import CC.Analyse
import CC.Types
import Control.Applicative
import Data.Aeson           (encode)
import Data.Monoid          ((<>))
import System.Directory     (getCurrentDirectory)
import System.FilePath.Glob (compile, globDir)

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = shFiles >>= analyseFiles >>= mapM_ printIssue

printIssue :: Issue -> IO ()
printIssue = BL.putStr . (<> "\0") . encode

shFiles :: IO [FilePath]
shFiles = map clean . concat . fst <$> globDir [compile "**/*.sh"] "."
  where
    clean :: [Char] -> [Char]
    clean ('.' : '/' : x) = x
    clean x               = x
