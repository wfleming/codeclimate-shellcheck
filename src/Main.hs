{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import CC.Analyse
import CC.Types
import Control.Applicative
import Data.Aeson           (FromJSON, decode, encode)
import Data.Maybe           (fromMaybe)
import Data.Monoid          ((<>))
import System.Directory     (doesFileExist, getCurrentDirectory)
import System.FilePath.Glob (compile, globDir)

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  x <- loadConfig "/config.json"
  y <- shFiles
  z <- analyseFiles ((_include_paths x) ++ y)
  mapM_ printIssue z

loadConfig :: FilePath -> IO Config
loadConfig x = do
    y <- doesFileExist x
    z <- if y then decode <$> BL.readFile x else return Nothing
    return $ fromMaybe (Config []) z

printIssue :: Issue -> IO ()
printIssue = BL.putStr . (<> "\0") . encode

shFiles :: IO [FilePath]
shFiles = fmap clean . concat . fst <$> globDir [compile "**/*.sh"] "."
  where
    clean :: [Char] -> [Char]
    clean ('.' : '/' : x) = x
    clean x               = x
