{-# LANGUAGE OverloadedStrings #-}

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
  y <- shFiles (_include_paths x)
  z <- analyseFiles y
  mapM_ printIssue z

loadConfig :: FilePath -> IO Config
loadConfig x = do
    y <- doesFileExist x
    z <- if y then decode <$> BL.readFile x else return Nothing
    return $ fromMaybe Config { _include_paths = [] } z

printIssue :: Issue -> IO ()
printIssue = BL.putStr . (<> "\0") . encode

shFiles :: [FilePath] -> IO [FilePath]
shFiles x =
  fmap concat $ sequence $ fmap (f . globDir [compile "**/*.sh"]) x
  where
    f :: IO ([[FilePath]], [FilePath]) -> IO [FilePath]
    f x = (concat . fst) <$> x
