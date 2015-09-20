{-# LANGUAGE OverloadedStrings #-}

module Main where

import CC.Analyze
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
  z <- analyzeFiles y
  mapM_ printIssue z

loadConfig :: FilePath -> IO Config
loadConfig x = do
    y <- doesFileExist x
    z <- if y then decode <$> BL.readFile x else return Nothing
    return $ fromMaybe Config { _include_paths = ["."] } z

printIssue :: Issue -> IO ()
printIssue = BL.putStr . (<> "\0") . encode

shFiles :: [FilePath] -> IO [FilePath]
shFiles x =
  fmap concat $ sequence $ fmap (f . globDir [compile "**/*.sh"]) x
  where
    f :: Functor m => m ([[a]], [b]) -> m [a]
    f x = (concat . fst) <$> x
