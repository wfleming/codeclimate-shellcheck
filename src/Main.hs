{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CC.Analyze
import           CC.Types
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as DM
import           Data.Maybe
import           Data.Monoid
import qualified Data.Yaml as YML
import           System.Directory
import           System.FilePath.Glob

--------------------------------------------------------------------------------

main :: IO ()
main = do
  config <- loadConfig "/config.json"
  env <- fromMaybe DM.empty <$> YML.decodeFile "data/mapping.yml"
  paths <- shFiles $! _include_paths config
  issues <- analyzeFiles env paths
  mapM_ printIssue issues

--------------------------------------------------------------------------------

loadConfig :: FilePath -> IO Config
loadConfig path = do
    fileExists <- doesFileExist path
    config <- if fileExists then decode <$> BSL.readFile path else return Nothing
    return $! fromMaybe Config { _include_paths = ["."] } config

--------------------------------------------------------------------------------

printIssue :: Issue -> IO ()
printIssue = BSL.putStr . (<> "\0") . encode

--------------------------------------------------------------------------------

shFiles :: [FilePath] -> IO [FilePath]
shFiles paths =
  fmap concat $! sequence $! fmap (matched . globDir [compile "**/*.sh"]) paths
  where
    matched :: Functor f => f ([[a]], [b]) -> f [a]
    matched x = (concat . fst) <$> x
