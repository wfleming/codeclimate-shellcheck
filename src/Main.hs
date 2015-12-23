{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CC.Analyze
import           CC.Types
import           Control.Monad
import           Control.Monad.Extra
import           Data.Aeson
import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List
import qualified Data.Map.Strict as DM
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import qualified Data.Yaml as YML
import           System.Directory
import           System.FilePath.Glob
import           System.FilePath.Posix

--------------------------------------------------------------------------------

main :: IO ()
main = do
  config <- loadConfig "/config.json"
  env <- fromMaybe DM.empty <$> YML.decodeFile "data/mapping.yml"
  paths <- shellScripts $! _include_paths config
  issues <- analyzeFiles env paths
  mapM_ printIssue issues

--------------------------------------------------------------------------------

loadConfig :: FilePath -> IO Config
loadConfig path = do
    fileExists <- doesFileExist path
    config <- if fileExists then decode <$> BSL.readFile path else return Nothing
    return $! fromMaybe Config { _include_paths = ["./"] } config

--------------------------------------------------------------------------------

printIssue :: Issue -> IO ()
printIssue = BSL.putStr . (<> "\0") . encode

--------------------------------------------------------------------------------

shellScripts :: [FilePath] -> IO [FilePath]
shellScripts paths = do
  dotShFiles <- concat . fst <$> globDir patterns "."
  otherFiles <- return files
  allScripts <- filterM validateScript $ dotShFiles ++ otherFiles
  return $ fmap clean allScripts
  where
    (dirs, files) = partition hasTrailingPathSeparator paths

    clean :: String -> String
    clean ('.' : '/' : x) = x
    clean x               = x

    patterns :: [Pattern]
    patterns = fmap (compile . (++ "**/*.sh")) dirs

    validateScript :: FilePath -> IO Bool
    validateScript x = doesFileExist x &&^ isShellScript x

--------------------------------------------------------------------------------

-- | Determines whether a file is a shell script that we can work with.
isShellScript :: FilePath -> IO Bool
isShellScript path =
  if hasExtension path
    then return hasShellExtension
    else do
      header <- readHeader
      if hasShebang header
        then case readShebang header of
          Just (Shebang x y) -> return $ hasValidInterpretter x y
          Nothing            -> return False
        else return False
  where
    ----------------------------------------------------------------------------

    carriageReturn :: Word8 -> Bool
    carriageReturn = (== 13)

    endOfLine :: Word8 -> Bool
    endOfLine x = newline x || carriageReturn x

    newline :: Word8 -> Bool
    newline = (== 10)

    whitespace :: Word8 -> Bool
    whitespace = (== 32)

    ----------------------------------------------------------------------------

    whiteList :: [BS.ByteString]
    whiteList = [ "sh"
                , "ash"
                , "dash"
                , "bash"
                , "ksh"
                ]

    ----------------------------------------------------------------------------

    hasShebang :: BSL.ByteString -> Bool
    hasShebang x = BSL.take 2 x == "#!"

    hasShellExtension :: Bool
    hasShellExtension = takeExtension path == ".sh"

    hasValidInterpretter :: BS.ByteString -> BS.ByteString -> Bool
    hasValidInterpretter interpretter arguments =
      if BS.isSuffixOf "env" interpretter
        then any (`BS.isPrefixOf` arguments) whiteList
        else any (`BS.isSuffixOf` interpretter) whiteList

    ----------------------------------------------------------------------------

    readHeader :: IO BSL.ByteString
    readHeader = do
      contents <- BSL.readFile path
      return $ BSL.takeWhile (not . endOfLine) contents

    readShebang :: BSL.ByteString -> Maybe Shebang
    readShebang x = maybeResult $ parse shebang x

    ----------------------------------------------------------------------------

    shebang :: Parser Shebang
    shebang = do
      _            <- string "#!"
      interpretter <- takeTill whitespace
      arguments    <- option "" $ do
        _          <- string " "
        takeTill endOfLine
      return $ Shebang interpretter arguments
