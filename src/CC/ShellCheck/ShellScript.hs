{-# LANGUAGE OverloadedStrings #-}

module CC.ShellCheck.ShellScript (
  -- * Validation
    isShellScript
  , hasShellExtension
  , hasValidInterpretter
  -- * Retrieval
  , findShellScripts
) where

import           Control.Monad.Extra
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import           Data.List
import           Data.Monoid
import           Data.Shebang (Shebang(..), Interpretter(..), Argument(..))
import qualified Data.Shebang as Shebang
import           System.Directory
import           System.FilePath.Glob
import           System.FilePath.Posix

--------------------------------------------------------------------------------

-- | List of shells the engine should be able to handle.
validShells :: [BS.ByteString]
validShells = ["sh", "ash", "dash", "bash", "ksh"]

--------------------------------------------------------------------------------

-- | List of valid shell file extensions.
validShellExtensions :: [BS.ByteString]
validShellExtensions = ("." <>) <$> validShells

--------------------------------------------------------------------------------

-- | Checks to see if file has correct extension.
hasShellExtension :: FilePath -> Bool
hasShellExtension path = extension `elem` validShellExtensions
  where
    extension :: BS.ByteString
    extension = Char8.pack $ takeExtension path

--------------------------------------------------------------------------------

-- | Checks to see if script has a valid interpretter.
hasValidInterpretter :: Shebang -> Bool
hasValidInterpretter (Shebang (Interpretter int) maybeArgument) =
  if BS.isSuffixOf "env" int
    then case maybeArgument of
      Nothing             -> False
      Just (Argument arg) -> any (`BS.isPrefixOf` arg) validShells
    else any (`BS.isSuffixOf` int) validShells

--------------------------------------------------------------------------------

-- | Determines whether a file is a valid shell script.
isShellScript :: FilePath -> IO Bool
isShellScript path =
  if hasExtension path
    then return $! hasShellExtension path
    else do
      header <- Shebang.readFirstLine path
      case Shebang.decode header of
        Just shebang -> return $! hasValidInterpretter shebang
        Nothing      -> return False

--------------------------------------------------------------------------------

-- | Retrieve shell scripts for a list of paths.
findShellScripts :: [FilePath] -> IO [FilePath]
findShellScripts paths = do
  dotShFiles <- concat . fst <$> globDir patterns "."
  allScripts <- filterM validateScript $! dotShFiles ++ otherFiles
  return $ fmap clean allScripts
  where
    dirs :: [FilePath]; otherFiles :: [FilePath]
    (dirs, otherFiles) = partition hasTrailingPathSeparator paths

    clean :: String -> String
    clean ('.':'/':x) = x
    clean x = x

    patterns :: [Pattern]
    patterns = fmap (compile . (++ "**/*")) dirs

    validateScript :: FilePath -> IO Bool
    validateScript x = doesFileExist x &&^ isShellScript x
