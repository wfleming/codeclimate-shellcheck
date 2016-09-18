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
import           Data.List
import           Data.Shebang (Shebang(..), Interpretter(..), Argument(..))
import qualified Data.Shebang as Shebang
import           System.Directory
import           System.FilePath.Glob
import           System.FilePath.Posix

--------------------------------------------------------------------------------

-- | Checks to see if file has correct extension.
hasShellExtension :: FilePath -> Bool
hasShellExtension path = takeExtension path == ".sh" || takeExtension path == ".bash" || takeExtension path == ".dash" || takeExtension path == ".ksh" || takeExtension path == ".ash"

--------------------------------------------------------------------------------

-- | Checks to see if script has a valid interpretter.
hasValidInterpretter :: Shebang -> Bool
hasValidInterpretter (Shebang (Interpretter int) maybeArgument) =
  if BS.isSuffixOf "env" int
    then case maybeArgument of
      Nothing             -> False
      Just (Argument arg) -> any (`BS.isPrefixOf` arg) shellScriptWhitelist
    else any (`BS.isSuffixOf` int) shellScriptWhitelist
  where
    shellScriptWhitelist :: [BS.ByteString]
    shellScriptWhitelist = ["sh", "ash", "dash", "bash", "ksh"]

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
