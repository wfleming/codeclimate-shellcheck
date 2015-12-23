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
import           Data.Shebang (Shebang(..))
import qualified Data.Shebang as Shebang
import           System.Directory
import           System.FilePath.Glob
import           System.FilePath.Posix

--------------------------------------------------------------------------------

-- | Checks to see if file has correct extension.
hasShellExtension :: FilePath -> Bool
hasShellExtension path = takeExtension path == ".sh"

--------------------------------------------------------------------------------

-- | Checks to see if script has a valid interpretter.
hasValidInterpretter :: Shebang -> Bool
hasValidInterpretter (Shebang interpretter arguments) =
  if BS.isSuffixOf "env" interpretter
    then any (`BS.isPrefixOf` arguments) shellScriptWhitelist
    else any (`BS.isSuffixOf` interpretter) shellScriptWhitelist
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
    patterns = fmap (compile . (++ "**/*.sh")) dirs

    validateScript :: FilePath -> IO Bool
    validateScript x = doesFileExist x &&^ isShellScript x
