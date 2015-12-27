{-# LANGUAGE OverloadedStrings #-}

module CC.ShellCheck.Env where

import           CC.ShellCheck.Types
import qualified Data.Map.Strict as DM
import           Data.Maybe
import qualified Data.Yaml as YML
import           System.Directory

--------------------------------------------------------------------------------

-- | Load environment that maps remediation points to textual content.
loadEnv :: FilePath -> IO Env
loadEnv path = do
  fileExists <- doesFileExist path
  config <- if fileExists
              then YML.decodeFile path
              else return Nothing
  return $! fromMaybe DM.empty config
