{-# LANGUAGE OverloadedStrings #-}

module CC.ShellCheck.Env where

import           CC.ShellCheck.Types
import qualified Data.Map.Strict as DM
import           Data.Maybe
import qualified Data.Yaml as YML
import           System.Directory

--------------------------------------------------------------------------------

-- | Load environment that maps remediation points to textual content.
loadEnv :: IO Env
loadEnv = do
  path <- getCurrentDirectory >>= \cwd -> return (cwd ++ "/data/env.yml")
  fromMaybe DM.empty <$> YML.decodeFile path
