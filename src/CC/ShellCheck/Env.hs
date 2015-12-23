{-# LANGUAGE OverloadedStrings #-}

module CC.ShellCheck.Env where

import           CC.ShellCheck.Types
import qualified Data.Map.Strict as DM
import           Data.Maybe
import qualified Data.Yaml as YML

--------------------------------------------------------------------------------

-- | Load environment that maps remediation points to textual content.
loadEnv :: IO Env
loadEnv = fromMaybe DM.empty <$> YML.decodeFile "./data/env.yml"
