{-# LANGUAGE OverloadedStrings #-}

module CC.ShellCheck.Types where

import           CC.Types
import           Control.Applicative
import           Data.Aeson
import qualified Data.Map.Strict as DM
import qualified Data.Text as T

--------------------------------------------------------------------------------

-- | Remediation points and associated textual content.
data Mapping = Mapping Int Content deriving Show

instance FromJSON Mapping where
  parseJSON (Object x) = do
    points  <- x .: "remediation_points"
    content <- x .: "content"
    return $! Mapping points content
  parseJSON _          = empty

--------------------------------------------------------------------------------

-- | Represents mappings between check names, content and remediation points.
type Env = DM.Map T.Text Mapping
