{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CC.Types where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics

--------------------------------------------------------------------------------

-- | Issues must be associated with one or more categories.
data Category = BugRisk
              | Clarity
              | Compatibility
              | Complexity
              | Duplication
              | Security
              | Style
              deriving Show

instance ToJSON Category where
  toJSON BugRisk = "Bug Risk"
  toJSON x       = toJSON $! show x

--------------------------------------------------------------------------------

-- | Line and column numbers are 1-based.
data LineColumn = LineColumn {
    _line   :: {-# UNPACK #-} !Int
  , _column :: {-# UNPACK #-} !Int
} deriving (Generic, Show)

instance ToJSON LineColumn where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

--------------------------------------------------------------------------------

-- | Positions refer to specific characters within a source file, and can be
-- expressed in two ways.
data Position = Coords !LineColumn
                -- ^ Line and column coordinates.
              | Offset {-# UNPACK #-} !Int
                -- ^ Absolute character offsets, for the entire source buffer.
              deriving Show

instance ToJSON Position where
  toJSON (Coords x) = toJSON x
  toJSON (Offset x) = object [ "offset" .= x ]

--------------------------------------------------------------------------------

-- | Line-based locations emit a beginning and end line number for the issue,
-- whereas position-based locations allow more precision.
data BeginEnd = PositionBased Position Position
              | LineBased {-# UNPACK #-} !Int {-# UNPACK #-} !Int
              deriving Show

instance ToJSON BeginEnd where
  toJSON z = object $! case z of
    PositionBased x y -> f x y
    LineBased     x y -> f x y
    where
      f :: (ToJSON a) => a -> a -> [Pair]
      f x y = [ "begin" .= x, "end" .= y ]

--------------------------------------------------------------------------------

-- | Locations refer to ranges of a source code file.
data Location = Location FilePath BeginEnd deriving Show

instance ToJSON Location where
  toJSON (Location x y) = object $! case y of
    PositionBased _ _ -> [ f x, "positions" .= y ]
    LineBased _ _     -> [ f x, "lines" .= y ]
    where
      f :: FilePath -> Pair
      f p = "path" .= stripPrefix "./" (T.pack p)

      -- A version that doesn't return Maybe but gives back the original value
      -- if the prefix wasn't present.
      stripPrefix :: Text -> Text -> Text
      stripPrefix p fp = fromMaybe fp $ T.stripPrefix p fp

--------------------------------------------------------------------------------

-- | A markdown snippet describing the issue, including deeper explanations and
-- links to other resources.
data Content = Body T.Text deriving Show

instance ToJSON Content where
  toJSON (Body x) = object [ "body" .=  x ]

instance FromJSON Content where
   parseJSON (Object x) = Body <$> x .: "body"
   parseJSON _          = empty

--------------------------------------------------------------------------------

-- | An issue represents a single instance of a real or potential code problem,
-- detected by a static analysis Engine.
data Issue = Issue {
    _check_name         :: !T.Text
  , _description        :: !T.Text
  , _categories         :: ![Category]
  , _location           :: !Location
  , _remediation_points :: !(Maybe Int)
  , _content            :: !(Maybe Content)
  , _other_locations    :: !(Maybe [Location])
  , _fingerprint        :: !T.Text
} deriving Show

instance ToJSON Issue where
  toJSON Issue{..} = (object . withoutNulls) [
        "type"               .= ("issue" :: T.Text)
      , "check_name"         .= _check_name
      , "description"        .= _description
      , "categories"         .= _categories
      , "location"           .= _location
      , "remediation_points" .= _remediation_points
      , "content"            .= _content
      , "other_locations"    .= _other_locations
      , "fingerprint"        .= _fingerprint
    ]
    where
      withoutNulls :: [(a, Value)] -> [(a, Value)]
      withoutNulls = filter (\(_, v) -> v /= Null)

--------------------------------------------------------------------------------

-- | Engine configuration mounted at /config.json.
data Config = Config { _include_paths :: ![FilePath] } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
