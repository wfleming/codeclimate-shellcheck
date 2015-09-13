{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module CC.Types where

import Data.Aeson       (ToJSON(..), (.=), genericToJSON, object)
import Data.Aeson.Types (Pair, Value(Null), defaultOptions, fieldLabelModifier)
import Data.Char        (isUpper, toLower)
import GHC.Generics     (Generic)
import Data.Text        (pack)

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
  toJSON BugRisk = toJSON "Bug Risk"
  toJSON x       = toJSON $ show x

-- | Line and column numbers are 1-based.
data LineColumn = LineColumn {
    _line   :: Int
  , _column :: Int
} deriving (Generic, Show)

instance ToJSON LineColumn where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

-- | Positions refer to specific characters within a source file, and can be
-- expressed in two ways.
data Position = Coords LineColumn
                -- ^ Line and column coordinates.
              | Offset Int
                -- ^ Absolute character offsets, for the entire source buffer.
              deriving Show

instance ToJSON Position where
  toJSON (Coords x) = toJSON x
  toJSON (Offset x) = toJSON $ object [ (pack "offset") .= x ]

-- | Line-based locations emit a beginning and end line number for the issue,
-- whereas position-based locations allow more precision.
data BeginEnd = PositionBased Position Position
              | LineBased Int Int
              deriving Show

instance ToJSON BeginEnd where
  toJSON z = object $ case z of
    PositionBased x y -> f x y
    LineBased     x y -> f x y
    where
      f :: (ToJSON a) => a -> a -> [Pair]
      f x y = [ (pack "begin") .= x, (pack "end") .= y ]

-- | Locations refer to ranges of a source code file.
data Location = Location FilePath BeginEnd deriving Show

instance ToJSON Location where
  toJSON (Location x y) = object $ case y of
    PositionBased _ _ -> [ f x, (pack "positions") .= y ]
    LineBased _ _     -> [ f x, (pack "lines") .= y ]
    where
      f :: FilePath -> Pair
      f = (.=) (pack "path")

-- | An issue represents a single instance of a real or potential code problem,
-- detected by a static analysis Engine.
data Issue = Issue {
    _check_name         :: String
  , _description        :: String
  , _categories         :: [Category]
  , _location           :: Location
  , _remediation_points :: Maybe Int
  , _content            :: Maybe String
  , _other_locations    :: Maybe [Location]
} deriving Show

instance ToJSON Issue where
  toJSON Issue{..} = object . withoutNulls $ [
        pack "type"               .= ("issue" :: String)
      , pack "check_name"         .= _check_name
      , pack "description"        .= _description
      , pack "categories"         .= _categories
      , pack "location"           .= _location
      , pack "remediation_points" .= _remediation_points
      , pack "content"            .= _content
      , pack "other_locations"    .= _other_locations
    ]
    where
      withoutNulls :: [(a, Value)] -> [(a, Value)]
      withoutNulls = filter (\(_, v) -> v /= Null)
