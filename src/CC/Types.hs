{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CC.Types where

import Data.Aeson       (ToJSON(..), (.=), genericToJSON, object)
import Data.Aeson.Types (Pair, Value(Null), defaultOptions, fieldLabelModifier)
import Data.Char        (isUpper, toLower)
import GHC.Generics     (Generic)

data BeginEnd = Positions Position Position
              | Lines Int Int
              deriving Show

instance ToJSON BeginEnd where
  toJSON x = object $ case x of
    Positions begin end -> f begin end
    Lines     begin end -> f begin end
    where
      f :: (ToJSON a) => a -> a -> [Pair]
      f x y = [ "begin" .= x, "end" .= y ]

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
  toJSON x       = toJSON $ show x

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
        "type"               .= ("issue" :: String)
      , "check_name"         .= _check_name
      , "description"        .= _description
      , "categories"         .= _categories
      , "location"           .= _location
      , "remediation_points" .= _remediation_points
      , "content"            .= _content
      , "other_locations"    .= _other_locations
    ]
    where
      withoutNulls :: [(a, Value)] -> [(a, Value)]
      withoutNulls = filter (\(_, v) -> v /= Null)

data LineColumn = LineColumn {
    _line   :: Int
  , _column :: Int
} deriving (Generic, Show)

instance ToJSON LineColumn where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

data Location = Location FilePath BeginEnd deriving Show

instance ToJSON Location where
  toJSON (Location x y) = object $ case y of
    Positions _ _ -> [ f x, "positions" .= y ]
    Lines _ _     -> [ f x, "lines" .= y ]
    where
      f :: FilePath -> Pair
      f = (.=) "path"

data Position = Coords LineColumn
              | Offset Int
              deriving Show

instance ToJSON Position where
  toJSON (Coords x) = toJSON x
  toJSON (Offset x) = object [ "offset" .= x ]
