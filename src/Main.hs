module Main where

data Category = BugRisk
              | Clarity
              | Compatibility
              | Complexity
              | Duplication
              | Security
              | Style

data LineColumn = LineColumn {
    _line   :: Int
  , _column :: Int
}

data Position = Coord LineColumn | Offset Int

data BeginEnd = BeginEnd {
    _begin :: Int
  , _end   :: Int
}

data Location = Location {
      _path      :: FilePath
    , _positions :: Maybe Position
    , _lines     :: Maybe BeginEnd
}

data Content

data Issue = Issue {
    _type               :: String
  , _check_name         :: String
  , _description        :: String
  , _categories         :: [Category]
  , _location           :: Location
  , _remediation_points :: Maybe Int
  , _content            :: Maybe Content
  , _other_locations    :: Maybe [Location]
}

main :: IO ()
main = putStrLn "hey"
