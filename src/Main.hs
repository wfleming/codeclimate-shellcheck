module Main where

-- Types.

data Category = BugRisk
              | Clarity
              | Compatibility
              | Complexity
              | Duplication
              | Security
              | Style

data BeginEnd = BeginEnd {
    _begin :: Int
  , _end   :: Int
}

data LineColumn = LineColumn {
    _line   :: Int
  , _column :: Int
}

data Position = Coord LineColumn | Offset Int

data Location = Location {
      _path  :: FilePath
    , _positions :: Maybe Position
    , _lines :: Maybe BeginEnd
}

data Content

data Issue = Issue {
    _type               :: String
  , _check_name         :: String
  , _description        :: String
  , _categories         :: [Category]
  , _location           :: Location
  , _remediation_points :: Int
  , _content            :: Content
  , _other_locations    :: [Location]
}

main :: IO ()
main = putStrLn "hey"
