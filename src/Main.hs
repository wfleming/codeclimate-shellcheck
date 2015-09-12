module Main where

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

data Location = Lines FilePath BeginEnd
              | Positions FilePath Position

data Issue = Issue {
    _type               :: String
  , _check_name         :: String
  , _description        :: String
  , _categories         :: [Category]
  , _location           :: Location
  , _remediation_points :: Maybe Int
  , _content            :: Maybe String
  , _other_locations    :: Maybe [Location]
}

main :: IO ()
main = putStrLn "hey"
