module Main where

-- Types.

data Location
data Content

data Issue = Issue {
    _type               :: String
  , _check_name         :: String
  , _description        :: String
  , _categories         :: [String]
  , _location           :: Location
  , _remediation_points :: Int
  , _content            :: Content
  , _other_locations    :: [Location]
}

main :: IO ()
main = putStrLn "hey"
