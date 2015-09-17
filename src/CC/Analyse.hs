{-# LANGUAGE RecordWildCards #-}

module CC.Analyse where

import CC.Types as CC
import Control.Applicative
import Control.Exception.Base (IOException, catch)
import Data.Monoid            ((<>))
import ShellCheck.Checker     (checkScript)
import ShellCheck.Interface   (CheckResult(..)
                             , CheckSpec(..)
                             , Comment(..)
                             , ErrorMessage
                             , Position(..)
                             , PositionedComment(..)
                             , Severity(..)
                             , SystemInterface(..)
                             , emptyCheckSpec)

analyseFiles :: [FilePath] -> IO [Issue]
analyseFiles = fmap concat . mapM analyse

-- | Main function that analyses a shell script.
analyse :: FilePath -> IO [Issue]
analyse x = do
  y <- readFile x
  z <- checkScript interface (emptyCheckSpec { csFilename = x, csScript = y })
  return $ transform z
  where
    interface :: SystemInterface IO
    interface = SystemInterface { siReadFile = defaultInterface }

-- | Maps Severity to Category.
categorise :: Severity -> Category
categorise ErrorC   = BugRisk
categorise InfoC    = BugRisk
categorise StyleC   = Style
categorise WarningC = BugRisk

-- | Builds default IO interface with error handling.
defaultInterface :: FilePath -> IO (Either ErrorMessage String)
defaultInterface x = catch (Right <$> readFile x) handler
  where
    handler :: IOException -> IO (Either ErrorMessage String)
    handler ex = return . Left $ show ex

-- | Maps CheckResult into issues.
transform :: CheckResult -> [Issue]
transform CheckResult{..} = fmap f crComments
  where
    f :: PositionedComment -> Issue
    f (PositionedComment Position{..} (Comment severity code description)) =
      Issue {
          _check_name         = "ShellCheck/" <> show code
        , _description        = description
        , _categories         = [categorise severity]
        , _location           = Location posFile (PositionBased coords coords)
        , _remediation_points = Nothing
        , _content            = Nothing
        , _other_locations    = Nothing
      }
      where
        coords :: CC.Position
        coords = Coords (LineColumn { _line = posLine, _column = posColumn })
