{-# LANGUAGE RecordWildCards #-}

module CC.Analyse (
  analyse
) where

import Control.Applicative

import CC.Types as CC

import Control.Exception.Base (IOException, catch)
import Data.Monoid            ((<>))
import ShellCheck.Checker     (checkScript)
import ShellCheck.Interface   (CheckResult(..),
                               CheckSpec(..),
                               Comment(..),
                               ErrorMessage,
                               Position(..),
                               PositionedComment(..),
                               Severity(..),
                               SystemInterface(..))

-- | Main function that analyses shell files.
analyse :: FilePath -> IO [Issue]
analyse path = do
  contents <- readFile path
  result <- checkScript ioInterface (toCheckSpec path contents)
  return $ toIssues result
  where
    ioInterface = SystemInterface { siReadFile = defaultInterface }

-- | Lifts path and body into CheckSpec.
toCheckSpec :: FilePath -> String -> CheckSpec
toCheckSpec path contents = CheckSpec {
      csFilename = path
    , csScript = contents
    , csExcludedWarnings = []
    , csShellTypeOverride = Nothing
}

-- | Builds default IO interface with error handling.
defaultInterface :: FilePath -> IO (Either ErrorMessage String)
defaultInterface x = (Right <$> readFile x) `catch` handler
  where
    handler :: IOException -> IO (Either ErrorMessage String)
    handler ex = return . Left $ show ex

-- | Maps Severity to Category.
categorise :: Severity -> Category
categorise ErrorC   = BugRisk
categorise InfoC    = BugRisk
categorise StyleC   = Style
categorise WarningC = BugRisk

-- | Maps CheckResult into issues.
toIssues :: CheckResult -> [Issue]
toIssues CheckResult{..} = fmap toIssue crComments
  where
    toIssue :: PositionedComment -> Issue
    toIssue (PositionedComment Position{..} (Comment severity code description)) =
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
