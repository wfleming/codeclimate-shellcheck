{-# LANGUAGE RecordWildCards #-}

module CC.Analyse where

import CC.Types as CC

import Data.Monoid          ((<>))
import ShellCheck.Interface ( CheckResult(..)
                            , Comment(..)
                            , Position(..)
                            , PositionedComment(..)
                            , Severity(..) )

toCategory :: Severity -> Category
toCategory ErrorC   = BugRisk
toCategory InfoC    = BugRisk
toCategory StyleC   = Style
toCategory WarningC = BugRisk

toIssue :: PositionedComment -> Issue
toIssue (PositionedComment Position{..} (Comment severity code description)) =
  Issue {
      _check_name         = "ShellCheck/" <> show code
    , _description        = description
    , _categories         = [toCategory severity]
    , _location           = Location posFile (PositionBased coords coords)
    , _remediation_points = Nothing
    , _content            = Nothing
    , _other_locations    = Nothing
  }
  where
    coords :: CC.Position
    coords = Coords (LineColumn { _line = posLine, _column = posColumn })

toIssues :: CheckResult -> [Issue]
toIssues CheckResult{..} = fmap toIssue crComments
