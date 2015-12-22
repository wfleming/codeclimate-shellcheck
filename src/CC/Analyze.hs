{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CC.Analyze where

import           CC.Types as CC
import           Control.Exception.Base
import qualified Data.Map.Strict as DM
import           Data.Monoid
import qualified Data.Text as T
import           ShellCheck.Checker
import           ShellCheck.Interface

--------------------------------------------------------------------------------

-- | Main function for analyzing a shell script.
analyze :: Env -> FilePath -> IO [Issue]
analyze env path = do
  shellScript <- readFile path
  result <- checkScript interface $ checkSpec path shellScript
  return $ fromCheckResult env result
  where
    checkSpec :: FilePath -> String -> CheckSpec
    checkSpec x y = emptyCheckSpec { csFilename = x, csScript = y }

    interface :: SystemInterface IO
    interface = SystemInterface { siReadFile = defaultInterface }

--------------------------------------------------------------------------------

-- | Builds default IO interface with error handling.
defaultInterface :: FilePath -> IO (Either ErrorMessage String)
defaultInterface path = catch (Right <$> readFile path) handler
  where
    handler :: IOException -> IO (Either ErrorMessage String)
    handler ex = return . Left $ show ex

--------------------------------------------------------------------------------

-- | The baseline remediation points value is 50,000, which is the time it takes
-- to fix a trivial code style issue like a missing semicolon on a single line,
-- including the time for the developer to open the code, make the change, and
-- confidently commit the fix. All other remediation points values are expressed
-- in multiples of that Basic Remediation Point Value.
defaultRemediationPoints :: Integer
defaultRemediationPoints = 50000

--------------------------------------------------------------------------------

-- | Maps Severity to Category.
fromSeverity :: Severity -> Category
fromSeverity ErrorC   = BugRisk
fromSeverity InfoC    = BugRisk
fromSeverity StyleC   = Style
fromSeverity WarningC = BugRisk

--------------------------------------------------------------------------------

-- | Maps CheckResult into issues.
fromCheckResult :: Env -> CheckResult -> [Issue]
fromCheckResult env CheckResult{..} = fmap (fromPositionedComment env) crComments

--------------------------------------------------------------------------------

-- | Maps from a PositionedComment to an Issue.
fromPositionedComment :: Env -> PositionedComment -> Issue
fromPositionedComment env (PositionedComment Position{..} (Comment severity code desc)) =
  Issue { _check_name         = checkName
        , _description        = description
        , _categories         = categories
        , _location           = location
        , _remediation_points = remediationPoints
        , _content            = content
        , _other_locations    = Nothing
        }
  where
    checkName :: T.Text
    checkName = "SC" <> T.pack (show code)

    description :: T.Text
    description = T.pack desc

    categories :: [Category]
    categories = [fromSeverity severity]

    coords :: CC.Position
    coords = Coords LineColumn { _line = posLine, _column = posColumn }

    location :: Location
    location = Location posFile $ PositionBased coords coords

    mapping :: Maybe Mapping
    mapping = DM.lookup checkName env

    remediationPoints :: Maybe Integer
    remediationPoints = case mapping of
      Just (Mapping x _) -> Just x
      Nothing            -> Just $! case severity of
        ErrorC   -> 4 * defaultRemediationPoints
        WarningC -> 3 * defaultRemediationPoints
        InfoC    -> 2 * defaultRemediationPoints
        StyleC   -> 1 * defaultRemediationPoints

    content :: Maybe Content
    content = fmap (\(Mapping _ x) -> x) mapping
