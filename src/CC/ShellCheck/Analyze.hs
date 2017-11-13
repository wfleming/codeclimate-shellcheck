{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CC.ShellCheck.Analyze where

import           CC.ShellCheck.Fingerprint
import           CC.ShellCheck.Types as CC
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
  result <- checkScript interface $! checkSpec shellScript
  return $! fromCheckResult env result shellScript
  where
    checkSpec :: String -> CheckSpec
    checkSpec x = emptyCheckSpec { csFilename = path, csScript = x }

    interface :: SystemInterface IO
    interface = SystemInterface { siReadFile = defaultInterface }

--------------------------------------------------------------------------------

-- | Builds default IO interface with error handling.
defaultInterface :: FilePath -> IO (Either ErrorMessage String)
defaultInterface path = catch (Right <$> readFile path) handler
  where
    handler :: IOException -> IO (Either ErrorMessage String)
    handler ex = return . Left $! show ex

--------------------------------------------------------------------------------

-- | The baseline remediation points value is 50,000, which is the time it takes
-- to fix a trivial code style issue like a missing semicolon on a single line,
-- including the time for the developer to open the code, make the change, and
-- confidently commit the fix. All other remediation points values are expressed
-- in multiples of that Basic Remediation Point Value.
defaultRemediationPoints :: Int
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
fromCheckResult :: Env -> CheckResult -> String -> [Issue]
fromCheckResult env CheckResult{..} shellScript = fmap (fromPositionedComment env shellScript) crComments

--------------------------------------------------------------------------------

-- | Maps from a PositionedComment to an Issue.
fromPositionedComment :: Env -> String -> PositionedComment -> Issue
fromPositionedComment env shellScript p@(PositionedComment Position{..} _ (Comment severity code desc)) =
  Issue { _check_name         = checkName
        , _description        = description
        , _categories         = categories
        , _location           = location
        , _remediation_points = remediationPoints
        , _content            = content
        , _other_locations    = Nothing
        , _fingerprint        = issueFingerprint p $ T.pack shellScript
        }
  where
    checkName :: T.Text
    checkName = "SC" <> T.pack (show code)

    description :: T.Text
    description = T.pack desc

    categories :: [Category]
    categories = [fromSeverity severity]

    coords :: CC.Position
    coords = Coords (LineColumn (fromIntegral posLine) (fromIntegral posColumn))

    location :: Location
    location = Location posFile $! PositionBased coords coords

    mapping :: Maybe Mapping
    mapping = DM.lookup checkName env

    remediationPoints :: Maybe Int
    remediationPoints = case mapping of
      Just (Mapping x _) -> Just x
      Nothing            -> Just $! case severity of
        ErrorC   -> 4 * defaultRemediationPoints
        WarningC -> 3 * defaultRemediationPoints
        InfoC    -> 2 * defaultRemediationPoints
        StyleC   -> 1 * defaultRemediationPoints

    content :: Maybe Content
    content = fmap (\(Mapping _ x) -> x) mapping
