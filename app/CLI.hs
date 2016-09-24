module CLI where

import Data.Monoid
import Options.Applicative

--------------------------------------------------------------------------------

-- | Represents arguments that can be passed to the CLI.
data CLIOpts = CLIOpts { configPath :: Maybe FilePath
                       , envPath :: Maybe FilePath
                       }

--------------------------------------------------------------------------------

-- | Parses CLIOpts and adds description when generating help text.
cliOpts :: ParserInfo CLIOpts
cliOpts =
  info (helper <*> cliOptsParser)
    (fullDesc
     <> progDesc "Print ShellCheck results as CodeClimate engine JSON"
     <> header "codeclimate-shellcheck - codeclimate shellcheck engine")

--------------------------------------------------------------------------------

-- | Parses CLIOpts.
cliOptsParser :: Parser CLIOpts
cliOptsParser =
  CLIOpts <$> optional (strOption (long "config"
                                   <> help "Location of engine config"))
          <*> optional (strOption (long "env"
                                   <> help "Location of engine data mapping"))
