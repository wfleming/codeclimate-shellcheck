module Main where

import CC
import CC.ShellCheck.Analyze
import CC.ShellCheck.Env
import CC.ShellCheck.ShellScript

--------------------------------------------------------------------------------

main :: IO ()
main = do
  config  <- loadConfig
  env     <- loadEnv
  scripts <- findShellScripts $! _include_paths config
  analyzeScripts env scripts >>= mapM_ printIssue
