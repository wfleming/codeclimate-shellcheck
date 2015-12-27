module Main where

import CC
import CC.ShellCheck.Analyze
import CC.ShellCheck.Env
import CC.ShellCheck.ShellScript
import CC.ShellCheck.Types
import Control.Concurrent
import Control.Monad

--------------------------------------------------------------------------------

main :: IO ()
main = do
  config  <- loadConfig
  env     <- loadEnv
  scripts <- findShellScripts $! _include_paths config
  chan0   <- newChan
  chan1   <- dupChan chan0
  _       <- forkIO (reporter chan0)
  _       <- forkIO (analyzer env scripts chan0)
  waitUntilReported chan1
  where
    waitUntilReported :: Chan Analysis -> IO ()
    waitUntilReported chan = do
      analysis <- readChan chan
      case analysis of
        Reported     -> return ()
        _            -> waitUntilReported chan

--------------------------------------------------------------------------------

-- | Represents three possible states of an analysis chan.
data Analysis = Result !Issue
              | Complete
              | Reported
              deriving Show

--------------------------------------------------------------------------------

-- | Takes a list of scripts, analyzes them and writes the results to a chan.
analyzer :: Env -> [FilePath] -> Chan Analysis -> IO ()
analyzer env paths chan = do
  forM_ paths $ \path -> do
    issues <- analyze env path
    mapM_ writeChanAndForce issues
  writeChan chan Complete
  where
    -- | To get brief locking and no space leaks, we need to use a trick.
    -- Source: Parallel and Concurrent Programming in Haskell
    --         MVar as a Building Block: Unbounded Channels, p135
    writeChanAndForce :: Issue -> IO ()
    writeChanAndForce result = do
      let cmd = Result result
      writeChan chan cmd
      seq cmd (return ())

--------------------------------------------------------------------------------

-- | Continually tries to report issue written to a chan.
reporter :: Chan Analysis -> IO ()
reporter chan = doUntilComplete
  where
    doUntilComplete :: IO ()
    doUntilComplete = do
      analysis <- readChan chan
      case analysis of
        Result issue -> printIssue issue >> doUntilComplete
        Complete     -> writeChan chan Reported
        Reported     -> return ()
