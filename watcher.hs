{-# Language OverloadedStrings, DeriveDataTypeable #-}

import System.Posix.Files
import System.Directory (canonicalizePath)
import Filesystem.Path (directory)
import Data.String (fromString)
import System.FSNotify
import System.Exit (exitSuccess)
import System.Process
import Control.Monad (void)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent.MVar
import System.Console.CmdArgs

data Watcher = Watcher { path :: FilePath , command :: String }
  deriving (Data,Typeable,Show)

data FileType = File | Directory deriving Eq

runCmd :: String -> Event -> IO ()
runCmd cmd _ = do
  putStrLn $ "Running " ++ cmd
  void (createProcess (proc cmd []))

watcher = cmdArgsMode $ Watcher
  { path    = ""   &= argPos 0 &= typ "<file/directory to watch>"
  , command = ""   &= argPos 1 &= typ "<command to run>"}
  &= summary "watcher: run a command when a file/directory is modified"


watch :: FileType -> WatchManager -> String -> String -> IO ()
watch Directory m path cmd  = watchTree m (fromString path) (const True) (runCmd cmd)
watch File m path cmd       = watchDir m (directory $ fromString path) isThisFile (runCmd cmd)
  where isThisFile (Modified p _) = p == fromString path
        isThisFile _              = False


main :: IO ()
main = do
  args <- cmdArgsRun watcher

  m <- startManager

  -- Create an empty MVar and install INT/TERM handlers that will fill it,
  -- letting us wait for one of these signals before cleaning up and exiting.
  interrupted <- newEmptyMVar
  installHandler sigINT  (Catch $ putMVar interrupted True) Nothing
  installHandler sigTERM (Catch $ putMVar interrupted True) Nothing

  canonicalPath <- canonicalizePath (path args)
  --
  -- check if path is a file or directory
  s <- getFileStatus canonicalPath
  let filetype = if isDirectory s then Directory else File

  watch filetype m canonicalPath (command args)
  putStr $ "Started to watch " ++ path args
  putStrLn $ if canonicalPath == path args then "" else " [→ " ++ canonicalPath ++ "]"

  _ <- readMVar interrupted
  putStrLn "\nStopping."
  stopManager m
  exitSuccess
