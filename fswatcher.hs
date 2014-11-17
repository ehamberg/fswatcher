import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFileStatus, isDirectory)
import System.Environment (getArgs, getProgName)
import System.Directory (canonicalizePath, getCurrentDirectory)
import Filesystem.Path ((</>), directory)
import Data.String (fromString)
import System.FSNotify (Event (..), StopListening, WatchManager, startManager,
       stopManager, watchTree, watchDir)
import System.Exit (ExitCode (..), exitSuccess, exitFailure)
import System.Process (createProcess, proc, waitForProcess)
import Control.Monad (void, when)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar

data FileType = File | Directory deriving Eq

-- Watches a file or directory and whenever a “modified” event is registered we
-- put () in the MVar that acts as a run trigger. `tryPutMVar` is used to avoid
-- re-running the command many times if the file/dir is changed more than once
-- while the command is already running.
watch :: FileType -> WatchManager -> String -> MVar () -> IO StopListening
watch filetype m path trigger =
  let watchFun = case filetype of
                   Directory -> watchTree m (fromString path) (const True)
                   File      -> watchDir  m (directory $ fromString path) isThisFile
   in watchFun (\_ -> void $ tryPutMVar trigger ())

  where isThisFile (Modified p _) = p == fromString path
        isThisFile _              = False

runCmd :: String -> [String] -> MVar () -> IO ()
runCmd cmd args trigger = do
  _ <- takeMVar trigger
  putStrLn $ "Running " ++ cmd ++ " " ++ unwords args ++  "..."
  (_, _, _, ph) <- createProcess (proc cmd args)
  exitCode <- waitForProcess ph
  hPutStrLn stderr $ case exitCode of
                       ExitSuccess   -> "Process completed successfully"
                       ExitFailure n -> "Process returned " ++ show n
  runCmd cmd args trigger

main :: IO ()
main = do
  argv <- getArgs
  when (length argv < 2) $ getProgName >>= usage >> exitFailure

  let [path,cmd]  = take 2 argv
  let args = drop 2 argv

  m <- startManager

  -- Create an empty MVar and install INT/TERM handlers that will fill it.
  -- We will wait for one of these signals before cleaning up and exiting.
  interrupted <- newEmptyMVar
  _ <- installHandler sigINT  (Catch $ putMVar interrupted ()) Nothing
  _ <- installHandler sigTERM (Catch $ putMVar interrupted ()) Nothing

  canonicalPath <- canonicalizePath path

  -- Check if path is a file or directory.
  s <- getFileStatus canonicalPath
  let filetype = if isDirectory s then Directory else File

  runTrigger <- newEmptyMVar
  runThread <- forkIO $ runCmd cmd args runTrigger
  stopWatcher <- watch filetype m canonicalPath runTrigger

  -- Calculate the full path in order to print the "real" file when watching a
  -- path with one or more symlinks.
  currDir <- getCurrentDirectory
  let fullPath = fromString currDir </> fromString path
  putStr $ "Started to watch " ++ path
  putStrLn $ if fromString canonicalPath == fullPath
                then ""
                else " (→ " ++ canonicalPath ++ ")"
  putStrLn "Press ^C to stop."

  _ <- readMVar interrupted
  putStrLn "\nStopping."
  stopWatcher
  stopManager m
  killThread runThread
  exitSuccess
    where usage n = hPutStrLn stderr $ "Usage: " ++ n
                             ++ " <file/directory to watch>"
                             ++ " <command to run> [arguments for command]"
