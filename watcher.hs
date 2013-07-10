import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFileStatus, isDirectory)
import System.Environment (getArgs, getProgName)
import System.Directory (canonicalizePath)
import Filesystem.Path (directory)
import Data.String (fromString)
import Data.List (intercalate)
import System.FSNotify (Event (..), WatchManager, startManager, stopManager, watchTree, watchDir)
import System.Exit (ExitCode (..), exitSuccess, exitFailure)
import System.Process (createProcess, proc, waitForProcess)
import Control.Monad (when)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent.MVar

data FileType = File | Directory deriving Eq

runCmd :: String -> [String] -> Event -> IO ()
runCmd cmd args _ = do
  putStrLn $ "Running " ++ cmd ++ " " ++ intercalate " " args ++  "..."
  (_, _, _, ph) <- createProcess (proc cmd args)
  exitCode <- waitForProcess ph
  hPutStrLn stderr $ case exitCode of
                       ExitSuccess   -> "Process completed successfully"
                       ExitFailure n -> "Process completed with exitcode " ++ show n

watch :: FileType -> WatchManager -> String -> String -> [String] -> IO ()
watch Directory m path cmd args  = watchTree m (fromString path) (const True) (runCmd cmd args)
watch File m path cmd args  = watchDir m (directory $ fromString path) isThisFile (runCmd cmd args)
  where isThisFile (Modified p _) = p == fromString path
        isThisFile _              = False

main :: IO ()
main = do
  argv <- getArgs
  when (length argv < 2) $ getProgName >>= usage >> exitFailure

  let path = head argv
  let cmd  = argv !! 1
  let args = drop 2 argv

  m <- startManager

  -- Create an empty MVar and install INT/TERM handlers that will fill it.
  -- We will wait for one of these signals before cleaning up and exiting.
  interrupted <- newEmptyMVar
  _ <- installHandler sigINT  (Catch $ putMVar interrupted True) Nothing
  _ <- installHandler sigTERM (Catch $ putMVar interrupted True) Nothing

  canonicalPath <- canonicalizePath path

  -- check if path is a file or directory
  s <- getFileStatus canonicalPath
  let filetype = if isDirectory s then Directory else File

  watch filetype m canonicalPath cmd args
  putStr $ "Started to watch " ++ path
  putStrLn $ if canonicalPath == path then "" else " [→ " ++ canonicalPath ++ "]"

  _ <- readMVar interrupted
  putStrLn "\nStopping."
  stopManager m
  exitSuccess
    where usage n = hPutStrLn stderr $ "Usage: " ++ n
                             ++ " <file/directory to watch>"
                             ++ " <command to run> [arguments for command]"
