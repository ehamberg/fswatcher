{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (id, (.))

import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFileStatus, isDirectory)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Data.Foldable (for_)
import Data.String (fromString)
import Data.Traversable (for)
import System.FSNotify (Event (..), StopListening, WatchManager, startManager,
       stopManager, watchTree, watchDir, eventPath)
import System.Exit (ExitCode (..), exitSuccess)
import System.Process (createProcess, proc, waitForProcess)
import Control.Category
import Control.Monad (void)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Text.Regex.PCRE
import Options.Applicative

import Opts
import Pipeline

data FileType = File | Directory deriving Eq

data FileDetails = FileDetails { givenPath    :: FilePath
                               , expandedPath :: FilePath
                               , filetype     :: FileType
                               } deriving Eq

-- Watches a file or directory and whenever a “modified” event is registered we
-- put () in the MVar that acts as a run trigger. `tryPutMVar` is used to avoid
-- re-running the command many times if the file/dir is changed more than once
-- while the command is already running.
watch :: WatchManager -> MVar () -> WatchOpt -> FileDetails -> IO StopListening
watch m trigger opt fileDetails = do
  let path = expandedPath fileDetails
  let watchFun = case filetype fileDetails of
                   Directory -> watchTree m path (matchFiles opt)
                   File      -> watchDir  m (takeDirectory path) isThisFile
   in watchFun (\_ -> void $ tryPutMVar trigger ())

  where isThisFile :: Event -> Bool
        isThisFile (Modified p _ _) = p == fromString (expandedPath fileDetails)
        isThisFile _                = False
        matchFiles :: WatchOpt -> Event -> Bool
        matchFiles wo event = let p = eventPath event
                                  includes = includePath wo
                                  excludes = excludePath wo
                              in
                                (null includes || p =~ includePath wo)
                                && (null excludes || not (p =~ excludePath wo))

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

runWatch :: WatchOpt -> IO ()
runWatch opt = do

  let paths = watchPaths opt
  let cmd = head $ actionCmd opt
  let args = drop 1 $ actionCmd opt

  m <- startManager

  -- Create an empty MVar and install INT/TERM handlers that will fill it.
  -- We will wait for one of these signals before cleaning up and exiting.
  interrupted <- newEmptyMVar
  _ <- installHandler sigINT  (Catch $ putMVar interrupted ()) Nothing
  _ <- installHandler sigTERM (Catch $ putMVar interrupted ()) Nothing

  allFileDetails <- for paths $ \path -> do
    canonicalPath <- canonicalizePath path

    -- Check if path is a file or directory.
    s <- getFileStatus canonicalPath
    let ft = if isDirectory s then Directory else File

    pure (FileDetails path canonicalPath ft)

  -- Check if throttling was requested.
  let delay = throttlingDelay opt
  let pipeline = if delay > 0 then throttle delay else id

  inputMVar <- newEmptyMVar
  (pipelineThreads, outputMVar) <- runPipeline pipeline inputMVar
  runThread <- forkIO $ runCmd cmd args outputMVar
  stopWatchers <- sequence_ <$> traverse (watch m inputMVar opt) allFileDetails

  let allThreads = runThread : pipelineThreads

  -- Calculate the full path in order to print the "real" file when watching a
  -- path with one or more symlinks.
  currDir <- getCurrentDirectory
  for_ allFileDetails $ \fileDetails -> do
    let path = givenPath fileDetails
    let canonicalPath = expandedPath fileDetails
    let fullPath = fromString currDir </> fromString path
    putStr $ "Started to watch " ++ path
    putStrLn $ if fromString canonicalPath == fullPath
                  then ""
                  else " (→ " ++ canonicalPath ++ ")"
  putStrLn "Press ^C to stop."

  _ <- readMVar interrupted
  putStrLn "\nStopping."
  stopWatchers
  stopManager m
  mapM_ killThread allThreads
  exitSuccess

main :: IO ()
main = execParser opts >>= runWatch
  where
    opts = info (helper <*> watchOpt)
      ( fullDesc
     <> progDesc "monitors a file or a directory for changes and runs a given command.")
