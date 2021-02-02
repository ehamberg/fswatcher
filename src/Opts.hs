module Opts where

import           Options.Applicative

data WatchOpt = WatchOpt { watchPath   :: String
                         , includePath :: String  -- ^ a regex to include particular files when watching dir
                         , excludePath :: String  -- ^ a regex to exclude particular files when watching dir
                         , throttlingDelay :: Int     -- ^ milliseconds to wait for duplicate events
                         , actionCmd   :: [String]
                         } deriving (Show)

watchOpt :: Parser WatchOpt
watchOpt = WatchOpt
     <$> strOption (long "path"
                    <> metavar "PATH"
                    <> help "directory / file to watch" )
     <*> strOption (long "include"
                    <> value []
                    <> metavar "INCLUDE"
                    <> help "pattern for including files")
     <*> strOption (long "exclude"
                    <> value []
                    <> metavar "EXCLUDE"
                    <> help "pattern for excluding files")
     <*> option auto (long "throttle"
                    <> value 0
                    <> metavar "MILLIS"
                    <> help "milliseconds to wait for duplicate events")
     <*> (some . strArgument) (metavar "COMMAND"
                      <> help "command to run" )
