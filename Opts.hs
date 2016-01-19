module Opts where

import           Options.Applicative

data WatchOpt = WatchOpt { watchPath   :: String
                         , includePath :: String  -- ^ an reg exp to include particular files when watching dir
                         , excludePath :: String  -- ^ an reg exp to exclude particular files when watching dir
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
     <*> (many . strArgument) (metavar "COMMAND"
                      <> help "command to run" )
