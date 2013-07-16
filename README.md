                     ___               _       _
                    |  _|___ _ _ _ ___| |_ ___| |_ ___ ___
                    |  _|_ -| | | | .'|  _|  _|   | -_|  _|
                    |_| |___|_____|__,|_| |___|_|_|___|_|

`fswatcher` monitors a file or a directory for changes and runs a given command
when the file is modified. If monitoring a directory, it runs the specified
command a file within the given directory is modified.

This is a really thin layer on top of the
[fsnotify](http://hackage.haskell.org/package/fsnotify) library. It works on
Linux, OS X and other BSDs, and Windows (untested).

Usage:

    watcher <file/directory to watch> <command to run> [arguments to command]
