{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Util
  (
  -- * File path validation
  -- ** Existing path check
    fileExists,  dirExists,  pathExists
  -- ** Existing paths check
  , filesExist, dirsExist, pathsExist
  -- ** Valid path
  , validPath
  ) where

import Control.Applicative
import Text.PrettyPrint

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.Err

import Control.Monad.IO.Class ( liftIO )

import System.Directory ( doesFileExist, doesDirectoryExist )
import System.FilePath  ( isValid )

doesFileOrDirExist :: String -> IO Bool
doesFileOrDirExist = liftA2 (||) <$> doesFileExist <*> doesDirectoryExist

check :: (String -> IO Bool) -> String -> String -> Err String
check test errStr path = do
  isDir <- liftIO $ test path
  if isDir
     then return path
     else msgFail $ no errStr path

validate :: (String -> IO Bool) -> String -> Term String -> Term String
validate test errStr = ret . fmap (check test errStr)

validates :: (String -> IO Bool) -> String -> Term [String] -> Term [String]
validates test errStr = ret . fmap (mapM $ check test errStr)

-- | 'fileExists' @term@ checks that 'String' in @term@ is a path to an existing
-- /file/. If it is not, exit with an explanatory message for the user.
fileExists :: Term String -> Term String
fileExists = validate doesFileExist      "file"

-- | 'dirExists' @term@ checks that 'String' in @term@ is a path to an existing
-- /directory/. If it is not, exit with an explanatory message for the user.
dirExists  :: Term String -> Term String
dirExists  = validate doesDirectoryExist "directory"

-- | 'pathExists' @term@ checks that 'String' in @term@ is a path to an existing
-- /file or directory/. If it is not, exit with an explanatory message for the
-- user.
pathExists :: Term String -> Term String
pathExists = validate doesFileOrDirExist "file or directory"

-- | 'filesExist' @term@ is as 'fileExists' but for a @term@ containing a list
-- of file paths.
filesExist :: Term [String] -> Term [String]
filesExist = validates doesFileExist      "file"

-- | 'dirsExist' @term@ is as 'dirExists' but for a @term@ containing a list
-- of directory paths.
dirsExist  :: Term [String] -> Term [String]
dirsExist  = validates doesDirectoryExist "directory"

-- | 'pathsExist' @term@ is as 'pathExists' but for a @term@ containing a list
-- of paths.
pathsExist :: Term [String] -> Term [String]
pathsExist = validates doesFileOrDirExist "file or directory"

-- | 'validPath' @term@ checks that 'String' in @term@ is a valid path under
-- the current operating system. If it is not, exit with an explanatory
-- message for the user.
validPath :: Term String -> Term String
validPath = ret . fmap check
  where
  check   str = if isValid str then return str else msgFail $ failDoc str
  failDoc str = quotes (text str) <+> text "is not a valid file path."
