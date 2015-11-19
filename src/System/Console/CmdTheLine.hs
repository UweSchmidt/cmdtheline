{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine
  ( module System.Console.CmdTheLine.Term
  , module System.Console.CmdTheLine.Arg
  , module System.Console.CmdTheLine.ArgVal
  , module System.Console.CmdTheLine.Util

  -- * Terms
  -- $term
  , Term()
  , TermInfo(..)
  , defTI

  -- * Manpages
  , ManBlock(..)

  -- * User error reporting
  -- $err
  , HelpFormat(..), Err()
  , msgFail, usageFail, helpFail
  , ret
  )
  where

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.Term
import System.Console.CmdTheLine.Arg
import System.Console.CmdTheLine.ArgVal
import System.Console.CmdTheLine.Err
import System.Console.CmdTheLine.Util

{-$term

  CmdTheLine is centered around the 'Term' Applicative Functor.  It allows us
  to define command line programs like the following.

> import System.Console.CmdTheLine
> import Control.Applicative
>
> import Control.Monad ( when )
>
> -- Define a flag argument under the names '--silent' and '-s'
> silent :: Term Bool
> silent = value . flag $ optInfo [ "silent", "s" ]
>
> -- Define the 0th positional argument, defaulting to the value '"world"' in
> -- absence.
> greeted :: Term String
> greeted = value $ pos 0 "world" posInfo { posName = "GREETED" }
> 
> hello :: Bool -> String -> IO ()
> hello silent str = when (not silent) . putStrLn $ "Hello, " ++ str ++ "!"
>
> term :: Term (IO ())
> term = hello <$> silent <*> greeted
> 
> termInfo :: TermInfo
> termInfo = defTI { termName = "Hello", version = "1.0" }
> 
> main :: IO ()
> main = run ( term, termInfo )

  CmdTheLine then generates usage, help in the form of man-pages, and manages
  all the related tedium of getting values from the command line into our
  program so we can go on thinking in regular Haskell functions.

  See the accompanying examples(including the above) provided under the
  @doc/examples@ directory of the distributed package, or go to
  <http://github.com/eli-frey/cmdtheline> and peruse them there.

-}

{-$err

  There is nothing stopping you from printing and formating your own error
  messages.  However, some of the time you will want more tight integration
  with the library.  That is what 'Fail', the 'Err' monad, and 'ret' are for.

  Here is a snippet of an example program that can be found at
  @doc\/examples\/fail.hs@ in the library distribution tarball, or at
  <http://github.com/eli-frey/cmdtheline>.

> import System.Console.CmdTheLine
> import Control.Applicative
>
> import Text.PrettyPrint ( fsep   -- Paragraph fill a list of 'Doc'.
>                         , text   -- Make a 'String' into a 'Doc'.
>                         , quotes -- Quote a 'Doc'.
>                         , (<+>)  -- Glue two 'Doc' together with a space.
>                         )
>
> import Data.List ( intersperse )
>
> failMsg, failUsage, success :: [String] -> Err String
> failMsg   strs = msgFail   . fsep $ map text strs
> failUsage strs = usageFail . fsep $ map text strs
> success   strs = return . concat $ intersperse " " strs
>
> help :: String -> Err String
> help name
>   | any (== name) cmdNames = helpFail Pager $ Just name
>   | name == ""             = helpFail Pager Nothing
>   | otherwise              =
>     usageFail $ quotes (text name) <+> text "is not the name of a command"
>
> noCmd :: Err String
> noCmd = helpFail Pager Nothing

  We can now turn any of these functions into a @Term String@ by lifting into
  'Term' and passing the result to 'ret' to fold the 'Err' monad into the
  library.  Here is an example of what it might look like to do this with @noCmd@.

> noCmdTerm :: Term (Err String)
> noCmdTerm = pure noCmd
>
> prepedNoCmdTerm :: Term String
> prepedNoCmdTerm = ret noCmdTerm

  For other examples of ways to use the 'Err' monad, see the source of the
  *Exists family of functions in "System.Console.CmdTheLine.Util".
-}
