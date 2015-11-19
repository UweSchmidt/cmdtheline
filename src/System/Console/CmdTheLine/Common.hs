{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Common where

import Data.Function    ( on )
import Text.PrettyPrint ( Doc, text )
import Control.Applicative ( Applicative(..) )

import qualified Data.Map as M

import Control.Monad.Trans.Except

data Absence = Absent
             | Present String
               deriving ( Eq )

data OptKind = FlagKind
             | OptKind
             | OptVal String
               deriving ( Eq )

data PosKind = PosAny
             | PosN Bool Int
             | PosL Bool Int
             | PosR Bool Int
               deriving ( Eq, Ord )

data ArgInfo = ArgInfo
  { absence    :: Absence
  , argDoc     :: String
  , argName    :: String
  , argSec     :: String
  , posKind    :: PosKind
  , optKind    :: OptKind
  , optNames   :: [String]
  , repeatable :: Bool
  }

instance Eq ArgInfo where
  ai == ai'
    | isPos ai && isPos ai' = ((==) `on` posKind) ai ai'
    | isOpt ai && isOpt ai' = ((==) `on` optNames) ai ai'
    | otherwise             = False

-- This Ord instance works for placing in 'Data.Map's, but not much else.
instance Ord ArgInfo where
  compare ai ai'
    | isPos ai && isPos ai' = (compare `on` posKind) ai ai'
    | isOpt ai && isOpt ai' = (compare `on` optNames) ai ai'
    | isOpt ai && isPos ai' = LT
    | otherwise             = GT


data Arg = Opt [( Int          -- The position were the argument was found.
                , String       -- The name by which the argument was supplied.
                , Maybe String -- If present, a value assigned to the argument.
                )]
         | Pos [String]        -- A list of positional arguments

type CmdLine = M.Map ArgInfo Arg

isOpt, isPos :: ArgInfo -> Bool
isOpt ai = optNames ai /= []
isPos ai = optNames ai == []

{- |
  Any 'String' argument to a 'ManBlock' constructor may contain the
  following significant forms for a limited kind of meta-programing.

  * $(i,text): italicizes @text@.

  * $(b,text): bolds @text@.

  * $(mname): evaluates to the name of the default term if there are choices
    of commands, or the only term otherwise.

  * $(tname): evaluates to the name of the currently evaluating term.

  Additionally, text inside the content portion of an 'I' constructor may
  contain one of the following significant forms.

  * $(argName): evaluates to the name of the argument being documented.

-}
data ManBlock = S String        -- ^ A section title.
              | P String        -- ^ A paragraph.
              | I String String -- ^ A label-content pair. As in an argument
                                --   definition and its accompanying
                                --   documentation.
              | NoBlank         -- ^ Suppress the normal blank line following
                                --   a 'P' or an 'I'.
                deriving ( Eq )

type Title = ( String, Int, String, String, String )

type Page = ( Title, [ManBlock] )

-- | Information about a 'Term'.  It is recommended that 'TermInfo's be
-- created by customizing 'defTI', as in
--
-- > termInfo = defTI
-- >   { termName = "caroline-no"
-- >   , termDoc  = "carry a line off"
-- >   }
data TermInfo = TermInfo
  {
  -- | The name of the command or program represented by the term. Defaults to
  -- @\"\"@.
    termName  :: String

  -- | Documentation for the term. Defaults to @\"\"@.
  , termDoc   :: String

  -- | The section under which to place the terms documentation.
  -- Defaults to @\"COMMANDS\"@.
  , termSec   :: String

  -- | The section under which to place a term's argument's
  -- documentation by default. Defaults to @\"OPTIONS\"@.
  , stdOptSec :: String

  -- | A version string.  Must be left blank for commands. Defaults to @\"\"@.
  , version   :: String

  -- | A list of 'ManBlock's to append to the default @[ManBlock]@. Defaults
  -- to @[]@.
  , man       :: [ManBlock]
  } deriving ( Eq )

-- | A default 'TermInfo'.
defTI :: TermInfo
defTI = TermInfo
  { termName  = ""
  , version   = ""
  , termDoc   = ""
  , termSec   = "COMMANDS"
  , stdOptSec = "OPTIONS"
  , man       = []
  }

type Command = ( TermInfo, [ArgInfo] )

data EvalInfo = EvalInfo
  { term    :: Command   -- The chosen term for this run.
  , main    :: Command   -- The default term.
  , choices :: [Command] -- A list of command-terms.
  }

-- | The format to print help in.
data HelpFormat = Pager | Plain | Groff deriving ( Eq )

data Fail = MsgFail   Doc
          | UsageFail Doc
          | HelpFail  HelpFormat (Maybe String)

-- | A monad for values in the context of possibly failing with a helpful
-- message.
type Err = ExceptT Fail IO


type Yield a = EvalInfo -> CmdLine -> Err a

-- | The underlying Applicative of the library.  A @Term@ represents a value
-- in the context of being computed from the command line arguments.
data Term a = Term [ArgInfo] (Yield a)

instance Functor Term where
  fmap = yield . result . result . fmap
    where
    yield f (Term ais y) = Term ais (f y)
    result = (.)

instance Applicative Term where
  pure v = Term [] (\ _ _ -> return v)

  (Term args f) <*> (Term args' v) = Term (args ++ args') wrapped
    where
    wrapped ei cl = f ei cl <*> v ei cl


data EvalKind = Simple   -- The program has no commands.
              | Main     -- The default program is running.
              | Choice   -- A command has been chosen.

evalKind :: EvalInfo -> EvalKind
evalKind ei
  | choices ei == []               = Simple
  | fst (term ei) == fst (main ei) = Main
  | otherwise                      = Choice

descCompare :: Ord a => a -> a -> Ordering
descCompare = flip compare

splitOn :: Eq a => a -> [a] -> ( [a], [a] )
splitOn sep xs = ( left, rest' )
  where
  rest' = if rest == [] then rest else tail rest -- Skip the 'sep'.
  ( left, rest ) = span (/= sep) xs

select :: a -> [( Bool, a )] -> a
select baseCase = foldr (uncurry (?)) baseCase
  where
  (?) True = const
  (?) False = flip const
