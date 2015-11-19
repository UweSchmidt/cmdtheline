{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Arg
  ( Arg
  -- * Argument Information
  , OptInfo( optName, optDoc, optSec ), PosInfo( posName, posDoc, posSec )
  , optInfo, posInfo

  -- * Optional arguments
  -- $opt

  -- ** Flag options
  , flag, flagAll, vFlag, vFlagAll

  -- ** Assignable options
  , opt, defaultOpt, optAll, defaultOptAll

  -- * Positional arguments
  -- $pos
  , pos, revPos, posAny, posLeft, posRight, revPosLeft, revPosRight

  -- * Arguments as Terms
  , value, required, nonEmpty, lastOf
  ) where

import System.Console.CmdTheLine.Common  hiding ( Arg )
import System.Console.CmdTheLine.CmdLine ( optArg, posArg )
import System.Console.CmdTheLine.ArgVal  ( ArgVal, pp, parser )
import qualified System.Console.CmdTheLine.Err  as E

import Control.Applicative
import Control.Arrow       ( second )

import Control.Monad.Trans.Except ( throwE )

import Text.PrettyPrint

import Data.List     ( sortBy, foldl' )
import Data.Function ( on )

argFail :: Doc -> Err a
argFail = throwE . MsgFail

-- | The type of command line arguments.
newtype Arg a = Arg (Term a)

-- | Information about an optional argument. Exposes the folowing fields.
--
-- [@optName@] :: String: defaults to @\"\"@.
--
-- [@optDoc@]  :: String: defaults to @\"\"@.
--
-- [@optSec@]  :: String: defaults to @\"OPTIONS\"@.
data OptInfo = OInf
  { unOInf  :: ArgInfo
  , optName :: String
  , optDoc  :: String
  , optSec  :: String
  }

fromOptInfo :: OptInfo -> ArgInfo
fromOptInfo oi = (unOInf oi)
  { argName = optName oi
  , argDoc  = optDoc  oi
  , argSec  = optSec  oi
  }

-- | Information about a positional argument. Exposes the folowing fields.
--
-- [@posName@] :: String: defaults to @\"\"@.
--
-- [@posDoc@]  :: String: defaults to @\"\"@.
--
-- [@posSec@]  :: String: defautts to @\"ARGUMENTS\"@.
data PosInfo = PInf
  { unPInf  :: ArgInfo
  , posName :: String
  , posDoc  :: String
  , posSec  :: String
  }

fromPosInfo :: PosInfo -> ArgInfo
fromPosInfo pi = (unPInf pi)
  { argName = posName pi
  , argDoc  = posDoc  pi
  , argSec  = posSec  pi
  }

mkInfo :: [String] -> ArgInfo
mkInfo names = ArgInfo
  { absence    = Present ""
  , argDoc     = ""
  , argName    = ""
  , argSec     = ""
  , posKind    = PosAny
  , optKind    = FlagKind
  , optNames   = map dash names
  , repeatable = False
  }
  where
  dash "" =
    error "System.Console.CmdTheLine.Arg.mkInfo recieved empty string as name"

  dash str@[_] = "-"  ++ str
  dash str     = "--" ++ str

-- | Initialize an 'OptInfo' by providing a list of names.  The fields
-- @optName@, @optDoc@, and @optSec@ can then be manipulated post-mortem,
-- as in
--
-- > inf =(optInfo    [ "i", "insufflation" ])
-- >     { optName = "INSUFFERABLE"
-- >     , optDoc  = "in the haunted house's harrow"
-- >     , optSec  = "NOT FOR AUGHT"
-- >     }
--
-- Names of one character in length will be prefixed by @-@ on the command line,
-- while longer names will be prefixed by @--@.
--
-- It is considered a programming error to provide an empty list of names to
-- optInfo.
optInfo :: [String] -> OptInfo
optInfo [] =
  error "System.Console.CmdTheLine.Arg.optInfo recieved empty list of names."
optInfo names = OInf (mkInfo names) "" "" "OPTIONS"

-- | Initialize a 'PosInfo'.  The fields @posName@, @posDoc@, and @posSec@
-- can then be manipulated post-mortem, as in
--
-- > inf = posInfo
-- >     { posName = "DEST"
-- >     , posDoc  = "A destination for the operation."
-- >     , posSec  = "DESTINATIONS"
-- >     }
--
-- The fields @posName@ and @posDoc@ must be non-empty strings for the argument
-- to be listed with its documentation under the section @posSec@ of generated
-- help.
posInfo :: PosInfo
posInfo = PInf (mkInfo []) "" "" "ARGUMENTS"


{- $opt

  An optional argument is specified on the command line by a /name/ possibly
  followed by a /value/.

  The name of an option can be /short/ or /long/.

  * A /short/ name is a dash followed by a single alphanumeric character:
    @-h@, @-q@, @-I@.

  * A /long/ name is two dashes followed by alphanumeric characters and dashes:
    @--help@, @--silent@, @--ignore-case@.

  More than one name may refer to the same optional argument.  For example in
  a given program the names @-q@, @--quiet@, and @--silent@ may all stand for
  the same boolean argument indicating the program to be quiet.  Long names can
  be specified by any non-ambiguous prefix.

  There are three ways to assign values to an optional argument on the command
  line.

  * As the next token on the command line: @-o a.out@, @--output a.out@.

  * Glued to a short name: @-oa.out@.

  * Glued to a long name after an equal character: @--output=a.out@.

  Glued forms are necessary if the value itself starts with a dash, as is the
  case for negative numbers, @--min=-10@.

-}

--
-- Flags
--

-- | Create a command line flag that can appear at most once on the
-- command line.  Yields @False@ in absence and @True@ in presence.
flag :: OptInfo -> Arg Bool
flag oi = Arg $ Term [ai] yield
  where
  ai = fromOptInfo oi
  yield _ cl = case optArg cl ai of
    []                  -> return False
    [( _, _, Nothing )] -> return True
    [( _, f, Just v  )] -> argFail $ E.flagValue f v

    (( _, f, _ ) :
     ( _, g, _ ) :
     _           ) -> argFail $ E.optRepeated f g

-- | As 'flag' but may appear an infinity of times. Yields a list of @True@s
-- as long as the number of times present.
flagAll :: OptInfo -> Arg [Bool]
flagAll oi = Arg $ Term [ai'] yield
  where
  ai  = fromOptInfo oi
  ai' = ai { repeatable = True }

  yield _ cl = case optArg cl ai' of
    [] -> return []
    xs -> mapM truth xs

  truth ( _, f, mv ) = case mv of
    Nothing -> return  True
    Just v  -> argFail $ E.flagValue f v

-- | 'vFlag' @v [ ( v1, ai1 ), ... ]@ is an argument that can be present at most
-- once on the command line. It takes on the value @vn@ when appearing as
-- @ain@.
vFlag :: a -> [( a, OptInfo )] -> Arg a
vFlag v assoc = Arg $ Term (map snd assoc') yield
  where
  assoc' = map (second fromOptInfo) assoc

  yield _ cl = go Nothing assoc'
    where
    go mv [] = case mv of
      Nothing       -> return v
      Just ( _, v ) -> return v

    go mv (( v, ai ) : rest) = case optArg cl ai of
      []                  -> go mv rest

      [( _, f, Nothing )] -> case mv of
        Nothing       -> go (Just ( f, v )) rest
        Just ( g, _ ) -> argFail $ E.optRepeated g f

      [( _, f, Just v )]  -> argFail $ E.flagValue f v

      (( _, f, _ ) :
       ( _, g, _ ) :
       _           ) -> argFail $ E.optRepeated g f

-- | 'vFlagAll' @vs assoc@ is as 'vFlag' except that it can be present an
-- infinity of times.  In absence, @vs@ is yielded.  When present, each
-- value is collected in the order they appear.
vFlagAll :: [a] -> [( a, OptInfo)] -> Arg [a]
vFlagAll vs assoc = Arg $ Term (map flag assoc') yield
  where
  assoc' = map (second fromOptInfo) assoc
  flag ( _, ai ) 
    | isPos ai  = error E.errNotOpt
    | otherwise = ai { repeatable = True }

  yield _ cl = do
    result <- foldl' addLookup (return []) assoc'
    case result of
      [] -> return vs
      _  -> return . map snd $ sortBy (compare `on` fst) result
    where
    addLookup acc ( v, ai ) = case optArg cl ai of
      [] -> acc
      xs -> (++) <$> mapM flagVal xs <*> acc
      where
      flagVal ( pos, f, mv ) = case mv of
        Nothing -> return  ( pos, v )
        Just v  -> argFail $ E.flagValue f v
   

--
-- Options
--

parseOptValue :: ArgVal a => String -> String -> Err a
parseOptValue f v = case parser v of
  Left  e -> throwE . UsageFail $ E.optParseValue f e
  Right v -> return v

mkOpt :: ArgVal a => Maybe a -> a -> OptInfo -> Arg a
mkOpt vopt v oi = Arg $ Term [ai'] yield
    where
    ai  = fromOptInfo oi
    ai' = ai { absence = Present . show $ pp v
             , optKind = case vopt of
                 Nothing -> OptKind
                 Just dv -> OptVal . show $ pp dv
             }
    yield _ cl = case optArg cl ai' of
      []                  -> return v
      [( _, f, Just v )]  -> parseOptValue f v

      [( _, f, Nothing )] -> case vopt of
        Nothing   -> argFail $ E.optValueMissing f
        Just optv -> return  optv

      (( _, f, _ ) :
       ( _, g, _ ) :
       _           ) -> argFail $ E.optRepeated g f

-- | 'opt' @v ai@ is an optional argument that yields @v@ in absence, or an
-- assigned value in presence.  If the option is present, but no value is
-- assigned, it is considered a user-error and usage is printed on exit.
opt :: ArgVal a => a -> OptInfo -> Arg a
opt = mkOpt Nothing

-- | 'defaultOpt' @def v ai@ is as 'opt' except if it is present and no value is
-- assigned on the command line, @def@ is the result.
defaultOpt :: ArgVal a => a -> a -> OptInfo -> Arg a
defaultOpt x = mkOpt $ Just x

mkOptAll :: ( ArgVal a, Ord a ) => Maybe a -> [a] -> OptInfo -> Arg [a]
mkOptAll vopt vs oi = Arg $ Term [ai'] yield
    where
    ai  = fromOptInfo oi
    ai' = ai { absence    = Present ""
             , repeatable = True
             , optKind    = case vopt of
                 Nothing -> OptKind
                 Just dv -> OptVal . show $ pp dv
             }

    yield _ cl = case optArg cl ai' of
      [] -> return vs
      xs -> map snd . sortBy (compare `on` fst) <$> mapM parse xs

    parse ( pos, f, mv' ) = case mv' of
      Just v  -> (,) pos <$> parseOptValue f v
      Nothing -> case vopt of
        Nothing -> argFail $ E.optValueMissing f
        Just dv -> return  ( pos, dv )

-- | 'optAll' @vs ai@ is like 'opt' except that it yields @vs@ in absence and
-- can appear an infinity of times.  The values it is assigned on the command
-- line are accumulated in the order they appear.
optAll :: ( ArgVal a, Ord a ) => [a] -> OptInfo -> Arg [a]
optAll = mkOptAll Nothing

-- | 'defaultOptAll' @def vs ai@ is like 'optAll' except that if it is present
-- without being assigned a value, the value @def@ takes its place in the list
-- of results.
defaultOptAll :: ( ArgVal a, Ord a ) => a -> [a] -> OptInfo -> Arg [a]
defaultOptAll x = mkOptAll $ Just x


{- $pos

  Positional arguments are tokens on the command line that are not option names
  or the values being assigned to an optional argument.

  Since positional arguments may be mistaken as the optional value of an
  optional argument or they may need to look like an optional name, anything
  that follows the special token @--@(with spaces on both sides) on the command
  line is considered to be a positional argument.

  Positional arguments are listed in documentation sections iff they are
  assigned both an @argName@ and an @argDoc@.

-}

--
-- Positional arguments.
--

parsePosValue :: ArgVal a => ArgInfo -> String -> Err a
parsePosValue ai v = case parser v of
  Left  e -> throwE . UsageFail $ E.posParseValue ai e
  Right v -> return v

mkPos :: ArgVal a => Bool -> Int -> a -> PosInfo -> Arg a
mkPos rev pos v oi = Arg $ Term [ai'] yield
  where
  ai  = fromPosInfo oi
  ai' = ai { absence = Present . show $ pp v
           , posKind = PosN rev pos
           }
  yield _ cl = case posArg cl ai' of
    []  -> return v
    [v] -> parsePosValue ai' v
    _   -> error "saw list with more than one member in pos converter"

-- | 'pos' @n v ai@ is an argument defined by the @n@th positional argument
-- on the command line. If absent the value @v@ is returned.
pos :: ArgVal a => Int -> a -> PosInfo -> Arg a
pos    = mkPos False

-- | 'revPos' @n v ai@ is as 'pos' but counting from the end of the command line
-- to the front.
revPos :: ArgVal a => Int -> a -> PosInfo -> Arg a
revPos = mkPos True

posList :: ArgVal a => PosKind -> [a] -> PosInfo -> Arg [a]
posList kind vs oi = Arg $ Term [ai'] yield
    where
    ai  = fromPosInfo oi
    ai' = ai { posKind = kind }
    yield _ cl = case posArg cl ai' of
      [] -> return vs
      xs -> mapM (parsePosValue ai') xs

-- | 'posAny' @vs ai@ yields a list of all positional arguments or @vs@ if none
-- are present.
posAny :: ArgVal a => [a] -> PosInfo -> Arg [a]
posAny = posList PosAny

-- | 'posLeft' @n vs ai@ yield a list of all positional arguments to the left of
-- the @n@th positional argument or @vs@ if there are none.
posLeft :: ArgVal a => Int -> [a] -> PosInfo -> Arg [a]
posLeft = posList . PosL False

-- | 'posRight' @n vs ai@ is as 'posLeft' except yielding all values to the right
-- of the @n@th positional argument.
posRight :: ArgVal a => Int -> [a] -> PosInfo -> Arg [a]
posRight = posList . PosR False

-- | 'revPosLeft' @n vs ai@ is as 'posLeft' except @n@ counts from the end of the
-- command line to the front.
revPosLeft :: ArgVal a => Int -> [a] -> PosInfo -> Arg [a]
revPosLeft = posList . PosL True

-- | 'revPosRight' @n vs ai@ is as 'posRight' except @n@ counts from the end of
-- the command line to the front.
revPosRight :: ArgVal a => Int -> [a] -> PosInfo -> Arg [a]
revPosRight = posList . PosR True


--
-- Arguments as terms.
--

absent :: [ArgInfo] -> [ArgInfo]
absent = map (\ ai -> ai { absence = Absent })

-- | 'value' @arg@ makes @arg@ into a 'Term'.
value :: Arg a -> Term a
value (Arg term) = term

-- | 'required' @arg@ converts @arg@ into a 'Term' such that it fails in the
-- 'Nothing' and yields @a@ in the 'Just'.
--
-- This is used for required positional arguments.  There is nothing
-- stopping you from using it with optional arguments, except that they
-- would no longer be optional and it would be confusing from a user's
-- perspective.
required :: Arg (Maybe a) -> Term a
required (Arg (Term ais yield)) = Term ais' yield'
  where
  ais' = absent ais
  yield' ei cl = aux =<< yield ei cl
  aux = maybe (argFail . E.argMissing $ head ais') return

-- | 'nonEmpty' @arg@ is a 'Term' that fails if its result is empty. Intended
-- for non-empty lists of positional arguments.
nonEmpty :: Arg [a] -> Term [a]
nonEmpty (Arg (Term ais yield)) = Term ais' yield'
  where
  ais' = absent ais
  yield' ei cl = aux =<< yield ei cl
  aux [] = argFail . E.argMissing $ head ais'
  aux xs = return xs

-- | 'lastOf' @arg@ is a 'Term' that fails if its result is empty and evaluates
-- to the last element of the resulting list otherwise.  Intended for lists
-- of flags or options where the last takes precedence.
lastOf :: Arg [a] -> Term a
lastOf (Arg (Term ais yield)) = Term ais yield'
  where
  yield' ei cl = aux =<< yield ei cl
  aux [] = argFail . E.argMissing $ head ais
  aux xs = return $ last xs
