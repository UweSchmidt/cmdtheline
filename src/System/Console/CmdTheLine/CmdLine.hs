{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.CmdLine
  ( create, optArg, posArg ) where

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.Err as E

import Control.Applicative
import Control.Arrow ( second )

import Control.Monad.Trans.Except ( throwE )

import Text.PrettyPrint
import Text.Parsec as P

import qualified System.Console.CmdTheLine.Trie as T
import qualified Data.Map as M

import Data.List ( sort, foldl' )

optArg :: CmdLine -> ArgInfo -> [( Int, String, Maybe String )]
optArg cl ai = case M.lookup ai cl of
  Nothing  -> error "ArgInfo passed to optArg does not index CmdLine"
  Just arg -> case arg of
    Opt opt -> opt
    _       -> error "ArgInfo passed to optArg indexes to positional argument"

posArg :: CmdLine -> ArgInfo -> [String]
posArg cl ai = case M.lookup ai cl of
  Nothing  -> error "ArgInfo passed to posArg does not index CmdLine"
  Just arg -> case arg of
    Pos opt -> opt
    _       -> error "ArgInfo passed to posArg indexes to positional argument"

{- Returns a trie mapping the names of optional arguments to their ArgInfo, a
 - list with all ArgInfo for positional arguments, and a CmdLine mapping each
 - ArgInfo to an empty list of Arg.
 -}
argInfoIndexes :: [ArgInfo] -> ( T.Trie ArgInfo, [ArgInfo], CmdLine )
argInfoIndexes = foldl' go ( T.empty, [], M.empty )
  where
  go ( optTrie, posAis, cl ) ai
    | isPos ai  = ( optTrie
                  , ai : posAis
                  , M.insert ai (Pos []) cl
                  )
    | otherwise = ( foldl' add optTrie $ optNames ai
                  , posAis
                  , M.insert ai (Opt []) cl
                  )
    where
    add t name = T.add name ai t

parseOptArg :: String -> ( String, Maybe String )
parseOptArg str
  -- 'str' is a short name.
  | str !! 1 /= '-' =
    if length str == 2
       then ( str,        Nothing )           -- No glued argument.
       else ( take 2 str, Just $ drop 2 str ) -- Glued argument.

  -- 'str' is a long name.
  | otherwise       = case P.parse assignment "" str of
    Left _       -> ( str, Nothing ) --  No glued argument
    Right result -> result           --  Glued argument
    where
    assignment = do
      label <- P.many1 $ P.satisfy (/= '=')
      value <- optionMaybe $ P.char '=' >> P.many1 P.anyChar
      return ( label, value )

{- Returns an updated CmdLine according to the options found in 'args'
 - with the trie index 'optTrie'.  Positional arguments are returned in order.
 -}
parseArgs :: T.Trie ArgInfo -> CmdLine -> [String]
          -> Err ( CmdLine, [String] )
parseArgs optTrie cl args = second ((++ rest) . reverse) <$> go 1 cl [] args'
  where
  -- Everything after '"--"' is a position argument.
  ( args', rest ) = splitOn "--" args
  go k cl posArgs args = case args of
    []         -> return ( cl, posArgs )
    str : rest ->
      if isOpt str
         then asignOptValue str rest
         else go (k + 1) cl (str : posArgs) rest
    where
    isOpt str = length str > 1 && head str == '-'

    asignOptValue str rest = either handleErr addOpt $ T.lookup name optTrie
      where
      ( name, value ) = parseOptArg str

      addOpt ai = go (k + 1) cl' posArgs rest'
        where
        cl'     = M.insert ai optArgs cl
        optArgs = Opt $ ( k, name, value' ) : optArg cl ai

        ( value', rest' )
          -- If the next string can't be assigned to this argument, don't
          -- skip it.
          | value /= Nothing || optKind ai == FlagKind ||
            rest == []       || isOpt (head rest)      = ( value, rest )
          -- Else the next string is the value of this argument, consume it.
          | otherwise                                  = ( Just $ head rest
                                                         , tail rest
                                                         )

      handleErr T.NotFound  = throwE $ UsageFail unknown
      handleErr T.Ambiguous = throwE $ UsageFail ambiguous

      unknown   = E.unknown   "option" name
      ambiguous = E.ambiguous "option" name ambs
        where
        ambs = sort $ T.ambiguities optTrie name


{- Returns an updated CmdLine in which each positional arg mentioned in the
 - list index 'posInfo', is given a value according to the list of positional
 - argument values 'args'.
 -}
processPosArgs :: [ArgInfo] -> ( CmdLine, [String] ) -> Err CmdLine
processPosArgs _       ( cl, [] ) = return cl
processPosArgs posInfo ( cl, args )
  | last <= maxSpec = return     cl'
  | otherwise       = throwE $ UsageFail excess
  where
  last   = length args - 1
  excess = E.posExcess . map text $ takeEnd (last - maxSpec) args

  ( cl', maxSpec ) = foldl' go ( cl, -1 ) posInfo

  takeEnd n = reverse . take n . reverse

  go ( cl, maxSpec ) ai = ( cl', maxSpec' )
    where
    cl'               = M.insert ai arg cl
    ( arg, maxSpec' ) = case posKind ai of
      PosAny       -> ( Pos args, last )
      PosN rev pos -> result rev pos False indexPositions
      PosL rev pos -> result rev pos False take
      PosR rev pos -> result rev pos True  (takeEnd . (last -))

    indexPositions pos args = [args !! pos]

    result rev pos maxIsLast getPositions
      | pos' < 0 || cmp pos' last = ( Pos [], maxSpec'' )
      | otherwise                 = ( Pos $ getPositions pos' args
                                    , maxSpec''
                                    )
      where
      pos'      = if rev       then last - pos else pos
      cmp       = if maxIsLast then (>=)       else (>)
      maxSpec'' = if maxIsLast then last       else max pos' maxSpec

create :: [ArgInfo] -> [String] -> Err CmdLine
create ais args = processPosArgs posAis =<< parseArgs optTrie cl args
  where
  ( optTrie, posAis, cl ) = argInfoIndexes ais
