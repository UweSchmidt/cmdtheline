{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
{-# LANGUAGE FlexibleInstances #-}
module System.Console.CmdTheLine.Term
  (
  -- * Evaluating Terms
  -- ** Simple command line programs
    eval, exec, run, unwrap

  -- ** Multi-command command line programs
  , evalChoice, execChoice, runChoice, unwrapChoice

  -- * Exit information for testing
  , EvalExit(..)
  ) where

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.CmdLine ( create )
import System.Console.CmdTheLine.Arg
import qualified System.Console.CmdTheLine.Err     as E
import qualified System.Console.CmdTheLine.Help    as H
import qualified System.Console.CmdTheLine.Trie    as T

import Control.Arrow       ( second )
import Control.Monad       ( join, (<=<) )

import Control.Monad.Trans.Except

import Data.List    ( find, sort )
import Data.Maybe   ( fromJust )

import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import System.IO

import Text.PrettyPrint


--
-- EvalErr
--

-- | Information about the way a 'Term' exited early.  Obtained by either
-- 'unwrap'ing or 'unwrapChoice'ing some Term.  Handy for testing programs when
-- it is undesirable to exit execution of the entire program when a Term exits
-- early.
data EvalExit = Help  HelpFormat (Maybe String)
              | Usage Doc
              | Msg   Doc
              | Version

type EvalErr = ExceptT EvalExit IO

fromFail :: Fail -> EvalExit
fromFail (MsgFail   d)         = Msg   d
fromFail (UsageFail d)         = Usage d
fromFail (HelpFail  fmt mName) = Help  fmt mName

fromErr :: Err a -> EvalErr a
fromErr = mapExceptT . fmap $ either (Left . fromFail) Right

printEvalErr :: EvalInfo -> EvalExit -> IO a
printEvalErr ei fail = case fail of
  Usage doc -> do E.printUsage   stderr ei doc
                  exitFailure
  Msg   doc -> do E.print        stderr ei doc
                  exitFailure
  Version   -> do H.printVersion stdout ei
                  exitSuccess
  Help fmt mName -> do either print (H.print fmt stdout) (eEi mName)
                       exitSuccess
  where
  -- Either we are in the default Term, or the commands name is in `mName`.
  eEi = maybe (Right ei { term = main ei }) process

  -- Either the command name exists, or else it does not and we're in trouble.
  process name = do
    cmd' <- case find (\ ( i, _ ) -> termName i == name) (choices ei) of
      Just x  -> Right x
      Nothing -> Left  $ E.errHelp (text name)
    return ei { term = cmd' } 


--
-- Standard Options
--

addStdOpts :: EvalInfo -> ( Yield (Maybe HelpFormat)
                          , Maybe (Yield Bool)
                          , EvalInfo
                          )
addStdOpts ei = ( hLookup, vLookup, ei' )
  where
  ( args, vLookup ) = case version . fst $ main ei of
    "" -> ( [],  Nothing )
    _  -> ( ais, Just lookup )
    where
    Term ais lookup = value
                    $ flag (optInfo ["version"])
                    { optSec = section
                    , optDoc = "Show version information."
                    }

  ( args', hLookup ) = ( ais ++ args, lookup )
    where
    Term ais lookup = value
                    $ defaultOpt (Just Pager) Nothing (optInfo ["help"])
                    { optSec  = section
                    , optName = "FMT"
                    , optDoc  = doc
                    }

  section = stdOptSec . fst $ term ei
  doc     = "Show this help in format $(argName) (pager, plain, or groff)."

  addArgs = second (args' ++)
  ei'     = ei { term = addArgs $ term ei
               , main = addArgs $ main ei
               , choices = map addArgs $ choices ei
               }


--
-- Evaluation of Terms
--

-- For testing of term, unwrap the value but do not handle errors.
unwrapTerm :: EvalInfo -> Yield a -> [String] -> IO (Either EvalExit a)
unwrapTerm ei yield args = runExceptT $ do
  cl <- fromErr $ create (snd $ term ei) args
  fromErr $ yield ei cl

evalTerm :: EvalInfo -> Yield a -> [String] -> IO a
evalTerm ei yield args = either handleErr return <=< runExceptT $ do
    ( cl, mResult ) <- fromErr $ do
      cl      <- create (snd $ term ei') args
      mResult <- helpArg ei' cl

      return ( cl, mResult )

    let success = fromErr $ yield ei' cl

    case ( mResult, versionArg ) of
      ( Just fmt, _         ) -> throwE $ Help fmt mName
      ( Nothing,  Just vArg ) -> do tf <- fromErr $ vArg ei' cl
                                    if tf
                                       then throwE Version
                                       else success
      _                       -> success

  where
  ( helpArg, versionArg, ei' ) = addStdOpts ei

  mName = if defName == evalName
             then Nothing
             else Just evalName

  defName  = termName . fst $ main ei'
  evalName = termName . fst $ term ei'

  handleErr = printEvalErr ei'


chooseTerm :: TermInfo -> [( TermInfo, a )] -> [String]
           -> Err ( TermInfo, [String] )
chooseTerm ti _       []              = return ( ti, [] )
chooseTerm ti choices args@( arg : rest )
  | length arg > 1 && head arg == '-' = return ( ti, args )

  | otherwise = case T.lookup arg index of
    Right choice      -> return ( choice, rest )
    Left  T.NotFound  -> throwE . UsageFail $ E.unknown   com arg
    Left  T.Ambiguous -> throwE . UsageFail $ E.ambiguous com arg ambs
    where
    index = foldl add T.empty choices
    add acc ( choice, _ ) = T.add (termName choice) choice acc

    com  = "command"
    ambs = sort $ T.ambiguities index arg

mkCommand :: ( Term a, TermInfo ) -> Command
mkCommand ( Term ais _, ti ) = ( ti, ais )
 
-- Prep an EvalInfo suitable for catching errors raised by 'chooseTerm'.
chooseTermEi :: ( Term a, TermInfo ) -> [( Term a, TermInfo )] -> EvalInfo
chooseTermEi mainTerm choices = EvalInfo command command eiChoices
  where
  command   = mkCommand mainTerm
  eiChoices = map mkCommand choices


--
-- User-Facing Functionality
--

type ProcessTo a b = EvalInfo -> Yield a -> [String] -> IO b

-- Share code between 'eval' and 'unwrap' with this HOF.
evalBy :: ProcessTo a b -> [String] -> ( Term a, TermInfo ) -> IO b
evalBy method args termPair@( term, _ ) = method ei yield args
  where
  (Term _ yield) = term
  command = mkCommand termPair
  ei = EvalInfo command command []

-- | 'eval' @args ( term, termInfo )@ allows the user to pass @args@ directly to
-- the evaluation mechanism.  This is useful if some kind of pre-processing is
-- required.  If you do not need to pre-process command line arguments, use one
-- of 'exec' or 'run'.  On failure the program exits.
eval :: [String] -> ( Term a, TermInfo ) -> IO a
eval = evalBy evalTerm

-- | 'exec' @( term, termInfo )@ executes a command line program, directly
-- grabbing the command line arguments from the environment and returning the
-- result upon successful evaluation of @term@.  On failure the program exits.
exec :: ( Term a, TermInfo ) -> IO a
exec term = do
  args <- getArgs
  eval args term

-- | 'run' @( term, termInfo )@ runs a @term@ containing an 'IO' action,
-- performs the action, and returns the result on success. On failure the
-- program exits.
run :: ( Term (IO a), TermInfo ) -> IO a
run = join . exec

-- | 'unwrap' @args ( term, termInfo )@ unwraps a 'Term' without handling errors.
-- The intent is for use in testing of Terms where the programmer would like
-- to consult error state without the program exiting.
unwrap :: [String] -> ( Term a, TermInfo ) -> IO (Either EvalExit a)
unwrap = evalBy unwrapTerm

-- Share code between 'evalChoice' and 'unwrapChoice' with this HOF.
evalChoiceBy :: ProcessTo a b
             -> [String] -> ( Term a, TermInfo ) -> [( Term a, TermInfo )] -> IO b
evalChoiceBy method args mainTerm@( _, termInfo ) choices = do
  ( chosen, args' ) <- either handleErr return =<<
    (runExceptT . fromErr $ chooseTerm termInfo eiChoices args)

  let (Term ais yield) = fst . fromJust . find ((== chosen) . snd)
                       $ mainTerm : choices

      ei = EvalInfo ( chosen, ais ) mainEi eiChoices

  method ei yield args'
  where
  mainEi    = mkCommand mainTerm
  eiChoices = map mkCommand choices

  -- Only handles errors caused by chooseTerm.
  handleErr = printEvalErr (chooseTermEi mainTerm choices)

-- | 'evalChoice' @args mainTerm choices@ is analogous to 'eval', but for
-- programs that provide a choice of commands.
evalChoice :: [String] -> ( Term a, TermInfo ) -> [( Term a, TermInfo )] -> IO a
evalChoice = evalChoiceBy evalTerm

-- | Analogous to 'exec', but for programs that provide a choice of commands.
execChoice :: ( Term a, TermInfo ) -> [( Term a, TermInfo )] -> IO a
execChoice main choices = do
  args <- getArgs
  evalChoice args main choices

-- | Analogous to 'run', but for programs that provide a choice of commands.
runChoice :: ( Term (IO a), TermInfo ) -> [( Term (IO a), TermInfo )] -> IO a
runChoice main = join . execChoice main

-- | Analogous to 'unwrap', but for programs that provide a choice of commands.
unwrapChoice :: [String] -> ( Term a, TermInfo ) -> [( Term a, TermInfo )]
             -> IO (Either EvalExit a)
unwrapChoice = evalChoiceBy unwrapTerm
