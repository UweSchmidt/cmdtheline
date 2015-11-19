{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Help
  ( printVersion, invocation, prepSynopsis, print ) where

import Prelude hiding ( print )

import System.Console.CmdTheLine.Common
import qualified System.Console.CmdTheLine.Manpage as Man

import Control.Applicative
import Control.Arrow       ( first )

import Data.Char     ( toUpper, toLower )
import Data.List     ( intersperse, sort, sortBy, partition, foldl' )
import Data.Function ( on )

import System.IO hiding ( print )


invocation :: Char -> EvalInfo -> String
invocation sep ei = case evalKind ei of
  Choice -> progName ++ [sep] ++ choiceName
  _      -> progName
  where
  progName   = termName . fst $ main ei
  choiceName = termName . fst $ term ei

title :: EvalInfo -> Title
title ei = ( invocName, 1, "", leftFooter, centerHeader )
  where
  invocName = map toUpper $ invocation '-' ei

  leftFooter = prog ++ ver
    where
    ver = case version . fst $ main ei of
      ""  -> ""
      str -> ' ' : str 

  centerHeader = prog ++ " Manual"

  prog = capitalize progName
    where
    capitalize = (:) <$> toUpper . head <*> drop 1
    progName   = termName . fst $ main ei

nameSection :: EvalInfo -> [ManBlock]
nameSection ei =
  [ S "NAME"
  , P $ invocation '-' ei ++ prep (termDoc . fst $ term ei)
  ]
  where
  prep "" = ""
  prep doc = " - " ++ doc

synopsis :: EvalInfo -> String
synopsis ei = case evalKind ei of
  Main   -> concat [ "$(b,", invocation ' ' ei, ") $(i,COMMAND) ..." ]
  _      -> concat [ "$(b,", invocation ' ' ei, ") [$(i,OPTION)]... ", args ]
  where
  args = concat . intersperse " " $ map snd args'
    where
    args' = sortBy compare' . foldl' formatPos [] . snd $ term ei

  formatPos acc ai
    | isOpt ai  = acc
    | otherwise = ( posKind ai, v'' ) : acc
    where
    v | argName ai == "" = "$(i,ARG)"
      | otherwise        = concat [ "$(i,", argName ai, ")" ]

    v' | absence ai == Absent = v
       | otherwise            = concat [ "[", v, "]" ]

    v'' = v' ++ followedBy

    followedBy = case posKind ai of
      PosN _ _ -> ""
      _        -> "..."

  compare' ( p, _ ) ( p', _ ) = case ( p', p ) of
    ( _,          PosAny    ) -> LT
    ( PosAny,     _         ) -> GT
    ( PosL  _  _, PosR  _ _ ) -> LT
    ( PosR  _  _, PosL  _ _ ) -> GT
    _ -> comparePos k k'
    where
    comparePos
      | getBool p && getBool p' = case ( p, p' ) of
        ( PosL _ _, PosN _ _ ) -> flip compare -- if k >= k' then LT else GT
        ( PosN _ _, PosL _ _ ) -> compare -- if k <= k' then GT else LT
        ( PosN _ _, _        ) -> flip compare -- if k >= k' then LT else GT
        _                      -> compare -- if k <= k' then GT else LT

      | otherwise = case ( p, p' ) of
        ( PosL _ _, PosN _ _ ) -> compare -- if k <= k' then LT else GT
        ( PosN _ _, PosL _ _ ) -> flip compare -- if k >= k' then GT else LT
        ( PosN _ _, _        ) -> compare -- if k <= k' then LT else GT
        _                      -> flip compare -- if k >= k' then GT else LT

    k  = getPos p
    k' = getPos p'

    getPos x = case x of
      PosL  _ pos -> pos
      PosR  _ pos -> pos
      PosN  _ pos -> pos
      _ -> undefined

    getBool x = case x of
      PosL  b _ -> b
      PosR  b _ -> b
      PosN  b _ -> b
      _ -> undefined

synopsisSection :: EvalInfo -> [ManBlock]
synopsisSection ei = [ S "SYNOPSIS", P (synopsis ei) ]

makeArgLabel :: ArgInfo -> String
makeArgLabel ai
  | isPos ai  = concat [ "$(i,", argName ai, ")" ]
  | otherwise = concat . intersperse ", " $ map (fmtName var) names
  where
  var | argName ai == "" = "VAL"
      | otherwise        = argName ai

  names = sort $ optNames ai

  fmtName var = case optKind ai of
    FlagKind   -> \ name -> concat [ "$(b,", name, ")" ]
    OptKind    -> mkOptMacro
    OptVal   _ -> mkOptValMacro
    where
    mkOptValMacro name = concat [ "$(b,", name, ")[", sep, "$(i,", var, ")]" ]
      where
      sep | length name > 2 = "="
          | otherwise       = ""

    mkOptMacro name = concat [ "$(b,", name, ")", sep, "$(i,", var, ")" ]
      where
      sep | length name > 2 = "="
          | otherwise       = " "

makeArgItems :: EvalInfo -> [( String, ManBlock )]
makeArgItems ei = map format xs
  where
  xs = sortBy revCmp . filter isArgItem . snd $ term ei

  isArgItem ai = not $ isPos ai && (argName ai == "" || argDoc ai == "")

  revCmp ai' ai = if secCmp /= EQ then secCmp else compare' ai ai'
    where
    secCmp = (compare `on` argSec) ai ai'

    compare' = case ( isOpt ai, isOpt ai' ) of
      ( True,  True  ) -> compare `on` key . optNames
      ( False, False ) -> compare `on` map toLower . argName
      ( True,  False ) -> const $ const LT
      ( False, True  ) -> const $ const GT

    key names
      | k !! 1 == '-' = drop 2 k
      | otherwise     = k
      where
      k = map toLower . head $ sortBy descCompare names

  format ai = ( argSec ai, I label text )
    where
    label = makeArgLabel ai ++ argvDoc
    text  = substDocName (argName ai) (argDoc ai)

    argvDoc = case ( absent, optvOpt ) of
      ( "", "" ) -> ""
      ( s,  "" ) -> concat [ " (", s, ")" ]
      ( "", s  ) -> concat [ " (", s, ")" ]
      ( s,  s' ) -> concat [ " (", s, ", ", s', ")" ]


    absent = case absence ai of
      Absent     -> ""
      Present "" -> ""
      Present v  -> "absent=" ++ v

    optvOpt = case optKind ai of
      OptVal v -> "default=" ++ v
      _        -> ""

  substDocName argName =
    Man.substitute (const id) [( "argName", ("$(i," ++ argName ++ ")") )]

makeCmdItems :: EvalInfo -> [( String, ManBlock )]
makeCmdItems ei = case evalKind ei of
  Simple -> []
  Choice -> []
  Main   -> sortBy (descCompare `on` fst) . foldl' addCmd [] $ choices ei
  where
  addCmd acc ( ti, _ ) = ( termSec ti, I (label ti) (termDoc ti) )
                       : acc
  label ti = "$(b," ++ termName ti ++ ")"

mergeOrphans :: ( [( String, ManBlock )], [Maybe ManBlock] ) -> [ManBlock]
mergeOrphans ( orphans, marked ) = fst $ foldl' go ( [], orphans ) marked
  where
  go ( acc, orphans ) (Just block) = ( block : acc, orphans )
  go ( acc, orphans ) Nothing      = ( acc',        []      )    
    where
    acc' = case orphans of
      []           -> acc
      ( s, _ ) : _ -> let ( result, s' ) = foldl' merge ( acc, s ) orphans
                      in  S s' : result

  merge ( acc, secName ) ( secName', item )
    | secName == secName' = ( item : acc,             secName  )
    | otherwise           = ( item : S secName : acc, secName' )

mergeItems :: [( String, ManBlock )] -> [ManBlock]
           -> ( [( String, ManBlock )], [Maybe ManBlock] )
mergeItems items blocks = ( orphans, marked )
  where
  ( marked, _, _, orphans ) = foldl' go ( [Nothing], [], False, items ) blocks
  
  -- 'toInsert' is a list of manblocks that belong in the current section.
  go ( acc, toInsert, mark, items ) block = case block of
    sec@(S _) -> transition sec
    t         -> ( Just t : acc, toInsert, mark, items )
    where
    transition sec@(S str) = ( acc', toInsert', mark', is' )
      where
      ( toInsert', is' ) = first (map snd) $ partition ((== str) . fst) items
      acc'               = Just sec : marked
      mark'              = str == "DESCRIPTION"
    transition _ = undefined

    marked = if mark then Nothing : acc' else acc'
      where
      acc' = map Just toInsert ++ acc

text :: EvalInfo -> [ManBlock]
text ei = mergeOrphans . mergeItems items . man . fst $ term ei
  where
  cmds  = makeCmdItems ei
  args  = makeArgItems ei
  cmp   = descCompare `on` fst
  items = sortBy cmp $ cmds ++ args

eiSubst :: EvalInfo -> [( String, String )]
eiSubst ei =
  [ ( "tname", termName . fst $ term ei )
  , ( "mname", termName . fst $ main ei )
  ]

page :: EvalInfo -> ( Title, [ManBlock] )
page ei = ( title ei, nameSection ei ++ synopsisSection ei ++ text ei )

print :: HelpFormat -> Handle -> EvalInfo -> IO ()
print fmt h ei = Man.print (eiSubst ei) fmt h (page ei)

prepSynopsis :: EvalInfo -> String
prepSynopsis ei = escape $ synopsis ei
  where
  escape = Man.substitute Man.plainEsc $ eiSubst ei

printVersion :: Handle -> EvalInfo -> IO ()
printVersion h ei = case version . fst $ main ei of
  ""  -> error "printVersion called on EvalInfo without version"
  str -> hPutStrLn h str
