{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
{-# LANGUAGE FlexibleInstances #-}
module System.Console.CmdTheLine.ArgVal
  (
  -- * Parsing values from the command line
    ArgParser, ArgPrinter, Converter, ArgVal(..), pp, parser

  -- ** Helpers for instantiating ArgVal
  , fromParsec
  , enum
  -- *** Maybe values
  , just
  -- *** List values
  , list
  -- *** Tuple values
  , pair, triple, quadruple, quintuple
  ) where

import System.Console.CmdTheLine.Common ( splitOn, select, HelpFormat(..) )
import qualified System.Console.CmdTheLine.Err as E
import qualified System.Console.CmdTheLine.Trie as T

import Control.Arrow ( first, (***) )
import Data.Function ( on )
import Data.List     ( sort, unfoldr, foldl' )
import Data.Ratio    ( Ratio )
import Data.Tuple    ( swap )

import Control.Applicative hiding ( (<|>), empty )
import Text.Parsec         hiding ( char )
import Text.PrettyPrint


-- | The type of parsers of individual command line argument values.
type ArgParser a = String -> Either Doc a

-- | The type of printers of values retrieved from the command line.
type ArgPrinter a = a -> Doc

-- | A converter is just a pair of a parser and a printer.
type Converter a = ( ArgParser a, ArgPrinter a )

-- | The class of values that can be converted from the command line.
class ArgVal a where
  converter :: Converter a

-- | The parsing part of a 'converter'.
parser :: ArgVal a => ArgParser  a
parser = fst converter

-- | The pretty printing part of a 'converter'.
pp     :: ArgVal a => ArgPrinter a
pp = snd converter

decPoint, digits, sign :: Parsec String () String
decPoint      = string "."
digits        = many1 digit
sign          = option "" $ string "-"

concatParsers :: [Parsec String () String] -> Parsec String () String
concatParsers = foldl' (liftA2 (++)) $ return []

pInteger  :: ( Read a, Integral a ) => Parsec String () a
pFloating :: ( Read a, Floating a ) => Parsec String () a
pInteger      = read <$> concatParsers [ sign, digits ]
pFloating     = read <$> concatParsers [ sign, digits, decPoint, digits ]

-- | 'fromParsec' @onErr p@ makes an 'ArgParser' from @p@ using @onErr@ to
-- produce meaningful error messages.  On failure, @onErr@ will receive a
-- raw string of the value found on the command line.
fromParsec :: ( String -> Doc) -> Parsec String () a -> ArgParser a
fromParsec onErr p str = either (const . Left $ onErr str) Right
                       $ parse p "" str

-- | A converter of 'Maybe' values of 'ArgVal' instances.
-- 
-- Parses as:
--
-- > fmap Just . parser
--
-- Pretty prints as:
--
-- > maybe empty pp
just :: ArgVal a => Converter (Maybe a)
just = ( fmap Just . parser, maybe empty pp )

-- | A converter of enumerated values conveyed as an association list of
-- @( string, value )@ pairs.  Unambiguous prefixes of @string@ map to
-- @value@.
enum :: Eq a => [( String, a )] -> Converter a
enum assoc = ( parser, pp )
  where
  pp val = select notFoundErr $ map (((== val) *** text) . swap) assoc
  notFoundErr = error $ unlines
    [ "System.Console.CmdTheLine.ArgVal.enum pretty printer saw value not in"
    , "provided association list"
    ]

  parser str = case T.lookup str trie of
    Right v           -> Right v
    Left  T.Ambiguous -> Left  $ E.ambiguous "enum value" str ambs
    Left  T.NotFound  -> Left  $ E.invalidVal (text str) expected
    where
    trie = T.fromList assoc

    expected = text "expected" <+> alts
    alts     = E.alts $ map fst assoc

    ambs = sort $ T.ambiguities trie str

-- | @'list' sep@ creates a converter of lists of an 'ArgVal' instance separated
-- by @sep@.
list :: ArgVal a => Char -> Converter [a]
list sep = ( parser', pp' )
  where
  pp' = fsep . punctuate (char sep) . map pp

  parser' str = either (Left . E.element "list" str)
                       Right
                       . sequence $ unfoldr parseElem str
    where
    parseElem []  = Nothing
    parseElem str = Just . first parser $ splitOn sep str

-- | @'pair' sep@ creates a converter of pairs of 'ArgVal' instances separated
-- by @sep@.
pair :: ( ArgVal a, ArgVal b ) => Char -> Converter ( a, b )
pair sep = ( parser', pp' )
  where
  pp' ( x, y ) = pp x <> char sep <+> pp y

  parser' str = do
    case yStr of
      [] -> Left $ E.sepMiss sep str
      _  -> return ()
    case ( eX, eY ) of
      ( Right x, Right y ) -> Right ( x, y )
      ( Left  e, _       ) -> Left $ E.element "pair" xStr e
      ( _,       Left e  ) -> Left $ E.element "pair" yStr e
    where
    ( eX, eY ) = parser *** parser $ xyStr
    xyStr@( xStr, yStr ) = splitOn sep str

-- | @'triple' sep@ creates a converter of triples of 'ArgVal' instances separated
-- by @sep@.
triple :: ( ArgVal a, ArgVal b, ArgVal c ) => Char -> Converter ( a, b, c )
triple sep = ( parser', pp' )
  where
  pp' ( x, y, z ) = pp x <> char sep <+> pp y <> char sep <+> pp z

  parser' str = do
    [ xStr, yStr, zStr ] <-
      if length strs == 3
         then Right strs
         else Left  $ E.sepMiss sep str
    case ( parser xStr, parser yStr, parser zStr ) of
      ( Right x, Right y, Right z ) -> Right ( x, y, z )
      ( Left  e, _     ,  _       ) -> Left $ E.element "pair" xStr e
      ( _,       Left e,  _       ) -> Left $ E.element "pair" yStr e
      ( _,       _     ,  Left e  ) -> Left $ E.element "pair" zStr e
    where
    strs = unfoldr split str

    split []  = Nothing
    split str = Just $ splitOn sep str

-- | @'quadruple' sep@ creates a converter of quadruples of 'ArgVal' instances
-- separated by @sep@.
quadruple :: ( ArgVal a, ArgVal b, ArgVal c, ArgVal d ) =>
  Char -> Converter ( a, b, c, d )
quadruple sep = ( parser', pp' )
  where
  pp' ( x, y, z, w ) =
    pp x <> char sep <+> pp y <> char sep <+> pp z <> char sep <+> pp w

  parser' str = do
    [ xStr, yStr, zStr, wStr ] <-
      if length strs == 4
         then Right strs
         else Left  $ E.sepMiss sep str

    case ( parser xStr, parser yStr, parser zStr, parser wStr ) of
      ( Right x, Right y, Right z, Right w ) -> Right ( x, y, z, w )
      ( Left  e, _     ,  _      , _       ) -> Left $ E.element "pair" xStr e
      ( _,       Left e,  _      , _       ) -> Left $ E.element "pair" yStr e
      ( _,       _     ,  Left e , _       ) -> Left $ E.element "pair" zStr e
      ( _,       _     ,  _      , Left e  ) -> Left $ E.element "pair" wStr e
    where
    strs = unfoldr split str

    split []  = Nothing
    split str = Just $ splitOn sep str

-- | @'quintuple' sep@ creates a converter of quintuples of 'ArgVal' instances
-- separated by @sep@.
quintuple :: ( ArgVal a, ArgVal b, ArgVal c, ArgVal d, ArgVal e ) =>
  Char -> Converter ( a, b, c, d, e )
quintuple sep = ( parser', pp' )
  where
  pp' ( x, y, z, w, v ) =
    pp x <> char sep <+> pp y <> char sep <+> pp z <> char sep <+>
      pp w <> char sep <+> pp v

  parser' str = do
    [ xStr, yStr, zStr, wStr, vStr ] <-
      if length strs == 3
         then Right strs
         else Left  $ E.sepMiss sep str
    case ( parser xStr, parser yStr, parser zStr, parser wStr, parser vStr ) of
      ( Right x, Right y, Right z, Right w, Right v ) -> Right ( x, y, z, w, v )
      ( Left  e, _     ,  _      , _      , _       ) ->
        Left $ E.element "pair" xStr e
      ( _,       Left e,  _      , _      , _       ) ->
        Left $ E.element "pair" yStr e
      ( _,       _     ,  Left e , _      , _       ) ->
        Left $ E.element "pair" zStr e
      ( _,       _     ,  _      , Left e , _       ) ->
        Left $ E.element "pair" wStr e
      ( _,       _     ,  _      , _      , Left e  ) ->
        Left $ E.element "pair" vStr e
    where
    strs = unfoldr split str

    split []  = Nothing
    split str = Just $ splitOn sep str

invalidVal :: String -> String -> Doc
invalidVal = E.invalidVal `on` text

instance ArgVal Bool where
  converter = enum [( "true", True ), ( "false", False )]

instance ArgVal (Maybe Bool) where
  converter = just

instance ArgVal [Char] where
  converter = ( Right, text )

instance ArgVal (Maybe [Char]) where
  converter = just

instance ArgVal Int where
  converter = ( parser, int )
    where
    parser = fromParsec onErr pInteger
      where
      onErr str = invalidVal str "expected an integer"

instance ArgVal (Maybe Int) where
  converter = just

instance ArgVal Integer where
  converter = ( parser, integer )
    where
    parser = fromParsec onErr pInteger
      where
      onErr str = invalidVal str "expected an integer"

instance ArgVal (Maybe Integer) where
  converter = just

instance ArgVal Float where
  converter = ( parser, float )
    where
    parser = fromParsec onErr pFloating
      where
      onErr str = invalidVal str "expected a floating point number"

instance ArgVal (Maybe Float) where
  converter = just

instance ArgVal Double where
  converter = ( parser, double )
    where
    parser = fromParsec onErr pFloating
      where
      onErr str = invalidVal str "expected a floating point number"

instance ArgVal (Maybe Double) where
  converter = just

instance ArgVal (Ratio Integer) where
  converter = ( parser, rational )
    where
    parser = fromParsec onErr
           $ read <$> concatParsers [ int <* spaces
                                    , string "%"
                                    , spaces >> int
                                    ]
      where
      int = concatParsers [ sign, digits ]
      onErr str =
        invalidVal str "expected a ratio in the form '<numerator> % <denominator>'"

instance ArgVal (Maybe (Ratio Integer)) where
  converter = just

instance ArgVal HelpFormat where
  converter = enum [ ( "pager", Pager )
                   , ( "plain", Plain )
                   , ( "groff", Groff )
                   ]

instance ArgVal (Maybe HelpFormat) where
  converter = just
