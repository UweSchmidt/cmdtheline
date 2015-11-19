{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Trie
  ( add, lookup, empty, isEmpty, fromList, ambiguities
  , Trie, LookupFail(..)
  ) where

import Prelude hiding ( lookup )
import qualified Data.Map as M
import Data.List (foldl')

{-
 - This implementation maps any non-ambiguous prefix of a key to its value.
 -}

type CMap = M.Map Char

data Value a = Pre a -- Value bound by a prefix key.
             | Key a -- Value bound by an entire key.
             | Amb   -- No Value bound due to ambiguity in the key.
             | Nil   -- Attempt to retrieve a Value from an empty Trie.
               deriving ( Eq )

data Trie a = Trie
  { val   :: Value a
  , nexts :: CMap (Trie a)
  } deriving ( Eq )

data LookupFail = Ambiguous | NotFound deriving (Show)

empty :: Trie a
empty = Trie Nil M.empty

isEmpty :: Eq a => Trie a -> Bool
isEmpty = (== empty)

add :: String -> a -> Trie a -> Trie a
add k v t = go t k
  where
  go t s = case s of
    []     -> Trie (Key v) next
    c:rest ->
      let t'       = maybe empty id $ M.lookup c next
          newNexts = M.insert c (go t' rest) next 
      in Trie newVal newNexts
    where
    next = nexts t

    newVal = case val t of
      Amb        -> Amb
      Pre _      -> Amb
      v'@(Key _) -> v'
      Nil        -> Pre v

findNode :: Trie a -> String -> Maybe (Trie a)
findNode t = foldl' go (Just t)
  where
  go acc c = M.lookup c . nexts =<< acc

lookup :: String -> Trie a -> Either LookupFail a
lookup k t = case findNode t k of
  Nothing -> Left NotFound
  Just t' -> case val t' of
    Key v -> Right v
    Pre v -> Right v
    Amb   -> Left  Ambiguous
    Nil   -> Left  NotFound

ambiguities :: Trie a -> String -> [String]
ambiguities t pre = case findNode t pre of
  Nothing -> []
  Just t' -> case val t' of
    Amb -> go [] pre $ M.toList (nexts t') : []
    _   -> []

  where
  go acc pre assocs = case assocs of
    []        -> error "saw lone empty list while searching for ambiguities"
    [[]]      -> acc
    [] : rest -> go acc (init pre) rest
    _         -> descend assocs
    where
    descend ((top : bottom) : rest) = go acc' pre' assocs'
      where
      ( c, t'' ) = top

      assocs'    = M.toList (nexts t'') : bottom : rest

      pre'       = pre ++ return c
      acc'       = case val t'' of
        Key _ -> pre' : acc
        Nil   -> error "saw Nil on descent"
        _     -> acc

    -- FIXME: Handle this better.  At least produce a meaningful error message.
    descend _ = undefined

fromList :: [( String, a )] -> Trie a
fromList assoc = foldl' consume empty assoc
  where
  consume t ( k, v ) = add k v t
