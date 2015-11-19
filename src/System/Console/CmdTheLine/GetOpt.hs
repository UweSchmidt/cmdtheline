-- | Adapter for "System.Console.GetOpt".
module System.Console.CmdTheLine.GetOpt where

import Data.Maybe
import Data.Traversable
import System.Console.GetOpt
import System.Console.CmdTheLine

-- | Sequence a list of @'OptDescr's@ into a term. Absent flags
-- (specified with 'NoArg') are filtered out.
optDescrsTerm :: [OptDescr a] -> Term [a]
optDescrsTerm = fmap catMaybes . sequenceA . map optDescrToTerm

-- | Convert an 'OptDescr' into a 'Term' which returns 'Nothing' if
-- 'NoArg' is specified and the flag is absent or 'Just' the argument
-- otherwise.
optDescrToTerm :: OptDescr a -> Term (Maybe a)
optDescrToTerm (Option shorts longs argDescr descr) =
    case argDescr of
      NoArg x        -> fmap (optional x) $ value $ flag                       $ optInf ""
      ReqArg to name -> fmap (fmap   to)  $ value $ opt                Nothing $ optInf name
      OptArg to name -> fmap (Just . to)  $ value $ defaultOpt Nothing Nothing $ optInf name
    where
      optional :: a -> Bool -> Maybe a
      optional x present | present   = Just x
                         | otherwise = Nothing

      optInf :: String -> OptInfo
      optInf name = (optInfo options) { optDoc  = descr
                                      , optName = name
                                      }

      options :: [String]
      options = map (:[]) shorts ++ longs
