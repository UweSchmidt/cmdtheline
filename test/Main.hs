{-# LANGUAGE CPP #-}
module Main where

import qualified FizzBuzz as FB
import qualified Arith    as Ar
import qualified Cipher   as Ci
import qualified CP

import Test.HUnit hiding ( Test )
import Test.Framework ( defaultMain, testGroup, Test )
import Test.Framework.Providers.HUnit

import GHC.IO.Handle ( hDuplicate, hDuplicateTo )
import System.IO
import System.Directory ( getTemporaryDirectory )
import System.FilePath  ( (</>) )

import System.Console.CmdTheLine ( unwrap, unwrapChoice, EvalExit )
import System.Console.CmdTheLine.Manpage ( substitute, plainEsc )

import Control.Applicative ( (<$>) )

isRight, isLeft :: Either a b -> Bool

isRight (Right _) = True
isRight (Left  _) = False

isLeft (Left  _) = True
isLeft (Right _) = False

devNull :: String
#if defined(mingw_32_HOST_OS) || defined(__MINGW32__)
devNull = "nul"
#else
devNull = "/dev/null"
#endif

mute :: IO a -> IO a
mute action = do
  withFile devNull WriteMode $ (\ h -> do
    stdout_bak <- hDuplicate stdout
    hDuplicateTo h stdout

    v <- action

    hDuplicateTo stdout_bak stdout
    return v)

withInput :: [String] -> IO a -> IO a
withInput input action = do
  tmpDir <- getTemporaryDirectory
  withFile (tmpDir </> "cmdtheline_test_input") ReadWriteMode (\ h -> do
    stdin_bak <- hDuplicate stdin
    hDuplicateTo h stdin

    mapM_ (hPutStrLn stdin) input
    v <- action

    hDuplicateTo stdin_bak stdin
    return v)

unwrapFB, unwrapAr, unwrapCP, unwrapCi :: [String] -> IO (Either EvalExit (IO ()))
unwrapFB args = unwrap args ( FB.term, FB.termInfo )
unwrapAr args = unwrap args ( Ar.arithTerm, Ar.termInfo )
unwrapCP args = unwrap args ( CP.term, CP.termInfo )
unwrapCi args = unwrapChoice args Ci.defaultTerm [ Ci.rotTerm, Ci.morseTerm ]

tests :: [Test]
tests =
  -- With FizzBuzz we'll test the different forms of option assignment and
  -- flags.
  [ testGroup "FizzBuzz"
    [ testCase " w/o args" . assert $ isRight <$> unwrapFB []
    , testCase " w/ good args" . assert $
      isRight <$> unwrapFB [ "-q", "-s", "-v"
                           , "-f", "bob", "--buzz", "ann", "-t20"
                           ]
    , testCase " w/ bad args" . assert $ isLeft <$> unwrapFB [ "-zork" ]
    ]

  -- With arith we'll test 'posRight' 'pos' positional argument partitioning.
  , testGroup "arith"
    [ testCase " w/o args" . assert $ isLeft <$> unwrapAr []
    , testCase " w/ good args many" . assert $
      isRight <$> unwrapAr [ "x^2+y*1", "x=2", "y=(11-1)/5" ]
    , testCase " w/ good args one" . assert $
      isRight <$> unwrapAr [ "x^2", "x=2" ]
    , testCase " w/ bad args" . assert $
      isLeft <$> unwrapAr [ "-pretty", "x^2", "x=2" ]
    ]

  -- With cipher we'll test subcommands.
  , testGroup "cipher"
    [ testCase " w/o args" . assert $ isLeft <$> unwrapCi []
    , testCase " morse" . assert $
      isRight <$> withInput ["bob"] (unwrapCi [ "morse" ])
    , testCase " rot" . assert $
      isRight <$> withInput ["bob"] (unwrapCi [ "rot" ])
    ]

  -- With cp we'll test 'revPosLeft' 'revPos' positional argument partitioning.
  , testGroup "cp"
    [ testCase " w/o args" . assert $ isLeft <$> unwrapCP []
    , testCase " w/ good args many" . assert $
      isRight <$> unwrapCP [ "-d", "cmdtheline.cabal", "LICENSE", "test" ]
    , testCase " w/ good args one" . assert $
      isRight <$> unwrapCP [ "-d", "cmdtheline.cabal", "foo" ]
    ]

  , testGroup "unittests"
    [ testCase " escaping $(i,...)" . assert $ substitute plainEsc [] "$(i,foo)" == "foo"
    , testCase " escaping $(b,...)" . assert $ substitute plainEsc [] "$(b,foo)" == "foo"
    ]
  ]

main :: IO ()
main = defaultMain tests
