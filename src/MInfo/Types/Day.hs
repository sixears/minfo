{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}

module MInfo.Types.Day
  ( Day( Day ), day, day', __day, __day' )
where

import Prelude  ( Integer, Integral, (+), (-), error, fromInteger, toInteger )

-- base --------------------------------

import Control.Monad  ( fail, return )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ), maybe )
import Data.Ord       ( Ord )
import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual ), fromText )
import Data.Textual.Integral  ( Decimal( Decimal ), nnUpTo )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( propInvertibleText, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.BoundedN        ( 𝕎, pattern 𝕎, 𝕨 )
import MInfo.Types.ToWord16  ( ToWord16( toWord16 ) )

--------------------------------------------------------------------------------

ePatSymExhaustive ∷ α
ePatSymExhaustive = error "https://gitlab.haskell.org/ghc/ghc/issues/10339"

------------------------------------------------------------

newtype Day = Day (𝕎 31)
  deriving (Eq,Ord,Show)

day ∷ Integral α ⇒ α → Maybe Day
day i = Day ⊳ 𝕨 (toInteger i-1)

day' ∷ Integer → Maybe Day
day' = day

__day ∷ Integral α ⇒ α → Day
__day i = case day i of
            Just  d → d
            Nothing → error $ [fmt|day %d out of range|] i

__day' ∷ Integer → Day
__day' = __day

instance ToWord16 Day where
  toWord16 (Day (𝕎 i)) = fromInteger i + 1
  toWord16 (Day _)    = ePatSymExhaustive

instance Printable Day where
  print d = P.text $ [fmt|%d|] (toWord16 d)

instance Textual Day where
  textual = do
    m ← nnUpTo Decimal 2
    maybe (fail $ [fmt|bad day value %d|] m) return $ day' m

dayTextualTests ∷ TestTree
dayTextualTests =
  testGroup "Textual"
            [ testCase "12" $ Just (__day' 12) ≟ fromText "12"
            , testCase  "0" $ Nothing @Day     ≟ fromText  "0"
            , testCase "32" $ Nothing @Day     ≟ fromText "32"
            , testCase "31" $ Just (__day' 31) ≟ fromText "31"
            , testProperty "invertibleText" (propInvertibleText @Day)
            ]


instance Arbitrary Day where
  arbitrary = Day ⊳ arbitrary

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Day" [ dayTextualTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
