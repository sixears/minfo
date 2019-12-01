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

module MInfo.Types.Month
  ( Month( Month ), month, month', __month, __month' )
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

import Test.QuickCheck.Arbitrary ( Arbitrary( arbitrary ) )

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

newtype Month = Month (𝕎 12)
  deriving (Eq,Ord,Show)

month ∷ Integral α ⇒ α → Maybe Month
month i = Month ⊳ 𝕨 (toInteger i-1)

month' ∷ Integer → Maybe Month
month' = month

__month ∷ Integral α ⇒ α → Month
__month i = case month i of
            Just  d → d
            Nothing → error $ [fmt|month %d out of range|] i

__month' ∷ Integer → Month
__month' = __month

instance ToWord16 Month where
  toWord16 (Month (𝕎 i)) = fromInteger i + 1
  toWord16 (Month _)      = ePatSymExhaustive

instance Printable Month where
  print m = P.text $ [fmt|%d|] (toWord16 m)

instance Textual Month where
  textual = do
    m ← nnUpTo Decimal 2
    maybe (fail $ [fmt|bad month value %d|] m) return $ month' m

monthTextualTests ∷ TestTree
monthTextualTests =
  testGroup "Textual"
            [ testCase "12" $ Just (__month' 12) ≟ fromText "12"
            , testCase  "0" $ Nothing @Month     ≟ fromText  "0"
            , testCase "13" $ Nothing @Month     ≟ fromText "13"
            , testProperty "invertibleText" (propInvertibleText @Month)
            ]

instance Arbitrary Month where
  arbitrary = Month ⊳ arbitrary

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Month" [ monthTextualTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
