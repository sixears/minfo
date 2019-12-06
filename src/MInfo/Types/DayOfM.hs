{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}

module MInfo.Types.DayOfM
  ( DayOfM( DayOfM ), dayOfM, tests )
where

import Prelude  ( (+), (-), error, fromInteger, toInteger )

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

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromText )
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

-- template-haskell --------------------

import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.BoundedN        ( 𝕎, pattern 𝕎, 𝕨 )
import MInfo.Util            ( __fromString, mkQuasiQuoterExp )

import MInfo.Types.FromI     ( FromI( fromI, fromI', __fromI' ) )
import MInfo.Types.ToWord16  ( ToWord16( toWord16 ) )

--------------------------------------------------------------------------------

ePatSymExhaustive ∷ α
ePatSymExhaustive = error "https://gitlab.haskell.org/ghc/ghc/issues/10339"

------------------------------------------------------------

newtype DayOfM = DayOfM (𝕎 31)
  deriving (Eq,Ord,Show)

instance FromI DayOfM where
  fromI i = DayOfM ⊳ 𝕨 (toInteger i-1)

instance ToWord16 DayOfM where
  toWord16 (DayOfM (𝕎 i)) = fromInteger i + 1
  toWord16 (DayOfM _)    = ePatSymExhaustive

instance Printable DayOfM where
  print d = P.text $ [fmt|%d|] (toWord16 d)

instance Textual DayOfM where
  textual = do
    m ← nnUpTo Decimal 2
    maybe (fail $ [fmt|bad day value %d|] m) return $ fromI' m

dayTextualTests ∷ TestTree
dayTextualTests =
  testGroup "Textual"
            [ testCase "12" $ Just (__fromI' 12) ≟ fromText @DayOfM "12"
            , testCase  "0" $ Nothing @DayOfM    ≟ fromText  "0"
            , testCase "32" $ Nothing @DayOfM    ≟ fromText "32"
            , testCase "31" $ Just (__fromI' 31) ≟ fromText @DayOfM "31"
            , testProperty "invertibleText" (propInvertibleText @DayOfM)
            ]


instance Arbitrary DayOfM where
  arbitrary = DayOfM ⊳ arbitrary

dayOfM ∷ QuasiQuoter
dayOfM = mkQuasiQuoterExp "DayOfM" (\ s → ⟦ __fromString @DayOfM s ⟧)

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DayOfM" [ dayTextualTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
