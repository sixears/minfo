{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module MInfo.Types.Month
  ( Month( Month, M ), month, tests )
where

import Prelude  ( Integer, Integral, (+), (-), error, fromInteger, toInteger )

-- base --------------------------------

import Control.Monad  ( fail, return )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ), maybe )
import Data.Ord       ( Ord )
import Data.String    ( String )
import GHC.Generics   ( Generic )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Read      ( readMaybe )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual           ( Printable( print ), Textual( textual )
                              , fromText, toString )
import Data.Textual.Integral  ( Decimal( Decimal ), nnUpTo )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary ( Arbitrary( arbitrary ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertAnyException, propInvertibleText
                  , runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ, Lit( IntegerL ), Pat( ConP, LitP ) )
import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.BoundedN        ( 𝕎, pattern 𝕎, 𝕨 )
import MInfo.Types.ToNum     ( ToNum( toNum, toNumW16 ) )
import MInfo.Util            ( mkQQCP )

import MInfo.Types.FromI     ( FromI( fromI, fromI', __fromI' ) )

--------------------------------------------------------------------------------

ePatSymExhaustive ∷ α
ePatSymExhaustive = error "https://gitlab.haskell.org/ghc/ghc/issues/10339"

------------------------------------------------------------

newtype Month = Month_ { unMonth ∷ 𝕎 12 }
  deriving (Eq,Generic,Lift,NFData,Ord,Show)

instance FromI Month where
  fromI i = Month_ ⊳ 𝕨 (toInteger i-1)

instance ToNum Month where
  toNum (Month_ (𝕎 i)) = fromInteger i + 1
  toNum (Month_ _)      = ePatSymExhaustive

instance Printable Month where
  print m = P.text $ [fmt|%02d|] (toNumW16 m)

monthPrintableTests ∷ TestTree
monthPrintableTests =
  let check s m = testCase s $ s ≟ toString m
   in testGroup "Printable"
                [ check "01"         (Month_ $ 𝕎 0)
                , check "09"         (Month_ $ 𝕎 8)
                , check "12"         (Month_ $ 𝕎 11)
                ]

instance Textual Month where
  textual = do
    m ← nnUpTo Decimal 2
    maybe (fail $ [fmt|bad month value %d|] m) return $ fromI' m

monthTextualTests ∷ TestTree
monthTextualTests =
  testGroup "Textual"
            [ testCase "12" $ Just (__fromI' 12) ≟ fromText @Month "12"
            , testCase  "0" $ Nothing @Month     ≟ fromText  "0"
            , testCase "13" $ Nothing @Month     ≟ fromText "13"
            , testProperty "invertibleText" (propInvertibleText @Month)
            ]

instance Arbitrary Month where
  arbitrary = Month_ ⊳ arbitrary

readY ∷ String → Maybe Month
readY s = readMaybe s ≫ fromI' @Month

readYI ∷ String → Maybe Integer
readYI = toInteger ∘ toNumW16 ⩺ readY

monthPat ∷ Integer → Pat
monthPat i = ConP 'Month_ [ConP '𝕎 [LitP (IntegerL (i-1))]]

monthQQ ∷ String → Maybe ExpQ
monthQQ = (\ m → ⟦m⟧) ⩺ readY

month ∷ QuasiQuoter
month = mkQQCP "Month" monthQQ
                       (\s → maybe (fail $ [fmt|failed to parse month '%s'|] s)
                                   (Just ∘ return ∘ monthPat) $ readYI s)
                                                       
----------------------------------------

pattern Month ∷ Integral α ⇒ α → Month
pattern Month i ← ((+1) ∘ toNum ∘ unMonth → i)
-- not bi-directional, because Month i would be partial (would fail on
-- out-of-bounds values)
--                  where Month i = __fromI i
{- | Short-name convenience alias for `pattern Month` -}
pattern M ∷ Integral α ⇒ α → Month
pattern M i ← ((+1) ∘ toNum ∘ unMonth → i)

monthPatternTests ∷ TestTree
monthPatternTests =
  let one    =  1 ∷ Integer
      seven  =  7 ∷ Integer
      twelve = 12 ∷ Integer
   in testGroup "Pattern"
                [ testCase  "7" $ let Month i = __fromI'  7 in i ≟ seven
                , testCase  "1" $ let Month i = __fromI'  1 in i ≟ one
                , testCase  "0" $ assertAnyException "0 out of bounds" $
                                  let Month i = __fromI'  0 in (i ∷ Integer)
                , testCase "12" $ let Month i = __fromI' 12 in i ≟ twelve
                , testCase "13" $ assertAnyException "13 out of bounds" $
                                  let Month i = __fromI' 13 in (i ∷ Integer)
                ]

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Month" [ monthPrintableTests, monthTextualTests
                          , monthPatternTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
