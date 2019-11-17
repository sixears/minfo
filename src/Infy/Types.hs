{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Infy.Types
  ( Artist, Dateish )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON( parseJSON ), ToJSON( toJSON )
                         , Value( String ), typeMismatch )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.String    ( IsString, String )
import Data.Word      ( Word8, Word16 )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual, toText )

-- finite-typelits ---------------------

import Data.Finite  ( Finite )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Tasty    ( (≟) )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary
                              ( Arbitrary( arbitrary ), arbitraryBoundedIntegral
                              , arbitrarySizedNatural )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( propInvertibleText, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Property, forAll, ioProperty, testProperty )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

newtype Artist = Artist Text
  deriving (Eq, FromJSON, IsString, Show, ToJSON)

instance Printable Artist where
  print (Artist t) = P.text t

{-
instance FromJSON Artist where
  parseJSON (String t) = return (Artist t)
  parseJSON invalid    = typeMismatch "String" invalid
-}

{-
instance ToJSON Artist where
  toJSON (Artist t) = String t
-}

------------------------------------------------------------

newtype Day   = Day   (Finite 31)
  deriving (Eq,Show)
newtype Month = Month (Finite 12)
  deriving (Eq,Show)
newtype Year  = Year  (Finite 200)
  deriving (Eq,Show)

{- | A specialist data type for Music/Info dates -}
data Dateish = Dateish    Year Month Day
             | DateishDs  Year Month (Day,Day)
             | DateishM   Year Month
             | DateishMs  Year (Month,Month)
             | DateishR   Year (Month,Day) (Month,Day)
             | DateishRY  (Year,Month,Day) (Year,Month,Day)
             | DateishY   Year
             | DateishYs  (Year,Year)
  deriving (Eq,Show)

instance Printable Dateish where
  print (Dateish (Year y) (Month m) (Day d)) =
    P.text $ [fmt|%04d-%02d-%02d|] y m d
  print (DateishDs (Year y) (Month m) (Day d0,Day d1)) =
    P.text $ [fmt|%04d-%02d-%02d:%02d|] y m d0 d1
  print (DateishM  (Year y) (Month m)) = P.text $ [fmt|%04d-%02d|] y m
  print (DateishMs (Year y) (Month m0,Month m1)) =
    P.text $ [fmt|%04d-%02d:%02d|] y m0 m1
  print (DateishR (Year y) (Month m0,Day d0) (Month m1,Day d1)) =
    P.text $ [fmt|%04d-%02d-%02d:%02d-%02d|] y m0 d0 m1 d1
  print (DateishRY (Year y0,Month m0,Day d0) (Year y1,Month m1,Day d1)) =
    P.text $ [fmt|%04d-%02d-%02d:%04d-%02d-%02d|] y0 m0 d0 y1 m1 d1
  print (DateishY (Year y)) = P.text $ [fmt|%04d|] y
  print (DateishYs (Year y0,Year y1)) = P.text $ [fmt|%04d:%04d|] y0 y1

dateishPrintableTests ∷ TestTree
dateishPrintableTests =
  testGroup "Printable"
            [ testCase "2019-11-14" $
                "2019-11-14" ≟ toText (Dateish (Year 2019) (Month 11) (Day 14))
            ]

instance Textual Dateish

instance Arbitrary Dateish

dateishTextualTests ∷ TestTree
dateishTextualTests =
  testGroup "Textual"
            [ testProperty "invertibleText" (propInvertibleText @Dateish) ]


dateishTests ∷ TestTree
dateishTests = testGroup "Dateish" [ dateishPrintableTests ]


-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Infy.Types" [ dateishTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
