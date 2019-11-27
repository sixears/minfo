{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}

module MInfo.Types
  ( Artist, Dateish )
where

import Prelude  ( (+), (-), error, fromIntegral, fromInteger )

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON( parseJSON ), ToJSON( toJSON )
                         , Value( String ), typeMismatch )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Bool      ( Bool )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.Ord       ( Ord, (>) )
import Data.String    ( IsString, String )
import Data.Word      ( Word8, Word16 )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode ( (∘) )
import Data.Ord.Unicode      ( (≤) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual, toText )

-- finite-typelits ---------------------

import Data.Finite  ( Finite )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

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

import Test.Tasty.QuickCheck  ( Gen, Property, forAll, ioProperty, oneof
                              , suchThat, testProperty )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import qualified  Data.Time.Calendar  as  Calendar
import Data.Time  ( fromGregorian )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.BoundedN ( 𝕎, pattern 𝕎, 𝕨 )

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

type 𝔹 = Bool

class ToWord16 α where
  toWord16 ∷ α → Word16

newtype Day = Day (𝕎 31)
  deriving (Arbitrary, Eq, Ord, Show)

day i = Day ⊳ 𝕨 (i-1)

__day i = case day i of
            Just  d → d
            Nothing → error $ [fmt|day %d out of range|] i

instance ToWord16 Day where
  toWord16 (Day (𝕎 i)) = fromInteger i + 1

instance Printable Day where
  print d = P.text $ [fmt|%d|] (toWord16 d)

------------------------------------------------------------

newtype Month = Month (𝕎 12)
  deriving (Arbitrary, Eq, Ord, Show)

month i = Month ⊳ 𝕨 (i-1)

__month i = case month i of
            Just  d → d
            Nothing → error $ [fmt|month %d out of range|] i

instance ToWord16 Month where
  toWord16 (Month (𝕎 i)) = fromInteger i + 1

instance Printable Month where
  print m = P.text $ [fmt|%d|] (toWord16 m)

------------------------------------------------------------

newtype Year = Year (𝕎 200)
  deriving (Arbitrary,Eq,Show)

year i = Year ⊳ 𝕨 (i-1900)

__year i = case year i of
            Just  d → d
            Nothing → error $ [fmt|year %d out of range|] i

instance ToWord16 Year where
  toWord16 (Year (𝕎 i)) = fromInteger i + 1900

instance Printable Year where
  print y = P.text $ [fmt|%d|] (toWord16 y)

------------------------------------------------------------

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
  print (Dateish y m d) =
    P.text $ [fmt|%04d-%02d-%02d|] (toWord16 y) (toWord16 m) (toWord16 d)
  print (DateishDs y m (d0, d1)) =
    P.text $ [fmt|%04d-%02d-%02d:%02d|] (toWord16 y) (toWord16 m) (toWord16 d0)
                                                                  (toWord16 d1)
  print (DateishM  y m) = P.text $ [fmt|%04d-%02d|] (toWord16 y) (toWord16 m)
  print (DateishMs y (m0,m1)) =
    P.text $ [fmt|%04d-%02d:%02d|] (toWord16 y) (toWord16 m0) (toWord16 m1)
  print (DateishR y (m0,d0) (m1,d1)) =
    P.text $ [fmt|%04d-%02d-%02d:%02d-%02d|]
                  (toWord16 y) (toWord16 m0) (toWord16 d0)
                               (toWord16 m1) (toWord16 d1)
  print (DateishRY (y0,m0,d0) (y1,m1,d1)) =
    P.text $ [fmt|%04d-%02d-%02d:%04d-%02d-%02d|]
                  (toWord16 y0) (toWord16 m0) (toWord16 d0)
                  (toWord16 y1) (toWord16 m1) (toWord16 d1)
  print (DateishY y) = P.text $ [fmt|%04d|] (toWord16 y)
  print (DateishYs (y0,y1)) =
    P.text $ [fmt|%04d:%04d|] (toWord16 y0) (toWord16 y1)

dateishPrintableTests ∷ TestTree
dateishPrintableTests =
  testGroup "Printable"
            [ testCase "2019-11-14" $
                "2019-11-14" ≟ toText (Dateish (__year 2019) (__month 11)
                                               (__day 14))
            ]

instance Textual Dateish

{- | convert a (Year,Month,Day) triple to a Gregorian day (for comparison) -}
dToG ∷ (Year,Month,Day) → Calendar.Day
dToG (y,m,d) = fromGregorian (fromIntegral $ toWord16 y)
                             (fromIntegral $ toWord16 m)
                             (fromIntegral $ toWord16 d)

instance Arbitrary Dateish where
  -- XXX generate all different sum members
  arbitrary = let suchThat' ∷ Arbitrary α ⇒ (α → 𝔹) → Gen α
                  suchThat' = suchThat arbitrary
                  pairIsOrdered ∷ Ord α ⇒ (α,α) → 𝔹
                  pairIsOrdered (a,b) = b > a
                  orderedGen ∷ (Arbitrary α,Ord α) ⇒ Gen (α,α)
                  orderedGen = suchThat' pairIsOrdered
                  orderedPair ∷ (Arbitrary α, Ord α) ⇒ Gen (α,α)
                  orderedPair = orderedGen
                  orderedDates = suchThat' (\ (a,b) → dToG a ≤ dToG b)

                  genDateish   = Dateish   ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary
                  genDateishDs = DateishDs ⊳ arbitrary ⊵ arbitrary ⊵ orderedPair
                  genDateishM  = DateishM  ⊳ arbitrary ⊵ arbitrary
                  genDateishMs = DateishMs ⊳ arbitrary ⊵ orderedPair
--                  genDateishR  = DateishR  ⊳ arbitrary ⊵ arbitrary
                  genDateishRY = DateishRY ⊳ arbitrary ⊵ arbitrary
               in oneof [ genDateish, genDateishDs, genDateishM, genDateishMs ]

dateishTextualTests ∷ TestTree
dateishTextualTests =
  testGroup "Textual"
            [ testProperty "invertibleText" (propInvertibleText @Dateish) ]


dateishTests ∷ TestTree
dateishTests =
  testGroup "Dateish" [ dateishPrintableTests, dateishTextualTests ]


-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MInfo.Types" [ dateishTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
