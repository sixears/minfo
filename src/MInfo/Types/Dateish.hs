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

module MInfo.Types.Dateish
  ( Dateish )
where

import Prelude  ( Integer, Integral, (+), (-)
                , error, fromIntegral, fromInteger, toInteger
                )

import Control.Applicative  ( (<*>), (<*) )
import Data.Functor         ( (<$>) )

-- base --------------------------------

import Control.Monad  ( Monad, fail, return )
import Data.Bool      ( not )
import Data.Either    ( Either( Right ) )
import Data.Eq        ( Eq )
import Data.Foldable  ( foldl1, toList )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ), maybe )
import Data.Ord       ( Ord, (<) )
import Data.String    ( String )
import Data.Word      ( Word16 )
import GHC.Generics   ( Generic )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Read      ( read )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode         ( (≡) )
import Data.Function.Unicode   ( (∘) )
import Data.Monoid.Unicode     ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromText, toText )
import Data.Textual.Integral  ( Decimal( Decimal ), nnUpTo )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid  ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (⋫), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ю, ф )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE  ( SeqNE, (⋗), pattern (:⫸) )

-- parsers ------------------------------

import Text.Parser.Char         ( CharParsing, digit, string )
import Text.Parser.Combinators  ( Parsing, count, try )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec' )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary
                              ( Arbitrary( arbitrary ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( propInvertibleText, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Gen, oneof, suchThat, testProperty )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import qualified  Data.Time.Calendar  as  Calendar
import Data.Time  ( fromGregorian, toGregorian )

-- validity ----------------------------

import Data.Validity  ( Validation, Validity( validate ), check, isValid )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.BoundedN ( 𝕎, pattern 𝕎, 𝕨 )

--------------------------------------------------------------------------------

class ToWord16 α where
  toWord16 ∷ α → Word16

{- | `try` the first thing, then the next thing, until the last thing (which
     isn't surrounded by a `try`) -}
tries ∷ Parsing η ⇒ SeqNE (η α) → η α
tries (ts :⫸ t) = foldl1 (∤) (toList ((try ⊳ ts) ⋗ t))
tries _          = ePatSymExhaustive "tries"

ePatSymExhaustive ∷ String → α
ePatSymExhaustive s =
    error $ s ⊕ "https://gitlab.haskell.org/ghc/ghc/issues/10339"

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
  toWord16 (Day (_))    = error "failed to pattern-match day"

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

dayTests ∷ TestTree
dayTests = testGroup "Day" [ dayTextualTests ]

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
  toWord16 (Month (_))    = error "failed to pattern-match month"

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

monthTests ∷ TestTree
monthTests = testGroup "Month" [ monthTextualTests ]

------------------------------------------------------------

newtype Year = Year (𝕎 200)
  deriving (Eq,Generic,Ord,Show)

year ∷ Integral α ⇒ α → Maybe Year
year i = Year ⊳ 𝕨 (toInteger i-1900)

year' ∷ Integer → Maybe Year
year' = year

__year ∷ Integral α ⇒ α → Year
__year i = case year i of
            Just  d → d
            Nothing → error $ [fmt|year %d out of range|] i

__year' ∷ Integer → Year
__year' = __year

instance ToWord16 Year where
  toWord16 (Year (𝕎 i)) = fromInteger i + 1900
  toWord16 (Year (_))    = error "failed to pattern-match year"

instance Printable Year where
  print y = P.text $ [fmt|%d|] (toWord16 y)

instance Textual Year where
  textual = do
    y ← read ⊳ count 4 digit
    maybe (fail $ [fmt|bad year value %d|] y) return $ year' y

yearTextualTests ∷ TestTree
yearTextualTests =
  testGroup "Textual"
            [ testCase "2014" $ Just (__year' 2014) ≟ fromText "2014"
            , testCase "2019" $ Just (__year' 2019) ≟ fromText "2019"
            , testProperty "invertibleText" (propInvertibleText @Year)
            ]

instance Arbitrary Year where
  arbitrary = Year ⊳ arbitrary

yearTests ∷ TestTree
yearTests = testGroup "Year" [ yearTextualTests ]

------------------------------------------------------------

{- | A specialist data type for Music/Info dates -}
data Dateish = Dateish    Year Month Day
             | DateishDs  Year Month (Day,Day)
             | DateishM   Year Month
             | DateishMs  Year (Month,Month)
             | DateishY   Year
             | DateishYs  (Year,Year)
             | DateishR   Year (Month,Day) (Month,Day)
             | DateishRY  (Year,Month,Day) (Year,Month,Day)
  deriving (Eq,Generic,Show)

{- | convert a (Year,Month,Day) triple to a Gregorian day (for comparison) -}
dToG ∷ (Year,Month,Day) → Calendar.Day
dToG (y,m,d) = fromGregorian (fromIntegral $ toWord16 y)
                             (fromIntegral $ toWord16 m)
                             (fromIntegral $ toWord16 d)

{- | Convert a Gregorian day to a (Year,Month,Day) triple (for checking).
     *PARTIAL*, because we only care to handle a 200-year span. -}
__gToD ∷ Calendar.Day → (Year,Month,Day)
__gToD dy = let (y,m,d) = toGregorian dy
             in (__year y, __month m, __day d)

checkDate ∷ (Year,Month,Day) → Validation
checkDate (y,m,d) = check ((__gToD $ dToG (y,m,d)) ≡ (y,m,d)) $
                      [fmt|%t is a valid date|] (textDate (y,m,d))

checkOrder ∷ (Year,Month,Day) → (Year,Month,Day) → Validation
checkOrder dy0 dy1 = check ((dToG dy0) < (dToG dy1)) $
                       [fmt|day %t < day %t|] (textDate dy0) (textDate dy1)

checkDates ∷ (Year,Month,Day) → (Year,Month,Day) → Validation
checkDates dy0 dy1 = checkDate dy0 ⊕ checkDate dy1 ⊕ checkOrder dy0 dy1

instance Validity Dateish where
  validate (Dateish   y m d)             = checkDate (y,m,d)
  validate (DateishM  _ _)               = ф
  validate (DateishDs y m (d0,d1))       = let dy0 = (y,m,d0)
                                               dy1 = (y,m,d1)
                                            in checkDates dy0 dy1
  validate (DateishMs y (m0,m1))         = let dy0 = (y,m0,__day' 1)
                                               dy1 = (y,m1,__day' 1)
                                            in checkDates dy0 dy1
  validate (DateishR  y (m0,d0) (m1,d1)) = let dy0 = (y,m0,d0)
                                               dy1 = (y,m1,d1)
                                            in checkDates dy0 dy1
  validate (DateishRY dy0 dy1)           = checkDates dy0 dy1
  validate (DateishY  _)                 = ф
  validate (DateishYs (y0,y1))           =
    check (y0 < y1) $ [fmt|year %d < year %d|] (toWord16 y0) (toWord16 y1)

dateishValidityTests ∷ TestTree
dateishValidityTests =
  testGroup "Validity"
            [ testCase "testDateish" $
                assertBool "2019-11-14" (isValid testDateish)
            , testCase "badDateish" $
                assertBool "2019-11-31" (not $ isValid badDateish)
            , testCase "testDateishDs" $
                assertBool "2019-11-14:29" (isValid testDateishDs)
            , testCase "badDateishDs" $
                assertBool "2019-11-20:19" (not $ isValid badDateishDs)
            , testCase "testDateishM" $
                assertBool "2019-11" (isValid testDateishM)
            , testCase "testDateishMs" $
                assertBool "2019-11:12" (isValid testDateishMs)
            , testCase "badDateishMs" $
                assertBool "2019-11:11" (not $ isValid badDateishMs)
            , testCase "testDateishR" $
                assertBool "2019-12:01-11:30" (isValid testDateishR)
            , testCase "badDateishR" $
                assertBool "2019-11:30-12:01" (not $ isValid badDateishR)
            , testCase "testDateishRY" $
                assertBool "2019-11:30-01:01" (isValid testDateishRY)
            , testCase "badDateishRY" $
                assertBool "2019-11:01-01:30" (not $ isValid badDateishRY)
            , testCase "testDateishY" $
                assertBool "2019" (isValid testDateishY)
            , testCase "testDateishYs" $
                assertBool "2019:2020" (isValid testDateishYs)
            , testCase "badDateishYs" $
                assertBool "2019:2019" (not $ isValid badDateishYs)
            , testProperty "genValid" (isValid @Dateish)
            ]

instance GenValid Dateish where
  genValid ∷ Gen Dateish
  genValid = let genUnchecked =
                   oneof [ Dateish   ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary
                         , DateishDs ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary
                         , DateishM  ⊳ arbitrary ⊵ arbitrary
                         , DateishMs ⊳ arbitrary ⊵ arbitrary
                         , DateishY  ⊳ arbitrary
                         , DateishYs ⊳ arbitrary
                         , DateishR  ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary
                         , DateishRY ⊳ arbitrary ⊵ arbitrary
                         ]
              in suchThat genUnchecked isValid

  shrinkValid ∷ Dateish → [Dateish]
  shrinkValid (Dateish (Year (𝕎 1)) (Month (𝕎 1)) (Day (𝕎 1))) = []
  shrinkValid (Dateish y@(Year (𝕎 1)) m@(Month (𝕎 1)) (Day (𝕎 d))) =
    [Dateish y m (Day (𝕎 (d-1)))]
  shrinkValid (Dateish y@(Year (𝕎 1)) (Month (𝕎 m)) d@(Day (𝕎 1))) =
    [Dateish y (Month (𝕎 (m-1))) d]
  shrinkValid (Dateish (Year (𝕎 y)) m@(Month (𝕎 1)) d@(Day (𝕎 1))) =
    [Dateish (Year (𝕎 (y-1))) m d]
  shrinkValid (Dateish y@(Year (𝕎 1)) (Month (𝕎 m)) (Day (𝕎 d))) =
    [Dateish y (Month (𝕎 (m-1))) (Day (𝕎 (d-1)))]
  shrinkValid (Dateish (Year (𝕎 y)) m@(Month (𝕎 1)) (Day (𝕎 d))) =
    [Dateish (Year (𝕎 (y-1))) m (Day (𝕎 (d-1)))]
  shrinkValid (Dateish (Year (𝕎 y)) (Month (𝕎 m)) d@(Day (𝕎 1))) =
    [Dateish (Year (𝕎 (y-1))) (Month (𝕎 (m-1))) d]
  shrinkValid (Dateish (Year (𝕎 y)) (Month (𝕎 m)) (Day (𝕎 d))) =
    [Dateish (Year (𝕎 (y-1))) (Month (𝕎 (m-1))) (Day (𝕎 (d-1)))]

  shrinkValid _ = []

textDate ∷ (Year,Month,Day) → Text
textDate (y,m,d) = [fmt|%4d-%02d-%02d|] (toWord16 y) (toWord16 m) (toWord16 d)

instance Printable Dateish where
  print (Dateish y m d) = P.text $ textDate (y,m,d)
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
            [ testCase "2019-11-14"       $ "2019-11-14" ≟ toText testDateish
            , testCase "2019-11"          $ "2019-11"    ≟ toText testDateishM
            , testCase "2019-11:12"       $ "2019-11:12" ≟ toText testDateishMs
            , testCase "2019-11-14:29"    $
                                         "2019-11-14:29" ≟ toText testDateishDs
            , testCase "2019"             $ "2019"       ≟ toText testDateishY
            , testCase "2019:2020"        $ "2019:2020"  ≟ toText testDateishYs
            , testCase "2019-11-30:12-01" $
                                     "2019-11-30:12-01"  ≟ toText testDateishR
            , testCase "2019-11-30:2020-01-01" $
                                 "2019-11-30:2020-01-01" ≟ toText testDateishRY
            ]

instance Parsecable Dateish where
  parser = textual

dateishParsecableTests ∷ TestTree
dateishParsecableTests =
  let nn = "" ∷ Text
      testDateishText = "2019-11-14" ∷ Text
   in testGroup "Parsecable"
                [ testCase "2019-11-14" $
                    Right testDateish ≟ parsec' @Dateish nn testDateishText
                , testCase "2019-11-14" $
                    Right testDateish ≟ parsec' @Dateish nn testDateishText
                ]

instance Arbitrary Dateish where
  arbitrary = genValid

instance Textual Dateish where
  textual =
    let dmy ∷ (Monad η, CharParsing η) ⇒ η (Year,Month,Day)
        dmy = (,,) ⊳ (textual ⋪ string "-") ⊵ textual ⊵ (string "-" ⋫ textual)

        dm ∷ (Monad η, CharParsing η) ⇒ η (Month,Day)
        dm = (,) <$> (textual <* string "-") <*> textual

     in tries $ [ DateishYs ⊳ ((,) ⊳ (textual ⋪ string ":") ⊵ textual)
                , DateishMs ⊳ (textual ⋪ string "-")
                            ⊵ ((,) ⊳ (textual ⋪ string ":") ⊵ textual)
                , DateishR  ⊳ (textual ⋪  string "-") ⊵ (dm ⋪ string ":") ⊵ dm
                , DateishRY ⊳ (dmy ⋪ string ":") ⊵ dmy
                , DateishDs ⊳ (textual ⋪ string "-") ⊵ (textual ⋪ string "-")
                            ⊵ ((,) ⊳ (textual ⋪ string ":") ⊵ textual)
                , Dateish   ⊳ textual ⋪ string "-" ⊵ textual ⋪ string "-"
                            ⊵ textual
                , DateishM  ⊳ (textual ⋪ string "-") ⊵ textual
                ]
              ⋗  (DateishY  ⊳ textual)

dateishTextualTests ∷ TestTree
dateishTextualTests =
  testGroup "Textual"
            [ testCase "2019-11-14" $ Just testDateish   ≟ fromText "2019-11-14"
            , testCase "2019-11"    $ Just testDateishM  ≟ fromText "2019-11"
            , testCase "2019-11:12" $ Just testDateishMs ≟ fromText "2019-11:12"
            , testCase "2019:2020"  $ Just testDateishYs ≟ fromText "2019:2020"
            , testCase "2019-11-14:29"  $
                Just testDateishDs ≟ fromText "2019-11-14:29"
            , testCase "2019-11-30:12-01" $
                Just testDateishR ≟ fromText "2019-11-30:12-01" 
            , testCase "2019-11-30:2020-01-01"  $
                Just testDateishRY ≟ fromText "2019-11-30:2020-01-01"
            , testProperty "invertibleText" (propInvertibleText @Dateish)
            ]

dateishTests ∷ TestTree
dateishTests =
  testGroup "Dateish" [ dateishPrintableTests, dateishTextualTests
                      , dateishValidityTests, dateishParsecableTests ]


-- testing ---------------------------------------------------------------------

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

testDateish ∷ Dateish
testDateish = Dateish (__year' 2019) (__month' 11) (__day' 14)

badDateish ∷ Dateish
badDateish = Dateish (__year' 2019) (__month' 11) (__day' 31)

testDateishDs ∷ Dateish
testDateishDs = DateishDs (__year' 2019) (__month' 11) ((__day' 14),(__day' 29))

badDateishDs ∷ Dateish
badDateishDs = DateishDs (__year' 2019) (__month' 11) ((__day' 20),(__day' 19))

testDateishM ∷ Dateish
testDateishM = DateishM (__year' 2019) (__month' 11)

testDateishMs ∷ Dateish
testDateishMs = DateishMs (__year' 2019) ((__month' 11),(__month' 12))

badDateishMs ∷ Dateish
badDateishMs = DateishMs (__year' 2019) ((__month' 11),(__month' 11))

testDateishR ∷ Dateish
testDateishR = DateishR (__year' 2019) (__month' 11,__day' 30)
                                      (__month' 12,__day' 1)

badDateishR ∷ Dateish
badDateishR = DateishR (__year' 2019) (__month' 12,__day' 1)
                                     (__month' 11,__day' 30)

testDateishRY ∷ Dateish
testDateishRY = DateishRY (__year' 2019,__month' 11,__day' 30)
                          (__year' 2020,__month' 1,__day' 1)

badDateishRY ∷ Dateish
badDateishRY = DateishRY (__year' 2019,__month' 11,__day' 1)
                         (__year' 2019,__month' 1,__day' 30)

testDateishY ∷ Dateish
testDateishY = DateishY (__year' 2019)

testDateishYs ∷ Dateish
testDateishYs = DateishYs ((__year' 2019),(__year' 2020))

badDateishYs ∷ Dateish
badDateishYs = DateishYs ((__year' 2019),(__year' 2019))


------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MInfo.Types" [ dayTests, monthTests, yearTests, dateishTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
