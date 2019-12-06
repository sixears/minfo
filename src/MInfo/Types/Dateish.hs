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
  ( Dateish( Dateish, DateishY )
  , __dateish, __dateish', dateish, dateish'
  , __dateishy, __dateishy', dateishy, dateishy'
  , tests
  )
where

import Prelude  ( Float, Integer, Integral, (-), error, fromIntegral )

import Control.Applicative  ( (<*>), (<*) )
import Data.Functor         ( (<$>) )

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Number, String ), typeMismatch )
       
-- base --------------------------------

import Control.Monad  ( Monad, fail, return )
import Data.Bool      ( not )
import Data.Either    ( Either( Left, Right ) )
import Data.Eq        ( Eq )
import Data.Foldable  ( foldl1, toList )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.Ord       ( (<) )
import Data.String    ( String )
import GHC.Generics   ( Generic )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode         ( (≡) )
import Data.Monoid.Unicode     ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), Printable( print )
                     , Textual( textual ), fromText, parseText, toText )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid  ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (⋫), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monoid       ( ф )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE  ( SeqNE, (⋗), pattern (:⫸) )

-- parsers ------------------------------

import Text.Parser.Char         ( CharParsing, string )
import Text.Parser.Combinators  ( Parsing, try )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec' )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary
                              ( Arbitrary( arbitrary ) )

-- scientific --------------------------

import Data.Scientific  ( floatingOrInteger )

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

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.BoundedN        ( pattern 𝕎 )
import MInfo.Types.DayOfM    ( DayOfM( DayOfM ) )
import MInfo.Types.FromI     ( fromI, __fromI, __fromI' )
import MInfo.Types.Month     ( Month( Month ) )
import MInfo.Types.Year      ( Year( Year ) )
import MInfo.Types.ToWord16  ( ToWord16( toWord16 ) )

--------------------------------------------------------------------------------

{- | `try` the first thing, then the next thing, until the last thing (which
     isn't surrounded by a `try`) -}
tries ∷ Parsing η ⇒ SeqNE (η α) → η α
tries (ts :⫸ t) = foldl1 (∤) (toList ((try ⊳ ts) ⋗ t))
tries _          = ePatSymExhaustive "tries"

ePatSymExhaustive ∷ String → α
ePatSymExhaustive s =
    error $ s ⊕ "https://gitlab.haskell.org/ghc/ghc/issues/10339"

------------------------------------------------------------

{- | A specialist data type for Music/Info dates -}
data Dateish = Dateish    Year Month DayOfM
             | DateishDs  Year Month (DayOfM,DayOfM)
             | DateishM   Year Month
             | DateishMs  Year (Month,Month)
             | DateishY   Year
             | DateishYs  (Year,Year)
             | DateishR   Year (Month,DayOfM) (Month,DayOfM)
             | DateishRY  (Year,Month,DayOfM) (Year,Month,DayOfM)
  deriving (Eq,Generic,Show)

{- | convert a (Year,Month,DayOfM) triple to a Gregorian day (for comparison) -}
dToG ∷ (Year,Month,DayOfM) → Calendar.Day
dToG (y,m,d) = fromGregorian (fromIntegral $ toWord16 y)
                             (fromIntegral $ toWord16 m)
                             (fromIntegral $ toWord16 d)

{- | Convert a Gregorian day to a (Year,Month,Day) triple (for checking).
     *PARTIAL*, because we only care to handle a 200-year span. -}
__gToD ∷ Calendar.Day → (Year,Month,DayOfM)
__gToD dy = let (y,m,d) = toGregorian dy
             in (__fromI y, __fromI m, __fromI d)

checkDate ∷ (Year,Month,DayOfM) → Validation
checkDate (y,m,d) = check ((__gToD $ dToG (y,m,d)) ≡ (y,m,d)) $
                      [fmt|%t is a valid date|] (textDate (y,m,d))

checkOrder ∷ (Year,Month,DayOfM) → (Year,Month,DayOfM) → Validation
checkOrder dy0 dy1 = check ((dToG dy0) < (dToG dy1)) $
                       [fmt|day %t < day %t|] (textDate dy0) (textDate dy1)

checkDates ∷ (Year,Month,DayOfM) → (Year,Month,DayOfM) → Validation
checkDates dy0 dy1 = checkDate dy0 ⊕ checkDate dy1 ⊕ checkOrder dy0 dy1

instance Validity Dateish where
  validate (Dateish   y m d)             = checkDate (y,m,d)
  validate (DateishM  _ _)               = ф
  validate (DateishDs y m (d0,d1))       = let dy0 = (y,m,d0)
                                               dy1 = (y,m,d1)
                                            in checkDates dy0 dy1
  validate (DateishMs y (m0,m1))         = let dy0 = (y,m0,__fromI' 1)
                                               dy1 = (y,m1,__fromI' 1)
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
  shrinkValid (Dateish (Year (𝕎 1)) (Month (𝕎 1)) (DayOfM (𝕎 1))) = []
  shrinkValid (Dateish y@(Year (𝕎 1)) m@(Month (𝕎 1)) (DayOfM (𝕎 d))) =
    [Dateish y m (DayOfM (𝕎 (d-1)))]
  shrinkValid (Dateish y@(Year (𝕎 1)) (Month (𝕎 m)) d@(DayOfM (𝕎 1))) =
    [Dateish y (Month (𝕎 (m-1))) d]
  shrinkValid (Dateish (Year (𝕎 y)) m@(Month (𝕎 1)) d@(DayOfM (𝕎 1))) =
    [Dateish (Year (𝕎 (y-1))) m d]
  shrinkValid (Dateish y@(Year (𝕎 1)) (Month (𝕎 m)) (DayOfM (𝕎 d))) =
    [Dateish y (Month (𝕎 (m-1))) (DayOfM (𝕎 (d-1)))]
  shrinkValid (Dateish (Year (𝕎 y)) m@(Month (𝕎 1)) (DayOfM (𝕎 d))) =
    [Dateish (Year (𝕎 (y-1))) m (DayOfM (𝕎 (d-1)))]
  shrinkValid (Dateish (Year (𝕎 y)) (Month (𝕎 m)) d@(DayOfM (𝕎 1))) =
    [Dateish (Year (𝕎 (y-1))) (Month (𝕎 (m-1))) d]
  shrinkValid (Dateish (Year (𝕎 y)) (Month (𝕎 m)) (DayOfM (𝕎 d))) =
    [Dateish (Year (𝕎 (y-1))) (Month (𝕎 (m-1))) (DayOfM (𝕎 (d-1)))]

  shrinkValid _ = []

textDate ∷ (Year,Month,DayOfM) → Text
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
    let dmy ∷ (Monad η, CharParsing η) ⇒ η (Year,Month,DayOfM)
        dmy = (,,) ⊳ (textual ⋪ string "-") ⊵ textual ⊵ (string "-" ⋫ textual)

        dm ∷ (Monad η, CharParsing η) ⇒ η (Month,DayOfM)
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

instance FromJSON Dateish where
  parseJSON (String t) = case parseText t of
                           Parsed      d → return d
                           Malformed _ e → fail $ [fmt|%s (%t)|] e  t
  parseJSON (Number n) = case floatingOrInteger @Float @Integer n of
                             Left  f → fail $ [fmt|fractional year: (%f)|] f
                             Right i → case fromI i of
                                         Just  y → return $ DateishY y
                                         Nothing → fail $ [fmt|bad year: %d|] i
  parseJSON invalid    = typeMismatch "Dateish" invalid

instance ToJSON Dateish where
  toJSON d = String $ toText d

------------------------------------------------------------

dateish ∷ (Integral α, Integral β, Integral γ) ⇒ α → β → γ → Maybe Dateish
dateish y m d = do
  y' ← fromI y
  m' ← fromI m
  d' ← fromI d
  return $ Dateish y' m' d'

dateish' ∷ Integer → Integer → Integer → Maybe Dateish
dateish' = dateish

--------------------

__dateish ∷ (Integral α, Integral β, Integral γ) ⇒ α → β → γ → Dateish
__dateish y m d = Dateish (__fromI y) (__fromI m) (__fromI d)

__dateish' ∷ Integer → Integer → Integer → Dateish
__dateish' = __dateish

----------------------------------------

dateishy ∷ Integral α ⇒ α → Maybe Dateish
dateishy y = do
  y' ← fromI y
  return $ DateishY y'

dateishy' ∷ Integer → Maybe Dateish
dateishy' = dateishy

--------------------

__dateishy ∷ Integral α ⇒ α → Dateish
__dateishy y = DateishY (__fromI y)

__dateishy' ∷ Integer → Dateish
__dateishy' = __dateishy

-- testing ---------------------------------------------------------------------

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

testDateish ∷ Dateish
testDateish = Dateish (__fromI' 2019) (__fromI' 11) (__fromI' 14)

badDateish ∷ Dateish
badDateish = Dateish (__fromI' 2019) (__fromI' 11) (__fromI' 31)

testDateishDs ∷ Dateish
testDateishDs = DateishDs (__fromI' 2019) (__fromI' 11) ((__fromI' 14),(__fromI' 29))

badDateishDs ∷ Dateish
badDateishDs = DateishDs (__fromI' 2019) (__fromI' 11) ((__fromI' 20),(__fromI' 19))

testDateishM ∷ Dateish
testDateishM = DateishM (__fromI' 2019) (__fromI' 11)

testDateishMs ∷ Dateish
testDateishMs = DateishMs (__fromI' 2019) ((__fromI' 11),(__fromI' 12))

badDateishMs ∷ Dateish
badDateishMs = DateishMs (__fromI' 2019) ((__fromI' 11),(__fromI' 11))

testDateishR ∷ Dateish
testDateishR = DateishR (__fromI' 2019) (__fromI' 11,__fromI' 30)
                                      (__fromI' 12,__fromI' 1)

badDateishR ∷ Dateish
badDateishR = DateishR (__fromI' 2019) (__fromI' 12,__fromI' 1)
                                     (__fromI' 11,__fromI' 30)

testDateishRY ∷ Dateish
testDateishRY = DateishRY (__fromI' 2019,__fromI' 11,__fromI' 30)
                          (__fromI' 2020,__fromI' 1,__fromI' 1)

badDateishRY ∷ Dateish
badDateishRY = DateishRY (__fromI' 2019,__fromI' 11,__fromI' 1)
                         (__fromI' 2019,__fromI' 1,__fromI' 30)

testDateishY ∷ Dateish
testDateishY = DateishY (__fromI' 2019)

testDateishYs ∷ Dateish
testDateishYs = DateishYs ((__fromI' 2019),(__fromI' 2020))

badDateishYs ∷ Dateish
badDateishYs = DateishYs ((__fromI' 2019),(__fromI' 2019))

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Dateish" [ dateishPrintableTests, dateishTextualTests
                            , dateishValidityTests, dateishParsecableTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
