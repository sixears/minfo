{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Date, with varying precisions - day, month, year. -}

module MInfo.Types.DateImprecise
  ( DateImprecise, dateDay, dateDay', dateMonth, dateYear, dateImprecise, toDate, toDay, tests )
where

import Prelude  ( Float, Integer )

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import Control.Monad  ( fail, return )
import Data.Either    ( Either( Left, Right ) )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import Data.Tuple     ( curry )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode  ( (≡) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed, Malformed ), Printable( print )
                     , Textual( textual ), fromText, parseText, toText )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE  ( (⋗) )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec' )

-- parsers ------------------------------

import Text.Parser.Char  ( string )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary ) )

-- scientific --------------------------

import Data.Scientific  ( floatingOrInteger )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( propInvertibleText, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Gen, oneof, testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import Data.Time  ( Day, fromGregorian, toGregorian )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON )
                  , Value( Number, String ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.Util            ( __fromString, mkQuasiQuoterExp, tries )
import MInfo.YamlPlus        ( unYaml )
import MInfo.YamlPlus.Error  ( YamlParseError )

import MInfo.Types.Date.Error  ( AsDateError, badDateError )
import MInfo.Types.DayOfM      ( DayOfM, dayOfM )
import MInfo.Types.FromI       ( fromI, __fromI )
import MInfo.Types.ToWord16    ( toIntegral, toWord16 )
import MInfo.Types.Month       ( Month, month )
import MInfo.Types.Year        ( Year, year )

--------------------------------------------------------------------------------

{- | A date, with variable precision -}
data DateImprecise = DateDay Day | DateMonth (Year,Month) | DateYear Year
  deriving (Eq,Show)

dateDay ∷ (AsDateError (Year,Month,DayOfM) ε, MonadError ε η) ⇒
          Year → Month → DayOfM → η DateImprecise
dateDay y m d = let (y_,m_,d_) = (toIntegral y, toIntegral m, toIntegral d)
                    result@(DateDay day) = dateDay' y m d
                    (y', m', d') = toGregorian day
                 in if (y',m',d') ≡ (y_,m_,d_)
                    then return result
                    else badDateError (y,m,d)

{- | Convert a day to a DateImprecise, with "clipping" as defined in `fromGregorian`;
     out-of-range days will be "clipped" back to the earliest available day
     in the month -}
dateDay' ∷ Year → Month → DayOfM → DateImprecise
dateDay' y m d =
  DateDay $ fromGregorian (toIntegral y) (toIntegral m) (toIntegral d)

dateMonth ∷ Year → Month → DateImprecise
dateMonth = curry DateMonth

dateYear ∷ Year → DateImprecise
dateYear = DateYear

{- | Convert a `DateImprecise` to a Day (the first available day in that
     "range"). -}
toDay ∷ DateImprecise → Day
toDay (DateDay d)       = d
toDay (DateMonth (y,m)) = fromGregorian (toIntegral y) (toIntegral m) 1
toDay (DateYear y)      = fromGregorian (toIntegral y) 1 1

cdayToDate ∷ Day → (Year,Month,DayOfM)
cdayToDate cday = let (y,m,d) = toGregorian cday
                   in (__fromI y, __fromI m, __fromI d)

{- | Convert a `DateImprecise` to a day (the first available day in that
     "range"); as a (y,m,d) triple. -}
toDate ∷ DateImprecise → (Year,Month,DayOfM)
toDate datep = let cday = toDay datep
                in cdayToDate cday

textDay ∷ Day → Text
textDay cday = let (y,m,d) = cdayToDate cday
                in textDate (y,m,d)

textDate ∷ (Year,Month,DayOfM) → Text
textDate (y,m,d) = [fmt|%4d-%02d-%02d|] (toWord16 y) (toWord16 m) (toWord16 d)

----------------------------------------

instance Printable DateImprecise where
  print (DateDay cday)    = P.text $ textDay cday
  print (DateMonth (y,m)) = P.text $ [fmt|%4d-%02d|] (toWord16 y) (toWord16 m)
  print (DateYear y)      = P.text $ [fmt|%4d|] (toWord16 y)

--------------------

printableTests ∷ TestTree
printableTests =
  testGroup "Printable"
            [ testCase "2019-11-14"       $ "2019-11-14" ≟ toText testDateImprecise
            , testCase "2019-11"          $ "2019-11"    ≟ toText testDateImpreciseM
            , testCase "2019"             $ "2019"       ≟ toText testDateImpreciseY
            ]

----------------------------------------

instance Parsecable DateImprecise where
  parser = textual

--------------------

parsecableTests ∷ TestTree
parsecableTests =
  let nn = "" ∷ Text
      testDateImpreciseText  = "2019-11-14" ∷ Text
      testDateImpreciseMText = "2019-11" ∷ Text
      testDateImpreciseYText = "2019" ∷ Text
   in testGroup "Parsecable"
                [ testCase "2019-11-14" $
                    Right testDateImprecise  ≟ parsec' @DateImprecise nn testDateImpreciseText
                , testCase "2019-11" $
                    Right testDateImpreciseM ≟ parsec' @DateImprecise nn testDateImpreciseMText
                , testCase "2019" $
                    Right testDateImpreciseY ≟ parsec' @DateImprecise nn testDateImpreciseYText
                ]

----------------------------------------

instance Textual DateImprecise where
  textual =
    tries $ [ dateDay'  ⊳ textual ⋪ string "-" ⊵ textual ⋪ string "-"
                        ⊵ textual
            , dateMonth ⊳ (textual ⋪ string "-") ⊵ textual
            ]
          ⋗ (DateYear  ⊳ textual)

--------------------

textualTests ∷ TestTree
textualTests =
  testGroup "Textual"
            [ testCase "2019-11-14" $ Just testDateImprecise   ≟ fromText "2019-11-14"
            , testCase "2019-11"    $ Just testDateImpreciseM  ≟ fromText "2019-11"
            , testCase "2019"       $ Just testDateImpreciseY  ≟ fromText "2019"
            , testProperty "invertibleText" (propInvertibleText @DateImprecise)
            ]

----------------------------------------

instance Arbitrary DateImprecise where
  arbitrary ∷ Gen DateImprecise
  arbitrary = oneof [ dateDay'  ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary
                    , dateMonth ⊳ arbitrary ⊵ arbitrary
                    , dateYear  ⊳ arbitrary
                    ]

----------------------------------------

instance FromJSON DateImprecise where
  parseJSON (String t) = case parseText t of
                           Parsed      d → return d
                           Malformed _ e → fail $ [fmt|%s (%t)|] e  t
  parseJSON (Number n) = case floatingOrInteger @Float @Integer n of
                             Left  f → fail $ [fmt|fractional year: (%f)|] f
                             Right i → case fromI i of
                                         Just  y → return $ dateYear y
                                         Nothing → fail $ [fmt|bad year: %d|] i
  parseJSON invalid    = typeMismatch "DateImprecise" invalid

--------------------

fromJSONTests ∷ TestTree
fromJSONTests =
  testGroup "FromJSON"
            [ testCase "2019-11-14" $
                Right testDateImprecise ≟ unYaml @YamlParseError "2019-11-14"
            , testCase "2019-11" $
                Right testDateImpreciseM ≟ unYaml @YamlParseError "2019-11"
            , testCase "2019" $
                Right testDateImpreciseY ≟ unYaml @YamlParseError "2019"
            ]

----------------------------------------

instance ToJSON DateImprecise where
  toJSON d = String $ toText d

--------------------

toJSONTests ∷ TestTree
toJSONTests =
  let check s d = testCase s $ String (pack s) ≟ toJSON d
   in testGroup "ToJSON" [ check "2019-11-14" testDateImprecise
                         , check "2019-11"    testDateImpreciseM
                         , check "2019"       testDateImpreciseY
                         ]

----------------------------------------

dateImprecise ∷ QuasiQuoter
dateImprecise = mkQuasiQuoterExp "DateImprecise"
                                  (\ s → ⟦ __fromString @DateImprecise s ⟧)

-- testing ---------------------------------------------------------------------

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

testDateImprecise ∷ DateImprecise
testDateImprecise = dateDay' [year|2019|] [month|11|] [dayOfM|14|]

testDateImpreciseM ∷ DateImprecise
testDateImpreciseM = dateMonth [year|2019|] [month|11|]

testDateImpreciseY ∷ DateImprecise
testDateImpreciseY = dateYear [year|2019|]

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DateImprecise" [ printableTests, parsecableTests
                                  , textualTests, fromJSONTests , toJSONTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
