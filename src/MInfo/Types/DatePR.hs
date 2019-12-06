{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnicodeSyntax #-}

{- | Date range, utilizing DateP for varying precisions -}

module MInfo.Types.DatePR
  ( DatePR, datePR )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import Control.Monad      ( Monad, fail, return )
import Data.Bool          ( Bool( False, True ) )
import Data.Either        ( Either( Left, Right ) )
import Data.Eq            ( Eq )
import Data.Function      ( ($) )
import Data.Maybe         ( Maybe( Just ) )
import Data.Ord           ( (<) )
import Data.String        ( String )
import Data.Tuple         ( uncurry )
import System.Exit        ( ExitCode )
import System.IO          ( IO )
import Text.Show          ( Show( show ) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed, Malformed ), Printable( print )
                     , Textual( textual )
                     , fromText, parseText, toString, toText
                     )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec' )

-- parsers ------------------------------

import Text.Parser.Char  ( string )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( propInvertibleText, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Gen, suchThat, testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON ), Value( String ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.Util              ( __fromString, mkQuasiQuoterExp )
import MInfo.YamlPlus          ( unYaml )
import MInfo.YamlPlus.Error    ( YamlParseError )

import MInfo.Types.DateP       ( DateP
                               , dateDay', dateMonth, dateP, dateYear, toDay )

import MInfo.Types.Date.Error  ( AsDateError, DateError_
                               , dateRangeError, dateRangeError_ )

import MInfo.Types.DayOfM      ( dayOfM )
import MInfo.Types.Month       ( month )
import MInfo.Types.Year        ( year )

--------------------------------------------------------------------------------

type DateError = DateError_ DateP

{- | A date, with variable precision -}
newtype DatePR = DatePR (DateP,DateP)
  deriving (Eq,Show)

datePRange ∷ (AsDateError DateP ε, MonadError ε η) ⇒ DateP → DateP → η DatePR
datePRange start end = if toDay start < toDay end
                       then return $ DatePR (start,end)
                       else dateRangeError start end

datePRangeM ∷ Monad η ⇒ DateP → DateP → η DatePR
datePRangeM start end = case datePRange @DateError start end of
                          Left  e → fail $ show e
                          Right r → return r

datePRangeTests ∷ TestTree
datePRangeTests =
  testGroup "datePRange"
            [ testCase "2019-11-14:2019-11-26" $
                  Right testDatePR
                ≟ datePRange @DateError [dateP|2019-11-14|] [dateP|2019-11-26|]
            , testCase "2019-11-14:2019-11-14" $
                  Left (dateRangeError_ [dateP|2019-11-14|] [dateP|2019-11-14|])
                ≟ datePRange @DateError [dateP|2019-11-14|] [dateP|2019-11-14|]
            ]



----------------------------------------

instance Printable DatePR where
  print (DatePR (d0,d1))  = P.text $ [fmt|%T:%T|] d0 d1

--------------------

datePRPrintableTests ∷ TestTree
datePRPrintableTests =
  let check s dpr = testCase s $ s ≟ toString dpr
   in testGroup "Printable"
                [ check "2019-11-14:2019-11-26" testDatePR
                , check "2017-11:2019-11"       testDatePRM
                , check "2017:2019"             testDatePRY
                ]

----------------------------------------

instance Parsecable DatePR where
  parser = textual

--------------------

datePRParsecableTests ∷ TestTree
datePRParsecableTests =
  let check s dpr = testCase s $ Right dpr ≟ parsec' (""∷Text) s
   in testGroup "Parsecable"
                [ check "2019-11-14:2019-11-26" testDatePR
                , check "2017-11:2019-11" testDatePRM
                , check "2017:2019" testDatePRY
                ]

----------------------------------------

instance Textual DatePR where
  textual = ((,) ⊳ textual ⋪ string ":" ⊵ textual) ≫ uncurry datePRangeM

--------------------

datePRTextualTests ∷ TestTree
datePRTextualTests =
  testGroup "Textual"
            [ testCase "2019-11-14" $
                Just testDatePR  ≟ fromText "2019-11-14:2019-11-26"
            , testCase "2019-11"    $
                Just testDatePRM ≟ fromText "2017-11:2019-11"
            , testCase "2019"       $
                Just testDatePRY ≟ fromText "2017:2019"
            , testProperty "invertibleText" (propInvertibleText @DateP)
            ]

----------------------------------------

instance Arbitrary DatePR where
  arbitrary ∷ Gen DatePR
  arbitrary = let isValid ∷ DatePR → Bool
                  isValid (DatePR (d0,d1)) = case datePRange @DateError d0 d1 of
                                               Left  _ → False
                                               Right _ → True
               in (DatePR ⊳ ((,) ⊳ arbitrary ⊵ arbitrary)) `suchThat` isValid

----------------------------------------

instance FromJSON DatePR where
  parseJSON (String t) = case parseText t of
                           Parsed      d → return d
                           Malformed _ e → fail $ [fmt|%s (%t)|] e  t
  parseJSON invalid    = typeMismatch "DateP" invalid

--------------------

datePRFromJSONTests ∷ TestTree
datePRFromJSONTests =
  testGroup "FromJSON"
            [ testCase "2019-11-14" $
                Right testDatePR ≟ unYaml @YamlParseError "2019-11-14:2019-11-26"
            , testCase "2019-11" $
                Right testDatePRM ≟ unYaml @YamlParseError "2017-11:2019-11"
            , testCase "2019" $
                Right testDatePRY ≟ unYaml @YamlParseError "2017:2019"
            ]

----------------------------------------

instance ToJSON DatePR where
  toJSON d = String $ toText d

--------------------

datePRToJSONTests ∷ TestTree
datePRToJSONTests =
  let check s d = testCase s $ String (pack s) ≟ toJSON d
   in testGroup "ToJSON" [ check "2019-11-14:2019-11-26" testDatePR
                         , check "2017-11:2019-11"       testDatePRM
                         , check "2017:2019"             testDatePRY
                         ]

----------------------------------------

datePR ∷ QuasiQuoter
datePR = mkQuasiQuoterExp "DateP" (\ s → ⟦ __fromString @DateP s ⟧)

-- testing ---------------------------------------------------------------------

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

testDateP0 ∷ DateP
testDateP0 = dateDay' [year|2019|] [month|11|] [dayOfM|14|]

testDateP1 ∷ DateP
testDateP1 = dateDay' [year|2019|] [month|11|] [dayOfM|26|]

testDatePR ∷ DatePR
testDatePR = DatePR (testDateP0,testDateP1)

testDatePM0 ∷ DateP
testDatePM0 = dateMonth [year|2017|] [month|11|]

testDatePM1 ∷ DateP
testDatePM1 = dateMonth [year|2019|] [month|11|]

testDatePRM ∷ DatePR
testDatePRM = DatePR (testDatePM0,testDatePM1)

testDatePY0 ∷ DateP
testDatePY0 = dateYear [year|2017|]

testDatePY1 ∷ DateP
testDatePY1 = dateYear [year|2019|]

testDatePRY ∷ DatePR
testDatePRY = DatePR (testDatePY0,testDatePY1)

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DateP" [ datePRangeTests, datePRPrintableTests
                          , datePRTextualTests, datePRParsecableTests
                          , datePRFromJSONTests, datePRToJSONTests
                          ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
