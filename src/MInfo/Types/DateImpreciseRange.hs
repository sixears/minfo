{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Date range, utilizing DateImprecise for varying precisions -}

module MInfo.Types.DateImpreciseRange
  ( DateImpreciseRange, dateImpreciseRange, tests )
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
import Data.String        ( String )
import Data.Tuple         ( uncurry )
import System.Exit        ( ExitCode )
import System.IO          ( IO )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Ord.Unicode  ( (≤) )

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

import MInfo.Util                 ( __fromString, mkQuasiQuoterExp )
import MInfo.YamlPlus             ( unYaml )
import MInfo.YamlPlus.Error       ( YamlParseError )

import MInfo.Types.DateImprecise  ( DateImprecise, dateDay', dateMonth
                                  , dateImprecise, dateYear, toDay )

import MInfo.Types.Date.Error     ( AsDateError, DateError_
                                  , dateRangeError, dateRangeError_ )

import MInfo.Types.DayOfM         ( dayOfM )
import MInfo.Types.Month          ( month )
import MInfo.Types.Year           ( year )

--------------------------------------------------------------------------------

type DateError = DateError_ DateImprecise

{- | A date, with variable precision -}
newtype DateImpreciseRange = DateImpreciseRange (DateImprecise,DateImprecise)
  deriving (Eq,Show)

dateImpreciseR ∷ (AsDateError DateImprecise ε, MonadError ε η) ⇒
                     DateImprecise → DateImprecise → η DateImpreciseRange
dateImpreciseR start end = if toDay start ≤ toDay end
                               then return $ DateImpreciseRange (start,end)
                               else dateRangeError start end

dateImpreciseRM ∷ Monad η ⇒
                      DateImprecise → DateImprecise → η DateImpreciseRange
dateImpreciseRM start end =
  case dateImpreciseR @DateError start end of
    Left  e → fail $ show e
    Right r → return r

dateImpreciseRangeTests ∷ TestTree
dateImpreciseRangeTests =
  testGroup "dateImpreciseRange"
            [ testCase "2019-11-14:2019-11-26" $
                  Right testDateImpreciseRange
                ≟ dateImpreciseR @DateError [dateImprecise|2019-11-14|]
                                                [dateImprecise|2019-11-26|]
            , testCase "2019-11-14:2019-11-13" $
                  Left (dateRangeError_ [dateImprecise|2019-11-14|]
                                        [dateImprecise|2019-11-13|])
                ≟ dateImpreciseR @DateError [dateImprecise|2019-11-14|]
                                            [dateImprecise|2019-11-13|]
            , testCase "2019-11-14:2019-11-14" $
                  Right (DateImpreciseRange ([dateImprecise|2019-11-14|]
                                            ,[dateImprecise|2019-11-14|]))
                ≟ dateImpreciseR @DateError [dateImprecise|2019-11-14|]
                                            [dateImprecise|2019-11-14|]
            , testCase "2019:2019" $
                  Right (DateImpreciseRange ([dateImprecise|2019|]
                                            ,[dateImprecise|2019|]))
                ≟ dateImpreciseR @DateError [dateImprecise|2019|]
                                            [dateImprecise|2019|]
            , testCase "2019:2018" $
                  Left (dateRangeError_ [dateImprecise|2019|]
                                        [dateImprecise|2018|])
                ≟ dateImpreciseR @DateError [dateImprecise|2019|]
                                            [dateImprecise|2018|]
            ]



----------------------------------------

instance Printable DateImpreciseRange where
  print (DateImpreciseRange (d0,d1))  = P.text $ [fmt|%T:%T|] d0 d1

--------------------

dateImpreciseRangePrintableTests ∷ TestTree
dateImpreciseRangePrintableTests =
  let check s dpr = testCase s $ s ≟ toString dpr
   in testGroup "Printable"
                [ check "2019-11-14:2019-11-26" testDateImpreciseRange
                , check "2017-11:2019-11"       testDateImpreciseRangeM
                , check "2017:2019"             testDateImpreciseRangeY
                ]

----------------------------------------

instance Parsecable DateImpreciseRange where
  parser = textual

--------------------

dateImpreciseRangeParsecableTests ∷ TestTree
dateImpreciseRangeParsecableTests =
  let check s dpr = testCase s $ Right dpr ≟ parsec' (""∷Text) s
   in testGroup "Parsecable"
                [ check "2019-11-14:2019-11-26" testDateImpreciseRange
                , check "2017-11:2019-11" testDateImpreciseRangeM
                , check "2017:2019" testDateImpreciseRangeY
                ]

----------------------------------------

instance Textual DateImpreciseRange where
  textual = ((,) ⊳ textual ⋪ string ":" ⊵ textual) ≫ uncurry dateImpreciseRM

--------------------

dateImpreciseRangeTextualTests ∷ TestTree
dateImpreciseRangeTextualTests =
  testGroup "Textual"
            [ testCase "2019-11-14" $
                Just testDateImpreciseRange  ≟ fromText "2019-11-14:2019-11-26"
            , testCase "2019-11"    $
                Just testDateImpreciseRangeM ≟ fromText "2017-11:2019-11"
            , testCase "2019"       $
                Just testDateImpreciseRangeY ≟ fromText "2017:2019"
            , testProperty "invertibleText" (propInvertibleText @DateImprecise)
            ]

----------------------------------------

instance Arbitrary DateImpreciseRange where
  arbitrary ∷ Gen DateImpreciseRange
  arbitrary = let isValid ∷ DateImpreciseRange → Bool
                  isValid (DateImpreciseRange (d0,d1)) =
                    case dateImpreciseR @DateError d0 d1 of
                      Left  _ → False
                      Right _ → True
               in (DateImpreciseRange ⊳ ((,) ⊳ arbitrary ⊵ arbitrary))
                  `suchThat` isValid

----------------------------------------

instance FromJSON DateImpreciseRange where
  parseJSON (String t) = case parseText t of
                           Parsed      d → return d
                           Malformed _ e → fail $ [fmt|%s (%t)|] e  t
  parseJSON invalid    = typeMismatch "DateImprecise" invalid

--------------------

dateImpreciseRangeFromJSONTests ∷ TestTree
dateImpreciseRangeFromJSONTests =
  testGroup "FromJSON"
            [ testCase "2019-11-14" $
                  Right testDateImpreciseRange
                ≟ unYaml @YamlParseError "2019-11-14:2019-11-26"
            , testCase "2019-11" $
                  Right testDateImpreciseRangeM
                ≟ unYaml @YamlParseError "2017-11:2019-11"
            , testCase "2019" $
                  Right testDateImpreciseRangeY
                ≟ unYaml @YamlParseError "2017:2019"
            ]

----------------------------------------

instance ToJSON DateImpreciseRange where
  toJSON d = String $ toText d

--------------------

dateImpreciseRangeToJSONTests ∷ TestTree
dateImpreciseRangeToJSONTests =
  let check s d = testCase s $ String (pack s) ≟ toJSON d
   in testGroup "ToJSON" [ check "2019-11-14:2019-11-26" testDateImpreciseRange
                         , check "2017-11:2019-11"       testDateImpreciseRangeM
                         , check "2017:2019"             testDateImpreciseRangeY
                         ]

----------------------------------------

dateImpreciseRange ∷ QuasiQuoter
dateImpreciseRange =
  mkQuasiQuoterExp "DateP" (\ s → ⟦ __fromString @DateImprecise s ⟧)

-- testing ---------------------------------------------------------------------

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

testDateP0 ∷ DateImprecise
testDateP0 = dateDay' [year|2019|] [month|11|] [dayOfM|14|]

testDateP1 ∷ DateImprecise
testDateP1 = dateDay' [year|2019|] [month|11|] [dayOfM|26|]

testDateImpreciseRange ∷ DateImpreciseRange
testDateImpreciseRange = DateImpreciseRange (testDateP0,testDateP1)

testDatePM0 ∷ DateImprecise
testDatePM0 = dateMonth [year|2017|] [month|11|]

testDatePM1 ∷ DateImprecise
testDatePM1 = dateMonth [year|2019|] [month|11|]

testDateImpreciseRangeM ∷ DateImpreciseRange
testDateImpreciseRangeM = DateImpreciseRange (testDatePM0,testDatePM1)

testDatePY0 ∷ DateImprecise
testDatePY0 = dateYear [year|2017|]

testDatePY1 ∷ DateImprecise
testDatePY1 = dateYear [year|2019|]

testDateImpreciseRangeY ∷ DateImpreciseRange
testDateImpreciseRangeY = DateImpreciseRange (testDatePY0,testDatePY1)

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DateImprecise" [ dateImpreciseRangeTests
                                  , dateImpreciseRangePrintableTests
                                  , dateImpreciseRangeTextualTests
                                  , dateImpreciseRangeParsecableTests
                                  , dateImpreciseRangeFromJSONTests
                                  , dateImpreciseRangeToJSONTests
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
