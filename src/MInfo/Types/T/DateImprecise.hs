{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MInfo.Types.T.DateImprecise
  ( tests )
where

-- base --------------------------------

import Data.Bool      ( Bool( False, True ), not )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just ) )
import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Tasty    ( (≟) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- time --------------------------------

import Data.Time  ( fromGregorian )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.Types.FromI          ( fromI' )
import MInfo.Types.DateImprecise  ( dateImprecise, dayDate
                                  , pattern MonthDate, pattern YearDate )
import MInfo.Types.Month          ( month )
import MInfo.Types.Year           ( year )

--------------------------------------------------------------------------------

dateImpreciseTests ∷ TestTree
dateImpreciseTests =
  testGroup "dateImprecise"
            [ testCase "DateImprecise 2019-12-24" $
                dayDate (fromGregorian 2019 12 24) ≟ [dateImprecise|2019-12-24|]
            , testCase "DateImprecise 2019-12" $
                MonthDate [year|2019|] [month|12|] ≟ [dateImprecise|2019-12|]
            , testCase "DateImprecise 2019" $
                YearDate [year|2019|]              ≟ [dateImprecise|2019|]
            ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DateImpreciseQQ" [ dateImpreciseTests ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
