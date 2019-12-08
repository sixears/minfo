{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MInfo.T.Tests
  ( tests )
where

-- base --------------------------------

import Data.String  ( String )
import System.Exit  ( ExitCode )
import System.IO    ( IO )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  MInfo.BoundedN
import qualified  MInfo.Types
import qualified  MInfo.Types.Dateish
import qualified  MInfo.Types.Month
import qualified  MInfo.Types.Year
import qualified  MInfo.Types.Track
import qualified  MInfo.Types.Tracks
import qualified  MInfo.Types.Info
import qualified  MInfo.Types.DayOfM
import qualified  MInfo.Types.DateImprecise
import qualified  MInfo.Types.DatePR
import qualified  MInfo.PYaml

--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "MInfo" [ MInfo.BoundedN.tests
                    , MInfo.Types.tests
                    , MInfo.Types.Dateish.tests
                    , MInfo.Types.Month.tests
                    , MInfo.Types.Year.tests
                    , MInfo.Types.Track.tests
                    , MInfo.Types.Tracks.tests
                    , MInfo.Types.Info.tests
                    , MInfo.Types.DayOfM.tests
                    , MInfo.Types.DatePR.tests
                    , MInfo.Types.DateImprecise.tests
                    , MInfo.PYaml.tests
                    ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
