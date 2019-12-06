{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}

module MInfo.Types.Year
  ( Year( Year ), year, tests )
where

import Prelude  ( (+), (-), error, fromInteger, toInteger )

-- base --------------------------------

import Control.Monad  ( fail, return )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just ), maybe )
import Data.Ord       ( Ord )
import Data.String    ( String )
import GHC.Generics   ( Generic )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Read      ( read )
import Text.Show      ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual ), fromText )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary ( Arbitrary( arbitrary ) )

-- parsers ------------------------------

import Text.Parser.Char         ( digit )
import Text.Parser.Combinators  ( count )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( propInvertibleText, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.BoundedN        ( 𝕎, pattern 𝕎, 𝕨 )
import MInfo.Util            ( __fromString, mkQuasiQuoterExp )

import MInfo.Types.FromI     ( FromI( fromI, fromI', __fromI' ) )
import MInfo.Types.ToWord16  ( ToWord16( toWord16 ) )

--------------------------------------------------------------------------------

ePatSymExhaustive ∷ α
ePatSymExhaustive = error "https://gitlab.haskell.org/ghc/ghc/issues/10339"

------------------------------------------------------------

newtype Year = Year (𝕎 200)
  deriving (Eq,Generic,Ord,Show)

instance FromI Year where
  fromI i = Year ⊳ 𝕨 (toInteger i-1900)

instance ToWord16 Year where
  toWord16 (Year (𝕎 i)) = fromInteger i + 1900
  toWord16 (Year _)      = ePatSymExhaustive

instance Printable Year where
  print y = P.text $ [fmt|%d|] (toWord16 y)

instance Textual Year where
  textual = do
    y ← read ⊳ count 4 digit
    maybe (fail $ [fmt|bad year value %d|] y) return $ fromI' y

yearTextualTests ∷ TestTree
yearTextualTests =
  testGroup "Textual"
            [ testCase "2014" $ Just (__fromI' 2014) ≟ fromText @Year "2014"
            , testCase "2019" $ Just (__fromI' 2019) ≟ fromText @Year "2019"
            , testProperty "invertibleText" (propInvertibleText @Year)
            ]

instance Arbitrary Year where
  arbitrary = Year ⊳ arbitrary

year ∷ QuasiQuoter
year = mkQuasiQuoterExp "Year" (\ s → ⟦ __fromString @Year s ⟧)

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Year" [ yearTextualTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
