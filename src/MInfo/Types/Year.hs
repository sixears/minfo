{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module MInfo.Types.Year
  ( Year, pattern Year, year, tests )
where

import Prelude  ( Integer, Integral, (+), (-), error, fromInteger, toInteger )

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
import Text.Read      ( read, readMaybe )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromText, toString )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Monad        ( (≫) )
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

import TastyPlus  ( assertAnyException, propInvertibleText
                  , runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ, Lit( IntegerL ), Pat( ConP, LitP ) )
import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.BoundedN        ( 𝕎, pattern 𝕎, 𝕨 )
import MInfo.Types.ToNum     ( ToNum( toNum, toNumW16 ) )
import MInfo.Util            ( mkQQCP )

import MInfo.Types.FromI     ( FromI( fromI, fromI', __fromI' ) )

--------------------------------------------------------------------------------

ePatSymExhaustive ∷ α
ePatSymExhaustive = error "https://gitlab.haskell.org/ghc/ghc/issues/10339"

------------------------------------------------------------

newtype Year = Year_ { unYear ∷ 𝕎 200 }
  deriving (Eq,Generic,Lift,Ord,Show)

instance FromI Year where
  fromI i = Year_ ⊳ 𝕨 (toInteger i-1900)

instance ToNum Year where
  toNum (Year_ (𝕎 i)) = fromInteger i + 1900
  toNum (Year_ _)      = ePatSymExhaustive

instance Printable Year where
  print y = P.text $ [fmt|%04d|] (toNumW16 y)

yearPrintableTests ∷ TestTree
yearPrintableTests =
  let check s m = testCase s $ s ≟ toString m
   in testGroup "Printable"
                [ check "1900"         (Year_ $ 𝕎 0)
                , check "1908"         (Year_ $ 𝕎 8)
                , check "2011"         (Year_ $ 𝕎 111)
                ]

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
  arbitrary = Year_ ⊳ arbitrary

readY ∷ String → Maybe Year
readY s = readMaybe s ≫ fromI' @Year

readYI ∷ String → Maybe Integer
readYI = toInteger ∘ toNumW16 ⩺ readY

yearPat ∷ Integer → Pat
yearPat i = ConP 'Year_ [ConP '𝕎 [LitP (IntegerL (i-1900))]]

yearQQ ∷ String → Maybe ExpQ
yearQQ = (\ y → ⟦y⟧) ⩺ readY

year ∷ QuasiQuoter
year = 
  -- λ> runQ [p| Month_ (W 1) |]
  -- ConP MInfo.Types.Month.Month_ [ConP MInfo.BoundedN.W [LitP (IntegerL 1)]]
  mkQQCP "Year" yearQQ
                (\ s → maybe (fail $ [fmt|failed to parse day-of-month '%s'|] s)
                             (Just ∘ return ∘ yearPat) $ readYI s)
--  mkQuasiQuoterExpP "Year" (\ s → ⟦ __fromString @Year s ⟧)
--                           (mkWPatQf (subtract 1900) 'Year_)

----------------------------------------

pattern Year ∷ Integral α ⇒ α → Year
pattern Year i ← ((+1900) ∘ toNum ∘ unYear → i)
-- not bi-directional, because Year i would be partial (would fail on
-- out-of-bounds values)
--                  where Year i = __fromI i

yearPatternTests ∷ TestTree
yearPatternTests =
  let noone      = 1901 ∷ Integer
   in testGroup "Pattern"
                [ testCase "1901" $ let Year i = __fromI'  1901 in i ≟ noone
                , testCase "1899" $ assertAnyException "1899 out of bounds" $
                                  let Year i = __fromI' 1899 in (i ∷ Integer)
                , testCase "2101" $ assertAnyException "2101 out of bounds" $
                                  let Year i = __fromI' 2101 in (i ∷ Integer)
                ]

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Year" [ yearPrintableTests, yearTextualTests
                         , yearPatternTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
