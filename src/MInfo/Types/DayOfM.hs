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

module MInfo.Types.DayOfM
  ( DayOfM( DayOfM ), dayOfM, tests )
where

import Prelude  ( Integer, Integral, (+), (-), error, fromInteger, toInteger )

-- base --------------------------------

import Control.Monad  ( fail, return )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ), maybe )
import Data.Ord       ( Ord )
import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Read      ( readMaybe )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromText, toString )
import Data.Textual.Integral  ( Decimal( Decimal ), nnUpTo )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary ) )

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

newtype DayOfM = DayOfM_ { unDayOfM ∷ 𝕎 31 }
  deriving (Eq,Lift,Ord,Show)

instance FromI DayOfM where
  fromI i = DayOfM_ ⊳ 𝕨 (toInteger i-1)

instance ToNum DayOfM where
  toNum (DayOfM_ (𝕎 i)) = fromInteger i + 1
  toNum (DayOfM_ _)      = ePatSymExhaustive

instance Printable DayOfM where
  print d = P.text $ [fmt|%02d|] (toNumW16 d)


dayOfMPrintableTests ∷ TestTree
dayOfMPrintableTests =
  let check s m = testCase s $ s ≟ toString m
   in testGroup "Printable"
                [ check "01"         (DayOfM_ $ 𝕎 0)
                , check "09"         (DayOfM_ $ 𝕎 8)
                , check "31"         (DayOfM_ $ 𝕎 30)
                ]

instance Textual DayOfM where
  textual = do
    m ← nnUpTo Decimal 2
    maybe (fail $ [fmt|bad day value %d|] m) return $ fromI' m

dayOfMTextualTests ∷ TestTree
dayOfMTextualTests =
  testGroup "Textual"
            [ testCase "12" $ Just (__fromI' 12) ≟ fromText @DayOfM "12"
            , testCase  "0" $ Nothing @DayOfM    ≟ fromText  "0"
            , testCase "32" $ Nothing @DayOfM    ≟ fromText "32"
            , testCase "31" $ Just (__fromI' 31) ≟ fromText @DayOfM "31"
            , testProperty "invertibleText" (propInvertibleText @DayOfM)
            ]


instance Arbitrary DayOfM where
  arbitrary = DayOfM_ ⊳ arbitrary

readY ∷ String → Maybe DayOfM
readY s = readMaybe s ≫ fromI' @DayOfM

readYI ∷ String → Maybe Integer
readYI = toInteger ∘ toNumW16 ⩺ readY

dayOfMPat ∷ Integer → Pat
dayOfMPat i = ConP 'DayOfM_ [ConP '𝕎 [LitP (IntegerL (i-1))]]

dayOfMQQ ∷ String → Maybe ExpQ
dayOfMQQ = (\ dom → ⟦dom⟧) ⩺ readY

dayOfM ∷ QuasiQuoter
dayOfM = 
  -- λ> runQ [p| Month_ (W 1) |]
  -- ConP MInfo.Types.Month.Month_ [ConP MInfo.BoundedN.W [LitP (IntegerL 1)]]
  mkQQCP "DayOfM" dayOfMQQ
                  (\ s → maybe (fail $ [fmt|failed to parse day-of-month '%s'|]
                                       s)
                              (Just ∘ return ∘ dayOfMPat) $ readYI s)


----------------------------------------

pattern DayOfM ∷ Integral α ⇒ α → DayOfM
pattern DayOfM i ← ((+1) ∘ toNum ∘ unDayOfM → i)
-- not bi-directional, because DayOfM i would be partial (would fail on
-- out-of-bounds values)
--                  where DayOfM i = __fromI i

dayOfMPatternTests ∷ TestTree
dayOfMPatternTests =
  let one        =  1 ∷ Integer
      seven      =  7 ∷ Integer
      twelve     = 12 ∷ Integer
      thirty_one = 31 ∷ Integer
   in testGroup "Pattern"
                [ testCase  "7" $ let DayOfM i = __fromI'  7 in i ≟ seven
                , testCase  "1" $ let DayOfM i = __fromI'  1 in i ≟ one
                , testCase  "0" $ assertAnyException "0 out of bounds" $
                                  let DayOfM i = __fromI'  0 in (i ∷ Integer)
                , testCase "12" $ let DayOfM i = __fromI' 12 in i ≟ twelve
                , testCase "31" $ let DayOfM i = __fromI' 31 in i ≟ thirty_one
                , testCase "32" $ assertAnyException "13 out of bounds" $
                                  let DayOfM i = __fromI' 32 in (i ∷ Integer)
                ]

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "DayOfM" [ dayOfMPrintableTests, dayOfMTextualTests
                           , dayOfMPatternTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
