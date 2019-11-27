{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module MInfo.BoundedN
  ( -- don't export the constructor, so clients can't create out-of-range values
    BoundedN, 𝕎, pattern 𝕎, pattern 𝕎', 𝕨
  )
where

import Prelude  ( Bounded, Enum( pred, succ ), Integer, Integral( toInteger )
                , enumFrom, enumFromThen, enumFromThenTo, enumFromTo, error
                , fromEnum, fromInteger, maxBound, minBound, toEnum, toInteger
                )

-- base --------------------------------

import Data.Bool              ( not, otherwise )
import Data.Eq                ( Eq )
import Data.Function          ( ($) )
import Data.Maybe             ( Maybe( Just, Nothing ) )
import Data.Ord               ( Ord, (<) )
import Data.String            ( String )
import GHC.Generics           ( Generic )
import GHC.TypeLits           ( KnownNat, Nat )
import System.Exit            ( ExitCode )
import System.IO              ( IO )
import Text.Read              ( Read )
import Text.Show              ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode        ( (∧) )
import Data.Eq.Unicode          ( (≡) )
import Data.Ord.Unicode         ( (≤), (≥) )
import Data.Function.Unicode    ( (∘) )
import Numeric.Natural.Unicode  ( ℕ )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- finite-typelits ---------------------

import Data.Finite  ( Finite, getFinite, packFinite )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Tasty    ( (≟) )

-- QuickCheck --------------------------

import Test.QuickCheck        ( Property, property )
import Test.QuickCheck.Arbitrary
                              ( Arbitrary( arbitrary ), arbitraryBoundedEnum )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertAnyException, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

newtype BoundedN (n ∷ Nat) = BoundedN { toFinite ∷ Finite n }
  deriving (Bounded,Enum,Eq,Generic,NFData,Ord,Read,Show)

type 𝕎 = BoundedN

----------------------------------------

{- | Convert an Integral to a 𝕎, hopefully. -}
toBoundedN ∷ (KnownNat ν, Integral α) ⇒ α → Maybe (𝕎 ν)
toBoundedN = BoundedN ⩺ packFinite ∘ toInteger

{- | Alias for `toBoundedN`, with Integer to avoid type ambiguity -}
toBoundedN' ∷ KnownNat ν ⇒ Integer → Maybe (𝕎 ν)
toBoundedN' = toBoundedN

--------------------

{- | Alias for @toBoundedN@, specifying Integer input for ease of literal
     use. -}
𝕨 ∷ KnownNat ν ⇒ Integer → Maybe (𝕎 ν)
𝕨 = toBoundedN

--------------------

toBoundedNTests ∷ TestTree
toBoundedNTests =
  testGroup "toBoundedN"
            [ testCase "toBoundedN 5" $ Just (𝕎 @6 5) ≟ toBoundedN' 5
            , testCase "toBoundedN 7" $ (Nothing ∷ Maybe (𝕎 6)) ≟ 𝕨 7
            , testCase "toBoundedN @6 7" $ Nothing      ≟ toBoundedN' @6 7
            , testCase "toBoundedN @8 7" $ Just (𝕎 7) ≟ toBoundedN' @8 7
            ]

----------------------------------------

{- | *PARTIAL* Convert an Integral to a 𝕎' (or bust). -}
__toBoundedN ∷ (KnownNat ν, Integral α) ⇒ α → 𝕎 ν
__toBoundedN i | i < 0     = error $ [fmt|%d < 0|] i
               | otherwise = case toBoundedN i of
                               Just n  → n
                               Nothing → error $ [fmt|out of bounds: %d|] i

{- | Alias for `__toBoundedN`, with Integer to avoid type ambiguity.
    *PARTIAL* Convert an Integral to a 𝕎' (or bust). -}
__toBoundedN' ∷ KnownNat ν ⇒ Integer → 𝕎 ν
__toBoundedN' = __toBoundedN

__toBoundedNTests ∷ TestTree
__toBoundedNTests =
  testGroup "__toBoundedN"
            [ testCase "__toBoundedN 5" $ (𝕎 @6 5 ∷ 𝕎 6) ≟ __toBoundedN' @6 5
            , testCase "__toBoundedN 7" $
                assertAnyException "__toBoundedN 7" $ __toBoundedN' @6 7
            ]

----------------------------------------

{- | Pattern to (de)construct a BoundedN (A.K.A., 𝕎') from any integral value.
     *BEWARE* that the constructor is *PARTIAL* - you can, for example, write
     𝕎' @3 (-1), and it will compile (but will diverge under evaluation.
 -}
pattern 𝕎 ∷ KnownNat ν ⇒ Integer → 𝕎 ν
pattern 𝕎 i ← ((getFinite ∘ toFinite) → i)
              where 𝕎 i = __toBoundedN i

{- | Alias for 𝕎, for any @Integral@. -}
pattern 𝕎' ∷ (KnownNat ν, Integral α) ⇒ α → 𝕎 ν
pattern 𝕎' i ← ((fromInteger ∘ getFinite ∘ toFinite) → i)
              where 𝕎' i = __toBoundedN i

--------------------

𝕨Tests ∷ TestTree
𝕨Tests =
  let five  = 𝕎 @7 5
      seven = 𝕎 @7 7
   in testGroup "𝕎'"
                [ testCase "five"  $ 5 ≟ (\ case (𝕎 x) → x; _ → -1) five
                , testCase "seven" $
                  assertAnyException "seven" $ (\ case (𝕎 x) → x; _ → 1) seven
                , testCase "five" $ five ≟ 𝕎 5
                , testCase "seven" $ assertAnyException "seven" $ 𝕎 @7 7
                , testCase "-1" $ assertAnyException "-1" $ 𝕎 @3 (-1)
                ]

instance KnownNat ν ⇒ Arbitrary (BoundedN ν) where
  arbitrary = BoundedN ⊳ arbitraryBoundedEnum

arbitraryTests ∷ TestTree
arbitraryTests =
  let propBounded ∷ KnownNat ν ⇒ BoundedN ν → Property
      propBounded n = property $ n ≥ 𝕎 0 ∧ n ≤ maxBound
   in testGroup "Arbitrary"
                [ testProperty "properlyBounded" (propBounded @137) ]

-- testing ---------------------------------------------------------------------

boundedTests ∷ TestTree
boundedTests =
  testGroup "Bounded"
    [ testCase "minBound" $ 𝕎 0 ≟ minBound @(𝕎 7)
    , testCase "maxBound" $ 𝕎 6 ≟ maxBound @(𝕎 7)
    ]

----------------------------------------

enumTests ∷ TestTree
enumTests =
  let assertFail ∷ String → 𝕎 7 → TestTree
      assertFail n v = testCase n $ assertAnyException n v
   in testGroup "Enum"
        [ testCase   "succ 5"   $ 𝕎 6 ≟ succ (𝕎 @7 5)
        , testCase   "pred 5"   $ 𝕎 4 ≟ pred (𝕎 @7 5)
        , assertFail "pred 0"   (pred $ 𝕎 0)
        , assertFail "succ 6"   (succ $ 𝕎 6)
        , testCase   "toEnum 4" $ (𝕎 @7 4) ≟ toEnum 4
        , assertFail "toEnum 7" (toEnum $ 7)
        , testCase   "fromEnum 4" $ 4 ≟ (fromEnum (𝕎 @7 4))
        , testCase   "enumFrom 4" $
            [𝕎 4, 𝕎 5, 𝕎 6] ≟ enumFrom (𝕎 @7 4)
        , testCase   "enumFromThen 1 3" $
            [𝕎 1, 𝕎 3, 𝕎 5] ≟ enumFromThen (𝕎 @7 1) (𝕎 3)
        , testCase   "enumFromTo 1 4" $
            [𝕎 1, 𝕎 2, 𝕎 3, 𝕎 4] ≟ enumFromTo (𝕎 @7 1) (𝕎 4)
        , testCase   "enumFromThenTo 8 5 0" $
              [𝕎 8, 𝕎 5, 𝕎 2]
            ≟ enumFromThenTo (𝕎 @9 8) (𝕎 5) (𝕎 0)
        ]

----------------------------------------

eqTests ∷ TestTree
eqTests =
  testGroup "Eq" [ testCase "2==2" $ 𝕎 2 ≟ (𝕎 @9 2)
                 , testCase "2/=3" $ assertBool "2/=3" (not $ 𝕎 2 ≡ 𝕎 @7 3)
                 ]
------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "BoundedN" [ boundedTests, enumTests, eqTests, arbitraryTests
                             , toBoundedNTests, __toBoundedNTests, 𝕨Tests
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
