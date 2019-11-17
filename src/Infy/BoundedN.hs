{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Infy.BoundedN
  ( -- don't export the constructor, so clients can't create out-of-range values
    BoundedN, 𝕎
  )
where

import Debug.Trace  ( trace,traceShow )

import Prelude  ( Bounded, Enum( pred, succ ), Integer, Integral( toInteger )
                , Num, Real
                , (+), (-), (*)
                , abs, enumFrom, enumFromThen, enumFromThenTo, enumFromTo, error
                , fromEnum, fromInteger, maxBound, minBound, mod, quot, quotRem
                , rem, signum, toEnum, toInteger, toRational )

-- base --------------------------------

import Control.DeepSeq        ( NFData )
import Control.Exception      ( SomeException, evaluate, handle )
import Control.Monad          ( guard, return )
import Data.Bool              ( Bool( False, True ) )
import Data.Either            ( Either( Left, Right ) )
import Data.Eq                ( Eq )
import Data.Function          ( ($), (.), const )
import Data.Int               ( Int )
import Data.List              ( isSuffixOf, last )
import Data.Ord               ( Ord, (<), (>) )
import Data.Ratio             ( (%) )
import Data.String            ( String, unlines )
import Data.Typeable          ( typeOf )
import GHC.Generics           ( Generic )
import GHC.Read               ( expectP )
import GHC.TypeLits           ( KnownNat, Nat, natVal )
import System.Exit            ( ExitCode )
import System.IO              ( IO )
import Text.ParserCombinators.ReadPrec
                              ( prec )
import Text.Read              ( Read( readPrec ), parens )
import Text.Read.Lex          ( Lexeme( Ident ) )
import Text.Show              ( Show( show ), showParen, showsPrec, showString )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode        ( (∧), (∨) )
import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )
import Data.Ord.Unicode         ( (≥) )
import Numeric.Natural.Unicode  ( ℕ )
import Prelude.Unicode          ( ℤ )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( force )

-- finite-typelits ---------------------

import Data.Finite  ( Finite, finite, getFinite )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Monad    ( (≫), (⪼) )
import Data.MoreUnicode.Tasty    ( (≟) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, assertBool, assertFailure, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertSuccess, propInvertibleText, runTestsP
                  , runTestsReplay, runTestTree )


-- text --------------------------------

import Data.Text  ( Text, pack, unpack )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

newtype BoundedN (n ∷ Nat) = BoundedN (Finite n)
  deriving (Bounded,Enum,Eq,Integral,Generic,NFData,Ord,Read,Real,Show)

type 𝕎 = BoundedN

boundedTests =
  testGroup "Bounded"
    [ testCase "minBound" $ 0 ≟ minBound @(𝕎 7)
    , testCase "maxBound" $ 6 ≟ maxBound @(𝕎 7)
    ]

{-
instance KnownNat n => Enum (𝕎 n) where
  fromEnum ∷ 𝕎 n → Int
  fromEnum = fromEnum ∘ toInteger
  toEnum ∷ Int → 𝕎 n
  toEnum = fromInteger ∘ toEnum
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if x ≥ y then minBound else maxBound)
-}

{- | Check that any exception is thrown.  Any exception will cause the test
     to pass; no exception will cause it to fail.
 -}
assertAnyException ∷ (NFData α) ⇒ String → IO α → IO ()
assertAnyException n = assertException n (const True)

{- | Check that an exception is thrown.  Any exception that is thrown is
     checked by the given predicate; the predicate pass to indicate that the
     exception is as desired; and thus a @False@ will cause a test failure.  The
     test itself, if it returns a value (without an exception) will pass; but
     note that being IO, it can itself run tests...
 -}
assertException ∷ (NFData α) ⇒ String → (SomeException → Bool) → IO α → IO ()
assertException n p io =
  handle (return ∘ Left) (Right ⊳ (io ≫ evaluate ∘ force)) ≫ \ case
    Left e → assertBool n (p e)
    Right r → assertFailure ("no exception thrown: " ⊕ n)

enumTests =
  let assertFail ∷ String → 𝕎 7 → TestTree
      assertFail n v = testCase n $ assertAnyException n (return v)
   in testGroup "Enum"
        [ testCase   "succ 5"   $ 6 ≟ succ @(𝕎 7) 5
        , testCase   "pred 5"   $ 4 ≟ pred @(𝕎 7) 5
        , assertFail "pred 0"   (pred 0)
        , assertFail "succ 6"   (succ 6)
        , testCase   "toEnum 4" $ (4 ∷ 𝕎 7) ≟ toEnum 4
        , assertFail "toEnum 7" (toEnum 7)
        , testCase   "fromEnum 4" $ 4 ≟ (fromEnum (4 ∷ 𝕎 7))
        , testCase   "enumFrom 4" $ [4,5,6] ≟ enumFrom (4 ∷ 𝕎 7)
        , testCase   "enumFromThen 1 3" $ [1,3,5] ≟ enumFromThen (1 ∷ 𝕎 7) 3
        , testCase   "enumFromTo 1 4" $ [1,2,3,4] ≟ enumFromTo   (1 ∷ 𝕎 7) 4
        , testCase   "enumFromThenTo 8 5 0" $
            [8,5,2] ≟ enumFromThenTo (8 ∷ 𝕎 9) 5 0
        ]

{-
instance KnownNat n ⇒ Integral (BoundedN n) where
  toInteger ∷ BoundedN n → ℤ
  toInteger (BoundedN i) = getFinite i
-}

{-
  toEnum = finite . toEnum
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if x ≥ y then minBound else maxBound)
-}

-- Enum,Integral

instance KnownNat n ⇒ Num (BoundedN n) where
  fromInteger ∷ Integer → BoundedN n
  fromInteger = BoundedN ∘ finite

----------------------------------------

{- | Convert an 'Integer' into a 'Finite', throwing an error if the input is
     out of bounds. -}
{-
-- finite ∷ (KnownNat m,KnownNat n) ⇒ Integer → Finite2 m n
finite x = result
           where result = if x < natVal result ∧ x ≥ 0
                          then Finite2 x
                          else error $ "finite: Integer " ⊕ show x ⊕ " is not representable in Finite " ⊕ show (natVal result)
-}
{-
-- | Convert a 'Finite' into the corresponding 'Integer'.
getFinite :: Finite2 m n -> Integer
getFinite (Finite2 x) = x

-- | Throws an error for @'Finite' 0@
instance KnownNat n => Bounded (Finite2 m n) where
    maxBound = result
        where
            result = if natVal result > 0
                then Finite2 $ natVal result - 1
                else error "maxBound: Finite 0 is uninhabited"
    minBound = result
        where
            result = if natVal result > 0
                then Finite2 0
                else error "minBound: Finite 0 is uninhabited"

instance KnownNat n => Enum (Finite2 m n) where
    fromEnum = fromEnum . getFinite
    toEnum = finite . toEnum
    enumFrom x = enumFromTo x maxBound
    enumFromThen x y = enumFromThenTo x y (if x ≥ y then minBound else maxBound)

instance Show (Finite2 m n) where
    showsPrec d (Finite2 x) = showParen (d > 9) $ showString "finite " . showsPrec 10 x

instance KnownNat n => Read (Finite2 m n) where
    readPrec = parens $ prec 10 $ do
                 expectP (Ident "finite")
                 x <- readPrec
                 let result = finite x
                 guard (x ≥ 0 ∧ x < natVal result)
                 return result

-- | Modular arithmetic. Only the 'fromInteger' function is supposed to be useful.
instance KnownNat n => Num (Finite2 m n) where
    fx@(Finite2 x) + Finite2 y = Finite2 $ (x + y) `mod` natVal fx
    fx@(Finite2 x) - Finite2 y = Finite2 $ (x - y) `mod` natVal fx
    fx@(Finite2 x) * Finite2 y = Finite2 $ (x * y) `mod` natVal fx
    abs fx = fx
    signum _ = fromInteger 1
    fromInteger x = result
        where
            result = if x < natVal result ∧ x ≥ 0
                then Finite2 x
                else error $ "fromInteger: Integer " ⊕ show x ⊕ " is not representable in Finite " ⊕ show (natVal result)

instance KnownNat n => Real (Finite2 m n) where
    toRational (Finite2 x) = x % 1

-- | __Not__ modular arithmetic.
instance KnownNat n => Integral (Finite2 m n) where
    quotRem (Finite2 x) (Finite2 y) = (Finite2 $ x `quot` y, Finite2 $ x `rem` y)
    toInteger (Finite2 x) = x

instance NFData (Finite2 m n)
-}

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "BoundedN" [ boundedTests, enumTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
