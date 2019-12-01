{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}

module MInfo.Types
  ( Artist )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON, ToJSON )

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.String    ( IsString, String )
import GHC.Generics   ( Generic )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural      ( ℕ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

newtype Artist = Artist Text
  deriving (Eq,FromJSON,Generic,IsString,Show,ToJSON)

instance Printable Artist where
  print (Artist t) = P.text t

{-
instance FromJSON Artist where
  parseJSON (String t) = return (Artist t)
  parseJSON invalid    = typeMismatch "String" invalid
-}

{-
instance ToJSON Artist where
  toJSON (Artist t) = String t
-}

-- testing ---------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MInfo.Types" [ ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
