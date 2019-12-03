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
  ( Artist, LiveType(..), TrackTitle, TrackVersion )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON( parseJSON ), ToJSON( toJSON )
                         , Value( String ), typeMismatch )

-- base --------------------------------

import Control.Monad   ( fail, return )
import Data.Eq         ( Eq )
import Data.Function   ( ($) )
import Data.Semigroup  ( Semigroup( (<>) ) )
import Data.String     ( IsString, String )
import GHC.Generics    ( Generic )
import System.Exit     ( ExitCode )
import System.IO       ( IO )
import Text.Show       ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

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

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

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

------------------------------------------------------------

newtype TrackTitle = TrackTitle Text
  deriving (Eq,FromJSON,Generic,IsString,Show,ToJSON)

instance Printable TrackTitle where
  print (TrackTitle t) = P.text t

------------------------------------------------------------

newtype TrackVersion = TrackVersion Text
  deriving (Eq,FromJSON,Generic,IsString,Show,ToJSON)

instance Printable TrackVersion where
  print (TrackVersion t) = P.text t

------------------------------------------------------------

data LiveType = NotLive | Live | Session | Demo
  deriving (Eq, Show)

instance Semigroup LiveType where
  NotLive <> b = b
  a <> _       = a

instance Printable LiveType where
  print NotLive = P.text ""
  print Live    = "Live"
  print Session = "Session"
  print Demo    = "Demo"

instance FromJSON LiveType where
  parseJSON (String "Live")    = return Live
  parseJSON (String "Session") = return Session
  parseJSON (String "Demo")    = return Demo
  parseJSON (String t)         = fail $ [fmt|unrecognized live type '%t'|] t
  parseJSON invalid    = typeMismatch "String" invalid

instance ToJSON LiveType where
  toJSON l = String (toText l)

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
