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
  ( Artist, Catno, HasLiveDate( liveDate ), HasLiveLocation( liveLocation )
  , HasLiveType( liveType ), LiveLocation, LiveType(..), MultiDisc(..), Source
  , SourceVersion, TrackTitle( TrackTitle ), TrackVersion

  , tests )
where

import Prelude  ( Float, Int )

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON( parseJSON ), ToJSON( toJSON )
                         , Value( Number, String ), typeMismatch )

-- base --------------------------------

import Control.Monad   ( fail, return )
import Data.Either     ( either )
import Data.Eq         ( Eq )
import Data.Function   ( ($) )
import Data.Maybe      ( Maybe )
import Data.Semigroup  ( Semigroup( (<>) ) )
import Data.String     ( IsString, String )
import GHC.Generics    ( Generic )
import System.Exit     ( ExitCode )
import System.IO       ( IO )
import Text.Show       ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- date-imprecise ----------------------

import DateImprecise.DateImpreciseRange  ( DateImpreciseRange )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens' )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural      ( ℕ )

-- scientific --------------------------

import Data.Scientific  ( floatingOrInteger )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( Text, pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

data MultiDisc = SingleDisc | MultiDisc ℕ

------------------------------------------------------------

newtype Artist = Artist Text
  deriving (Eq,FromJSON,Generic,IsString,Show,ToJSON)

instance Printable Artist where
  print (Artist t) = P.text t

------------------------------------------------------------

newtype Source = Source Text
  deriving (Eq,FromJSON,Generic,IsString,Show,ToJSON)

instance Printable Source where
  print (Source t) = P.text t

------------------------------------------------------------

newtype SourceVersion = SourceVersion Text
  deriving (Eq,FromJSON,Generic,IsString,Show,ToJSON)

instance Printable SourceVersion where
  print (SourceVersion t) = P.text t

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

newtype LiveLocation = LiveLocation Text
  deriving (Eq,FromJSON,Generic,IsString,Show,ToJSON)

instance Printable LiveLocation where
  print (LiveLocation t) = P.text t

class HasLiveLocation α where
  liveLocation ∷ Lens' α (Maybe LiveLocation)

class HasLiveDate α where
  liveDate ∷ Lens' α (Maybe DateImpreciseRange)

------------------------------------------------------------

newtype Catno = Catno Text
  deriving (Eq, IsString, Show)

instance Printable Catno where
  print (Catno t) = P.text t

instance FromJSON Catno where
  parseJSON (String t) = return (Catno t)
  parseJSON (Number n) =
    return (Catno ∘ pack $ either show show (floatingOrInteger @Float @Int n))
  parseJSON invalid    = typeMismatch "String" invalid

instance ToJSON Catno where
  toJSON (Catno t) = String t

------------------------------------------------------------

data LiveType = NotLive | Live | LiveVocal | Session | Demo
  deriving (Eq, Show)

instance Semigroup LiveType where
  NotLive <> b = b
  a <> _       = a

instance Printable LiveType where
  print NotLive = P.text ""
  print Live      = "Live"
  print LiveVocal = "Live Vocal"
  print Session   = "Session"
  print Demo      = "Demo"

instance FromJSON LiveType where
  parseJSON (String "Live")       = return Live
  parseJSON (String "Live Vocal") = return LiveVocal
  parseJSON (String "Session")    = return Session
  parseJSON (String "Demo")       = return Demo
  parseJSON (String t)            = fail $ [fmt|unrecognized live type '%t'|] t
  parseJSON invalid               = typeMismatch "String" invalid

instance ToJSON LiveType where
  toJSON l = String (toText l)

class HasLiveType α where
  liveType ∷ Lens' α LiveType

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
