{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module MInfo.Types.Track
  ( Track( Track )
  , album, album_version, artist, blankTrack, liveDate, liveLocation, liveType
  , title, version

  , tests
  , _track1, _track2, _track3, _track4, _track5
  )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Object, String ), (.:?), (.!=), typeMismatch )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Either    ( Either( Right ) )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ), maybe )
import Data.String    ( String )
import GHC.Generics   ( Generic )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import qualified  Data.ByteString  as  BS
import Data.ByteString  ( ByteString )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- date-imprecise ----------------------

import DateImprecise.DateImpreciseRange  ( DateImpreciseRange
                                         , dateImpreciseRange )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Natural      ( ℕ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text     ( Text, dropEnd, intercalate, unlines )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON )
                  , (.=), object )


-- yaml-plus ---------------------------

import YamlPlus        ( unYaml )
import YamlPlus.Error  ( YamlParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.Types  ( Artist, HasLiveDate( liveDate )
                    , HasLiveLocation( liveLocation ), HasLiveType( liveType )
                    , LiveLocation, LiveType( Demo, Live, LiveVocal, NotLive
                                            , Session )
                    , Source, SourceVersion, TrackTitle( TrackTitle )
                    , TrackVersion
                    )

--------------------------------------------------------------------------------

data Track = Track { _artist        ∷ Maybe Artist
                   , _title         ∷ Maybe TrackTitle
                   , _version       ∷ Maybe TrackVersion
                   , _live_type     ∷ LiveType
                   , _live_location ∷ Maybe LiveLocation
                   , _live_date     ∷ Maybe DateImpreciseRange
                   , _album         ∷ Maybe Source
                   , _album_version ∷ Maybe SourceVersion
                   }
  deriving (Eq, Generic, Show)

artist        ∷ Lens' Track (Maybe Artist)
artist        = lens _artist        (\ t a → t { _artist = a })

title         ∷ Lens' Track (Maybe TrackTitle)
title         = lens _title         (\ r t → r { _title = t })

version       ∷ Lens' Track (Maybe TrackVersion)
version       = lens _version       (\ t v → t { _version = v })

album         ∷ Lens' Track (Maybe Source)
album         = lens _album         (\ t a → t { _album = a })

album_version ∷ Lens' Track (Maybe SourceVersion)
album_version = lens _album_version (\ t v → t { _album_version = v })

instance HasLiveType Track where
  liveType   ∷ Lens' Track LiveType
  liveType   = lens _live_type     (\ r y → r { _live_type = y })

instance HasLiveLocation Track where
  liveLocation ∷ Lens' Track (Maybe LiveLocation)
  liveLocation = lens _live_location (\ r l → r { _live_location = l })

instance HasLiveDate Track where
  liveDate     ∷ Lens' Track (Maybe DateImpreciseRange)
  liveDate     = lens _live_date     (\ r d → r { _live_date = d })

instance FromJSON Track where
  parseJSON json = case json of
      Object o → Track ⊳ do a ← o .:? "artist"
                            s ← o .:? "artists"
                            return $ a ∤ s
                       ⊵ o .:? "title"
                       ⊵ o .:? "version"
                       ⊵ o .:? "live_type" .!= NotLive
                       ⊵ o .:? "live_location"
                       ⊵ o .:? "live_date"
                       ⊵ o .:? "album"
                       ⊵ o .:? "album_version"
      String s → return $ Track Nothing (Just $ TrackTitle s) Nothing
                                NotLive Nothing Nothing Nothing Nothing
      invalid  → typeMismatch "String|Object (Track)" invalid

trackFromJSONTests ∷ TestTree
trackFromJSONTests =
  let t0 ∷ ByteString
      t0 = BS.intercalate "\n" [ "title: Condemnation" ]
      t1 ∷ ByteString
      t1 = BS.intercalate "\n" [ "title: Judas"
                               , "artist: Depeche"
                               , "live_type: Live"
                               , "live_date: 1993-07-29"
                               ]
      t6 ∷ ByteString
      t6 = BS.intercalate "\n" [ "title: Two Minute Warning"
                               , "artists: DM"
                               , "album: My Album"
                               , "album_version: AVersion"
                               ]
      t9 = BS.intercalate "\n" [ "Shelter from the Rain" ]
      e0 ∷ Track
      e0 = Track Nothing (Just "Condemnation") Nothing NotLive Nothing Nothing
                 Nothing Nothing
      e1 ∷ Track
      e1 = Track (Just "Depeche") (Just "Judas") Nothing Live Nothing
                 (Just $ [dateImpreciseRange|1993-07-29|]) Nothing Nothing
   in testGroup "trackFromJSON"
                [ testCase "t0" $ Right e0 @=? unYaml @YamlParseError t0
                , testCase "t1" $ Right e1 @=? unYaml @YamlParseError t1
                , testCase "t6" $ Right _track6 @=? unYaml @YamlParseError t6
                , testCase "t9" $ Right _track9 @=? unYaml @YamlParseError t9
                ]

instance ToJSON Track where
  toJSON (Track a t v y l d _ _) =
    let maybel k x = maybe [] (\ x' → [ k .= toJSON x' ]) x
        fields = ю [ maybel "artist" a
                   , [ "title" .= t ]
                   , maybel "version" v
                   , case y of
                       NotLive → []
                       _       → ю [ [ "live_type" .= toJSON y ]
                                   , maybel "live_location" l
                                   , maybel "live_date" d
                                   ]
                   ]
     in object fields

{- | Yaml quote: quote a string as necessary for yaml. -}
yquote ∷ Text → Text
yquote t = "\"" ⊕ t ⊕ "\""

instance Printable Track where
  print (Track a t v y l d _ _) =
    let toj ∷ Show α ⇒ Maybe α → Text
        toj Nothing  = "~"
        toj (Just x) = toText (show x)
        toj' ∷ Printable α ⇒ Maybe α → Text
        toj' Nothing  = "~"
        toj' (Just x) = yquote $ toText x
        tot ∷ Show α ⇒ Text → Maybe α → Text
        tot i x = i ⊕ ": " ⊕ toj x
        tot' ∷ Printable α ⇒ Text → Maybe α → Text
        tot' i x = i ⊕ ": " ⊕ toj' x
        tom ∷ Show α ⇒ Text → Maybe α → [Text]
        tom _ Nothing  = []
        tom i (Just x) = [ tot i (Just x) ]
        tom' ∷ Printable α ⇒ Text → Maybe α → [Text]
        tom' _ Nothing  = []
        tom' i (Just x) = [ tot' i (Just x) ]
        unl ∷ [Text] → Text
        unl = dropEnd 1 ∘ unlines
     in P.text ∘ unl $ (tom "artist" (toText ⊳ a))
                     ⊕ [tot' "title" t]
                     ⊕ (tom "version" v)
                     ⊕ case y of
                         NotLive → []
                         _ → ["live_type: "⊕ toText y]
                     ⊕ (tom' "live_location" l)
                     ⊕ (tom "live_date" d)

trackPrintableTests ∷ TestTree
trackPrintableTests =
  let e1 = intercalate "\n" [ "artist: \"Depeche Mode\""
                            , "title: \"Can't Get Enough\""
                            , "live_type: Live"
                            , "live_location: \"Hammersmith Odeon\""
                            ]
      t1 = Track (Just "Depeche Mode") (Just "Can't Get Enough") Nothing
                 Live (Just "Hammersmith Odeon") Nothing Nothing Nothing
   in testGroup "Printable" [ testCase "t1" $ e1 ≟ toText t1
                            ]

blankTrack ∷ Track
blankTrack = Track Nothing Nothing Nothing NotLive Nothing Nothing Nothing
                   Nothing

instance Printable [Track] where
  print ts = P.text $ intercalate "\n" (toText ⊳ ts)

--------------------------------------------------------------------------------
--                                 test data                                  --
--------------------------------------------------------------------------------

_track1 ∷ Track
_track1 = Track (Just "Tricky") (Just "Judas") Nothing Demo Nothing
                (Just $ [dateImpreciseRange|1993-07-29|]) Nothing Nothing
_track2 ∷ Track
_track2 = Track Nothing (Just "Mercy in You") Nothing Session Nothing
                (Just $ [dateImpreciseRange|1993-07-29|]) Nothing Nothing
_track3 ∷ Track
_track3 = Track Nothing (Just "I Feel You") Nothing LiveVocal Nothing
                (Just $ [dateImpreciseRange|1993-07-29|]) Nothing Nothing

_track4 ∷ Track
_track4 = Track Nothing (Just "Something to Do") Nothing NotLive Nothing Nothing
                Nothing Nothing

_track5 ∷ Track
_track5 = Track Nothing (Just "Two Minute Warning") Nothing
                NotLive Nothing Nothing Nothing Nothing

_track6 ∷ Track
_track6 = Track (Just "DM") (Just "Two Minute Warning") Nothing
                NotLive Nothing Nothing (Just "My Album") (Just "AVersion")

_track9 ∷ Track
_track9 = Track Nothing (Just "Shelter from the Rain") Nothing
                NotLive Nothing Nothing Nothing Nothing

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Track" [ trackPrintableTests, trackFromJSONTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
