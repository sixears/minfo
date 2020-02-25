{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MInfo.Types.TrackInfo
  ( TrackInfo, fromInfo

  , tests
  )
where

-- aeson -------------------------------

import Data.Aeson.TH  ( defaultOptions, fieldLabelModifier, deriveJSON )

-- aeson-pretty ------------------------

import Data.Aeson.Encode.Pretty  ( encodePretty )

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.List      ( drop )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import GHC.Generics   ( Generic )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- date-imprecise ----------------------

import DateImprecise.DateImprecise       ( DateImprecise, dateImprecise )
import DateImprecise.DateImpreciseRange  ( DateImpreciseRange
                                         , dateImpreciseRange )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Functor      ( (⊲) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Semigroup    ( (◇) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  MInfo.Types.ReleaseInfo  as  ReleaseInfo

import MInfo.SongTitle          ( liveName, songTitle )
import MInfo.Types              ( Artist, HasLiveDate( liveDate )
                                , HasLiveLocation( liveLocation ), LiveLocation
                                , LiveType( Demo, Live, LiveVocal, NotLive
                                          , Session )
                                , Source, SourceVersion, TrackTitle
                                , TrackVersion
                                , liveType
                                )
import MInfo.Types.Info         ( track, _info1, _info5, _info7 )
import MInfo.Types.ReleaseInfo  ( HasReleaseInfo( releaseInfo )
                                , original_release, release )
import MInfo.Types.Track        ( artist, title, version )
import MInfo.Types.Tracks       ( TrackIndex )

--------------------------------------------------------------------------------

data TrackInfo = TrackInfo { _album_artist     ∷ Artist
                           , _album            ∷ Maybe Source
                           , _album_version    ∷ Maybe SourceVersion
                           , _release          ∷ Maybe DateImprecise
                           , _original_release ∷ Maybe DateImprecise
                           , _artist           ∷ Maybe Artist
                             --incl. Live, etc.
                           , _song_title       ∷ Maybe TrackTitle
                           , _version          ∷ Maybe TrackVersion
                           , _live_type        ∷ LiveType
                           , _live_location    ∷ Maybe LiveLocation
                           , _live_date        ∷ Maybe DateImpreciseRange
                           , _live_version     ∷ Maybe Text
                           , _title            ∷ Maybe TrackTitle
                           }
  deriving (Eq, Generic, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 1 }  'TrackInfo)

instance Printable TrackInfo where
  print ti = P.lazyUtf8 (encodePretty ti)

--------------------

fromInfo ∷ (HasReleaseInfo τ, TrackIndex τ ι) ⇒ τ → ι → Maybe TrackInfo
fromInfo info n =
  let album_artist  = info ⊣ releaseInfo ∘ ReleaseInfo.artist
   in track info n ⊲ \ tinfo →
        let rinfo = info ⊣ releaseInfo
         in TrackInfo { _album_artist     = album_artist
                      , _album            = rinfo ⊣ ReleaseInfo.source
                      , _album_version    = rinfo ⊣ ReleaseInfo.sourceVersion
                      , _release          = rinfo ⊣ release
                      , _original_release = rinfo ⊣ original_release
                      , _artist           = tinfo ⊣ artist ∤ Just album_artist
                      , _title            = songTitle info tinfo
                      , _version          = tinfo ⊣ version
                      , _live_type        =
                            (tinfo ⊣ liveType) ◇ (rinfo ⊣ liveType)
                      , _live_location    =
                            tinfo ⊣ liveLocation ∤ rinfo ⊣ liveLocation
                      , _live_date        = tinfo ⊣ liveDate ∤ rinfo ⊣ liveDate
                      , _live_version     = liveName rinfo tinfo
                      , _song_title       = tinfo ⊣ title
                      }

newtype P = P (Maybe TrackInfo)
  deriving Eq

instance Printable P where
  print (P (Just ti)) = print ti
  print (P Nothing)   = P.text "-"

fromInfoTests ∷ TestTree
fromInfoTests =
  let twwli   = "World We Live in and Live in Hamburg,The"
      ldate   = [dateImpreciseRange|1984-12-14|]
      std     = "Something to Do  [Live Hamburg 1984-12-14]"
      p       ∷ TrackInfo → P
      p ti    = P (Just ti)
      pInfo   ∷ (HasReleaseInfo τ, TrackIndex τ ι) ⇒ τ → ι → P
      pInfo i = P ∘ fromInfo i
      tinfo1 = TrackInfo { _album_artist     = "Depeche Mode"
                         , _album            = Just twwli
                         , _album_version    = Nothing
                         , _original_release = Nothing
                         , _release          = Nothing
                         , _artist           = Just "Depeche Mode"
                         , _title            = Just std
                         , _version          = Nothing
                         , _live_type        = Live
                         , _live_location    = Just "Hamburg"
                         , _live_date        = Just ldate
                         , _live_version     = Just "Live Hamburg 1984-12-14"
                         , _song_title       = Just "Something to Do"
                         }
      tinfo5_3_2 = TrackInfo { _album_artist     = "Depeche Mode"
                             , _album            = Just "Sounds of the Universe"
                             , _album_version    = Just "Deluxe Box Set"
                             , _release          =
                                   Just [dateImprecise|2009-04-17|]
                             , _original_release =
                                   Just [dateImprecise|2009-01-01|]
                             , _artist           = Just "Depeche Mode"
                             , _title            =
                                   Just "Stories of Old  [Session 2008-12-08]"
                             , _version          = Nothing
                             , _live_type        = Session
                             , _live_location    = Nothing
                             , _live_date        =
                                   Just[dateImpreciseRange|2008-12-08|]
                             , _live_version     = Just "Session 2008-12-08"
                             , _song_title       = Just "Stories of Old"
                             }
      tinfo5_2_21 = TrackInfo { _album_artist     = "Depeche Mode"
                              , _album            =
                                    Just "Sounds of the Universe"
                              , _album_version    = Just "Deluxe Box Set"
                              , _release          =
                                    Just [dateImprecise|2009-04-17|]
                              , _original_release =
                                    Just [dateImprecise|2009-01-01|]
                              , _artist           = Just "Depeche Mode"
                              , _title            =
                                    Just "Jezebel  (SixTøes Remix)"
                              , _version          = Just "SixTøes Remix"
                              , _live_type        = NotLive
                              , _live_location    = Nothing
                              , _live_date        = Nothing
                              , _live_version     = Nothing
                              , _song_title       = Just "Jezebel"
                              }
      tinfo7_0 = TrackInfo { _album_artist     = "Various Artists"
                           , _album            = Just "Compilation"
                           , _album_version    = Nothing
                           , _release          = Nothing
                           , _original_release =
                                 Just [dateImprecise|2009-01-01|]
                           , _artist           = Just "Tricky"
                           , _title            =
                                 Just "Judas  [Demo 1993-07-29]"
                           , _version          = Nothing
                           , _live_type        = Demo
                           , _live_location    = Nothing
                           , _live_date        =
                               Just [dateImpreciseRange|1993-07-29|]
                           , _live_version     = Just "Demo 1993-07-29"
                           , _song_title       = Just "Judas"
                           }
      tinfo7_2 = TrackInfo { _album_artist     = "Various Artists"
                           , _album            = Just "Compilation"
                           , _album_version    = Nothing
                           , _release          = Nothing
                           , _original_release =
                                 Just [dateImprecise|2009-01-01|]
                           , _artist           = Just "Various Artists"
                           , _title            =
                                 Just "I Feel You  [Live Vocal 1993-07-29]"
                           , _version          = Nothing
                           , _live_type        = LiveVocal
                           , _live_location    = Nothing
                           , _live_date        =
                               Just [dateImpreciseRange|1993-07-29|]
                           , _live_version     = Just "Live Vocal 1993-07-29"
                           , _song_title       = Just "I Feel You"
                           }
   in testGroup "MInfo.Types.TrackInfo"
                [ testCase "tinfo1"      $ p tinfo1     ≟ pInfo _info1 (0∷ℕ)
                , testCase "tinfo10"     $ P Nothing    ≟ pInfo _info1 (10∷ℕ)
                , testCase "tinfo5_3_2"  $ p tinfo5_3_2 ≟ pInfo _info5 (3∷ℕ,2∷ℕ)
                , testCase "tinfo_5_2_21" $
                    p tinfo5_2_21 ≟ pInfo _info5 (2∷ℕ,21∷ℕ)
                , testCase "tinfo5_4_21" $ P Nothing    ≟ pInfo _info5(4∷ℕ,21∷ℕ)
                , testCase "tinfo7_0"    $ p tinfo7_0   ≟ pInfo _info7 (0∷ℕ)
                , testCase "tinfo7_2"    $ p tinfo7_2   ≟ pInfo _info7 (2∷ℕ)
                ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- TEST VARIOUS ARTISTS
-- RELEASE DATE (TDRL http://id3.org/id3v2.4.0-frames)
-- TSOA, TSOT, TSOP

tests ∷ TestTree
tests = testGroup "MInfo.Types.TrackInfo" [ fromInfoTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
