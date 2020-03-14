{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
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

import Prelude  ( (-) )

-- aeson -------------------------------

import Data.Aeson.TH  ( defaultOptions, fieldLabelModifier, deriveJSON )

-- aeson-pretty ------------------------

import Data.Aeson.Encode.Pretty  ( confCompare, defConfig, encodePretty' )

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.List      ( drop, elemIndex )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.Ord       ( Ordering( EQ, GT, LT ), compare )
import Data.String    ( String )
import Data.Tuple     ( fst )
import GHC.Generics   ( Generic )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- date-imprecise ----------------------

import DateImprecise.DateImprecise       ( DateImprecise, dateImprecise )
import DateImprecise.DateImpreciseRange  ( DateImpreciseRange
                                         , dateImpreciseRange )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Functor      ( (⊲), (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monad        ( (≫) )
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

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  MInfo.Types.Track        as  Track
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
import MInfo.Types.Info         ( Info, track, _info1, _info5, _info7, _info8 )
import MInfo.Types.ReleaseInfo  ( HasReleaseInfo( releaseInfo )
                                , discnames, original_release, release )
import MInfo.Types.Track        ( artist, title, version )
import MInfo.Types.Tracks       ( TrackIndex
                                , discCount, discTrackCount, trackCount )

--------------------------------------------------------------------------------

data TrackInfo = TrackInfo { _album_artist     ∷ Artist
                           , _album            ∷ Maybe Source
                           , _album_version    ∷ Maybe SourceVersion
                             -- incl. version, if any
                           , _album_title      ∷ Maybe Text
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
                           , _discid           ∷ ℕ
                           , _track_total_id   ∷ ℕ
                           , _track_count      ∷ ℕ
                           , _track_in_disc    ∷ ℕ
                           , _disc_count       ∷ ℕ
                           , _tracks_on_disc   ∷ ℕ
                           , _discname         ∷ Maybe Source
                           , _discversion      ∷ Maybe SourceVersion
                           , _disctitle        ∷ Maybe Text
                           , _work             ∷ Maybe Text
                           }
  deriving (Eq, Generic, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 1 }  'TrackInfo)

instance Printable TrackInfo where
  print ti =
    let key_order = [ "artist", "song_title", "title", "version"
                    , "live_type", "live_date", "live_location"
                    , "album_artist"
                    , "album", "album_version"
                    , "discname", "discversion"
                    , "original_release", "release", "track_count"
                    , "discid", "track_in_disc", "track_total_id"

                    , "disc_count", "tracks_on_disc"

                    , "album_title", "live_version", "disctitle"
                    ]
        cmp x y = case (elemIndex x key_order, elemIndex y key_order) of
                    (Just a , Just b ) → a `compare` b
                    (Just _ , Nothing) → LT
                    (Nothing, Just _ ) → GT
                    (Nothing, Nothing) → EQ
     in P.lazyUtf8 (encodePretty' defConfig { confCompare = cmp } ti)

--------------------

fromInfo ∷ TrackIndex Info τ ⇒ Info → τ → Maybe TrackInfo
fromInfo info n =
  let rinfo         = info ⊣ releaseInfo
      album_artist  = info ⊣ releaseInfo ∘ ReleaseInfo.artist
      album         ∷ Maybe Source
      album         = rinfo ⊣ ReleaseInfo.source
      album_version ∷ Maybe SourceVersion
      album_version = rinfo ⊣ ReleaseInfo.sourceVersion
   in track info n ⊲ \ (tracknum, discid, trackid, tinfo) →
        let discname      =   tinfo ⊣ Track.album
                            ∤ ReleaseInfo.discname rinfo (discid-1)
            discversion   =   tinfo ⊣ Track.album_version
                            ∤ ReleaseInfo.discversion rinfo (discid-1)
            (album_title,work,disctitle) =
              case (discname, album, discversion, album_version) of
                (Nothing,Nothing,_,_)           → (Nothing,Nothing,Nothing)
                (Nothing,Just a,_,Nothing)      →
                    (Just $ toText a,Just $ toText a,Just $ toText a)
                (Nothing,Just a,_,Just v)       →
                    let t = Just ([fmt|%T  (%T)|] a v) in (t,t,t)
                (Just d,Just a,Nothing,Nothing) →
                    (Just (toText d),Just (toText a),Just ([fmt|%T  <%T>|] d a))
                (Just d,Nothing,Nothing,_)      →
                    let t = Just (toText d) in (t,t,t)
                (Just d,Nothing,Just c,_)       →
                    let t = Just ([fmt|%T  (%T)|] d c) in (t,t,t)
                (Just d,Just a,Just c,Nothing)  →
                    (Just ([fmt|%T  (%T)|] d c),Just (toText a),
                     Just ([fmt|%T  (%T)  <%T>|] d c a))
                (Just d,Just a,Nothing,Just v)   →
                    (Just (toText d),Just ([fmt|%T  (%T)|] a v),
                     Just ([fmt|%T  <%T  (%T)>|] d a v))
                (Just d,Just a,Just c,Just v)   →
                    (Just ([fmt|%T  (%T)|] d c),Just ([fmt|%T  (%T)|] a v),
                     Just ([fmt|%T  (%T)  <%T  (%T)>|] d c a v))

         in TrackInfo { _album_artist     = album_artist
                      , _album            = album
                      , _album_version    = album_version
                      , _album_title      = album_title
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
                      , _discid           = discid
                      , _track_total_id   = tracknum
                      , _track_count      = trackCount info
                      , _track_in_disc    = trackid
                      , _disc_count       = discCount info
                      , _tracks_on_disc   =
                          case discTrackCount info (discid-1) of
                            Nothing → 0
                            Just tc → tc
                      , _discname         = discname
                      , _discversion      = discversion
                      , _disctitle        = disctitle
                      , _work             = work
                      }

newtype P = P (Maybe TrackInfo)
  deriving Eq

instance Printable P where
  print (P (Just ti)) = print ti
  print (P Nothing)   = P.text "-"

fromInfoTests ∷ TestTree
fromInfoTests =
  let twwli   = "World We Live in and Live in Hamburg,The"
      twwlit  = "World We Live in and Live in Hamburg,The"
      ldate   = [dateImpreciseRange|1984-12-14|]
      std     = "Something to Do  [Live Hamburg 1984-12-14]"
      p       ∷ TrackInfo → P
      p ti    = P (Just ti)
      pInfo   ∷ TrackIndex Info τ ⇒ Info → τ → P
      pInfo i = P ∘ fromInfo i
      tinfo1 = TrackInfo { _album_artist     = "Depeche Mode"
                         , _album            = Just twwli
                         , _album_version    = Nothing
                         , _album_title      = Just twwlit
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
                         , _discid           = 1
                         , _track_total_id   = 1
                         , _track_in_disc    = 1
                         , _track_count      = 2
                         , _disc_count       = 1
                         , _tracks_on_disc   = 2

                         , _discname         = Nothing
                         , _discversion      = Nothing
                         , _disctitle        = Just twwlit
                         , _work             = Just twwlit
                         }
      tinfo5_3_2 = TrackInfo { _album_artist     = "Depeche Mode"
                             , _album            = Just "Sounds of the Universe"
                             , _album_version    = Just "Deluxe Box Set"
                             , _album_title      =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
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
                             , _discid           = 4
                             , _track_total_id   = 64
                             , _track_in_disc    = 3
                             , _track_count      = 65
                             , _disc_count       = 4
                             , _tracks_on_disc   = 4

                             , _discname         = Nothing
                             , _discversion      = Nothing
                             , _disctitle        =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
                             , _work             =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
                             }
      tinfo5_2_21 = TrackInfo { _album_artist     = "Depeche Mode"
                              , _album            =
                                    Just "Sounds of the Universe"
                              , _album_version    = Just "Deluxe Box Set"
                              , _album_title      =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
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
                              , _discid           = 3
                              , _track_total_id   = 55
                              , _track_in_disc    = 22
                              , _track_count      = 65
                              , _disc_count       = 4
                              , _tracks_on_disc   = 28

                              , _discname         = Nothing
                              , _discversion      = Nothing
                              , _disctitle        =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
                              , _work             =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
                              }
      tinfo7_0 = TrackInfo { _album_artist     = "Various Artists"
                           , _album            = Just "Compilation"
                           , _album_version    = Nothing
                           , _album_title      = Just "Compilation"
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
                           , _discid           = 1
                           , _track_in_disc    = 1
                           , _track_total_id   = 1
                           , _track_count      = 3
                           , _disc_count       = 2
                           , _tracks_on_disc   = 2

                           , _discname         = Nothing
                           , _discversion      = Nothing
                           , _disctitle        = Just "Compilation"
                           , _work             = Just "Compilation"
                           }
      tinfo7_2 = TrackInfo { _album_artist     = "Various Artists"
                           , _album            = Just "Compilation"
                           , _album_version    = Nothing
                           , _album_title      = Just "Compilation"
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
                           , _discid           = 2
                           , _track_in_disc    = 1
                           , _track_total_id   = 3
                           , _track_count      = 3
                           , _disc_count       = 2
                           , _tracks_on_disc   = 1

                           , _discname         = Nothing
                           , _discversion      = Nothing
                           , _disctitle        = Just "Compilation"
                           , _work             = Just "Compilation"
                           }
      tinfo8_0 = TrackInfo  { _album_artist     = "Depeche Mode"
                            , _album            =
                                  Just "Sounds of the Universe"
                            , _album_version    = Just "Deluxe Box Set"
                            , _album_title      =
                               Just "Sounds of the Universe  (Deluxe Box Set)"
                            , _release          =
                                  Just [dateImprecise|2009-04-17|]
                            , _original_release = Nothing

                            , _artist           = Just "Depeche Mode"
                            , _title            = Just "In Chains"
                            , _version          = Nothing
                            , _live_type        = NotLive
                            , _live_location    = Nothing
                            , _live_date        = Nothing
                            , _live_version     = Nothing
                            , _song_title       = Just "In Chains"
                            , _discid           = 1
                            , _track_in_disc    = 1
                            , _track_total_id   = 1
                            , _track_count      = 5
                            , _disc_count       = 3
                            , _tracks_on_disc   = 2

                            , _discname         = Nothing
                            , _discversion      = Nothing
                            , _disctitle        =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
                            , _work             =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
                            }
      tinfo8_1 = TrackInfo  { _album_artist     = "Depeche Mode"
                            , _album            =
                                  Just "Sounds of the Universe"
                            , _album_version    = Just "Deluxe Box Set"
                            , _album_title      = Just "Bonus  (BB)"
                            , _release          =
                                  Just [dateImprecise|2009-04-17|]
                            , _original_release = Nothing

                            , _artist           = Just "Depeche Mode"
                            , _title            = Just "Hole to Feed"
                            , _version          = Nothing
                            , _live_type        = NotLive
                            , _live_location    = Nothing
                            , _live_date        = Nothing
                            , _live_version     = Nothing
                            , _song_title       = Just "Hole to Feed"
                            , _discid           = 1
                            , _track_in_disc    = 2
                            , _track_total_id   = 2
                            , _track_count      = 5
                            , _disc_count       = 3
                            , _tracks_on_disc   = 2

                            , _discname         = Just "Bonus"
                            , _discversion      = Just "BB"
                            , _disctitle        =
                                  Just $   "Bonus  (BB)  "
                                         ⊕ "<Sounds of the Universe  "
                                         ⊕ "(Deluxe Box Set)>"
                            , _work             =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
                            }
      tinfo8_2 = TrackInfo  { _album_artist     = "Depeche Mode"
                            , _album            =
                                  Just "Sounds of the Universe"
                            , _album_version    = Just "Deluxe Box Set"
                            , _album_title      = Just "Remixen  (R)"
                            , _release          =
                                  Just [dateImprecise|2009-04-17|]
                            , _original_release = Nothing

                            , _artist           = Just "Depeche Mode"
                            , _song_title       = Just "Wrong"
                            , _title            =
                                  Just "Wrong  (Trentemøller Remix)"
                            , _version          = Just "Trentemøller Remix"
                            , _live_type        = NotLive
                            , _live_location    = Nothing
                            , _live_date        = Nothing
                            , _live_version     = Nothing
                            , _discid           = 2
                            , _track_in_disc    = 1
                            , _track_total_id   = 3
                            , _track_count      = 5
                            , _disc_count       = 3
                            , _tracks_on_disc   = 2

                            , _discname         = Just "Remixen"
                            , _discversion      = Just "R"
                            , _disctitle        =
                                  Just $ "Remixen  (R)  " ⊕
                                         "<Sounds of the Universe  " ⊕
                                         "(Deluxe Box Set)>"
                            , _work             =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
                            }
      tinfo8_3 = TrackInfo  { _album_artist     = "Depeche Mode"
                            , _album            =
                                  Just "Sounds of the Universe"
                            , _album_version    = Just "Deluxe Box Set"
                            , _album_title      =
                               Just "Bonus  (BB)"
                            , _release          =
                                  Just [dateImprecise|2009-04-17|]
                            , _original_release = Nothing

                            , _artist           = Just "Depeche Mode"
                            , _title            = Just "Perfect  (Drone Mix)"
                            , _song_title       = Just "Perfect"
                            , _version          = Just "Drone Mix"
                            , _live_type        = NotLive
                            , _live_location    = Nothing
                            , _live_date        = Nothing
                            , _live_version     = Nothing
                            , _discid           = 2
                            , _track_in_disc    = 2
                            , _track_total_id   = 4
                            , _track_count      = 5
                            , _disc_count       = 3
                            , _tracks_on_disc   = 2

                            , _discname         = Just "Bonus"
                            , _discversion      = Just "BB"
                            , _disctitle        =
                                  Just $ "Bonus  (BB)  " ⊕
                                         "<Sounds of the Universe  " ⊕
                                         "(Deluxe Box Set)>"
                            , _work             =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
                            }
      tinfo8_4 = TrackInfo  { _album_artist     = "Depeche Mode"
                            , _album            =
                                  Just "Sounds of the Universe"
                            , _album_version    = Just "Deluxe Box Set"
                            , _album_title      = Just "Third"
                            , _release          =
                                  Just [dateImprecise|2009-04-17|]
                            , _original_release = Nothing

                            , _artist           = Just "Depeche Mode"
                            , _title            = Just "Jezebel"
                            , _song_title       = Just "Jezebel"
                            , _version          = Nothing
                            , _live_type        = NotLive
                            , _live_location    = Nothing
                            , _live_date        = Nothing
                            , _live_version     = Nothing
                            , _discid           = 3
                            , _track_in_disc    = 1
                            , _track_total_id   = 5
                            , _track_count      = 5
                            , _disc_count       = 3
                            , _tracks_on_disc   = 1

                            , _discname         = Just "Third"
                            , _discversion      = Nothing
                            , _disctitle        =
                                  Just $ "Third  " ⊕
                                         "<Sounds of the Universe  " ⊕
                                         "(Deluxe Box Set)>"
                            , _work             =
                                 Just "Sounds of the Universe  (Deluxe Box Set)"
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
                , testCase "tinfo8_0"    $ p tinfo8_0   ≟ pInfo _info8 (0∷ℕ)
                , testCase "tinfo8_1"    $ p tinfo8_1   ≟ pInfo _info8 (1∷ℕ)
                , testCase "tinfo8_2"    $ p tinfo8_2   ≟ pInfo _info8 (2∷ℕ)
                , testCase "tinfo8_3"    $ p tinfo8_3   ≟ pInfo _info8 (3∷ℕ)
                , testCase "tinfo8_4"    $ p tinfo8_4   ≟ pInfo _info8 (4∷ℕ)
                ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- TEST VARIOUS ARTISTS
-- RELEASE DATE (TDRL http://id3.org/id3v2.4.0-frames)
-- TSOA, TSOT, TSOP

tests ∷ TestTree
tests = testGroup "TrackInfo" [ fromInfoTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
