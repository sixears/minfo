{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module MInfo.Types.Info
  ( Info( Info )
  , blankInfo, flatTracks, _info6, releaseInfo, trackCount, track, tracks

  , tests
  -- test data
  , _info1, _info5, _info7
  )
where

import Prelude  ( fromIntegral )

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Object ), (.:), withObject )

-- base --------------------------------

import Data.Either    ( Either( Left, Right ) )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.List      ( replicate )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
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

import DateImprecise.DateImprecise       ( dateImprecise )
import DateImprecise.DateImpreciseRange  ( DateImpreciseRange )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Natural      ( ℕ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertListEqR, runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( intercalate )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON ), object )

-- yaml-plus ---------------------------

import PYaml           ( pyaml )
import YamlPlus        ( unYaml )
import YamlPlus.Error  ( YamlParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  MInfo.T.TestData         as  TestData
import qualified  MInfo.Types.Tracks       as  Tracks

import MInfo.Types              ( HasLiveDate( liveDate )
                                , HasLiveLocation( liveLocation )
                                , HasLiveType( liveType ), LiveLocation
                                , LiveType( NotLive )
                                )
import MInfo.Types.ReleaseInfo  ( HasReleaseInfo( releaseInfo )
                                , ReleaseInfo( ReleaseInfo )
                                , blankReleaseInfo, releaseInfoFields
                                , _rinfo1, _rinfo2, _rinfo3, _rinfo4, _rinfo5
                                , _rinfo7
                                )
import MInfo.Types.Track        ( Track( Track )
                                , blankTrack, _track4, _track5 )
import MInfo.Types.Tracks       ( FlatTracks( flatTracks )
                                , Tracks( Tracks, unTracks )
                                , TrackIndex( track )
                                , _ts1, _ts2, _ts3, _ts4, _ts5
                                )

--------------------------------------------------------------------------------

data Info = Info { _releaseInfo ∷ ReleaseInfo
                 , _tracks      ∷ Tracks
                 }
  deriving (Generic, Eq, Show)

instance HasReleaseInfo Info where
  releaseInfo ∷ Lens' Info ReleaseInfo
  releaseInfo = lens _releaseInfo (\ i r → i { _releaseInfo = r })

tracks ∷ Lens' Info Tracks
tracks = lens _tracks (\ i ts → i { _tracks = ts })

instance FlatTracks Info where
  flatTracks ∷ Info → [Track]
  flatTracks = Tracks.flatTracks ∘ _tracks

instance TrackIndex Info ℕ where
  track = track ∘ _tracks

instance TrackIndex Info (ℕ,ℕ) where
  track = track ∘ _tracks

instance FromJSON Info where
  parseJSON = withObject "Info" $
    \ v → Info ⊳ parseJSON (Object v) ⊵ v .: "tracks"

instance HasLiveDate Info where
  liveDate ∷ Lens' Info (Maybe DateImpreciseRange)
  liveDate = releaseInfo ∘ liveDate

instance HasLiveLocation Info where
  liveLocation ∷ Lens' Info (Maybe LiveLocation)
  liveLocation = releaseInfo ∘ liveLocation

instance HasLiveType Info where
  liveType ∷ Lens' Info LiveType
  liveType = releaseInfo ∘ liveType

--------------------

infoFromJSONTests ∷ TestTree
infoFromJSONTests =
  let splitInfo ∷ Info → (ReleaseInfo,Tracks)
      splitInfo (Info ri tr) = (ri,tr)
      splitEPair ∷ Either ε (α,β) → (Either ε α, Either ε β)
      splitEPair (Left l) = (Left l,Left l)
      splitEPair (Right (a,b)) = (Right a, Right b)
      checkInfo name inf expected =
        let (rinfo,trcks) = splitEPair (splitInfo ⊳ unYaml @YamlParseError inf)
            Info erinfo etrcks = expected
            nme t = name ⊕ ": " ⊕ t
         in ю [ [ testCase      (nme "release info") $ Right erinfo @=? rinfo ]
                , assertListEqR (nme "tracks")
                                (Tracks.flatTracks ⊳ trcks)
                                (Tracks.flatTracks etrcks)
                , assertListEqR (nme "flat tracks")
                                (unTracks ⊳trcks) (unTracks etrcks)
                , [ testCase (nme "info") $
                      Right _info2 @=? unYaml @YamlParseError TestData.info2T
                  ]
                ]

   in testGroup "infoFromJSON"
                (ю [ [ testCase "info1'" $
                         Right _info1 @=? unYaml @YamlParseError TestData.info1T
                     ]
                   , checkInfo "_info2" TestData.info2T _info2
                   , checkInfo "_info3" TestData.info3T _info3
                   , checkInfo "_info4" TestData.info4T _info4
                   , checkInfo "_info5" TestData.info5T _info5
                   , checkInfo "_info6" TestData.info6T _info6
                   ]
                )

instance ToJSON Info where
  toJSON (Info r ts) = object (("tracks",toJSON ts) : releaseInfoFields r)

instance Printable Info where
  print i = P.text $ pyaml i

blankInfo ∷ ℕ → Info
blankInfo n =
  Info blankReleaseInfo $ Tracks [replicate (fromIntegral n) (blankTrack)]

infoPrintableTests ∷ TestTree
infoPrintableTests =
  let exp = intercalate "\n" [ "artist : ''"
                             , "catno  : ~"
                             , "source : ~"
                             , "tracks :"
                             , "  - title : ~"
                             , "  - title : ~"
                             ]
   in testGroup "Printable" [ testCase "blank 2" $ exp ≟ (toText $ blankInfo 2)
                            ]

trackCount ∷ Info → ℕ
trackCount = length ∘ flatTracks

trackCountTests ∷ TestTree
trackCountTests =
  testGroup "trackCount"
            [ testCase "info1" $
                    Right  2
                @=? trackCount ⊳ (unYaml @YamlParseError TestData.info1T)
            , testCase "_info2" $
                    Right 19
                @=? trackCount ⊳ (unYaml @YamlParseError TestData.info2T)
            , testCase "_info3" $
                    Right 12
                @=? trackCount ⊳ (unYaml @YamlParseError TestData.info3T)
            , testCase "_info4" $
                    Right 39
                @=? trackCount ⊳ (unYaml @YamlParseError TestData.info4T)
            , testCase "info5" $
                    Right 65
                @=? trackCount ⊳ (unYaml @YamlParseError TestData.info5T)

            , testCase "info6" $
                    Right  4
                @=? trackCount ⊳ (unYaml @YamlParseError TestData.info6T)
            ]

--------------------------------------------------------------------------------
--                                 test data                                  --
--------------------------------------------------------------------------------

_info1 ∷ Info
_info1 = Info _rinfo1 (Tracks [ [ _track4 , _track5 ] ])

--------------------


_info2 ∷ Info
_info2 = Info _rinfo2 _ts2

--------------------

_info3 ∷ Info
_info3 = Info _rinfo3 _ts3

--------------------


_info4 ∷ Info
_info4 = Info _rinfo4 _ts4

--------------------

_info5 ∷ Info
_info5 = Info _rinfo5 _ts5

--------------------

_info6 ∷ Info
_info6 = Info (ReleaseInfo ("Depeche Mode") Nothing
                           (Just ([dateImprecise|2009-04-17|]))
                           Nothing (Just "Sounds of the Universe")
                           (Just "Deluxe Box Set") NotLive Nothing Nothing)
              (Tracks [ [ Track Nothing (Just "In Chains") Nothing
                                NotLive Nothing Nothing
                        , Track Nothing (Just "Hole to Feed") Nothing
                                NotLive Nothing Nothing
                        ]
                      , [ Track Nothing
                                (Just "Wrong") (Just "Trentemøller Remix")
                                NotLive Nothing Nothing
                        , Track Nothing
                                (Just "Perfect")
                                (Just "Drone Mix")
                                NotLive Nothing Nothing
                        ]
                      ])


_info7 ∷ Info
_info7 = Info _rinfo7 _ts1

------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "Info" [ infoPrintableTests, infoFromJSONTests, trackCountTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
