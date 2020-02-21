{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

module MInfo.Types.Tracks
  ( FlatTracks( flatTracks ), Tracks( Tracks, unTracks ), TrackIndex( track )

  , tests
  , _ts1, _ts2, _ts3, _ts4, _ts5
  )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Array ), typeMismatch )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( return, sequence )
import Data.Either          ( Either( Right ) )
import Data.Eq              ( Eq )
import Data.Function        ( ($) )
import Data.Functor         ( fmap )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.String          ( String )
import GHC.Exts             ( fromList, toList )
import Numeric.Natural      ( Natural )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import qualified  Data.ByteString  as  BS
import Data.ByteString  ( ByteString )

-- date-imprecise ----------------------

import DateImprecise.DateImpreciseRange  ( dateImpreciseRange )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- index -------------------------------

import Index ( HasIndex( Elem, Indexer, index ), (!!) )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Monoid   ( ю )
import Data.MoreUnicode.Natural  ( ℕ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( unlines )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- vector ------------------------------

import Data.Vector  ( (!?) )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON ) )

-- yaml-plus ---------------------------

import YamlPlus        ( unYaml )
import YamlPlus.Error  ( YamlParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.Types        ( LiveType( Demo, Live, NotLive, Session ) )
import MInfo.Types.Track  ( Track( Track ), _track1, _track2, _track3 )

--------------------------------------------------------------------------------

newtype Tracks = Tracks { unTracks ∷ [[Track]] }
  deriving (Eq,Show)

----------------------------------------

instance Printable Tracks where
  print tss = P.text ∘ unlines $ toText ⊳ flatTracks tss

----------------------------------------

instance FromJSON Tracks where
  parseJSON (Array ts) =
    case ts !? 0 of
      Nothing        → return $ Tracks [[]]
      Just (Array _) → Tracks ⊳ (sequence $ parseJSON ⊳ toList ts)
      Just _         → (Tracks ∘ pure) ⊳ (sequence $ parseJSON ⊳ toList ts)
  parseJSON invalid = typeMismatch "Array" invalid

--------------------

tracksFromJSONTests ∷ TestTree
tracksFromJSONTests =
  testGroup "tracksFromJSON"
            [ testCase "t1"  $
                Right [_track1,_track2] @=? unYaml @YamlParseError t1
            , testCase "t1'" $
                Right (Tracks [[_track1,_track2]]) @=? unYaml @YamlParseError t1
            , testCase "t3" $
                Right _ts1 @=? unYaml @YamlParseError t3
            ]

----------------------------------------

instance ToJSON Tracks where
  toJSON = Array ∘ fromList ∘ fmap toJSON ∘ flatTracks

----------------------------------------

class FlatTracks α where
  flatTracks ∷ α → [Track]

instance FlatTracks Tracks where
  flatTracks (Tracks tss) = ю tss

------------------------------------------------------------

instance HasIndex Tracks where
  type Elem Tracks = Track
  type Indexer Tracks = ℕ
  index i ts = flatTracks ts !! i

{- | `Tracks` may be indexed either by a single ℕ (i.e., index into
     `flatTracks`), or by a pair of ℕ (disc, then trackid). -}
class TrackIndex τ ι where
  track ∷ τ → ι → Maybe Track

instance TrackIndex Tracks Natural where
  track ts n = ts !! n

instance TrackIndex Tracks (Natural,Natural) where
  track (Tracks tss) (d,i) = tss !! d ≫ index i
  
hasIndexTests ∷ TestTree
hasIndexTests =
  testGroup "hasIndex" [ testCase "_track1" $ Just _track1 @=? _ts1 !! 0
                       , testCase "_track2" $ Just _track2 @=? _ts1 !! 1
                       , testCase "_track3" $ Just _track3 @=? index 2 _ts1
                       , testCase "-" $ Nothing @=? index 3 _ts1
                       , testCase "_track1(2)" $
                           Just _track1 @=? track _ts1 (0∷ℕ,0∷ℕ)
                       , testCase "_track2(2)" $
                           Just _track2 @=? track _ts1 (0∷ℕ,1∷ℕ)
                       , testCase "-(2)"  $
                           Nothing @=? track _ts1 (0∷ℕ,2∷ℕ)
                       , testCase "_track3(2)" $
                           Just _track3 @=? track _ts1 (1∷ℕ,0∷ℕ)
                       , testCase "-(3)"  $ Nothing @=? track _ts1 (1∷ℕ,1∷ℕ)
                       ]

--------------------------------------------------------------------------------
--                                 test data                                  --
--------------------------------------------------------------------------------

t1 ∷ ByteString
t1 = BS.intercalate "\n" [ "- title: Judas"
                         , "  artist: Tricky"
                         , "  live_type: Demo"
                         , "  live_date: 1993-07-29"
                         , "- title: Mercy in You"
                         , "  live_type: Session"
                         , "  live_date: 1993-07-29"
                         ]
t3 ∷ ByteString
t3 = BS.intercalate "\n" [ "-"
                         , "  - title: Judas"
                         , "    artist: Tricky"
                         , "    live_type: Demo"
                         , "    live_date: 1993-07-29"
                         , "  - title: Mercy in You"
                         , "    live_type: Session"
                         , "    live_date: 1993-07-29"
                         , "-"
                         , "  - title: I Feel You"
                         , "    live_type: Live"
                         , "    live_date: 1993-07-29"
                         ]

_ts1 ∷ Tracks
_ts1 = Tracks [[_track1,_track2],[_track3]]

----------

_ts2 ∷ Tracks
_ts2 = let mkTrack t = Track Nothing (Just t) Nothing Live
                       (Just "Stade Couvert Régional, Liévin, France")
                       (Just [dateImpreciseRange|1993-07-29|])
        in Tracks [ mkTrack ⊳ [ "Higher Love"
                              , "World in my Eyes"
                              , "Walking in my Shoes"
                              , "Behind the Wheel"
                              , "Stripped"
                              , "Condemnation"
                              , "Judas"
                              , "Mercy in You"
                              , "I Feel You"
                              , "Never Let Me Down Again"
                              , "Rush"
                              , "In your Room"
                              , "Personal Jesus"
                              , "Enjoy the Silence"
                              , "Fly on the Windscreen"
                              , "Everything Counts"
                              , "Credits - Death's Door"
                              , "Halo"
                              , "Policy of Truth"
                              ]
                  ]

----------

_ts3 ∷ Tracks
_ts3 = let mkTrack t = Track Nothing (Just t) Nothing NotLive Nothing Nothing
        in Tracks [ mkTrack ⊳ [ "Walking in my Shoes"
                              , "Halo"
                              , "Stripped"
                              , "Condemnation"
                              , "Judas"
                              , "I Feel You"
                              , "Never Let Me Down Again"
                              , "Rush"
                              , "In your Room"
                              , "Personal Jesus"
                              , "Enjoy the Silence"
                              , "Everything Counts"
                              ]
                  ]

----------

_ts4 ∷ Tracks
_ts4 = let mkTrack t = Track Nothing (Just t) Nothing NotLive Nothing Nothing
           mkTrack' (t,v) = Track Nothing (Just t) (Just v)
                                  NotLive Nothing Nothing
           mkTrackD t = Track Nothing (Just t) Nothing
                              Demo Nothing Nothing
        in Tracks [ mkTrack ⊳ [ "In Chains"
                              , "Hole to Feed"
                              , "Wrong"
                              , "Fragile Tension"
                              , "Little Soul"
                              , "In Sympathy"
                              , "Peace"
                              , "Come Back"
                              , "Spacewalker"
                              , "Perfect"
                              , "Miles Away - the Truth is"
                              , "Jezebel"
                              , "Corrupt"
                              , "Interlude #5"
                              ]
                  ,   (mkTrack ⊳ [ "Light"
                                 , "Sun and the Moon and the Stars,The"
                                 , "Ghost"
                                 , "Esque"
                                 , "Oh Well"
                                 ]
                      )
                    ⊕ (mkTrack' ⊳ [ ("Corrupt","Efdemin Remix")
                                  , ("In Chains","Minilogue's Earth Remix")
                                  , ("Little Soul",
                                     "Thomas Fehlmann Flowing Ambient Mix")
                                  , ("Jezebel","SixToes Remix")
                                  , ("Perfect",
                                     "Drone Mix")
                                  , ("Wrong","Caspa Remix")
                                  ]
                      )
                  ,   (mkTrackD ⊳ [ "Little 15"
                                  , "Clean"
                                  , "Sweetest Perfection"
                                  , "Walking in my Shoes"
                                  , "I Feel You"
                                  , "Judas"
                                  , "Surrender"
                                  , "Only When I Lose Myself"
                                  , "Nothing's Impossible"
                                  , "Corrupt"
                                  , "Peace"
                                  , "Jezebel"
                                  , "Come Back"
                                  , "In Chains"
                                  ]
                      )
                  ]

----------

_ts5 ∷ Tracks
_ts5 = let mkTrack t = Track Nothing (Just t) Nothing NotLive Nothing Nothing
           mkTrack' (t,v) = Track Nothing (Just t) (Just v)
                                  NotLive Nothing Nothing
           mkTrackD t = Track Nothing (Just t) (Just "Demo")
                              NotLive Nothing Nothing
           mkTrackS t = Track Nothing (Just t) Nothing
                              Session Nothing
                              (Just [dateImpreciseRange|2008-12-08|])
        in Tracks [ mkTrack ⊳ [ "In Chains"
                              , "Hole to Feed"
                              , "Wrong"
                              , "Fragile Tension"
                              , "Little Soul"
                              , "In Sympathy"
                              , "Peace"
                              , "Come Back"
                              , "Spacewalker"
                              , "Perfect"
                              , "Miles Away / The Truth Is"
                              , "Jezebel"
                              , "Corrupt"
                              , "Interlude #5"
                              , "Light"
                              , "Sun and the Moon and the Stars,The"
                              , "Ghost"
                              , "Esque"
                              , "Oh Well"
                              ]
                  ,   (mkTrackD ⊳ [ "Little 15"
                                  , "Clean"
                                  , "Sweetest Perfection"
                                  , "Walking in my Shoes"
                                  , "I Feel You"
                                  , "Judas"
                                  , "Surrender"
                                  , "Only When I Lose Myself"
                                  , "Nothing's Impossible"
                                  , "Corrupt"
                                  , "Peace"
                                  , "Jezebel"
                                  , "Come Back"
                                  , "In Chains"
                                  ]
                      )
                  ,   (mkTrack' ⊳ [ ("Oh Well","Single Edit")
                                  , ("Wrong","Studio Session Mix")
                                  , ("Come Back","Studio Session Mix")
                                  , ("Corrupt","Studio Session Mix")
                                  , ("Miles Away / The Truth is",
                                     "Lagos Boys Choir Remix")
                                  , ("Sun and the Moon and the Stars,The",
                                     "Electronic Periodic's Microdrum Mix")
                                  , ("Ghost","Le Weekend Remix")
                                  , ("In Chains",
                                     "Minilogue's Air Extend Remix")
                                  , ("Martyr","Sound for the Universe Mix")
                                  , ("Hole to Feed","Demo")
                                  , ("Wrong","Extended Remix Edit")
                                  , ("Wrong","Frankie's Bromantic Club Mix")
                                  , ("Come Back","Studio Session 2 Mix")
                                  , ("Wrong","Thin White Duke Remix")
                                  , ("Come Back","Vinyl Mix")
                                  , ("Wrong","D.I.M. vs. Boys Noize Remix")
                                  , ("Corrupt","Efdemin Remix")
                                  , ("Wrong","Peter Rauhofer Vocal Mix")
                                  , ("In Chains","Minilogue's Earth Remix")
                                  , ("Little Soul",
                                     "Thomas Fehlman Ambient Mix")
                                  , ("Wrong","Magda's Scallop Funk Remix")
                                  , ("Jezebel","SixTøes Remix")
                                  , ("Wrong","Trentemøller Remix")
                                  , ("Perfect",
                                     "Drone Mix")
                                  , ("Wrong","Caspa Remix")
                                  , ("Oh Well","Black Light Odyssey Dub")
                                  , ("Sun and the Moon and the Stars,The",
                                     "Electronic Periodic's Microdrum Mix")
                                  , ("Oh Well","Black Light Odyssey Remix")
                                  ]
                      )
                  ,   (mkTrackS ⊳ [ "Corrupt"
                                  , "Little Soul"
                                  , "Stories of Old"
                                  , "Come Back"
                                  ]
                      )
                  ]

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Tracks" [ tracksFromJSONTests, hasIndexTests ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
