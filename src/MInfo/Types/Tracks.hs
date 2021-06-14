{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

module MInfo.Types.Tracks
  ( HasDiscCount( discCount )
  , HasTracks( flatTracks, discTrackCount, trackCount, tracks )
  , Tracks( Tracks, unTracks ), TrackIndex( track )

  , tests
  , _ts1, _ts2, _ts3, _ts4, _ts5, _ts6, _ts8, _ts9
  )
where

import Prelude  ( (-) )

-- aeson -------------------------------

import Data.Aeson.Types  ( Parser, Value( Array, Object )
                         , (.:), (.:?), typeMismatch )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( return, sequence )
import Data.Either          ( Either( Right ) )
import Data.Eq              ( Eq )
import Data.Function        ( ($), id )
import Data.Functor         ( Functor, fmap )
import Data.List            ( filter, zip )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.String          ( String )
import GHC.Exts             ( fromList, toList )
import Numeric.Natural      ( Natural )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡) )
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

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Lens    ( Lens' )
import Control.Lens.Tuple   ( _4 )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣), (⊮) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Natural      ( ℕ )

-- natural -----------------------------

import Natural  ( length )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEqR, runTestsP, runTestsReplay, runTestTree )

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
import MInfo.Types.Track  ( Track( Track )
                          , album, album_version, _track1, _track2, _track3 )

--------------------------------------------------------------------------------

newtype Tracks = Tracks { unTracks ∷ [[Track]] }
  deriving (Eq,Show)

----------------------------------------

instance Printable Tracks where
  print tss = P.text ∘ unlines $ toText ∘ view _4 ⊳ flatTracks tss

----------------------------------------

instance FromJSON Tracks where
  parseJSON json =
    let parseTs ∷ Value → Parser [Track]
        parseTs xs@(Array _) = parseJSON xs
        parseTs (Object o) = do (ts,d,v) ← (,,) ⊳ (o .:  "tracks")
                                                ⊵ (o .:? "discname")
                                                ⊵ (o .:? "discname_version")
                                return $ (album ⊮ d) ∘ (album_version ⊮ v) ⊳ ts
        parseTs invalid      = typeMismatch "Array|Object (parseTs)" invalid

        parseTss ∷ Value → Parser [[Track]]
        parseTss (Array xs) = sequence $ parseTs ⊳ toList xs
        parseTss invalid = typeMismatch "Array (parseTss)" invalid
     in case json of
          Array ts →
            case ts !? 0 of
              Nothing         → return $ Tracks [[]]
              Just (Array _)  → Tracks ⊳ parseTss (Array ts)
              Just _          →
                (Tracks ∘ pure) ⊳ (sequence $ parseJSON ⊳ toList ts)
          invalid  → typeMismatch "Array (parseJSON @Tracks)" invalid

--------------------

newtype TrackShow = TrackShow Track
  deriving (Eq,Show)

instance Printable TrackShow where
  print (TrackShow t) = P.string $ show t

tracksShow ∷ HasTracks τ ⇒ τ → [TrackShow]
tracksShow tss = (TrackShow ∘ view _4) ⊳ flatTracks tss

tracksShow' ∷ Tracks → [TrackShow]
tracksShow' = tracksShow

tracksFromJSONTests ∷ TestTree
tracksFromJSONTests =
  testGroup "tracksFromJSON"
            [ testCase "t1"  $
                Right [_track1,_track2] @=? unYaml @YamlParseError t1
            , testCase "t1'" $
                Right (Tracks [[_track1,_track2]]) @=? unYaml @YamlParseError t1
            , testCase "t3" $
                Right _ts1 @=? unYaml @YamlParseError t3
            , testGroup "t8" $
                assertListEqR "t8" (tracksShow' ⊳ unYaml @YamlParseError t8)
                                   (tracksShow' _ts8)
            , testCase "t8" $
                Right _ts8 @=? unYaml @YamlParseError t8
            ]

----------------------------------------

instance ToJSON Tracks where
  toJSON = Array ∘ fromList ∘ fmap toJSON ∘ flatTracks

----------------------------------------

class HasTracks τ where
  tracks ∷ Lens' τ Tracks
  {- | List of all the tracks, each preceded by total trackid, discid, and
       number-of-track-in-disc-id. -}
  flatTracks ∷ τ → [(ℕ,ℕ,ℕ,Track)]
  flatTracks tss =
    let indexn ∷ [α] → [(ℕ,α)]
        indexn = zip [1..]
        indexnn ∷ [[α]] → [(ℕ, [(ℕ,α)])]
        indexnn = indexn ∘ fmap indexn
        unroll (a,(b,c)) = (a,b,c)
        unroll2 (a,(b,c,d)) = (a,b,c,d)
        unrolls ∷ Functor ψ ⇒ (α, ψ (β,γ)) → ψ (α,β,γ)
        unrolls (x,ys) = unroll ⊳ (x,) ⊳ ys
     in unroll2 ⩺ indexn ∘ ю $ unrolls ⊳ (indexnn (unTracks $ tss ⊣ tracks))
  trackCount ∷ τ → ℕ
  trackCount = length ∘ flatTracks
  discTrackCount ∷ τ → ℕ → Maybe ℕ
  discTrackCount tss d = length ⊳ unTracks (tss ⊣ tracks) !! d

instance HasTracks Tracks where
  tracks = id

------------------------------------------------------------

instance HasIndex Tracks where
  type Elem Tracks = Track
  type Indexer Tracks = ℕ
  index i ts = view _4 ⊳ flatTracks ts !! i

{- | `Tracks` may be indexed either by a single ℕ (i.e., index into
     `flatTracks`), or by a pair of ℕ (disc, then trackid). -}
class TrackIndex τ ι where
  {- | If present, returns the DiscID (1-based), Track-on-Disc-ID (1-based),
       Total TrackID (1-based), and the Track. -}
  track ∷ τ → ι → Maybe (ℕ, ℕ, ℕ, Track)

instance TrackIndex Tracks Natural where
  track ts n = flatTracks ts !! n

instance TrackIndex Tracks (Natural,Natural) where
  track ts (d,i) =
    case filter (\(_,d',i',_)→ d ≡ d'-1 ∧ i ≡ i'-1 ) (flatTracks ts) of
      x:_ → Just x
      []  → Nothing

hasIndexTests ∷ TestTree
hasIndexTests =
  testGroup "hasIndex" [ testCase "_track1" $ Just _track1 @=? _ts1 !! 0
                       , testCase "_track2" $ Just _track2 @=? _ts1 !! 1
                       , testCase "_track3" $ Just _track3 @=? index 2 _ts1
                       , testCase "-"       $ Nothing @=? index 3 _ts1

                       , testCase "_track1(1)" $
                             Just (1,1,1,_track1) @=? track _ts1 (0∷ℕ)
                       , testCase "_track2(1)" $
                             Just (2,1,2,_track2) @=? track _ts1 (1∷ℕ)
                       , testCase "_track3(1)" $
                             Just (3,2,1,_track3) @=? track _ts1 (2∷ℕ)
                       , testCase "-(1 (1))"  $ Nothing @=? track _ts1 (3∷ℕ)

                       , testCase "_track1(2)" $
                             Just (1,1,1,_track1) @=? track _ts1 (0∷ℕ,0∷ℕ)
                       , testCase "_track2(2)" $
                             Just (2,1,2,_track2) @=? track _ts1 (0∷ℕ,1∷ℕ)
                       , testCase "-(2 (0))"  $
                             Nothing @=? track _ts1 (0∷ℕ,2∷ℕ)
                       , testCase "_track3(2)" $
                             Just (3,2,1,_track3) @=? track _ts1 (1∷ℕ,0∷ℕ)
                       , testCase "-(2 (1))"  $
                             Nothing @=? track _ts1 (1∷ℕ,1∷ℕ)
                       ]

----------------------------------------

class HasDiscCount α where
  discCount ∷ α → ℕ

instance HasDiscCount Tracks where
  discCount ∷ Tracks → ℕ
  discCount (Tracks tss) = length tss

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

_ts1 ∷ Tracks
_ts1 = Tracks [[_track1,_track2],[_track3]]

----------

_ts2 ∷ Tracks
_ts2 = let mkTrack t = Track Nothing (Just t) Nothing Live
                       (Just "Stade Couvert Régional, Liévin, France")
                       (Just [dateImpreciseRange|1993-07-29|]) Nothing Nothing
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
                         , "    live_type: Live Vocal"
                         , "    live_date: 1993-07-29"
                         ]

_ts3 ∷ Tracks
_ts3 = let mkTrack t = Track Nothing (Just t) Nothing NotLive Nothing Nothing
                             Nothing Nothing
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
                             Nothing Nothing
           mkTrack' (t,v) = Track Nothing (Just t) (Just v)
                                  NotLive Nothing Nothing Nothing Nothing
           mkTrackD t = Track Nothing (Just t) Nothing
                              Demo Nothing Nothing Nothing Nothing
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
                             Nothing Nothing
           mkTrack' (t,v) = Track Nothing (Just t) (Just v)
                                  NotLive Nothing Nothing Nothing Nothing
           mkTrackD t = Track Nothing (Just t) (Just "Demo")
                              NotLive Nothing Nothing Nothing Nothing
           mkTrackS t = Track Nothing (Just t) Nothing
                              Session Nothing
                              (Just [dateImpreciseRange|2008-12-08|])
                              Nothing Nothing
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
                              , "Oh Well" -- 19
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
                                  , "In Chains" -- 14
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
                                  , ("Hole to Feed","Demo") -- 10
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
                                     "Thomas Fehlman Ambient Mix") -- 20
                                  , ("Wrong","Magda's Scallop Funk Remix")
                                  , ("Jezebel","SixTøes Remix")
                                  , ("Wrong","Trentemøller Remix")
                                  , ("Perfect",
                                     "Drone Mix")
                                  , ("Wrong","Caspa Remix")
                                  , ("Oh Well","Black Light Odyssey Dub")
                                  , ("Sun and the Moon and the Stars,The",
                                     "Electronic Periodic's Microdrum Mix")
                                  , ("Oh Well","Black Light Odyssey Remix") --28
                                  ]
                      )
                  ,   (mkTrackS ⊳ [ "Corrupt"
                                  , "Little Soul"
                                  , "Stories of Old"
                                  , "Come Back"
                                  ]
                      )
                  ]

_ts6 ∷ Tracks
_ts6 = Tracks [ [ Track Nothing (Just "In Chains") Nothing
                        NotLive Nothing Nothing Nothing Nothing
                , Track Nothing (Just "Hole to Feed") Nothing
                        NotLive Nothing Nothing Nothing Nothing
                ]
              , [ Track Nothing
                        (Just "Wrong") (Just "Trentemøller Remix")
                        NotLive Nothing Nothing Nothing Nothing
                , Track Nothing
                        (Just "Perfect")
                        (Just "Drone Mix")
                        NotLive Nothing Nothing Nothing Nothing
                ]
              ]

----------

t8 ∷ ByteString
t8 = BS.intercalate "\n" [ "-"
                         , "    - title: 'In Chains'"
                         , "    - title: 'Hole to Feed'"
                         , "      album: Bonus"
                         , "      album_version: BB"
                         , "-"
                         , "    tracks:"
                         , "      - title: 'Wrong'"
                         , "        version: Trentem\195\184ller Remix"
                         , "      - title: 'Perfect'"
                         , "        version: Drone Mix"
                         , "        album: Bonus"
                         , "        album_version: BB"
                         , "-"
                         , "    discname: Third"
                         , "    tracks:"
                         , "      - Jezebel"
                         ]
_ts8 ∷ Tracks
_ts8 = Tracks [ [ Track Nothing (Just "In Chains") Nothing
                        NotLive Nothing Nothing Nothing Nothing
                , Track Nothing (Just "Hole to Feed") Nothing
                        NotLive Nothing Nothing (Just "Bonus") (Just "BB")
                ]
              , [ Track Nothing
                        (Just "Wrong") (Just "Trentemøller Remix")
                        NotLive Nothing Nothing Nothing Nothing
                , Track Nothing
                        (Just "Perfect")
                        (Just "Drone Mix")
                        NotLive Nothing Nothing (Just "Bonus") (Just "BB")
                ]
              , [ Track Nothing
                        (Just "Jezebel") Nothing
                        NotLive Nothing Nothing (Just "Third") Nothing
                ]
              ]

----------

_ts9 ∷ Tracks
_ts9 = Tracks [ [ Track Nothing (Just "Shelter from the Rain") Nothing
                        NotLive Nothing Nothing Nothing Nothing
                ]
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
