{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module MInfo.Types.Info
  ( Info( Info )
  , blankInfo, flacName, flacNames, info1, infos, mp3Name, mp3Names
  , releaseInfo, trackCount

  , tests )
where

import Prelude  ( fromIntegral )

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Object ), (.:), withObject )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( return, sequence )
import Data.Either          ( Either( Left, Right ) )
import Data.Eq              ( Eq )
import Data.Function        ( ($) )
import Data.Functor         ( fmap )
import Data.List            ( replicate, zip )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( Maybe( Just, Nothing ), catMaybes )
import Data.String          ( String )
import GHC.Generics         ( Generic )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), fromText, toText )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- fpath -------------------------------

import FPath.Error.FPathComponentError ( AsFPathComponentError )
import FPath.FileLike                  ( (⊙) )
import FPath.PathComponent             ( PathComponent, parsePathC, pc )
import FPath.RelFile                   ( RelFile, relfile )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (∤) )
import Data.MoreUnicode.Functor      ( (⊲), (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Semigroup    ( (◇) )
import Data.MoreUnicode.Tasty        ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.IsNonEmpty  ( fromNonEmpty )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEqR, runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( Text, intercalate, replace )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON ), object )

-- yaml-plus ---------------------------

import PYaml           ( pyaml )
import YamlPlus        ( unYaml )
import YamlPlus.Error  ( YamlParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  MInfo.Types.ReleaseInfo  as  ReleaseInfo
import qualified  MInfo.T.TestData         as  TestData
import qualified  MInfo.Types.Track        as  Track

import MInfo.Errors                    ( AsInfoError, InfoError, InfoFPCError
                                       , YamlParseInfoFPCError
                                       , throwIllegalFileName
                                       )

import MInfo.Types.DateImprecise       ( dateImprecise )
import MInfo.Types.DateImpreciseRange  ( DateImpreciseRange
                                       , dateImpreciseRange )
import MInfo.Types                     ( LiveLocation
                                       , LiveType( Demo, Live, NotLive
                                                 , Session )
                                       )
import MInfo.Types.ReleaseInfo         ( ReleaseInfo( ReleaseInfo )
                                       , blankReleaseInfo, releaseInfoFields )
import MInfo.Types.Track               ( Track( Track ), blankTrack )
import MInfo.Types.Tracks              ( Tracks( Tracks, unTracks )
                                       , flatTracks )

--------------------------------------------------------------------------------

data MultiDisc = SingleDisc | MultiDisc ℕ

------------------------------------------------------------

data Info = Info { _releaseInfo ∷ ReleaseInfo
                 , _tracks      ∷ Tracks
                 }
  deriving (Generic, Eq, Show)

releaseInfo ∷ Lens' Info ReleaseInfo
releaseInfo = lens _releaseInfo (\ i r → i { _releaseInfo = r })

tracks ∷ Info → [Track]
tracks i = flatTracks (_tracks i)

instance FromJSON Info where
  parseJSON = withObject "Info" $
    \ v → Info ⊳ parseJSON (Object v) ⊵ v .: "tracks"

info1 ∷ Info
info1 = Info (ReleaseInfo ("Depeche Mode") Nothing Nothing Nothing
                          (Just "World We Live in and Live in Hamburg,The")
                          Nothing
                          Live
                          (Just "Hamburg")
                          (Just ([dateImpreciseRange|1984-12-14|]))
             )
             (Tracks [ [ Track Nothing (Just "Something to Do") Nothing
                               NotLive Nothing Nothing
                       , Track Nothing (Just "Two Minute Warning") Nothing
                               NotLive Nothing Nothing
                       ]
                     ])

--------------------

releaseInfo2 ∷ ReleaseInfo
releaseInfo2 = ReleaseInfo ("Depeche Mode") (Just "DMDVD4") Nothing
                           Nothing (Just "Devotional")
                           Nothing NotLive Nothing Nothing
tracks2 ∷ Tracks
tracks2 = let mkTrack t = Track Nothing (Just t) Nothing
                          Live
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

info2 ∷ Info
info2 = Info releaseInfo2 tracks2

--------------------

releaseInfo3 ∷ ReleaseInfo
releaseInfo3 = ReleaseInfo ("Depeche Mode") (Just "12345")
                           (Just ([dateImprecise|1993|])) Nothing
                           (Just "Radio 1 in Concert") Nothing
                           Live (Just "Crystal Palace")
                           (Just ([dateImpreciseRange|1993-07-31|]))
tracks3 ∷ Tracks
tracks3 = let mkTrack t = Track Nothing (Just t) Nothing NotLive Nothing Nothing
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

info3 ∷ Info
info3 = Info releaseInfo3 tracks3

--------------------

releaseInfo4 ∷ ReleaseInfo
releaseInfo4 =
  ReleaseInfo ("Depeche Mode") (Just "BX Stumm 300")
              (Just ([dateImprecise|2009-04-17|]))
              Nothing
              (Just "Sounds of the Universe  (Deluxe Box Set)")
                Nothing
              NotLive Nothing Nothing
tracks4 ∷ Tracks
tracks4 = let mkTrack t = Track Nothing (Just t) Nothing NotLive Nothing Nothing
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

info4 ∷ Info
info4 = Info releaseInfo4 tracks4

--------------------

releaseInfo5 ∷ ReleaseInfo
releaseInfo5 =
  ReleaseInfo ("Depeche Mode") Nothing
              (Just ([dateImprecise|2009-04-17|]))
              Nothing
              (Just "Sounds of the Universe  (Deluxe Box Set)") Nothing
              NotLive Nothing Nothing

tracks5 ∷ Tracks
tracks5 = let mkTrack t = Track Nothing (Just t) Nothing NotLive Nothing Nothing
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

info5 ∷ Info
info5 = Info releaseInfo5 tracks5

--------------------

infos ∷ Info
infos = Info (ReleaseInfo ("Depeche Mode") Nothing
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
         in ю [ [ testCase      (nme "release info") $ Right erinfo ≟ rinfo ]
                , assertListEqR (nme "tracks")
                                (flatTracks ⊳ trcks) (flatTracks etrcks)
                , assertListEqR (nme "flat tracks")
                                (unTracks ⊳trcks) (unTracks etrcks)
                , [ testCase (nme "info") $
                      Right info2 ≟ unYaml @YamlParseError TestData.info2T
                  ]
                ]

   in testGroup "infoFromJSON"
                (ю [ [ testCase "info1'" $
                         Right info1 ≟ unYaml @YamlParseError TestData.info1T
                     ]
                   , checkInfo "info2" TestData.info2T info2
                   , checkInfo "info3" TestData.info3T info3
                   , checkInfo "info4" TestData.info4T info4
                   , checkInfo "info5" TestData.info5T info5
                   , checkInfo "infos" TestData.infosT infos
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
trackCount = length ∘ tracks

trackCountTests ∷ TestTree
trackCountTests =
  testGroup "trackCount"
            [ testCase "info1" $
                Right  2 ≟ trackCount ⊳ (unYaml @YamlParseError TestData.info1T)
            , testCase "info2" $
                Right 19 ≟ trackCount ⊳ (unYaml @YamlParseError TestData.info2T)
            , testCase "info3" $
                Right 12 ≟ trackCount ⊳ (unYaml @YamlParseError TestData.info3T)
            , testCase "info4" $
                Right 39 ≟ trackCount ⊳ (unYaml @YamlParseError TestData.info4T)
            , testCase "info5" $
                Right 65 ≟ trackCount ⊳ (unYaml @YamlParseError TestData.info5T)
            , testCase "infos" $
                Right  4 ≟ trackCount ⊳ (unYaml @YamlParseError TestData.infosT)
            ]

----------------------------------------

lName ∷ LiveType → Maybe LiveLocation → Maybe DateImpreciseRange → Maybe Text
lName NotLive _ _ = Nothing
lName lType lLocY lDateY =
  Just $ intercalate " " (toText lType : catMaybes [ toText ⊳ lLocY
                                                   , toText ⊳ lDateY ])

--------------------

lNameTests ∷ TestTree
lNameTests =
  testGroup "lName"
            [ testCase "nothing" $ Nothing ≟ lName NotLive Nothing Nothing
            , testCase "live" $
                  Just "Live Hammersmith Odeon 1970-01-01"
                ≟ lName Live (Just "Hammersmith Odeon")
                             (Just [dateImpreciseRange|1970-01-01|])
            ]

----------------------------------------

liveName ∷ ReleaseInfo → Track → Maybe Text
liveName r t = lName ((t ⊣ Track.live_type) ◇ (r ⊣ ReleaseInfo.live_type))
                     (t ⊣ Track.live_location ∤ r ⊣ ReleaseInfo.live_location)
                     (t ⊣ Track.live_date ∤ r ⊣ ReleaseInfo.live_date)

--------------------

liveNameTests ∷ TestTree
liveNameTests = testGroup "liveName"
                          [ testCase "track1" $
                              Nothing ≟ liveName releaseInfo1 track1
                          , testCase "trackL" $
                                Just "Live Hammersmith Odeon 1970-01-01"
                              ≟ liveName releaseInfo1 trackL
                          ]

----------------------------------------

fileName ∷ (AsInfoError ε, MonadError ε η) ⇒
           ReleaseInfo → ℕ → Track → η PathComponent
fileName relnfo num trck =
  let gone = replace "/" "-" (go trck)
      encompass  l r t = l ⊕ t ⊕ r
      parens   = encompass "(" ")"
      brackets = encompass "[" "]"
      go t = case t ⊣ Track.title of
               Nothing     → [fmt|%02d|] num
               ti@(Just _) → let vv = (parens ∘ toText) ⊳ t ⊣ Track.version
                                 ll = brackets ⊳ liveName relnfo t
                              in [fmt|%02d-%t|]
                                 num (intercalate "  " $ catMaybes [toText ⊳ ti,vv,ll])
   in case fromText gone of
        Nothing → throwIllegalFileName $ [fmt|illegal file name '%t'|] gone
        Just f  → return f

--------------------

fileNameTests ∷ TestTree
fileNameTests =
  let liveT = [pc|10-live track  [Live Hammersmith Odeon 1970-01-01]|]
      seshT = [pc|100-Sesh  (Acoustic)  [Session 1980-01-01]|]
   in testGroup "fileName"
                [ testCase "track1" $
                      Right [pc|02-track title|]
                    ≟ fileName @InfoError releaseInfo1 2 track1
                , testCase "trackL" $
                      Right liveT ≟ fileName @InfoError releaseInfo1 10 trackL
                , testCase "trackS" $
                      Right seshT ≟ fileName @InfoError releaseInfo1 100 trackS
                , testCase "trackL'-rl" $
                      Right [pc|11-Live Track  [Live Sweden 1990-02-02]|]
                    ≟ fileName @YamlParseInfoFPCError releaseInfol 11 trackL'
                ]

----------------------------------------

trackFile ∷ (AsInfoError ε, AsFPathComponentError ε, MonadError ε η) ⇒
            ReleaseInfo → MultiDisc → ℕ → Track → η RelFile
trackFile ri SingleDisc i trck =
  (fromNonEmpty ∘ pure) ⊳ fileName ri i trck
trackFile ri (MultiDisc disc) i trck = do
  d ← parsePathC $ [fmtT|Disc %02d|] disc
  f ← fileName ri i trck
  return $ fromNonEmpty (d :| [f])

----------------------------------------

fileNames ∷ (AsInfoError ε, AsFPathComponentError ε, MonadError ε η) ⇒
             Info → η [RelFile]
fileNames inf =
  let Info rinfo trcks = inf
      trckss ∷ [[Track]] = unTracks trcks
      multi d = if 1 ≡ length trckss then SingleDisc else (MultiDisc d)
      index ∷ [α] → [(ℕ,α)]
      index xs = zip [1..] xs

   in sequence [ trackFile rinfo (multi discid) i trck
               | (discid,ts) ← index trckss, (i,trck) ← index ts ]

----------------------------------------

flacName ∷ (AsInfoError ε, MonadError ε η) ⇒
           ReleaseInfo → ℕ → Track → η PathComponent
flacName r n t = fileName r n t ⊲ (⊙ [pc|flac|])

--------------------

flacNameTests ∷ TestTree
flacNameTests =
  testGroup "flacName"
                [ testCase "track1" $
                      Right [pc|02-track title.flac|]
                    ≟ flacName @InfoError releaseInfo1 2 track1
                ]

----------------------------------------

flacNames ∷ (AsInfoError ε, AsFPathComponentError ε, MonadError ε η) ⇒
             Info → η [RelFile]
flacNames = fmap (⊙ [pc|flac|]) ⩺ fileNames


--------------------

flacNamesTests ∷ TestTree
flacNamesTests =
  let info1Tr1 = [relfile|01-Something to Do  [Live Hamburg 1984-12-14].flac|]
      info1Tr2 = [relfile|02-Two Minute Warning  [Live Hamburg 1984-12-14].flac|]

      infosTr1 = [relfile|Disc 01/01-In Chains.flac|]
      infosTr2 = [relfile|Disc 01/02-Hole to Feed.flac|]
      infosTr3 = [relfile|Disc 02/01-Wrong  (Trentemøller Remix).flac|]
      infosTr4 = [relfile|Disc 02/02-Perfect  (Drone Mix).flac|]
      check name expect inf =
        assertListEqR name (flacNames @InfoFPCError inf) expect
   in testGroup "flacNames" $
                 ю [ check "info1" [info1Tr1,info1Tr2]                   info1
                   , check "infos" [infosTr1,infosTr2,infosTr3,infosTr4] infos
                   ]

----------------------------------------

mp3Name ∷ (AsInfoError ε, MonadError ε η) ⇒
          ReleaseInfo → ℕ → Track → η PathComponent
mp3Name r n t = fileName r n t ⊲ (⊙ [pc|mp3|])

mp3Names ∷ (AsInfoError ε, AsFPathComponentError ε, MonadError ε η) ⇒
           Info → η [RelFile]
mp3Names = fmap (⊙ [pc|mp3|]) ⩺ fileNames

--------------------

mp3NamesTests ∷ TestTree
mp3NamesTests =
  let info1Tr1 = [relfile|01-Something to Do  [Live Hamburg 1984-12-14].mp3|]
      info1Tr2 = [relfile|02-Two Minute Warning  [Live Hamburg 1984-12-14].mp3|]

      infosTr1 = [relfile|Disc 01/01-In Chains.mp3|]
      infosTr2 = [relfile|Disc 01/02-Hole to Feed.mp3|]
      infosTr3 = [relfile|Disc 02/01-Wrong  (Trentemøller Remix).mp3|]
      infosTr4 = [relfile|Disc 02/02-Perfect  (Drone Mix).mp3|]
      check name expect inf =
        assertListEqR name (mp3Names @InfoFPCError inf) expect
   in testGroup "mp3Names" $
                 ю [ check "info1" [info1Tr1,info1Tr2]                   info1
                   , check "infos" [infosTr1,infosTr2,infosTr3,infosTr4] infos
                   ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data -----------------------------------------------

track1 ∷ Track
track1 = Track Nothing (Just "track title") Nothing NotLive Nothing Nothing

trackL ∷ Track
trackL = Track Nothing (Just "live track") Nothing
               Live (Just "Hammersmith Odeon")
               (Just [dateImpreciseRange|1970-01-01|])

trackL' ∷ Track
trackL' = Track Nothing (Just "Live Track") Nothing
                NotLive Nothing (Just [dateImpreciseRange|1990-02-02|])

trackS ∷ Track
trackS = Track Nothing (Just "Sesh") (Just "Acoustic")
               Session Nothing (Just [dateImpreciseRange|1980-01-01|])

releaseInfo1 ∷ ReleaseInfo
releaseInfo1 = ReleaseInfo ("artie") (Just "123X")
                           (Just [dateImprecise|1979-12-31|])
                           Nothing (Just "Elpee") Nothing NotLive Nothing
                           Nothing

releaseInfol ∷ ReleaseInfo
releaseInfol = ReleaseInfo ("simon") (Just "124XX")
                           (Just [dateImprecise|1979-12-31|])
                           Nothing
                           (Just "An LP Title") Nothing
                           Live (Just "Sweden")
                           (Just [dateImpreciseRange|1990|])

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Info" [ infoPrintableTests, infoFromJSONTests
                         , trackCountTests, lNameTests, liveNameTests
                         , fileNameTests, mp3NamesTests, flacNameTests
                         , flacNamesTests
                         ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
