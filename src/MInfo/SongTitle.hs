{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module MInfo.SongTitle
  ( lName, flacName, flacNames, liveName, mp3Name, mp3Names, songTitle

  , tests
  )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( return, sequence )
import Data.Either          ( Either( Right ) )
import Data.Function        ( ($) )
import Data.Functor         ( fmap )
import Data.List            ( zip )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( Maybe( Just, Nothing ), catMaybes )
import Data.String          ( String )
import System.Exit          ( ExitCode )
import System.IO            ( IO )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( fromText, toText )

-- date-imprecise ----------------------

import DateImprecise.DateImprecise       ( dateImprecise )
import DateImprecise.DateImpreciseRange  ( DateImpreciseRange
                                         , dateImpreciseRange )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Functor      ( (⊲), (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Semigroup    ( (◇) )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- fpath -------------------------------

import FPath.Error.FPathComponentError ( AsFPathComponentError )
import FPath.FileLike                  ( (⊙) )
import FPath.PathComponent             ( PathComponent, parsePathC, pc )
import FPath.RelFile                   ( RelFile, relfile )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.IsNonEmpty  ( fromNonEmpty )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEqR, runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( Text, intercalate, replace )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.Errors             ( AsInfoError, InfoError, InfoFPCError
                                , YamlParseInfoFPCError, throwIllegalFileName )
import MInfo.Types              ( HasLiveDate( liveDate )
                                , HasLiveLocation( liveLocation )
                                , HasLiveType( liveType ), LiveLocation
                                , LiveType( Live, NotLive, Session )
                                , MultiDisc( MultiDisc, SingleDisc )
                                , TrackTitle( TrackTitle )
                                )
import MInfo.Types.Info         ( Info
                                , _info1, _info6 )
import MInfo.Types.ReleaseInfo  ( HasReleaseInfo( releaseInfo )
                                , ReleaseInfo( ReleaseInfo ), _rinfo2 )
import MInfo.Types.Track        ( Track( Track )
                                , title, version, _track4, _track5 )
import MInfo.Types.Tracks       ( unTracks, tracks )

--------------------------------------------------------------------------------

lName ∷ LiveType → Maybe LiveLocation → Maybe DateImpreciseRange → Maybe Text
lName NotLive _ _ = Nothing
lName lType lLocY lDateY =
  Just $ intercalate " " (toText lType : catMaybes [ toText ⊳ lLocY
                                                   , toText ⊳ lDateY ])
----------

lNameTests ∷ TestTree
lNameTests =
  testGroup "lName"
            [ testCase "nothing" $ Nothing @=? lName NotLive Nothing Nothing
            , testCase "live" $
                    Just "Live Hammersmith Odeon 1970-01-01"
                @=? lName Live (Just "Hammersmith Odeon")
                               (Just [dateImpreciseRange|1970-01-01|])
            ]

----------------------------------------

liveName ∷ (HasLiveDate ρ, HasLiveLocation ρ, HasLiveType ρ) ⇒
           ρ → Track → Maybe Text
liveName r t = lName ((t ⊣ liveType) ◇ (r ⊣ liveType))
                     (t ⊣ liveLocation ∤ r ⊣ liveLocation)
                     (t ⊣ liveDate ∤ r ⊣ liveDate)

----------

liveNameTests ∷ TestTree
liveNameTests =
  testGroup "liveName"
            [ testCase "nothing" $ Nothing @=? liveName _rinfo2 _track4
            , testCase "live" $
                    Just "Live Hamburg 1984-12-14"
                @=? liveName _info1 _track4
            ]

----------------------------------------

songTitle ∷ HasReleaseInfo ρ ⇒ ρ → Track → Maybe TrackTitle
songTitle r t =
  let encompass  x y i = x ⊕ i ⊕ y
      parens   = encompass "(" ")"
      brackets = encompass "[" "]"
   in case t ⊣ title of
        Nothing → Nothing
        Just ti → let vv   = (parens ∘ toText) ⊳ t ⊣ version
                      ll   = brackets ⊳ liveName (r ⊣ releaseInfo) t
                      bits = [ Just $ toText ti, vv, ll ]
                   in Just (TrackTitle $ intercalate "  " (catMaybes bits))

----------

songTitleTests ∷ TestTree
songTitleTests =
  testGroup "songTitle"
            [ testCase "nothing" $
                Just "Two Minute Warning" @=? songTitle _rinfo2 _track5
            , testCase "live" $
                    Just "Something to Do  [Live Hamburg 1984-12-14]"
                @=? songTitle _info1 _track4
            ]

----------------------------------------

fileName ∷ (AsInfoError ε, MonadError ε η) ⇒
           ReleaseInfo → ℕ → Track → η PathComponent
fileName relnfo num trck =
  let gone = replace "/" "-" (go trck)
      encompass  l r t = l ⊕ t ⊕ r
      parens   = encompass "(" ")"
      brackets = encompass "[" "]"
      go t = case t ⊣ title of
               Nothing     → [fmt|%02d|] num
               ti@(Just _) → let vv   = (parens ∘ toText) ⊳ t ⊣ version
                                 ll   = brackets ⊳ liveName relnfo t
                                 bits = [toText ⊳ ti,vv,ll]
                              in [fmt|%02d-%t|]
                                 num $ intercalate "  " (catMaybes bits)
   in case fromText gone of
        Nothing → throwIllegalFileName $ [fmt|illegal file name '%t'|] gone
        Just f  → return f

--------------------

fileNameTests ∷ TestTree
fileNameTests =
  let liveT = [pc|10-live track  [Live Hammersmith Odeon 1970-01-01]|]
      seshT = [pc|100-Sesh  (Acoustic)  [Session 1980-01-01]|]
   in testGroup "fileName"
                [ testCase "_track5" $
                      Right [pc|02-Two Minute Warning|]
                    @=? fileName @InfoError releaseInfo6 2 _track5
                , testCase "trackL" $
                      Right liveT @=? fileName @InfoError releaseInfo6 10 trackL
                , testCase "trackS" $
                      Right seshT @=? fileName @InfoError releaseInfo6 100 trackS
                , testCase "trackL'-rl" $
                        Right [pc|11-Live Track  [Live Sweden 1990-02-02]|]
                    @=? fileName @YamlParseInfoFPCError releaseInfol 11 trackL'
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
  let -- Info rinfo trcks = inf
      trckss ∷ [[Track]] = unTracks (inf ⊣ tracks)
      multi d = if 1 ≡ length trckss then SingleDisc else (MultiDisc d)
      index ∷ [α] → [(ℕ,α)]
      index xs = zip [1..] xs

   in sequence [ trackFile (inf ⊣ releaseInfo) (multi discid) i trck
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
                        Right [pc|02-Two Minute Warning.flac|]
                    @=? flacName @InfoError releaseInfo6 2 _track5
                ]

----------------------------------------

flacNames ∷ (AsInfoError ε, AsFPathComponentError ε, MonadError ε η) ⇒
             Info → η [RelFile]
flacNames = fmap (⊙ [pc|flac|]) ⩺ fileNames

--------------------

flacNamesTests ∷ TestTree
flacNamesTests =
  let info1Tr1 =[relfile|01-Something to Do  [Live Hamburg 1984-12-14].flac|]
      info1Tr2 =[relfile|02-Two Minute Warning  [Live Hamburg 1984-12-14].flac|]

      infosTr1 =[relfile|Disc 01/01-In Chains.flac|]
      infosTr2 =[relfile|Disc 01/02-Hole to Feed.flac|]
      infosTr3 =[relfile|Disc 02/01-Wrong  (Trentemøller Remix).flac|]
      infosTr4 =[relfile|Disc 02/02-Perfect  (Drone Mix).flac|]
      check name expect inf =
        assertListEqR name (flacNames @InfoFPCError inf) expect
   in testGroup "flacNames" $
                 ю [ check "info1" [info1Tr1,info1Tr2]                   _info1
                   , check "infos" [infosTr1,infosTr2,infosTr3,infosTr4] _info6
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
                 ю [ check "info1" [info1Tr1,info1Tr2]                   _info1
                   , check "info6" [infosTr1,infosTr2,infosTr3,infosTr4] _info6
                   ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

----------------------------------------
--             test data              --
----------------------------------------

trackL ∷ Track
trackL = Track Nothing (Just "live track") Nothing
               Live (Just "Hammersmith Odeon")
               (Just [dateImpreciseRange|1970-01-01|]) Nothing Nothing

trackL' ∷ Track
trackL' = Track Nothing (Just "Live Track") Nothing
                NotLive Nothing (Just [dateImpreciseRange|1990-02-02|])
                Nothing Nothing

trackS ∷ Track
trackS = Track Nothing (Just "Sesh") (Just "Acoustic")
               Session Nothing (Just [dateImpreciseRange|1980-01-01|])
               Nothing Nothing

releaseInfo6 ∷ ReleaseInfo
releaseInfo6 = ReleaseInfo ("artie") (Just "123X")
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
tests = testGroup "MInfo.SongTitle"
                  [ lNameTests, liveNameTests, songTitleTests
                         , fileNameTests, mp3NamesTests, flacNameTests
                         , flacNamesTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
