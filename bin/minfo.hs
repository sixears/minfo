{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

import Prelude  ( error )

-- base --------------------------------

import Control.Applicative     ( pure )
import Control.Monad           ( forM_, mapM_, return, sequence )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool )
import Data.Either             ( Either( Left, Right ) )
import Data.Foldable           ( Foldable )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.List               ( zip )
import Data.List.NonEmpty      ( NonEmpty( (:|) ) )
import Data.Maybe              ( Maybe( Just, Nothing ), catMaybes )
import Data.String             ( String )
import Data.Typeable           ( Typeable, typeOf )
import Data.Word               ( Word8 )
import Numeric.Natural         ( Natural )
import System.Exit             ( ExitCode )
import System.IO               ( IO )
import Text.Printf             ( printf )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), Printable, Textual
                     , fromText, parseText, toString, toText )

-- exited ------------------------------

import Exited  ( doMain )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- fpath -------------------------------

import FPath.Error.FPathComponentError ( AsFPathComponentError )
import FPath.File                      ( File( FileR ) )
import FPath.FileLike                  ( (⊙) )
import FPath.PathComponent             ( PathComponent, parsePathC, pc )
import FPath.RelFile                   ( RelFile, relfile )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- monaderror-io -----------------------

import MonadError  ( ѥ )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴), (∤) )
import Data.MoreUnicode.Functor      ( (⊲), (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Semigroup    ( (◇) )
import Data.MoreUnicode.Tasty        ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.IsNonEmpty  ( fromNonEmpty )

-- optparse-applicative ----------------

import Options.Applicative  ( ArgumentFields, CommandFields, Mod, Parser, ReadM
                            , action, argument, auto, command, completer
                            , customExecParser, eitherReader, failureCode
                            , fullDesc, helper, info, listCompleter, metavar
                            , prefs, progDesc, showHelpOnEmpty, showHelpOnError
                            , subparser, value
                            )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEqR, runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text     ( Text, intercalate, pack, replace )
import Data.Text.IO  ( putStrLn )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.YamlPlus             ( unYamlFile )
import MInfo.YamlPlus.Error       ( AsYamlParseError )

import qualified  MInfo.Types.ReleaseInfo  as  ReleaseInfo
import qualified  MInfo.Types.Track        as  Track

import MInfo.Errors                    ( AsInfoError, InfoError, InfoFPCError
                                       , YamlParseInfoFPCError
                                       , throwIllegalFileName
                                       )
import MInfo.Types                     ( LiveLocation
                                       , LiveType( Live, NotLive, Session )
                                       )
import MInfo.Types.DateImprecise       ( dateImprecise )
import MInfo.Types.DateImpreciseRange  ( DateImpreciseRange
                                       , dateImpreciseRange )

import MInfo.Types.Info                ( Info( Info ), blankInfo, info1, infos
                                       , trackCount )
import MInfo.Types.ReleaseInfo         ( ReleaseInfo( ReleaseInfo ) )
import MInfo.Types.Track               ( Track( Track ) )
import MInfo.Types.Tracks              ( Tracks( unTracks ) )

--------------------------------------------------------------------------------

-- this looks like a monadic fold, or somesuch.  Maybe foldM or MaybeT?
maybeList ∷ [Maybe α] → Maybe α
maybeList [] = Nothing
maybeList (Just a : _)   = Just a
maybeList (Nothing : as) = maybeList as

lName ∷ LiveType → Maybe LiveLocation → Maybe DateImpreciseRange → Maybe Text
lName NotLive _ _ = Nothing
lName lType lLocY lDateY =
  Just $ intercalate " " (toText lType : catMaybes [ toText ⊳ lLocY
                                                   , toText ⊳ lDateY ])

lNameTests ∷ TestTree
lNameTests =
  testGroup "lName"
            [ testCase "nothing" $ Nothing ≟ lName NotLive Nothing Nothing
            , testCase "live" $
                  Just "Live Hammersmith Odeon 1970-01-01"
                ≟ lName Live (Just "Hammersmith Odeon")
                             (Just [dateImpreciseRange|1970-01-01|])
            ]

liveName ∷ ReleaseInfo → Track → Maybe Text
liveName r t = lName ((t ⊣ Track.live_type) ◇ (r ⊣ ReleaseInfo.live_type))
                     (t ⊣ Track.live_location ∤ r ⊣ ReleaseInfo.live_location)
                     (t ⊣ Track.live_date ∤ r ⊣ ReleaseInfo.live_date)

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
           ReleaseInfo → Natural → Track → η PathComponent
fileName relnfo num trck =
  let gone = replace "/" "-" (go trck)
      encompass  l r t = l ⊕ t ⊕ r
      parens   = encompass "(" ")"
      brackets = encompass "[" "]"
      go t = case t ⊣ Track.title of
               Nothing     → pack $ printf "%02d" num
               ti@(Just _) → let vv = (parens ∘ toText) ⊳ t ⊣ Track.version
                                 ll = brackets ⊳ liveName relnfo t
                              in [fmt|%02d-%t|]
                                 num (intercalate "  " $ catMaybes [toText ⊳ ti,vv,ll])
   in case fromText gone of
        Nothing → throwIllegalFileName $ [fmt|illegal file name '%t'|] gone
        Just f  → return f

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


-- ADD TESTS

flacName ∷ (AsInfoError ε, MonadError ε η) ⇒
           ReleaseInfo → Natural → Track → η PathComponent
flacName r n t = fileName r n t ⊲ (⊙ [pc|flac|])

type 𝔹 = Bool

data MultiDisc = SingleDisc | MultiDisc ℕ

trackFile ∷ (AsInfoError ε, AsFPathComponentError ε, MonadError ε η) ⇒
            ReleaseInfo → MultiDisc → ℕ → Track → η RelFile
trackFile ri SingleDisc i trck =
  (fromNonEmpty ∘ pure) ⊳ fileName ri i trck
trackFile ri (MultiDisc disc) i trck = do
  d ← parsePathC $ [fmtT|Disc %02d|] disc
  f ← fileName ri i trck
  return $ fromNonEmpty (d :| [f])

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

flacNames ∷ (AsInfoError ε, AsFPathComponentError ε, MonadError ε η) ⇒
             Info → η [RelFile]
flacNames = fmap (⊙ [pc|flac|]) ⩺ fileNames

flacNameTests ∷ TestTree
flacNameTests =
  testGroup "flacName"
                [ testCase "track1" $
                      Right [pc|02-track title.flac|]
                    ≟ flacName @InfoError releaseInfo1 2 track1
                ]

flacNamesTests ∷ TestTree
flacNamesTests =
  let info1Tr1 = [relfile|01-Something to Do  [Live Alsterdorfer Sporthalle, Hamburg 1984-12-14].flac|]
      info1Tr2 = [relfile|02-Two Minute Warning  [Live Alsterdorfer Sporthalle, Hamburg 1984-12-14].flac|]

      infosTr1 = [relfile|Disc 01/01-In Chains.flac|]
      infosTr2 = [relfile|Disc 01/02-Hole to Feed.flac|]
      infosTr3 = [relfile|Disc 02/01-Wrong  (Trentemøller Remix).flac|]
      infosTr4 = [relfile|Disc 02/02-Perfect  (Electronic Periodic Dark Drone Mix).flac|]
      check name expect inf =
        assertListEqR name (flacNames @InfoFPCError inf) expect
   in testGroup "flacNames" $
                 ю [ check "info1" [info1Tr1,info1Tr2]                   info1
                   , check "infos" [infosTr1,infosTr2,infosTr3,infosTr4] infos
                   ]

mp3Name ∷ (AsInfoError ε, MonadError ε η) ⇒
          ReleaseInfo → Natural → Track → η PathComponent
mp3Name r n t = fileName r n t ⊲ (⊙ [pc|mp3|])

mp3Names ∷ (AsInfoError ε, AsFPathComponentError ε, MonadError ε η) ⇒ Info → η [RelFile]
mp3Names = fmap (⊙ [pc|mp3|]) ⩺ fileNames

mp3NamesTests ∷ TestTree
mp3NamesTests =
  let info1Tr1 = [relfile|01-Something to Do  [Live Alsterdorfer Sporthalle, Hamburg 1984-12-14].mp3|]
      info1Tr2 = [relfile|02-Two Minute Warning  [Live Alsterdorfer Sporthalle, Hamburg 1984-12-14].mp3|]

      infosTr1 = [relfile|Disc 01/01-In Chains.mp3|]
      infosTr2 = [relfile|Disc 01/02-Hole to Feed.mp3|]
      infosTr3 = [relfile|Disc 02/01-Wrong  (Trentemøller Remix).mp3|]
      infosTr4 = [relfile|Disc 02/02-Perfect  (Electronic Periodic Dark Drone Mix).mp3|]
      check name expect inf =
        assertListEqR name (mp3Names @InfoFPCError inf) expect
   in testGroup "mp3Names" $
                 ю [ check "info1" [info1Tr1,info1Tr2]                   info1
                   , check "infos" [infosTr1,infosTr2,infosTr3,infosTr4] infos
                   ]

------------------------------------------------------------

data RunMode = ModeWrite Natural
             | ModeTrackCount File
             | ModeFlacList File
             | ModeMp3List File
  deriving Show

trackCountP ∷ Parser Natural
trackCountP = let c = completer (listCompleter $ show ⊳ [ 1∷Natural .. 99])
               in argument auto (metavar "TRACK-COUNT" ⊕ c)

class PrintOut σ where
  toP ∷ Printable ρ ⇒ ρ → σ

instance PrintOut Text where
  toP = toText

instance PrintOut String where
  toP = toString

{- | Parse a printable value, give user-friendly error messages. -}
parseTextual ∷ ∀ β τ α .
      (Textual β, PrintOut τ, Printable α, Typeable β) ⇒
      α → Either τ β
parseTextual (toText → z) =
  let fromParsed (Parsed a)      = a
      -- this function exists solely to provide a hypothetical value to reflect
      -- on
      fromParsed (Malformed _ _) = error "this should never be evaluated"
      parsedZ                    = parseText z
      typ                        = typeOf $ fromParsed parsedZ
   in case parsedZ of
        Parsed a       → Right a
        Malformed [] x → Left ∘ toP $
                           [fmtT|failed to parse '%t' as '%w': %s|] z typ x
        Malformed xs x → let msg = [fmtT|failed to parse '%t' as '%w': [%L] %s|]
                                   z typ xs x
                          in Left (toP msg)

readS ∷ (Textual α, Typeable α) ⇒ ReadM α
readS = eitherReader parseTextual

argS ∷ (Textual α, Typeable α) ⇒ Mod ArgumentFields α → Parser α
argS = argument readS

modeP ∷ Parser RunMode
modeP =
  let writeC      ∷ Mod CommandFields RunMode
      writeC      = command "write"
                            (ModeWrite ⊳
                               info trackCountP
                                    (progDesc "write a blank info.yaml for CD"))

      trackCountDesc = "count the tracks in an info.yaml"

      trackCountC ∷ Mod CommandFields RunMode
      trackCountC = command "track-count"
                            (info (ModeTrackCount
                                   ⊳ argS (value (FileR [relfile|info.yaml|]) ⊕ metavar "YAMLFILE" ⊕ action "file"))
                                  (progDesc trackCountDesc))

      flacListC   ∷ Mod CommandFields RunMode
      flacListC   = command "flac-list"
                            (info (ModeFlacList
                                   ⊳ argS (value (FileR [relfile|info.yaml|]) ⊕ metavar "YAMLFILE" ⊕ action "file"))
                                  (progDesc "list flacs from info.yaml"))
      mp3ListC   ∷ Mod CommandFields RunMode
      mp3ListC   = command "mp3-list"
                            (info (ModeMp3List
                                   ⊳ argS (value (FileR [relfile|info.yaml|]) ⊕ metavar "YAMLFILE" ⊕ action "file"))
                                  (progDesc "list mp3s from info.yaml"))
   in subparser (writeC ⊕ trackCountC ⊕ flacListC ⊕ mp3ListC)

------------------------------------------------------------

data Options = Options { _runMode ∷ RunMode}
  deriving Show

runMode ∷ Lens' Options RunMode
runMode = lens _runMode (\ o r → o { _runMode = r })

--------------------

optParser :: (MonadIO μ) => Text     -- prog description
                         -> Parser α -- options parser
                         -> μ α
optParser t i =
  let infoMod = fullDesc ⊕ progDesc (toString t) ⊕ failureCode 2
      parserPrefs = prefs $ showHelpOnError ⊕ showHelpOnEmpty

   in liftIO ∘ customExecParser parserPrefs $ info (i ⊴ helper) infoMod

--------------------

parseOpts ∷ Parser Options
parseOpts = Options ⊳ modeP

----------------------------------------

-- move to MonadIO
say ∷ (MonadIO μ, Printable τ) ⇒ τ → μ ()
say = liftIO ∘ putStrLn ∘ toText

------------------------------------------------------------

------------------------------------------------------------

{- | Print some function of Info. -}
pInfo ∷ (MonadIO μ, AsYamlParseError ε, MonadError ε μ, Printable τ) ⇒
        (Info → [τ]) → File → μ ()
pInfo f fn = unYamlFile fn ≫ mapM_ say ∘ f

pInfo' ∷ (MonadIO μ,AsYamlParseError ε,MonadError ε μ,Foldable φ,Printable τ) ⇒
         (Info → μ (φ τ)) → File → μ ()

pInfo' f fn = do
  inf ← ѥ $ unYamlFile fn
  xs  ← inf ≫ f
  forM_ xs say
  return ()


main ∷ IO ()
main = doMain @YamlParseInfoFPCError @Word8 $ do
  opts ← optParser "read & write info.yaml" parseOpts

  case opts ⊣ runMode of
    ModeWrite      tc → say $ blankInfo tc
    ModeTrackCount fn → pInfo  ((:[]) ∘ show ∘ trackCount) fn
    ModeFlacList   fn → pInfo' flacNames fn
    ModeMp3List    fn → pInfo' mp3Names fn

  return 0

--------------------------------------------------------------------------------

track1 ∷ Track
track1 = Track Nothing (Just "track title") Nothing NotLive Nothing Nothing

trackL ∷ Track
trackL = Track Nothing (Just "live track") Nothing
               Live (Just "Hammersmith Odeon") (Just [dateImpreciseRange|1970-01-01|])

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
tests = testGroup "minfo" [ lNameTests, liveNameTests, fileNameTests
                          , flacNameTests, flacNamesTests, mp3NamesTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------

