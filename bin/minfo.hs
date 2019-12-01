{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

import Prelude  ( Float, Int, (-), error, fromIntegral )

-- aeson -------------------------------

import qualified  Data.Aeson.Types  as  AesonT

import Data.Aeson.Types  ( Value( Array, Bool, Null, Number, Object, String )
                         , (.:?), (.:)
                         , defaultOptions, fieldLabelModifier, genericParseJSON
                         , typeMismatch, withObject
                         )

-- base --------------------------------

import Control.Applicative     ( pure )
import Control.Exception       ( Exception )
import Control.Monad           ( forM_, join, mapM_, return, sequence )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first, second )
import Data.Bool               ( Bool( True, False ) )
import Data.Either             ( Either( Left, Right ), either )
import Data.Eq                 ( Eq( (==) ) )
import Data.Foldable           ( Foldable, maximum )
import Data.Function           ( ($), id )
import Data.Functor            ( fmap )
import Data.List               ( replicate, sortOn, zip )
import Data.List.NonEmpty      ( NonEmpty( (:|) ) )
import Data.Maybe              ( Maybe( Just, Nothing )
                               , catMaybes, maybe )
import Data.Ord                ( max )
import Data.String             ( String )
import Data.Tuple              ( fst )
import Data.Typeable           ( Typeable, typeOf )
import Data.Word               ( Word8 )
import GHC.Exts                ( IsString, fromList, toList )
import GHC.Generics            ( Generic )
import Numeric.Natural         ( Natural )
import System.Exit             ( ExitCode )
import System.IO               ( FilePath, IO )
import Text.Printf             ( printf )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import qualified  Data.ByteString  as  BS
import Data.ByteString  ( ByteString )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), Printable( print ), Textual
                     , fromText, parseText, toString, toText )

-- exited ------------------------------

import Exited  ( doMain )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- fpath -------------------------------

import FPath.AsFilePath       ( filepath )
import FPath.Error.FPathComponentError
                              ( AsFPathComponentError( _FPathComponentError )
                              , FPathComponentError )
import FPath.File             ( File( FileR ) )
import FPath.FileLike         ( (⊙) )
import FPath.PathComponent    ( PathComponent, parsePathC, pc )
import FPath.RelFile          ( RelFile, relfile )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )
import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- ListLike ----------------------------

import qualified Data.ListLike

-- monaderror-io -----------------------

import MonadError  ( ѥ, fromRight )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴), (⊵), (∤) )
import Data.MoreUnicode.Functor      ( (⊲), (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣), (⫥) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

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

-- scientific --------------------------

import Data.Scientific  ( Scientific, floatingOrInteger )

-- tasty -------------------------------

import Test.Tasty  ( TestName, TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEqR, runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import qualified  Data.Text  as  Text

import Data.Text     ( Text, dropEnd, init, intercalate, lines, pack, replace
                     , unlines )
import Data.Text.IO  ( putStrLn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

-- vector ------------------------------

import qualified  Data.Vector  as  Vector
import Data.Vector  ( (!?) )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ParseException, ToJSON( toJSON )
                  , (.=), decodeEither', decodeFileEither, encode, object )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  MInfo.T.TestData  as  TestData

import MInfo.Types          ( Artist )
import MInfo.Types.Dateish  ( Dateish, __dateish', __dateishy' )

--------------------------------------------------------------------------------

isCompoundValue ∷ Value → Bool
isCompoundValue (Object _) = True
isCompoundValue (Array  _) = True
isCompoundValue _          = False

pyaml_ ∷ Value → Text
pyaml_ Null      = "~"
pyaml_ (Bool _ ) = error "not implemented Bool"
pyaml_ (Number n ) = [fmt|%f|] n
pyaml_ (String t) = yamlText t
pyaml_ (Array (toList → [])) = "[]"
pyaml_ (Array (toList → xs)) =
  intercalate "\n" $ ю [ ("- " ⊕ t) : (("  " ⊕) ⊳ ts) | x ← toList xs, let (t:ts) = lines (pyaml_ x) ]

pyaml_ (Object m) =
    let maxLen ∷ ℕ
        maxLen = fromIntegral (maximum $ Text.length ⊳ HashMap.keys m)
        pad ∷ Text → Text
        pad t = t ⊕ spaces (fromIntegral (maxLen - tlength t) `max` 0)
     in case length m of
          0 → "{}"
          _ → intercalate "\n" $
                ю [ if isCompoundValue v
                    then ( [fmt|%t :|] (pad k) : indent 2 (t:ts) )
                    else [ [fmt|%t : %t|] (pad k) t ]
                  | (k,v) ← sortOn fst (HashMap.toList m)
                  , let (t:ts) = lines (pyaml_ v)
                  ]

pyaml ∷ ToJSON α ⇒ α → Text
pyaml = pyaml_ ∘ toJSON

array ∷ [Value] → Value
array = Array ∘ fromList

arrayN ∷ [Scientific] → Value
arrayN = array ∘ fmap Number

objectTs ∷ [(Text,Text)] → Value
objectTs = object ∘ fmap (second String)

pyamlTests ∷ TestTree
pyamlTests =
  let foo  = "foo" ∷ Text
      _bob = "'bob" ∷ Text
      bar  = "bar" ∷ Text
      x    = "x" ∷ Text
      y    = "y" ∷ Text
      quux = "quux" ∷ Text
      tlist = [] ∷ [Text]

      decodeText ∷ Text → Either String Value
      decodeText = first show ∘ decodeEither' ∘ convStringLike

      check ∷ TestName → Text → Value → TestTree
      check name expect val =
        testGroup name
                  [ testCase "expect" $ expect ≟ pyaml val
                  , testCase "parse"  $ Right val ≟ decodeText (pyaml val)]


{-
      check' ∷ TestName → Text → Value → TestTree
      check' name expect value =
        testGroup name
                  [ testCase "expect" $ expect ≟ pyaml value
                  , testCase "parse"  $ Right value ≟ decodeTexts (pyaml value)]
-}

   in testGroup "pyaml"
                [ check "foo"  foo       (String foo)
                  -- I would like to fix this, but not today
                , check "y"     "'y'"     (String y)
                , check "bo'b"  "bo'b"    (String "bo'b")
                , check "'bob"  "'''bob'" (String _bob)
                , check "\"bob" "'\"bob'" (String "\"bob")

                , check "7" "7" (Number 7)

                , check "list0" "[]"            (array (String ⊳ tlist))
                , check "list1" "- 1"           (arrayN [ 1 ])
                , check "list2" "- 1\n- 1"      (arrayN [ 1, 1 ])
                , check "list3" "- 1\n- 1\n- 2" (arrayN [ 1, 1, 2 ])

                , check "map0" "{}"             (object ([]∷[(Text,Value)]))
                , check "map1" "foo : bar"      (object [(foo,String bar)])
                , check "map1'" "foo : '''bob'" (object [(foo,String _bob)])
                , check "map2" "foo  : bar\nquux : 'y'\nx    : 'y'"
                               (objectTs [(foo,bar),(x,y),(quux,y)])

                , check "list of lists"
                        (intercalate "\n" [ "- - 1"
                                          , "  - 1"
                                          , "  - 2"
                                          , "- - 3"
                                          , "  - 5"
                                          , "- - 8"
                                          ])
                        (array [ arrayN [1,1,2], arrayN [3,5], arrayN [8] ])


                , check "map of maps"
                        (intercalate "\n" [ "one :"
                                          , "  foo  : bar"
                                          , "  quux : 'y'"
                                          , "  x    : 'y'"
                                          , "two :"
                                          , "  foo : bar"
                                          , "  x   : 'y'"
                                          ])
                        (object [ ("one", objectTs [(foo,bar), (x,y), (quux,y)])
                                , ("two", objectTs [(foo,bar), (x,y)]) ]
                        )

                , check "list of maps"
                        (intercalate "\n" [ "- foo  : bar"
                                          , "  quux : 'y'"
                                          , "  x    : 'y'"
                                          , "- foo : bar"
                                          , "  x   : 'y'"
                                          ])
                        (array [ objectTs [(foo,bar),(x,y),(quux,y)]
                               , objectTs [(foo,bar),(x,y)] ])

                , check "map of lists"
                        (intercalate "\n" [ "foo  :"
                                          , "  - 1"
                                          , "  - 1"
                                          , "  - 2"
                                          , "quux :"
                                          , "  - 8"
                                          , "x    :"
                                          , "  - 3"
                                          , "  - 5"
                                          ])
                        (object [ (foo, arrayN [1,1,2])
                                , (x, arrayN [3,5]), (quux, arrayN[8]) ])
                ]

convStringLike ∷ (Data.ListLike.StringLike α,Data.ListLike.StringLike β) ⇒ α → β
convStringLike = Data.ListLike.fromString ∘ Data.ListLike.toString

yamlText ∷ Text → Text
yamlText = let safeInit "" = ""
               safeInit t  = init t
            in safeInit ∘ convStringLike ∘ encode


spaces ∷ ℕ → Text
spaces n = Text.replicate (fromIntegral n) " "

indent ∷ ℕ → [Text] → [Text]
indent n = fmap (spaces n ⊕)

tlength ∷ Text → ℕ
tlength = fromIntegral ∘ Text.length

------------------------------------------------------------

data Track = Track { __artist        ∷ Maybe Artist
                   , __title         ∷ Maybe Text
                   , __version       ∷ Maybe Text
                   , __live_type     ∷ Maybe Text
                   , __live_location ∷ Maybe Text
                   , __live_date     ∷ Maybe Text
                   }
  deriving (Eq, Generic, Show)

trackTitle ∷ Lens' Track (Maybe Text)
trackTitle = lens __title (\ r t → r { __title = t })

trackVersion ∷ Lens' Track (Maybe Text)
trackVersion = lens __version (\ r v → r { __version = v })

trackLiveType ∷ Lens' Track (Maybe Text)
trackLiveType = lens __live_type (\ r y → r { __live_type = y })

trackLiveLocation ∷ Lens' Track (Maybe Text)
trackLiveLocation = lens __live_location (\ r l → r { __live_location = l })

trackLiveDate ∷ Lens' Track (Maybe Text)
trackLiveDate = lens __live_date (\ r d → r { __live_date = d })

instance FromJSON Track where
  parseJSON = let drop_ (_ : _ : s) = s
                  drop_ s           = s
               in genericParseJSON defaultOptions { fieldLabelModifier = drop_ }

trackFromJSONTests ∷ TestTree
trackFromJSONTests =
  let t1 ∷ ByteString
      t1 = BS.intercalate "\n" [ "title: Judas"
                               , "live_type: Live"
                               , "live_date: 1993-07-29"
                               ]
      e1 ∷ Track
      e1 = Track Nothing (Just "Judas") Nothing (Just "Live") Nothing
                 (Just "1993-07-29")
   in testGroup "trackFromJSON"
                [ testCase "t1" $ Right e1 ≟ unYaml @ParseError t1
                ]

instance ToJSON Track where
  toJSON (Track a t v y l d) =
    let fields = ю [ maybe [] (\ a' → [ "artist" .= toJSON a' ]) a
                   , [ "title" .= t ]
                   , maybe [] (\ v' → [ "version" .= toJSON v' ]) v
                   , maybe [] (\ y' → [ "live_type" .= toJSON y' ]) y
                   , maybe [] (\ l' → [ "live_location" .= toJSON l' ]) l
                   , maybe [] (\ d' → [ "live_date" .= toJSON d' ]) d
                   ]
     in object fields

instance Printable Track where
  print (Track a t v y l d) = let toj ∷ Show α ⇒ Maybe α → Text
                                  toj Nothing  = "~"
                                  toj (Just x) = toText (show x)
                                  tot ∷ Show α ⇒ Text → Maybe α → Text
                                  tot i x = i ⊕ ": " ⊕ toj x
                                  tom ∷ Show α ⇒ Text → Maybe α → [Text]
                                  tom _ Nothing  = []
                                  tom i (Just x) = [ tot i (Just x) ]
                                  unl ∷ [Text] → Text
                                  unl = dropEnd 1 ∘ unlines
                               in P.text ∘ unl $ (tom "artist" (toText ⊳ a))
                                               ⊕ [tot "title" t]
                                               ⊕ (tom "version" v)
                                               ⊕ (tom "live_type" y)
                                               ⊕ (tom "live_location" l)
                                               ⊕ (tom "live_date" d)

trackPrintableTests ∷ TestTree
trackPrintableTests =
  let e1 = intercalate "\n" [ "artist: \"Depeche Mode\""
                            , "title: \"Can't Get Enough\""
                            , "live_type: \"Live\""
                            , "live_location: \"Hammersmith Odeon\""
                            ]
      t1 = Track (Just "Depeche Mode") (Just "Can't Get Enough") Nothing
                 (Just "Live") (Just "Hammersmith Odeon") Nothing
   in testGroup "Printable" [ testCase "t1" $ e1 ≟ toText t1
                            ]

blankTrack ∷ Track
blankTrack = Track Nothing Nothing Nothing Nothing Nothing Nothing

trackTests ∷ TestTree
trackTests = testGroup "Track" [ trackPrintableTests, trackFromJSONTests ]

------------------------------------------------------------

-- this looks like a monadic fold, or somesuch.  Maybe of MaybeT?
maybeList ∷ [Maybe α] → Maybe α
maybeList [] = Nothing
maybeList (Just a : _)   = Just a
maybeList (Nothing : as) = maybeList as

lName ∷ Maybe Text → Maybe Text → Maybe Text → Maybe Text
lName Nothing _ _ = Nothing
lName (Just lType) lLocY lDateY =
  Just $ intercalate " " (lType : catMaybes [lLocY, lDateY])

lNameTests ∷ TestTree
lNameTests =
  testGroup "lName"
            [ testCase "nothing" $ Nothing ≟ lName Nothing Nothing Nothing
            , testCase "live" $
                  Just "Live Hammersmith Odeon 1970-01-01"
                ≟ lName (Just "Live") (Just "Hammersmith Odeon")
                        (Just "1970-01-01")
            ]

liveName ∷ ReleaseInfo → Track → Maybe Text
liveName r t = lName (t ⊣ trackLiveType ∤ r ⊣ live_type)
                      (t ⊣ trackLiveLocation ∤ r ⊣ live_location)
                      (t ⊣ trackLiveDate ∤ r ⊣ live_date)

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
      go t = case t ⊣ trackTitle of
               Nothing → pack $ printf "%02d" num
               Just ti → let vv = parens   ⊳ t ⊣ trackVersion
                             ll = brackets ⊳ liveName relnfo t
                          in [fmt|%02d-%t|]
                             num (intercalate "  " $ catMaybes [Just ti,vv,ll])
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
                    ≟ fileName @ParseInfoFPCError releaseInfol 11 trackL'
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

newtype Tracks = Tracks { unTracks ∷ [[Track]] }
  deriving (Eq,Show)

tracks_ ∷ Tracks → [Track]
tracks_ (Tracks tss) = ю tss

instance Printable Tracks where
  print tss = P.text ∘ unlines $ toText ⊳ tracks_ tss

instance FromJSON Tracks where
  parseJSON (Array ts) =
    case ts !? 0 of
      Nothing        → return $ Tracks [[]]
      Just (Array _) → Tracks ⊳ (sequence $ parseJSON ⊳ toList ts)
      Just _         → let -- xs' = parseJSON ⊳ x
                           ts'    ∷ [AesonT.Parser Track]   = parseJSON ⊳ toList ts
                           ts'''  ∷ AesonT.Parser [[Track]] = pure ⊳ sequence ts'
                           ts'''' = Tracks ⊳ ts'''
                        in ts'''' -- (Tracks ∘ pure) ⊳ sequence (parseJSON ⊳ ts)
  parseJSON invalid = typeMismatch "Array" invalid


tracksFromJSONTests ∷ TestTree
tracksFromJSONTests =
  let t1 ∷ ByteString
      t1 = BS.intercalate "\n" [ "- title: Judas"
                               , "  live_type: Live"
                               , "  live_date: 1993-07-29"
                               , "- title: Mercy in You"
                               , "  live_type: Live"
                               , "  live_date: 1993-07-29"
                               ]
      e1 ∷ Track
      e1 = Track Nothing (Just "Judas") Nothing (Just "Live") Nothing
                 (Just "1993-07-29")
      e2 ∷ Track
      e2 = Track Nothing (Just "Mercy in You") Nothing (Just "Live") Nothing
                 (Just "1993-07-29")
      t3 ∷ ByteString
      t3 = BS.intercalate "\n" [ "-"
                               , "  - title: Judas"
                               , "    live_type: Live"
                               , "    live_date: 1993-07-29"
                               , "  - title: Mercy in You"
                               , "    live_type: Live"
                               , "    live_date: 1993-07-29"
                               , "-"
                               , "  - title: I Feel You"
                               , "    live_type: Live"
                               , "    live_date: 1993-07-29"
                               ]
      e3 ∷ Track
      e3 = Track Nothing (Just "I Feel You") Nothing (Just "Live") Nothing
                 (Just "1993-07-29")
   in testGroup "tracksFromJSON"
                [ testCase "t1"  $ Right [e1,e2] ≟ unYaml @ParseError t1
                , testCase "t1'" $
                    Right (Tracks [[e1,e2]]) ≟ unYaml @ParseError t1
                , testCase "t3" $
                    Right (Tracks [[e1,e2],[e3]]) ≟ unYaml @ParseError t3
                ]

{-
    case ts !? 0 of
      Just (Object _) → Tracks  ⊳ (sequence $ parseJSON ⊳ toList ts)
      Just (Array  _) → Trackss ⊳ (sequence $ withArray "Tracks" (\ v → sequence $ parseJSON ⊳ toList v) ⊳ (toList ts))
--      Just (Array _)  → Trackss ⊳ (sequence $ sequence ⊳ (fmap parseJSON ⊳ toList ⊳ toList ts))
-}

instance ToJSON Tracks where
  toJSON = Array ∘ Vector.fromList ∘ fmap toJSON ∘ tracks_

tracksTests ∷ TestTree
tracksTests = testGroup "Tracks" [ tracksFromJSONTests ]

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

data ReleaseInfo = ReleaseInfo { _artist           ∷ Artist
                               , _catno            ∷ Maybe Catno
                               , _release          ∷ Maybe Dateish
                               , _original_release ∷ Maybe Dateish
                               , _source           ∷ Maybe Text
                               , _source_version   ∷ Maybe Text
                               , _live_type        ∷ Maybe Text
                               , _live_location    ∷ Maybe Text
                               , _live_date        ∷ Maybe Text
                               }
  deriving (Eq,Show)


live_type ∷ Lens' ReleaseInfo (Maybe Text)
live_type = lens _live_type (\ i y → i { _live_type = y})

live_location ∷ Lens' ReleaseInfo (Maybe Text)
live_location = lens _live_location (\ i l → i { _live_location = l})

live_date ∷ Lens' ReleaseInfo (Maybe Text)
live_date = lens _live_date (\ i d → i { _live_date = d})

instance ToJSON ReleaseInfo where
  toJSON = object ∘ releaseInfoFields

releaseInfoFields ∷ ReleaseInfo → [(Text,Value)]
releaseInfoFields (ReleaseInfo a c r o s v t l d) =
  ю [ [ "artist" .= a ]
    , [ "catno"  .= c ]
    , maybe [] (\ r' → [ "release"          .= toJSON r' ]) r
    , maybe [] (\ o' → [ "original_release" .= toJSON o' ]) o
    , [ "source" .= s ]
    , maybe [] (\ v' → [ "source_version"   .= toJSON v' ]) v

    , maybe [] (\ t' → [ "live_type"     .= toJSON t' ]) t
    , maybe [] (\ l' → [ "live_location" .= toJSON l' ]) l
    , maybe [] (\ d' → [ "live_date"     .= toJSON d' ]) d
    ]


blankReleaseInfo ∷ ReleaseInfo
blankReleaseInfo = ReleaseInfo "" Nothing Nothing Nothing
                               Nothing Nothing Nothing Nothing Nothing

------------------------------------------------------------

data Info = Info { _releaseInfo ∷ ReleaseInfo
                 , _tracks      ∷ Tracks
                 }
  deriving (Generic, Eq, Show)

releaseInfo ∷ Lens' Info ReleaseInfo
releaseInfo = lens _releaseInfo (\ i r → i { _releaseInfo = r })

tracks ∷ Info → [Track]
tracks i = tracks_ (_tracks i)

instance FromJSON Info where
  parseJSON = withObject "Info" $ \ v → Info
    ⊳ (ReleaseInfo ⊳ v .: "artist"
                   ⊵ v .:? "catno"
                   ⊵ v .:? "release"
                   ⊵ v .:? "original_release"
                   ⊵ v .:? "source"
                   ⊵ v .:? "source_version"
                   ⊵ v .:? "live_type"
                   ⊵ v .:? "live_location"
                   ⊵ v .:? "live_date"
      )
   ⊵ v .: "tracks"

info1 ∷ Info
info1 = Info (ReleaseInfo ("Depeche Mode") Nothing Nothing Nothing
                          (Just "World We Live in and Live in Hamburg,The")
                          Nothing
                          (Just "Live")
                          (Just "Alsterdorfer Sporthalle, Hamburg")
                          (Just "1984-12-14")
             )
             (Tracks [ [ Track Nothing (Just "Something to Do") Nothing
                               Nothing Nothing Nothing
                       , Track Nothing (Just "Two Minute Warning") Nothing
                               Nothing Nothing Nothing
                       ]
                     ])

--------------------

releaseInfo2 ∷ ReleaseInfo
releaseInfo2 = ReleaseInfo ("Depeche Mode") (Just "DMDVD4") Nothing
                           Nothing (Just "Devotional")
                           Nothing Nothing Nothing Nothing
tracks2 ∷ Tracks
tracks2 = let mkTrack t = Track Nothing (Just t) Nothing
                          (Just "Live")
                          (Just "Stade Couvert Régional, Liévin, France")
                          (Just "1993-07-29")
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
                           (Just (__dateishy' 1993)) Nothing
                           (Just "Radio 1 in Concert") Nothing
                           (Just "Live") (Just "Crystal Palace")
                           (Just "1993-07-31")
tracks3 ∷ Tracks
tracks3 = let mkTrack t = Track Nothing (Just t) Nothing Nothing Nothing Nothing
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
              (Just (__dateish' 2009 04 17))
              Nothing
              (Just "Sounds of the Universe  (Deluxe Box Set)")
                Nothing
              Nothing Nothing Nothing
tracks4 ∷ Tracks
tracks4 = let mkTrack t = Track Nothing (Just t) Nothing Nothing Nothing Nothing
              mkTrack' (t,v) = Track Nothing (Just t) (Just v)
                                     Nothing Nothing Nothing
              mkTrackD t = Track Nothing (Just t) Nothing
                                 (Just "Demo") Nothing Nothing
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
                                        "Electronic Periodic Dark Drone Mix")
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
              (Just (__dateish' 2009 04 17))
              Nothing
              (Just "Sounds of the Universe  (Deluxe Box Set)") Nothing
              Nothing Nothing Nothing

tracks5 ∷ Tracks
tracks5 = let mkTrack t = Track Nothing (Just t) Nothing Nothing Nothing Nothing
              mkTrack' (t,v) = Track Nothing (Just t) (Just v)
                                     Nothing Nothing Nothing
              mkTrackD t = Track Nothing (Just t) (Just "Demo")
                                 Nothing Nothing Nothing
              mkTrackS t = Track Nothing (Just t) Nothing
                                 (Just "Session") Nothing (Just "2008-12-08")
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
                                        "Electronic Periodic Dark Drone Mix")
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
                          (Just (__dateish' 2009 04 17))
                          Nothing (Just "Sounds of the Universe")
                          (Just "Deluxe Box Set") Nothing Nothing Nothing)
             (Tracks [ [ Track Nothing (Just "In Chains") Nothing
                               Nothing Nothing Nothing
                       , Track Nothing (Just "Hole to Feed") Nothing
                               Nothing Nothing Nothing
                       ]
                     , [ Track Nothing
                               (Just "Wrong") (Just "Trentemøller Remix")
                               Nothing Nothing Nothing
                       , Track Nothing
                               (Just "Perfect")
                               (Just "Electronic Periodic Dark Drone Mix")
                               Nothing Nothing Nothing
                       ]
                     ])

--------------------

instance Printable [Track] where
  print ts = P.text $ intercalate "\n" (toText ⊳ ts)

infoFromJSONTests ∷ TestTree
infoFromJSONTests =
  let splitInfo ∷ Info → (ReleaseInfo,Tracks)
      splitInfo (Info ri tr) = (ri,tr)
      splitEPair ∷ Either ε (α,β) → (Either ε α, Either ε β)
      splitEPair (Left l) = (Left l,Left l)
      splitEPair (Right (a,b)) = (Right a, Right b)
      checkInfo name inf expected =
        let (rinfo,trcks) = splitEPair (splitInfo ⊳ unYaml @ParseError inf)
            Info erinfo etrcks = expected
            nme t = name ⊕ ": " ⊕ t
         in ю [ [ testCase      (nme "release info") $ rinfo ≟ Right erinfo ]
                , assertListEqR (nme "tracks")
                                (tracks_ ⊳ trcks) (tracks_ etrcks)
                , assertListEqR (nme "flat tracks")
                                (unTracks ⊳trcks) (unTracks etrcks)
                , [ testCase (nme "info") $
                      Right info2 ≟ unYaml @ParseError TestData.info2T
                  ]
                ]

   in testGroup "infoFromJSON"
                (ю [ [ testCase "info1'" $
                         Right info1 ≟ unYaml @ParseError TestData.info1T
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

blankInfo ∷ Natural → Info
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
                Right  2 ≟ trackCount ⊳ (unYaml @ParseError TestData.info1T)
            , testCase "info2" $
                Right 19 ≟ trackCount ⊳ (unYaml @ParseError TestData.info2T)
            , testCase "info3" $
                Right 12 ≟ trackCount ⊳ (unYaml @ParseError TestData.info3T)
            , testCase "info4" $
                Right 39 ≟ trackCount ⊳ (unYaml @ParseError TestData.info4T)
            , testCase "info5" $
                Right 65 ≟ trackCount ⊳ (unYaml @ParseError TestData.info5T)
            , testCase "infos" $
                Right  4 ≟ trackCount ⊳ (unYaml @ParseError TestData.infosT)
            ]

infoTests ∷ TestTree
infoTests = testGroup "Info" [ infoPrintableTests, infoFromJSONTests
                             , trackCountTests ]


------------------------------------------------------------

{-
class HasInfoYaml α where
  infoYaml ∷ Lens' α FilePath
-}

data RunMode = ModeWrite Natural
             | ModeTrackCount File
             | ModeFlacList File
             | ModeMp3List File
  deriving Show

{-
instance HasInfoYaml RunMode where
  infoYaml = lens get set
             where get (ModeWrite _    f) = f
                   get (ModeTrackCount f) = f
                   get (ModeFlacList   f) = f
                   get (ModeMp3List    f) = f
                   set (ModeWrite n    _) f = ModeWrite n f
                   set (ModeTrackCount _) f = ModeTrackCount f
                   set (ModeFlacList   _) f = ModeFlacList   f
                   set (ModeMp3List    _) f = ModeMp3List    f
-}

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

{-
instance HasInfoYaml Options where
  infoYaml = lens get set
             where get (Options rm) = rm ⊣ infoYaml
                   set (Options rm) f = runMode ∘ infoYaml
-}

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

data InfoError = IllegalFileName Text
  deriving (Eq,Show)

instance Exception InfoError

instance Printable InfoError where
  print (IllegalFileName t) = P.text $ [fmt|Illegal file name: '%t'|] t

class AsInfoError ε where
  _InfoError :: Prism' ε InfoError

instance AsInfoError InfoError where
  _InfoError = id

throwIllegalFileName :: (AsInfoError ε, MonadError ε η) ⇒ Text → η α
throwIllegalFileName t = throwError $ (_InfoError #) (IllegalFileName t)

------------------------------------------------------------

newtype ParseError = ParseError ParseException
  deriving Show

instance Exception ParseError

instance Eq ParseError where
  a == b = show a ≡ show b

instance Printable ParseError where
  print = P.string ∘ show

class AsParseError ε where
  _ParseError ∷ Prism' ε ParseError

instance AsParseError ParseError where
  _ParseError = id

asParseError ∷ AsParseError ε ⇒ Either ParseException α → Either ε α
asParseError = first ((_ParseError #) ∘ ParseError)

------------------------------------------------------------

data InfoFPCError = IFPCInfoError             InfoError
                  | IFPCFPathComponenentError FPathComponentError
  deriving (Eq,Show)

instance Exception InfoFPCError

instance Printable InfoFPCError where
  print (IFPCInfoError e)             = print e
  print (IFPCFPathComponenentError e) = print e

instance AsInfoError InfoFPCError where
  _InfoError = prism IFPCInfoError
                     (\ case IFPCInfoError e → Right e; e → Left e)

instance AsFPathComponentError InfoFPCError where
  _FPathComponentError = prism IFPCFPathComponenentError
                               (\ case IFPCFPathComponenentError e → Right e
                                       e                           → Left  e)

------------------------------------------------------------

data ParseInfoFPCError = PIFPCParseError   ParseError
                       | PIFPCInfoFPCError InfoFPCError
  deriving (Eq,Show)

_PIFPCInfoFPCError ∷ Prism' ParseInfoFPCError InfoFPCError
_PIFPCInfoFPCError = prism PIFPCInfoFPCError
                           (\ case PIFPCInfoFPCError e → Right e; e → Left e)

instance Exception ParseInfoFPCError

instance Printable ParseInfoFPCError where
  print (PIFPCParseError   e) = print e
  print (PIFPCInfoFPCError e) = print e

instance AsParseError ParseInfoFPCError where
  _ParseError = prism PIFPCParseError
                      (\ case PIFPCParseError  e -> Right e; e -> Left e)

instance AsInfoError ParseInfoFPCError where
  _InfoError = _PIFPCInfoFPCError ∘ _InfoError

instance AsFPathComponentError ParseInfoFPCError where
  _FPathComponentError = _PIFPCInfoFPCError ∘ _FPathComponentError
  
------------------------------------------------------------

unYaml ∷ ∀ ε α μ . (FromJSON α, MonadError ε μ, AsParseError ε) ⇒
         ByteString → μ α
unYaml = fromRight ∘ asParseError ∘ decodeEither'

{- | Decode a yaml file; IO errors (e.g., file not found) are thrown as
     ParseErrors (this is the doing of `Data.Yaml.decodeFileEither`, not me). -}
unYamlFile ∷ (MonadIO μ, MonadError ε μ, AsParseError ε) ⇒ File → μ Info
unYamlFile = let go ∷ (MonadIO μ, AsParseError ε) ⇒ FilePath → μ (Either ε Info)
                 go = liftIO  ∘ fmap asParseError ∘ decodeFileEither
              in join ∘ (fromRight ⩺ go ∘ (⫥ filepath))

{- | Print some function of Info. -}
pInfo ∷ (MonadIO μ, AsParseError ε, MonadError ε μ, Printable τ) ⇒
        (Info → [τ]) → File → μ ()
pInfo f fn = unYamlFile fn ≫ mapM_ say ∘ f

pInfo' ∷ (MonadIO μ,AsParseError ε,MonadError ε μ,Foldable φ,Printable τ) ⇒
         (Info → μ (φ τ)) → File → μ ()

pInfo' f fn = do
  inf ← ѥ $ unYamlFile fn
  xs ← inf ≫ f
  forM_ xs say
  return ()


main ∷ IO ()
main = doMain @ParseInfoFPCError @Word8 $ do
  opts ← optParser "read & write info.yaml" parseOpts

  case opts ⊣ runMode of
    ModeWrite      tc → say $ blankInfo tc
    ModeTrackCount fn → pInfo  ((:[]) ∘ show ∘ trackCount) fn
    ModeFlacList   fn → pInfo' flacNames fn
    ModeMp3List    fn → pInfo' mp3Names fn

  return 0

--------------------------------------------------------------------------------

track1 ∷ Track
track1 = Track Nothing (Just "track title") Nothing Nothing Nothing Nothing

trackL ∷ Track
trackL = Track Nothing (Just "live track") Nothing
               (Just "Live") (Just "Hammersmith Odeon") (Just "1970-01-01")

trackL' ∷ Track
trackL' = Track Nothing (Just "Live Track") Nothing
                Nothing Nothing (Just "1990-02-02")

trackS ∷ Track
trackS = Track Nothing (Just "Sesh") (Just "Acoustic")
               (Just "Session") Nothing (Just "1980-01-01")

releaseInfo1 ∷ ReleaseInfo
releaseInfo1 = ReleaseInfo ("artie") (Just "123X")
                           (Just (__dateish' 1979 12 31))
                           Nothing (Just "Elpee") Nothing Nothing Nothing
                           Nothing

releaseInfol ∷ ReleaseInfo
releaseInfol = ReleaseInfo ("simon") (Just "124XX")
                           (Just (__dateish' 1979 12 31))
                           Nothing
                           (Just "An LP Title") Nothing
                           (Just "Live") (Just "Sweden") (Just "1990")

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "minfo" [ pyamlTests, trackTests, tracksTests, lNameTests
                          , infoTests, liveNameTests, fileNameTests
                          , flacNameTests, flacNamesTests, mp3NamesTests
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

