{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns       #-}

import Prelude  ( Int, (-), error, fromIntegral, undefined )

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Array, Object )
                         , (.:?), (.:)
                         , defaultOptions, fieldLabelModifier, genericParseJSON
                         , typeMismatch, withObject
                         )

-- base --------------------------------

import Control.Applicative     ( pure )
import Control.Exception       ( Exception )
import Control.Monad           ( forM_, join, mapM_, return, sequence )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first )
import Data.Either             ( Either( Left, Right ) )
import Data.Eq                 ( Eq( (==) ) )
import Data.Foldable           ( Foldable, maximum )
import Data.Function           ( ($), id )
import Data.Functor            ( fmap )
import Data.List               ( length, replicate, sort, zip )
import Data.Maybe              ( Maybe( Just, Nothing )
                               , catMaybes, fromMaybe, maybe )
import Data.Monoid             ( mconcat )
import Data.String             ( String )
import Data.Typeable           ( Typeable, typeOf )
import Data.Word               ( Word8 )
import GHC.Exts                ( toList )
import GHC.Generics            ( Generic )
import Numeric.Natural         ( Natural )
import System.Exit             ( ExitCode( ExitFailure ) )
import System.IO               ( FilePath, IO, stderr )
import Text.Printf             ( printf )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import Data.ByteString  ( ByteString )

-- containers --------------------------

import Data.Map  ( Map, foldrWithKey, fromList, keys )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), Printable( print ), Textual
                     , fromText, parseText, toString, toText )

-- exited ------------------------------

import Exited  ( doMain, exitWith )

-- fpath -------------------------------

import FPath.AsFilePath     ( filepath )
import FPath.File           ( File( FileR ) )
import FPath.FileLike       ( (⊙) )
import FPath.PathComponent  ( pc )
import FPath.RelFile        ( RelFile, relfile )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )
import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- ListLike ----------------------------

import qualified Data.ListLike

-- monaderror-io -----------------------

import MonadError           ( ѥ, fromRight )
import MonadError.IO.Error  ( AsIOError, IOError, userE )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴), (⊵), (∤) )
import Data.MoreUnicode.Functor      ( (⊲), (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣), (⫥) )
import Data.MoreUnicode.Monad        ( (⪼), (≫) )
import Data.MoreUnicode.Natural      ( ℕ )
import Data.MoreUnicode.Tasty        ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- optparse-applicative ----------------

import Options.Applicative  ( ArgumentFields, CommandFields, Mod, Parser, ReadM
                            , action, argument, auto, command, completer
                            , customExecParser, eitherReader, failureCode
                            , fullDesc, helper, info, listCompleter, metavar
                            , prefs, progDesc, showHelpOnEmpty, showHelpOnError
                            , strArgument, subparser, value
                            )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEqR, assertListEq, propInvertibleText, runTestsP
                  , runTestsReplay, runTestTree )

-- text --------------------------------

import qualified  Data.Text  as  Text

import Data.Text     ( Text, dropEnd, init, intercalate, lines, pack, replace
                     , unpack, unlines )
import Data.Text.IO  ( hPutStrLn, putStrLn )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ParseException, ToJSON( toJSON )
                  , (.=), decodeFileEither, encode, object )

--------------------------------------------------------------------------------

{- | Translate a value into yaml lines; somewhat nicer output to my eyes than
     Data.Yaml gives.
 -}
class PYaml α where
  pYaml ∷ α → [Text]

yamlText ∷ Text → Text
yamlText t = let safeInit "" = ""
                 safeInit t  = init t
                 bsToText ∷ ByteString → Text
                 bsToText = Data.ListLike.fromString ∘ Data.ListLike.toString
              in safeInit ∘ bsToText $ encode t

instance PYaml Text where
  pYaml t = [ yamlText t ]

instance PYaml (Map Text Text) where
  pYaml m =
    let maxLen ∷ Int
        maxLen = maximum $ Text.length ⊳ keys m
        pad ∷ Text → Text
        pad t = t ⊕ Text.replicate (maxLen - Text.length t) " "
     in foldrWithKey (\ k v ts → [fmt|%t : %t|] (pad k) (yamlText v) : ts) [] m

pYamlTests ∷ TestTree
pYamlTests =
  let foo  = "foo" ∷ Text
      _bob = "'bob" ∷ Text
      bar  = "bar" ∷ Text
      x    = "x" ∷ Text
      y    = "y" ∷ Text
      quux = "quux" ∷ Text
   in testGroup "pYaml"
                [ testCase "foo"   $ [ foo ]       ≟ pYaml foo
                  -- I would like to fix this, but not today
                , testCase "y"     $ [ "'y'" ]     ≟ pYaml y
                , testCase "bo'b"  $ [ "bo'b" ]    ≟ pYaml @Text "bo'b"
                , testCase "'bob"  $ [ "'''bob'" ] ≟ pYaml _bob
                , testCase "\"bob" $ [ "'\"bob'" ] ≟ pYaml @Text "\"bob"

                , testCase "map0" $ [ ] ≟ pYaml (fromList ([] ∷ [(Text,Text)]))
                , testCase "map1" $
                    [ "foo : bar" ] ≟ pYaml (fromList [(foo,bar)])
                , testCase "map1'" $
                    [ "foo : '''bob'" ] ≟ pYaml (fromList [(foo,_bob)])
                , testCase "map2" $
                      [ "foo  : bar", "quux : 'y'", "x    : 'y'" ]
                    ≟ sort (pYaml (fromList[(foo,bar),(x,y),(quux,y)]))
                ]

------------------------------------------------------------

data Track = Track { __title         ∷ Maybe Text
                   , __version       ∷ Maybe Text
                   , __live_type     ∷ Maybe Text
                   , __live_location ∷ Maybe Text
                   , __live_date     ∷ Maybe Text
                   }
  deriving (Generic, Show)

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

instance ToJSON Track where
  toJSON (Track t v y l d) =
    let fields = mconcat [ [ "title" .= t ]
                         , maybe [] (\ v' → [ "version" .= toJSON v' ]) v
                         , maybe [] (\ y' → [ "live_type" .= toJSON y' ]) y
                         , maybe [] (\ l' → [ "live_location" .= toJSON l' ]) l
                         , maybe [] (\ d' → [ "live_date" .= toJSON d' ]) d
                         ]
     in object fields

instance Printable Track where
  print (Track t v y l d) = let toj Nothing  = "~"
                                toj (Just x) = toText (show x)
                                tot i x = i ⊕ ": " ⊕ toj x
                                tom _ Nothing  = []
                                tom i (Just x) = [ tot i (Just x) ]
                                unl ∷ [Text] → Text
                                unl = dropEnd 1 ∘ unlines
                             in P.text ∘ unl $ [tot "title" t]
                                             ⊕ (tom "version" v)
                                             ⊕ (tom "live_type" y)
                                             ⊕ (tom "live_location" l)
                                             ⊕ (tom "live_date" d)

blankTrack ∷ Track
blankTrack = Track Nothing Nothing Nothing Nothing Nothing


-- this looks like a monadic fold, or somesuch
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

liveName ∷ Maybe Text → Maybe Text → Maybe Text → Track → Maybe Text
liveName lTypeY lLocY lDateY t =
  lName (t ⊣ trackLiveType ∤ lTypeY) (t ⊣ trackLiveLocation ∤ lLocY)
        (t ⊣ trackLiveDate ∤ lDateY)

liveName' ∷ ReleaseInfo → Track → Maybe Text
liveName' r t = lName (t ⊣ trackLiveType ∤ r ⊣ live_type)
                      (t ⊣ trackLiveLocation ∤ r ⊣ live_location)
                      (t ⊣ trackLiveDate ∤ r ⊣ live_date)

liveNameTests ∷ TestTree
liveNameTests = testGroup "liveName"
                          [ testCase "track1" $
                              Nothing ≟ liveName' releaseInfo1 track1
                          , testCase "trackL" $
                                Just "Live Hammersmith Odeon 1970-01-01"
                              ≟ liveName' releaseInfo1 trackL
                          ]

----------------------------------------

fileName ∷ (AsInfoError ε, MonadError ε η) ⇒
           Natural → ReleaseInfo → Track → η RelFile
fileName n r t =
  let gone = replace "/" "-" (go t)
      encompass  l r t = l ⊕ t ⊕ r
      parens   = encompass "(" ")"
      brackets = encompass "[" "]"
      go t = case t ⊣ trackTitle of
               Nothing → pack $ printf "%02d" n
               Just ti → let vv = parens   ⊳ t ⊣ trackVersion
                             ll = brackets ⊳ liveName' r t
                          in [fmt|%02d-%t|]
                             n (intercalate "  " $ catMaybes [Just ti,vv,ll])
   in case fromText gone of
        Nothing → throwIllegalFileName $ [fmt|illegal file name '%t'|] gone
        Just f  -> return f

fileNameTests ∷ TestTree
fileNameTests =
  testGroup "fileName"
            [ testCase "track1" $
                  Right [relfile|02-track title|]
                ≟ fileName @InfoError 2 releaseInfo1 track1 
            , testCase "trackL" $
                  Right [relfile|10-live track  [Live Hammersmith Odeon 1970-01-01]|]
                ≟ fileName @InfoError 10 releaseInfo1 trackL
            , testCase "trackS" $
                  Right [relfile|100-Sesh  (Acoustic)  [Session 1980-01-01]|]
                ≟ fileName @InfoError 100 releaseInfo1 trackS
            , testCase "trackL'-rl" $
                  Right [relfile|11-Live Track  [Live Sweden 1990-02-02]|]
                ≟ fileName @ParseInfoError 11 releaseInfol trackL'
            ]


-- ADD TESTS

flacName ∷ (AsInfoError ε, MonadError ε η) ⇒
           ReleaseInfo → Natural → Track → η RelFile
flacName r n t = fileName n r t ⊲ (⊙ [pc|flac|])

flacNames ∷ (AsInfoError ε, MonadError ε η) ⇒ Info → η [RelFile]
flacNames inf =
  sequence [ flacName (inf ⊣ releaseInfo) i t | (t,i) ← zip (tracks inf) [1..] ]

mp3Name ∷ (AsInfoError ε, MonadError ε η) ⇒
          ReleaseInfo → Natural → Track → η RelFile
mp3Name r n t = fileName n r t ⊲ (⊙ [pc|mp3|])

mp3Names ∷ (AsInfoError ε, MonadError ε η) ⇒ Info → η [RelFile]
mp3Names inf =
  sequence [ mp3Name (inf ⊣ releaseInfo) i t | (t,i) ← zip (tracks inf) [1..] ]


------------------------------------------------------------

data Tracks = Tracks [[Track]]
  deriving Show

tracks_ ∷ Tracks → [Track]
tracks_ (Tracks tss) = mconcat tss

instance Printable Tracks where
  print tss = P.text ∘ unlines $ toText ⊳ tracks_ tss

instance FromJSON Tracks where
  parseJSON (Array ts) = Tracks ⊳ (sequence $ parseJSON ⊳ toList ts)
{-
    case ts !? 0 of
      Just (Object _) → Tracks  ⊳ (sequence $ parseJSON ⊳ toList ts)
      Just (Array  _) → Trackss ⊳ (sequence $ withArray "Tracks" (\ v → sequence $ parseJSON ⊳ toList v) ⊳ (toList ts))
--      Just (Array _)  → Trackss ⊳ (sequence $ sequence ⊳ (fmap parseJSON ⊳ toList ⊳ toList ts))
-}      
  parseJSON invalid = typeMismatch "Array" invalid

------------------------------------------------------------

data ReleaseInfo = ReleaseInfo { _artist           ∷ Maybe Text
                               , _catno            ∷ Maybe Text
                               , _release          ∷ Maybe Text
                               , _original_release ∷ Maybe Text
                               , _source           ∷ Maybe Text
                               , _source_version   ∷ Maybe Text
                               , _live_type        ∷ Maybe Text
                               , _live_location    ∷ Maybe Text
                               , _live_date        ∷ Maybe Text
                               }
  deriving Show


live_type ∷ Lens' ReleaseInfo (Maybe Text)
live_type = lens _live_type (\ i y → i { _live_type = y})

live_location ∷ Lens' ReleaseInfo (Maybe Text)
live_location = lens _live_location (\ i l → i { _live_location = l})

live_date ∷ Lens' ReleaseInfo (Maybe Text)
live_date = lens _live_date (\ i d → i { _live_date = d})

------------------------------------------------------------

data Info = Info { _releaseInfo ∷ ReleaseInfo
                 , _tracks      ∷ Tracks
                 }
  deriving (Generic, Show)

releaseInfo ∷ Lens' Info ReleaseInfo
releaseInfo = lens _releaseInfo (\ i r → i { _releaseInfo = r })

tracks ∷ Info → [Track]
tracks i = tracks_ (_tracks i)

instance FromJSON Info where
{-
  parseJSON = let drop_ (_ : s) = s
                  drop_ s       = s
               in genericParseJSON defaultOptions { fieldLabelModifier = drop_ }
-}
  parseJSON = withObject "Info" $ \ v → Info
    ⊳ (ReleaseInfo ⊳ v .:? "artist"
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

instance Printable Info where
  print (Info (ReleaseInfo a c r e s v y l d) ts) =
    let toj Nothing  = "~"
        toj (Just x) = toText (show $ toJSON x)
        tot t x = t ⊕ ": " ⊕ toj x
        tom _ Nothing  = []
        tom i (Just x) = [ tot i (Just x) ]
        unl ∷ [Text] → Text
        unl = dropEnd 1 ∘ unlines
        indents xs = unl (("  " ⊕) ⊳ xs)
        lindent t = case lines t of
                      []       → ""
                      (x : []) → ("- " ⊕ x)
                      (x : xs) → unl $ ("- " ⊕ x) : [indents xs]

     in P.text $ unl (mconcat [ [ "---"
                                , tot "artist"         a
                                , tot "catno"          c
                                , tot "release"        r
                                ]
                              , tom "original_release" e
                              , [ tot "source"         s
                                , tot "source_version" v
                                ]
                              , tom "live_type" y
                              , tom "live_location" l
                              , tom "live_date" d
                              , [ "tracks:"
                                ]
                              , [ lindent $ toText ts ]
                              ])

blankReleaseInfo ∷ ReleaseInfo
blankReleaseInfo = ReleaseInfo Nothing Nothing Nothing Nothing
                               Nothing Nothing Nothing Nothing Nothing

blankInfo ∷ Natural → Info
blankInfo n = 
  Info blankReleaseInfo $ Tracks [replicate (fromIntegral n) (blankTrack)]

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

warn ∷ (MonadIO μ, Printable τ) ⇒ τ → μ ()
warn = liftIO ∘ hPutStrLn stderr ∘ toText

{-
withFile ∷ (MonadIO μ, Printable τ) ⇒ FilePath → (Info → Either τ Text) → μ ()
withFile fn f = do
  decodeFileEither fn ≫ \ case
    Left  e    → warn (show e) ⪼ exitWith (ExitFailure 3)
    Right infy → case f infy of
                   Left  e'  → warn e' ⪼ exitWith (ExitFailure 4)
                   Right txt → say txt
-}

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

data ParseInfoError = PIParseError ParseError | PIInfoError InfoError
  deriving (Eq,Show)

instance Exception ParseInfoError

instance Printable ParseInfoError where
  print (PIParseError e) = print e
  print (PIInfoError  e) = print e

instance AsParseError ParseInfoError where
  _ParseError = prism PIParseError
                      (\ case PIParseError e -> Right e; e -> Left e )

instance AsInfoError ParseInfoError where
  _InfoError = prism PIInfoError (\ case PIInfoError e -> Right e; e -> Left e )

------------------------------------------------------------

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
main = doMain @ParseInfoError @Word8 $ do
  opts ← optParser "read & write info.yaml" parseOpts

  case opts ⊣ runMode of
    ModeWrite      tc → say $ blankInfo tc
    ModeTrackCount fn → pInfo (pure ∘ length ∘ tracks) fn
    ModeFlacList   fn → pInfo' flacNames fn
    ModeMp3List    fn → pInfo' mp3Names fn

  return 0

--------------------------------------------------------------------------------

track1 ∷ Track
track1 = Track (Just "track title") Nothing Nothing Nothing Nothing

trackL ∷ Track
trackL = Track (Just "live track") Nothing
               (Just "Live") (Just "Hammersmith Odeon") (Just "1970-01-01")

trackL' ∷ Track
trackL' = Track (Just "Live Track") Nothing Nothing Nothing (Just "1990-02-02")

trackS ∷ Track
trackS = Track (Just "Sesh") (Just "Acoustic")
               (Just "Session") Nothing (Just "1980-01-01")

releaseInfo1 ∷ ReleaseInfo
releaseInfo1 = ReleaseInfo (Just "artie") (Just "123X") (Just "1979-12-31")
                           Nothing (Just "Elpee") Nothing Nothing Nothing
                           Nothing

releaseInfol ∷ ReleaseInfo
releaseInfol = ReleaseInfo (Just "simon") (Just "124XX") (Just "1979-12-31")
                           Nothing
                           (Just "An LP Title") Nothing
                           (Just "Live") (Just "Sweden") (Just "1990")

tests ∷ TestTree
tests = testGroup "infy" [ pYamlTests, lNameTests
                         , liveNameTests, fileNameTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
  
