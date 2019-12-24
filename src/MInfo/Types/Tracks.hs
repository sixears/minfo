{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module MInfo.Types.Tracks
  ( Tracks( Tracks, unTracks ), flatTracks, tests )
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
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import qualified  Data.ByteString  as  BS
import Data.ByteString  ( ByteString )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Monoid   ( ю )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Tasty    ( (≟) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

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

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.YamlPlus             ( unYaml )

import MInfo.Types.DateImprecise  ( dateImprecise )
import MInfo.Types                ( LiveType( Demo, Live, Session ) )
import MInfo.Types.Track          ( Track( Track ) )
import MInfo.YamlPlus.Error       ( YamlParseError )

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
  let t1 ∷ ByteString
      t1 = BS.intercalate "\n" [ "- title: Judas"
                               , "  live_type: Demo"
                               , "  live_date: 1993-07-29"
                               , "- title: Mercy in You"
                               , "  live_type: Session"
                               , "  live_date: 1993-07-29"
                               ]
      e1 ∷ Track
      e1 = Track Nothing (Just "Judas") Nothing Demo Nothing
                 (Just $ [dateImprecise|1993-07-29|])
      e2 ∷ Track
      e2 = Track Nothing (Just "Mercy in You") Nothing Session Nothing
                 (Just $ [dateImprecise|1993-07-29|])
      t3 ∷ ByteString
      t3 = BS.intercalate "\n" [ "-"
                               , "  - title: Judas"
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
      e3 ∷ Track
      e3 = Track Nothing (Just "I Feel You") Nothing Live Nothing
                 (Just $ [dateImprecise|1993-07-29|])
   in testGroup "tracksFromJSON"
                [ testCase "t1"  $ Right [e1,e2] ≟ unYaml @YamlParseError t1
                , testCase "t1'" $
                    Right (Tracks [[e1,e2]]) ≟ unYaml @YamlParseError t1
                , testCase "t3" $
                    Right (Tracks [[e1,e2],[e3]]) ≟ unYaml @YamlParseError t3
                ]

----------------------------------------

instance ToJSON Tracks where
  toJSON = Array ∘ fromList ∘ fmap toJSON ∘ flatTracks

----------------------------------------

flatTracks ∷ Tracks → [Track]
flatTracks (Tracks tss) = ю tss

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Tracks" [ tracksFromJSONTests ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
