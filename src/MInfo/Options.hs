{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MInfo.Options
  ( Options, OptsOpts( OptsOpts ), Resolvable( resolve ), RunMode(..)
  , optsParse, runMode )
where

import Prelude  ( (-) )

-- base --------------------------------

import Control.Applicative     ( optional )
import Control.Monad.IO.Class  ( MonadIO )
import Control.Monad           ( return )
import Data.Bool               ( Bool( False ) )
import Data.Char               ( isDigit )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe )
import Data.String             ( String )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡), (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toString, toText )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.Basename          ( basename )
import FPath.Error.FPathError  ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Lens    ( Lens', lens )

-- monaderror-io -----------------------

import MonadError           ( MonadError )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO.FPath  ( pResolve )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Natural      ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks, runReader )

-- optparse-applicative ----------------

import Options.Applicative  ( CommandFields, Mod, Parser
                            , action, argument, auto, command, completer
                            , completeWith, info, listCompleter, metavar
                            , progDesc, strArgument, subparser, value
                            )

-- optparse-plus -----------------------

import OptParsePlus  ( parseOpts )

-- text --------------------------------

import Data.Text  ( Text, all, drop, length, take, takeWhile )

--------------------------------------------------------------------------------

data RunMode' = ModeWrite' ℕ
              | ModeTrackCount' Text
              | ModeFlacList'   Text
              | ModeMp3List'    Text
              | ModeTrackInfo'  Text ℕ (Maybe ℕ)
  deriving Show

isDiscDir ∷ AbsDir → Bool
isDiscDir d = let t = toText (basename d)
                  allDigits "" = False
                  allDigits x  = all isDigit x
                  -- safe version of Text.init
                  init x = take (length x - 1) x
               in "Disc " ≡ take 5 t ∧ allDigits (takeWhile (≢ ' ') ∘ init $ drop 5 t)

info_yaml_P ∷ MonadReader OptsOpts η ⇒ η (Parser Text)
info_yaml_P = do
  -- let dflt = value "info.yaml"
  c ← asks (view cwd)
  let dflt = if isDiscDir c then "../info.yaml" else "info.yaml"
  return $ strArgument (ю [ value dflt, metavar "YAMLFILE", action "file" ])

trackCountP ∷ Parser ℕ
trackCountP = let c = completer (listCompleter $ show ⊳ [ 1∷ℕ .. 99])
               in argument auto (metavar "TRACK-COUNT" ⊕ c)

writeC ∷ Mod CommandFields RunMode'
writeC = let desc = progDesc "write a blank info.yaml for CD"
          in command "write" (ModeWrite' ⊳ info trackCountP desc)

trackCountC ∷ MonadReader OptsOpts η ⇒ η (Mod CommandFields RunMode')
trackCountC = do
  let desc = progDesc "count the tracks in an info.yaml"
  i ← info_yaml_P
  return $ command "track-count" (info (ModeTrackCount' ⊳ i) desc)

flacListC ∷ MonadReader OptsOpts η ⇒ η (Mod CommandFields RunMode')
flacListC = do
  let desc = progDesc "list flacs from info.yaml"
  i ← info_yaml_P
  return $ command "flac-list" (info (ModeFlacList' ⊳ i) desc)

mp3ListC ∷ MonadReader OptsOpts η ⇒ η (Mod CommandFields RunMode')
mp3ListC = do
  let desc = progDesc "list mp3s from info.yaml"
  i ← info_yaml_P
  return $ command "mp3-list" (info (ModeMp3List' ⊳ i) desc)

trackInfoC ∷ MonadReader OptsOpts η ⇒ η (Mod CommandFields RunMode')
trackInfoC = do
  let desc = progDesc "produce JSON output track information"
      completeN = completeWith $ show ⊳ [1∷ℕ ..99]
      narg ∷ String → Parser ℕ
      narg nme = argument auto (metavar nme ⊕ completeN)
      n1 = narg "TRACKID/DISCID"
      n2 = narg "TRACKID"
      tiP ∷ Parser Text → Parser RunMode'
      tiP i = ModeTrackInfo' ⊳ i ⊵ n1 ⊵ optional n2
  i ← info_yaml_P
  return $ command "track-info" (info (tiP i) desc)

modeP ∷ MonadReader OptsOpts η ⇒ η (Parser RunMode')
modeP = do
  m ← mp3ListC
  t ← trackCountC
  f ← flacListC
  c ← trackInfoC
  return $ subparser (ю [ writeC, t, f, m, c ])

------------------------------------------------------------

{- | Options for configuring the options -}
newtype OptsOpts = OptsOpts { _cwd ∷ AbsDir }

cwd ∷ Lens' OptsOpts AbsDir
cwd = lens _cwd (\ o d → o { _cwd = d })

data Options' = Options' RunMode'
  deriving Show

--------------------

parseOptions ∷ OptsOpts → Parser Options'
parseOptions o = Options' ⊳ runReader (modeP) o

--------------------

data RunMode = ModeWrite ℕ
             | ModeTrackCount AbsFile
             | ModeFlacList   AbsFile
             | ModeMp3List    AbsFile
             | ModeTrackInfo  AbsFile ℕ (Maybe ℕ)
  deriving Show

type family ResolvesTo α
class Resolvable α where
  resolve ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
            α → μ (ResolvesTo α)

type instance ResolvesTo RunMode' = RunMode
instance Resolvable RunMode' where
  resolve (ModeWrite' n)       = return $ ModeWrite n
  resolve (ModeTrackCount' fn) = ModeTrackCount ⊳ pResolve fn
  resolve (ModeFlacList' fn)   = ModeFlacList ⊳ pResolve fn
  resolve (ModeMp3List' fn)    = ModeMp3List ⊳ pResolve fn
  resolve (ModeTrackInfo' fn x y) =
    pResolve fn ≫ \ fn' → return $ ModeTrackInfo fn' x y

data Options = Options { _runMode ∷ RunMode }
  deriving Show

type instance ResolvesTo Options' = Options
instance Resolvable Options' where
  resolve (Options' rm') = Options ⊳ resolve rm'

runMode ∷ Lens' Options RunMode
runMode = lens _runMode (\ o r → o { _runMode = r })

optsParse ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
            Maybe Text → Text → OptsOpts → μ Options
optsParse progn descn o =
  parseOpts progn (progDesc $ toString descn) (parseOptions o) ≫ resolve

-- that's all, folks! ----------------------------------------------------------
