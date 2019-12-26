{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MInfo.Options
  ( Options, RunMode(..), parseOptions, runMode )
where

-- base --------------------------------

import Data.Function  ( ($) )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- fpath -------------------------------

import FPath.File     ( File( FileR ) )
import FPath.RelFile  ( relfile )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Monoid   ( ю )
import Data.MoreUnicode.Natural  ( ℕ )

-- optparse-applicative ----------------

import Options.Applicative  ( CommandFields, Mod, Parser
                            , action, argument, auto, command, completer, info
                            , listCompleter, metavar, progDesc, subparser, value
                            )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import OptParsePlus  ( argS )

--------------------------------------------------------------------------------

data RunMode = ModeWrite ℕ
             | ModeTrackCount File
             | ModeFlacList File
             | ModeMp3List File
  deriving Show

info_yaml ∷ File
info_yaml = FileR [relfile|info.yaml|]

info_yaml_P ∷ Parser File
info_yaml_P = argS (ю [ value info_yaml, metavar "YAMLFILE", action "file" ])

trackCountP ∷ Parser ℕ
trackCountP = let c = completer (listCompleter $ show ⊳ [ 1∷ℕ .. 99])
               in argument auto (metavar "TRACK-COUNT" ⊕ c)

writeC ∷ Mod CommandFields RunMode
writeC = let desc = progDesc "write a blank info.yaml for CD"
               in command "write" (ModeWrite ⊳ info trackCountP desc)

trackCountC ∷ Mod CommandFields RunMode
trackCountC =
  let desc = progDesc "count the tracks in an info.yaml"
   in command "track-count" (info (ModeTrackCount ⊳ info_yaml_P) desc)

flacListC ∷ Mod CommandFields RunMode
flacListC = let desc = progDesc "list flacs from info.yaml"
               in command "flac-list" (info (ModeFlacList ⊳ info_yaml_P) desc)

mp3ListC ∷ Mod CommandFields RunMode
mp3ListC = let desc = progDesc "list mp3s from info.yaml"
              in command "mp3-list" (info (ModeMp3List ⊳ info_yaml_P) desc)

modeP ∷ Parser RunMode
modeP = subparser (ю [ writeC, trackCountC, flacListC, mp3ListC ])

------------------------------------------------------------

data Options = Options { _runMode ∷ RunMode}
  deriving Show

runMode ∷ Lens' Options RunMode
runMode = lens _runMode (\ o r → o { _runMode = r })

--------------------

parseOptions ∷ Parser Options
parseOptions = Options ⊳ modeP

-- that's all, folks! ----------------------------------------------------------
