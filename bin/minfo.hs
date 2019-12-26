{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}

-- base --------------------------------

import Control.Monad  ( forM_, mapM_, return )
import Data.Foldable  ( Foldable )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Nothing ) )
import Data.Word      ( Word8 )
import System.IO      ( IO )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- exited ------------------------------

import Exited  ( doMain )

-- fpath -------------------------------

import FPath.File     ( File( FileR ) )
import FPath.RelFile  ( relfile )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- monaderror-io -----------------------

import MonadError  ( ѥ )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import Options.Applicative  ( CommandFields, Mod, Parser
                            , action, argument, auto, command, completer, info
                            , listCompleter, metavar, progDesc, subparser, value
                            )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO               ( MonadIO, say )
import OptParsePlus          ( argS, parseOpts )
import MInfo.YamlPlus        ( unYamlFile )
import MInfo.YamlPlus.Error  ( AsYamlParseError )

import MInfo.Errors          ( YamlParseInfoFPCError )

import MInfo.Types.Info      ( Info
                             , blankInfo, flacNames, mp3Names, trackCount )

--------------------------------------------------------------------------------

data RunMode = ModeWrite ℕ
             | ModeTrackCount File
             | ModeFlacList File
             | ModeMp3List File
  deriving Show

trackCountP ∷ Parser ℕ
trackCountP = let c = completer (listCompleter $ show ⊳ [ 1∷ℕ .. 99])
               in argument auto (metavar "TRACK-COUNT" ⊕ c)

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

parseOptions ∷ Parser Options
parseOptions = Options ⊳ modeP

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
  opts ← parseOpts Nothing "read & write info.yaml" parseOptions
  
  case opts ⊣ runMode of
    ModeWrite      tc → say $ blankInfo tc
    ModeTrackCount fn → pInfo  ((:[]) ∘ show ∘ trackCount) fn
    ModeFlacList   fn → pInfo' flacNames fn
    ModeMp3List    fn → pInfo' mp3Names fn

  return 0

-- that's all, folks! ----------------------------------------------------------

