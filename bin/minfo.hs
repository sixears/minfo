{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE UnicodeSyntax      #-}

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

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- exited ------------------------------

import Exited  ( doMain )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile )
import FPath.File     ( File( FileA ) )
import FPath.IO       ( getCwd )

-- monaderror-io -----------------------

import MonadError  ( ѥ )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, say )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Monad    ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- yaml-plus ---------------------------

import YamlPlus        ( unYamlFile )
import YamlPlus.Error  ( AsYamlParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.Errors          ( YamlFPathIOParseInfoFPCError )
import MInfo.Options         ( OptsOpts( OptsOpts )
                             , RunMode( ModeFlacList, ModeMp3List
                                      , ModeTrackCount, ModeWrite )
                             , optsParse, runMode
                             )

import MInfo.Types.Info      ( Info
                             , blankInfo, flacNames, mp3Names, trackCount )

--------------------------------------------------------------------------------

------------------------------------------------------------

{- | Print some function of Info. -}
pInfo ∷ (MonadIO μ, AsYamlParseError ε, MonadError ε μ, Printable τ) ⇒
        (Info → [τ]) → AbsFile → μ ()
pInfo f fn = unYamlFile (FileA fn) ≫ mapM_ say ∘ f

{- | Print some function of Info, that returns a foldable of printables (within
     a `MonadError`. -}
pInfo' ∷ (MonadIO μ,AsYamlParseError ε,MonadError ε μ,Foldable φ,Printable τ) ⇒
         (Info → μ (φ τ)) → AbsFile → μ ()

pInfo' f fn = do
  inf ← ѥ $ unYamlFile (FileA fn)
  xs  ← inf ≫ f
  forM_ xs say
  return ()


main ∷ IO ()
main = doMain @YamlFPathIOParseInfoFPCError @Word8 $ do
  cwd ← getCwd

  let summary = "read & write info.yaml"
  opts ← optsParse Nothing summary (OptsOpts cwd)
  case opts ⊣ runMode of
    ModeWrite      tc → say $ blankInfo tc
    ModeTrackCount fn → pInfo  ((:[]) ∘ show ∘ trackCount) fn
    ModeFlacList   fn → pInfo' flacNames fn
    ModeMp3List    fn → pInfo' mp3Names fn

  return 0

-- that's all, folks! ----------------------------------------------------------

