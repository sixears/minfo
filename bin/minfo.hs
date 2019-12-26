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

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- exited ------------------------------

import Exited  ( doMain )

-- fpath -------------------------------

import FPath.File     ( File )

-- monaderror-io -----------------------

import MonadError  ( ѥ )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊣) )
import Data.MoreUnicode.Monad  ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO               ( MonadIO, say )
import OptParsePlus          ( parseOpts )
import MInfo.Options         ( RunMode( ModeFlacList, ModeMp3List
                                      , ModeTrackCount, ModeWrite )
                             , parseOptions, runMode
                             )
import MInfo.YamlPlus        ( unYamlFile )
import MInfo.YamlPlus.Error  ( AsYamlParseError )

import MInfo.Errors          ( YamlParseInfoFPCError )

import MInfo.Types.Info      ( Info
                             , blankInfo, flacNames, mp3Names, trackCount )

--------------------------------------------------------------------------------

------------------------------------------------------------

{- | Print some function of Info. -}
pInfo ∷ (MonadIO μ, AsYamlParseError ε, MonadError ε μ, Printable τ) ⇒
        (Info → [τ]) → File → μ ()
pInfo f fn = unYamlFile fn ≫ mapM_ say ∘ f

{- | Print some function of Info, that returns a foldable of printables (within
     a `MonadError`. -}
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

