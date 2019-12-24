{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

import Prelude  ( error )

-- base --------------------------------

import Control.Monad           ( forM_, mapM_, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool )
import Data.Either             ( Either( Left, Right ) )
import Data.Foldable           ( Foldable )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.String             ( String )
import Data.Typeable           ( Typeable, typeOf )
import Data.Word               ( Word8 )
import Numeric.Natural         ( Natural )
import System.IO               ( IO )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), Printable, Textual
                     , parseText, toString, toText )

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

import Data.MoreUnicode.Applicative  ( (⊴) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monad        ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import Options.Applicative  ( ArgumentFields, CommandFields, Mod, Parser, ReadM
                            , action, argument, auto, command, completer
                            , customExecParser, eitherReader, failureCode
                            , fullDesc, helper, info, listCompleter, metavar
                            , prefs, progDesc, showHelpOnEmpty, showHelpOnError
                            , subparser, value
                            )

-- text --------------------------------

import Data.Text     ( Text )
import Data.Text.IO  ( putStrLn )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.YamlPlus        ( unYamlFile )
import MInfo.YamlPlus.Error  ( AsYamlParseError )

import MInfo.Errors          ( YamlParseInfoFPCError )

import MInfo.Types.Info      ( Info
                             , blankInfo, flacNames, mp3Names, trackCount )

--------------------------------------------------------------------------------

type 𝔹 = Bool

------------------------------------------------------------

-- this looks like a monadic fold, or somesuch.  Maybe foldM or MaybeT?
maybeList ∷ [Maybe α] → Maybe α
maybeList [] = Nothing
maybeList (Just a : _)   = Just a
maybeList (Nothing : as) = maybeList as

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

-- that's all, folks! ----------------------------------------------------------

