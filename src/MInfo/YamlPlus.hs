{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MInfo.YamlPlus
  ( unYaml, unYamlFile )
where

-- base --------------------------------

import Control.Monad           ( join )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Either             ( Either )
import Data.Functor            ( fmap )
import System.IO               ( FilePath )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import Data.ByteString  ( ByteString )

-- fpath -------------------------------

import FPath.AsFilePath  ( filepath )
import FPath.File        ( File )

-- monaderror-io -----------------------

import MonadError  ( fromRight )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⩺) )
import Data.MoreUnicode.Lens     ( (⫥) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON, decodeEither', decodeFileEither )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.YamlPlus.Error  ( AsYamlParseError, asYamlParseError )

--------------------------------------------------------------------------------

unYaml ∷ ∀ ε α μ . (FromJSON α, MonadError ε μ, AsYamlParseError ε) ⇒
         ByteString → μ α
unYaml = fromRight ∘ asYamlParseError ∘ decodeEither'

{- | Decode a yaml file; IO errors (e.g., file not found) are thrown as
     YamlParseErrors (this is the doing of `Data.Yaml.decodeFileEither`, not
     me).
 -}
unYamlFile ∷ (FromJSON α, MonadIO μ, MonadError ε μ, AsYamlParseError ε) ⇒
             File → μ α
unYamlFile = let go ∷ (FromJSON α, MonadIO μ,AsYamlParseError ε) ⇒
                      FilePath → μ (Either ε α)
                 go = liftIO  ∘ fmap asYamlParseError ∘ decodeFileEither
              in join ∘ (fromRight ⩺ go ∘ (⫥ filepath))

-- that's all, folks! ----------------------------------------------------------
