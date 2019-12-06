{-# LANGUAGE UnicodeSyntax #-}

module MInfo.Types.ToWord16
  ( ToWord16, toIntegral, toWord16 )
where

import Prelude  ( Integral, fromIntegral )

-- base --------------------------------

import Data.Word  ( Word16 )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

--------------------------------------------------------------------------------

class ToWord16 α where
  toWord16 ∷ α → Word16
  toIntegral ∷ Integral β ⇒ α → β
  toIntegral = fromIntegral ∘ toWord16

-- that's all, folks! ----------------------------------------------------------
