{-# LANGUAGE UnicodeSyntax #-}

module MInfo.Types.ToWord16
  ( ToWord16, toWord16 )
where

-- base --------------------------------

import Data.Word  ( Word16 )

--------------------------------------------------------------------------------

class ToWord16 α where
  toWord16 ∷ α → Word16

-- that's all, folks! ----------------------------------------------------------
