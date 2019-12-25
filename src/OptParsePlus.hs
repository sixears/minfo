{-# LANGUAGE UnicodeSyntax #-}

module OptParsePlus
  ( argS, readT )
where

-- base --------------------------------

import Data.Typeable  ( Typeable )

-- data-textual ------------------------

import Data.Textual  ( Textual )

-- optparse-applicative ----------------

import Options.Applicative  ( ArgumentFields, Mod, Parser, ReadM
                            , argument, eitherReader )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TextualPlus  ( parseTextual )

--------------------------------------------------------------------------------

readT ∷ (Textual α, Typeable α) ⇒ ReadM α
readT = eitherReader parseTextual

argS ∷ (Textual α, Typeable α) ⇒ Mod ArgumentFields α → Parser α
argS = argument readT

-- that's all, folks! ----------------------------------------------------------
