{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module QuasiQuoting
  ( MkQQOpts, QuasiQuoter, dec, defaultMkQQOpts, exp, pat, typ, mkQQ )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Monad  ( fail )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Nothing, Just ), maybe )
import Data.String    ( String )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- template-haskell --------------------

import Language.Haskell.TH         ( DecsQ, ExpQ, PatQ, TypeQ )
import Language.Haskell.TH.Quote   ( QuasiQuoter( QuasiQuoter, quoteDec
                                                , quoteExp, quotePat
                                                , quoteType
                                                )
                                   )

-- text --------------------------------

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

{- | `error`, but for any printable, and with loud capital letters and some
     leading underscores, to emphasize the "partial"-ness (is it still "partial"
     if it /always/ diverges?)
 -}
__ERROR__ ∷ Printable τ ⇒ τ → α
__ERROR__ = error ∘ toString

{- | `__ERROR__`, specialized to `Text` -}
__ERROR'__ ∷ Text → α
__ERROR'__ = __ERROR__

{- | `__ERROR__`, specialized to `String` -}
__'ERROR__ ∷ String → α
__'ERROR__ = __ERROR__

----------------------------------------

data MkQQOpts = MkQQOpts { _exp ∷ Maybe (String → Maybe ExpQ)
                         , _dec ∷ Maybe (String → Maybe DecsQ)
                         , _pat ∷ Maybe (String → Maybe PatQ)
                         , _typ ∷ Maybe (String → Maybe TypeQ)
                         }

defaultMkQQOpts ∷ MkQQOpts
defaultMkQQOpts = MkQQOpts Nothing Nothing Nothing Nothing

instance Default MkQQOpts where
  def = defaultMkQQOpts

exp ∷ Lens' MkQQOpts (Maybe (String → Maybe ExpQ))
exp = lens _exp (\ m f → (m { _exp = f }))
-- exp ∷ Lens' MkQQOpts (String → Maybe ExpQ)
-- exp = lens (fromMaybe (const Nothing)  ∘ _exp) (\ o f → o { _exp = Just f })

dec ∷ Lens' MkQQOpts (Maybe (String → Maybe DecsQ))
dec = lens _dec (\ m f → (m { _dec = f }))

pat ∷ Lens' MkQQOpts (Maybe (String → Maybe PatQ))
pat = lens _pat (\ m f → (m { _pat = f }))

typ ∷ Lens' MkQQOpts (Maybe (String → Maybe TypeQ))
typ = lens _typ (\ m f → (m { _typ = f }))

mkQQ ∷ Text → MkQQOpts -> QuasiQuoter
mkQQ nm opts =
  let __ERROR__ t = error $ [fmt|%t %t not implemented|] nm t
      mkQQx t = \ f s → case f s of
                          Nothing → fail $ [fmt|(%t) not a valid %t: '%s'|]
                                           t nm s
                          Just x → x
      go t t' = maybe (__ERROR__ t) (mkQQx t')
   in QuasiQuoter { quoteDec  = go "quoteDec"  "D" $ opts ⊣ dec
                  , quoteType = go "quoteType" "T" $ opts ⊣ typ
                  , quotePat  = go "quotePat"  "P" $ opts ⊣ pat
                  , quoteExp  = go "quoteExp"  "E" $ opts ⊣ exp
                  }

-- that's all, folks! ----------------------------------------------------------
