{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MInfo.Util
  ( QuasiQuoter, mkQQC, mkQQCP )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Monad  ( fail )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Nothing, Just ) )
import Data.String    ( String )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ, PatQ )
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

{- | Make a quasi-quoter for data-type construction & pattern-matching -}
mkQQC ∷ Text → (String → Maybe ExpQ) → QuasiQuoter
mkQQC nm {- ^ typename -} f {- ^ maybe c'tor -} =
  QuasiQuoter { quoteDec  = __ERROR'__ $ [fmt|%t quoteDec not implemented|]  nm
              , quoteType = __ERROR'__ $ [fmt|%t quoteType not implemented|] nm
              , quotePat  = __ERROR'__ $ [fmt|%t quotePat not implemented|] nm
              , quoteExp  = \ s → case f s of
                                    Nothing → fail $
                                                [fmt|not a valid %t: '%s'|] nm s
                                    Just x  → x
              }

----------------------------------------

{- | Make a quasi-quoter for data-type construction & pattern-matching -}
mkQQCP ∷ Text → (String → Maybe ExpQ) → (String → Maybe PatQ) → QuasiQuoter
mkQQCP nm {- ^ typename -} f {- ^ maybe c'tor -} p =
  QuasiQuoter { quoteDec  = __ERROR'__ $ [fmt|%t quoteDec not implemented|]  nm
              , quoteType = __ERROR'__ $ [fmt|%t quoteType not implemented|] nm
              , quotePat  = \ s → case p s of
                                    Nothing → fail $
                                                [fmt|(P) not a valid %t: '%s'|]
                                                nm s
                                    Just x  → x
              , quoteExp  = \ s → case f s of
                                    Nothing → fail $
                                                [fmt|not a valid %t: '%s'|] nm s
                                    Just x  → x
              }

-- that's all, folks! ----------------------------------------------------------
