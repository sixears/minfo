{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MInfo.Util
  ( QuasiQuoter, __fromString, ePatSymExhaustive, mkQuasiQuoterExp, tries )
where

import Prelude  ( error )

-- base --------------------------------

import Data.Foldable  ( foldl1, toList )
import Data.Function  ( ($), id )
import Data.Maybe     ( maybe )
import Data.String    ( String )
import Data.Typeable  ( Typeable, typeOf )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Textual, fromString, toString, toText )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE  ( SeqNE, (⋗), pattern (:⫸) )

-- parsers ------------------------------

import Text.Parser.Combinators  ( Parsing, try )

-- template-haskell --------------------

import Language.Haskell.TH        ( ExpQ )
import Language.Haskell.TH.Quote  ( QuasiQuoter( QuasiQuoter, quoteDec
                                               , quoteExp, quotePat, quoteType )
                                  )

-- text --------------------------------

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

__ERROR__ ∷ Text → α
__ERROR__ = error ∘ toString

----------------------------------------

mkQuasiQuoterExp ∷ Text → (String → ExpQ) → QuasiQuoter
mkQuasiQuoterExp (toText → n) f =
  let notImpl u = __ERROR__ $ n ⊕ " " ⊕ u ⊕ " not implemented"
   in QuasiQuoter { quoteDec  = notImpl "quoteDec"
                  , quoteType = notImpl "quoteType"
                  , quotePat  = notImpl "quotePat"
                  , quoteExp = f
                  }

----------------------------------------

__fromString ∷ (Textual α, Typeable α) ⇒ String → α
__fromString s =
  let emsg t = [fmt|failed to parse %w '%s'|] (typeOf result) t
      result = maybe (error $ emsg s) id $ fromString s
   in result

----------------------------------------

ePatSymExhaustive ∷ String → α
ePatSymExhaustive s =
    error $ s ⊕ "https://gitlab.haskell.org/ghc/ghc/issues/10339"

----------------------------------------

{- | `try` the first thing, then the next thing, until the last thing (which
     isn't surrounded by a `try`) -}
tries ∷ Parsing η ⇒ SeqNE (η α) → η α
tries (ts :⫸ t) = foldl1 (∤) (toList ((try ⊳ ts) ⋗ t))
tries _          = ePatSymExhaustive "tries"

-- that's all, folks! ----------------------------------------------------------
