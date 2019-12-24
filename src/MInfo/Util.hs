{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MInfo.Util
  ( QuasiQuoter, __fromString, ePatSymExhaustive
  , mkQQC, mkQQCP, mkQuasiQuoterExp, mkQuasiQuoterExpP, mkWPatQf, mkWPatQm1, tries )
where

import Prelude  ( Integer, error, subtract )

-- base --------------------------------

import Control.Monad  ( fail, return )
import Data.Either    ( Either( Left, Right ) )
import Data.Foldable  ( foldl1, toList )
import Data.Function  ( ($), id )
import Data.Maybe     ( Maybe( Nothing, Just ), maybe )
import Data.String    ( String )
import Data.Typeable  ( Typeable, typeOf )
import Text.Read      ( readEither )


-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Textual, fromString, toString, toText )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqNE             ( (⋗), pattern (:⫸) )
import NonEmptyContainers.SeqNEConversions  ( ToMonoSeqNonEmpty( toSeqNE ) )

-- parsers ------------------------------

import Text.Parser.Combinators  ( Parsing, try )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ, Name, Lit( IntegerL )
                                   , Pat( ConP, LitP ), PatQ )
import Language.Haskell.TH.Quote   ( QuasiQuoter( QuasiQuoter, quoteDec
                                                , quoteExp, quotePat
                                                , quoteType
                                                )
                                   )

-- text --------------------------------

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.BoundedN  ( pattern 𝕎 )

--------------------------------------------------------------------------------

__ERROR__ ∷ Text → α
__ERROR__ = error ∘ toString

----------------------------------------

mkQuasiQuoterExp ∷ Text → (String → ExpQ) → QuasiQuoter
mkQuasiQuoterExp (toText → n) f =
  QuasiQuoter { quoteDec  = __ERROR__ $ n ⊕ " quoteDec not implemented"
              , quoteType = __ERROR__ $ n ⊕ " quoteType not implemented"
              , quotePat  = __ERROR__ $ n ⊕ " quotePat not implemented"
              , quoteExp = f
              }


----------------------------------------

{- | Make a quasi-quoter for data-type construction & pattern-matching -}
mkQQC ∷ Text → (String → Maybe ExpQ) → QuasiQuoter
mkQQC nm {- ^ typename -} f {- ^ maybe c'tor -} =
  QuasiQuoter { quoteDec  = __ERROR__ $ [fmt|%t quoteDec not implemented|]  nm
              , quoteType = __ERROR__ $ [fmt|%t quoteType not implemented|] nm
              , quotePat  = __ERROR__ $ [fmt|%t quotePat not implemented|] nm
              , quoteExp  = \ s → case f s of
                                    Nothing → fail $
                                                [fmt|not a valid %t: '%s'|] nm s
                                    Just x  → x
              }

----------------------------------------

{- | Make a quasi-quoter for data-type construction & pattern-matching -}
mkQQCP ∷ Text → (String → Maybe ExpQ) → (String → Maybe PatQ) → QuasiQuoter
mkQQCP nm {- ^ typename -} f {- ^ maybe c'tor -} p =
  QuasiQuoter { quoteDec  = __ERROR__ $ [fmt|%t quoteDec not implemented|]  nm
              , quoteType = __ERROR__ $ [fmt|%t quoteType not implemented|] nm
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

----------------------------------------

mkQuasiQuoterExpP ∷ Text → (String → ExpQ) → (String → PatQ) → QuasiQuoter
mkQuasiQuoterExpP (toText → n) f p =
  QuasiQuoter { quoteDec  = __ERROR__ $ n ⊕ " quoteDec not implemented"
              , quoteType = __ERROR__ $ n ⊕ " quoteType not implemented"
              , quotePat  = p
              , quoteExp  = f
              }

----------------------------------------

{- | Make a `PatQ` generator for a constructor that wraps `𝕎`, with an
     adjustment function `f`. -}
-- λ> runQ [p| Month_ (W 1) |]
-- ConP MInfo.Types.Month.Month_ [ConP MInfo.BoundedN.W [LitP (IntegerL 1)]]
mkWPatQf ∷ (Integer → Integer) → Name → String → PatQ
mkWPatQf f nm s = case readEither @Integer s of
                    Left  e → error e
                    Right i → let w = ConP '𝕎 [LitP (IntegerL (f i))]
                               in return $ ConP nm [w]

--------------------

{- | Make a `PatQ` generator for a constructor that wraps `𝕎`, with an
     adjustment function of `subtract 1`. -}
mkWPatQm1 ∷ Name → String → PatQ
mkWPatQm1 = mkWPatQf (subtract 1)

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

tries ∷ (ToMonoSeqNonEmpty ψ, Parsing η, Element ψ ~ η α) ⇒ ψ → η α
tries (toSeqNE → ts :⫸ t) = foldl1 (∤) (toList ((try ⊳ ts) ⋗ t))
tries _                        = ePatSymExhaustive "tries"

-- that's all, folks! ----------------------------------------------------------
