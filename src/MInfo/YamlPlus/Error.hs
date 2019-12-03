{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MInfo.YamlPlus.Error
  ( YamlParseError, AsYamlParseError( _YamlParseError ), asYamlParseError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Bifunctor     ( first )
import Data.Either        ( Either )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( id )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- yaml -------------------------------

import Data.Yaml  ( ParseException )

--------------------------------------------------------------------------------

newtype YamlParseError = YamlParseError ParseException
  deriving Show

instance Exception YamlParseError

instance Eq YamlParseError where
  a == b = show a ≡ show b

instance Printable YamlParseError where
  print = P.string ∘ show

class AsYamlParseError ε where
  _YamlParseError ∷ Prism' ε YamlParseError

instance AsYamlParseError YamlParseError where
  _YamlParseError = id

asYamlParseError ∷ AsYamlParseError ε ⇒ Either ParseException α → Either ε α
asYamlParseError = first ((_YamlParseError #) ∘ YamlParseError)


-- that's all, folks! ----------------------------------------------------------
