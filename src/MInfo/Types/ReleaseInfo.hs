{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MInfo.Types.ReleaseInfo
  ( ReleaseInfo( ReleaseInfo )
  , blankReleaseInfo, live_date, live_type, live_location, releaseInfoFields )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( Value, (.:?), (.:), (.!=), withObject )

-- base --------------------------------

import Data.Maybe     ( Maybe( Nothing ), maybe )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- date-imprecise ----------------------

import DateImprecise.DateImprecise       ( DateImprecise )
import DateImprecise.DateImpreciseRange  ( DateImpreciseRange )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monoid       ( ю )

-- text --------------------------------

import Data.Text     ( Text )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON ), (.=), object )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.Types                     ( Artist, Catno, LiveLocation
                                       , LiveType( NotLive ), Source
                                       , SourceVersion )

--------------------------------------------------------------------------------

data ReleaseInfo = ReleaseInfo { _artist           ∷ Artist
                               , _catno            ∷ Maybe Catno
                               , _release          ∷ Maybe DateImprecise
                               , _original_release ∷ Maybe DateImprecise
                               , _source           ∷ Maybe Source
                               , _source_version   ∷ Maybe SourceVersion
                               , _live_type        ∷ LiveType
                               , _live_location    ∷ Maybe LiveLocation
                               , _live_date        ∷ Maybe DateImpreciseRange
                               }
  deriving (Eq,Show)


live_type ∷ Lens' ReleaseInfo LiveType
live_type = lens _live_type (\ i y → i { _live_type = y})

live_location ∷ Lens' ReleaseInfo (Maybe LiveLocation)
live_location = lens _live_location (\ i l → i { _live_location = l})

live_date ∷ Lens' ReleaseInfo (Maybe DateImpreciseRange)
live_date = lens _live_date (\ i d → i { _live_date = d})

instance FromJSON ReleaseInfo where
  parseJSON = withObject "ReleaseInfo" $
    \ v → ReleaseInfo ⊳ v .: "artist"
                      ⊵ v .:? "catno"
                      ⊵ v .:? "release"
                      ⊵ v .:? "original_release"
                      ⊵ v .:? "source"
                      ⊵ v .:? "source_version"
                      ⊵ v .:? "live_type" .!= NotLive
                      ⊵ v .:? "live_location"
                      ⊵ v .:? "live_date"

instance ToJSON ReleaseInfo where
  toJSON = object ∘ releaseInfoFields

releaseInfoFields ∷ ReleaseInfo → [(Text,Value)]
releaseInfoFields (ReleaseInfo a c r o s v t l d) =
  ю [ [ "artist" .= a ]
    , [ "catno"  .= c ]
    , maybe [] (\ r' → [ "release"          .= toJSON r' ]) r
    , maybe [] (\ o' → [ "original_release" .= toJSON o' ]) o
    , [ "source" .= s ]
    , maybe [] (\ v' → [ "source_version"   .= toJSON v' ]) v

    , case t of
        NotLive → []
        _       → ю [ [ "live_type"     .= toJSON t ]
                    , maybe [] (\ l' → [ "live_location" .= toJSON l' ]) l
                    , maybe [] (\ d' → [ "live_date"     .= toJSON d' ]) d
                    ]
    ]


blankReleaseInfo ∷ ReleaseInfo
blankReleaseInfo = ReleaseInfo "" Nothing Nothing Nothing
                               Nothing Nothing NotLive Nothing Nothing

-- that's all, folks! ----------------------------------------------------------
