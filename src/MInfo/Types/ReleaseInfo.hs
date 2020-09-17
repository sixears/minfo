{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MInfo.Types.ReleaseInfo
  ( HasReleaseInfo( releaseInfo ), ReleaseInfo( ReleaseInfo )
  , artist, blankReleaseInfo, discname, discnames, discversion
  , liveDate, liveType, liveLocation
  , original_release, release, releaseInfoFields, source, sourceVersion

  , _rinfo1, _rinfo2, _rinfo3, _rinfo4, _rinfo5, _rinfo6, _rinfo7, _rinfo8
  , _rinfo9
  )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Null, Object )
                         , (.:?), (.:), (.!=), typeMismatch, withObject )

-- base --------------------------------

import Control.Monad  ( join, return )
import Data.Functor   ( fmap )
import Data.Maybe     ( Maybe( Just, Nothing ), maybe )
import Data.Eq        ( Eq )
import Data.Function  ( ($), id )
import Data.Tuple     ( fst, snd )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- date-imprecise ----------------------

import DateImprecise.DateImprecise       ( DateImprecise, dateImprecise )
import DateImprecise.DateImpreciseRange  ( DateImpreciseRange
                                         , dateImpreciseRange )

-- index -------------------------------

import Index ( (!!) )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Natural      ( ℕ )

-- text --------------------------------

import Data.Text     ( Text )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), ToJSON( toJSON ), (.=), object )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MInfo.Types  ( Artist, Catno, HasLiveDate( liveDate )
                    , HasLiveLocation( liveLocation ), HasLiveType( liveType )
                    , HasMaybeSource( source )
                    , HasMaybeSourceVersion( sourceVersion ), LiveLocation
                    , LiveType( Live, NotLive ), Source
                    , SourceVersion
                    )

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
                               , _discnames        ∷ [Maybe(Source,
                                                            Maybe SourceVersion)
                                                     ]
                               }
  deriving (Eq,Show)

class HasReleaseInfo α where
  releaseInfo ∷ Lens' α ReleaseInfo

instance HasReleaseInfo ReleaseInfo where
  releaseInfo = id

artist ∷ Lens' ReleaseInfo Artist
artist = lens _artist (\ i a → i { _artist = a })

release ∷ Lens' ReleaseInfo (Maybe DateImprecise)
release = lens _release (\ i r → i { _release = r })

original_release ∷ Lens' ReleaseInfo (Maybe DateImprecise)
original_release = lens _original_release (\ i o → i { _original_release = o })

discnames ∷ Lens' ReleaseInfo [Maybe(Source,Maybe SourceVersion)]
discnames = lens _discnames (\ i dns → i { _discnames = dns })

discname ∷ ReleaseInfo → ℕ → Maybe Source
discname r n = fst ⊳ join ((r ⊣ discnames) !! n)

discversion ∷ ReleaseInfo → ℕ → Maybe SourceVersion
discversion r n = join $ fmap snd $ join ((r ⊣ discnames) !! n)

instance HasMaybeSource ReleaseInfo where
  source ∷ Lens' ReleaseInfo (Maybe Source)
  source = lens _source (\ i s → i { _source = s })

instance HasMaybeSourceVersion ReleaseInfo where
  sourceVersion ∷ Lens' ReleaseInfo (Maybe SourceVersion)
  sourceVersion = lens _source_version (\ i v → i { _source_version = v })

instance HasLiveLocation ReleaseInfo where
  liveLocation ∷ Lens' ReleaseInfo (Maybe LiveLocation)
  liveLocation = lens _live_location (\ i l → i { _live_location = l})

instance HasLiveDate ReleaseInfo where
  liveDate ∷ Lens' ReleaseInfo (Maybe DateImpreciseRange)
  liveDate = lens _live_date (\ i d → i { _live_date = d})

instance HasLiveType ReleaseInfo where
  liveType ∷ Lens' ReleaseInfo LiveType
  liveType = lens _live_type (\ i y → i { _live_type = y})

data DiscName = DiscName (Maybe (Source,Maybe SourceVersion))

instance FromJSON DiscName where
  parseJSON (Object o) =
    let mk ∷ Source → Maybe SourceVersion → DiscName
        mk s v = DiscName (Just (s,v))
     in mk ⊳ o .: "title" ⊵ o .:? "version"
  parseJSON Null = return $ DiscName Nothing
  parseJSON invalid = typeMismatch "Object (DiscName)" invalid

fromDiscName ∷ DiscName → Maybe (Source,Maybe SourceVersion)
fromDiscName (DiscName (Just (s,sv))) = Just (s,sv)
fromDiscName (DiscName Nothing)       = Nothing

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
                      ⊵ maybe [] (\ xs → fromDiscName ⊳ xs) ⊳ (v .:? "discnames")

instance ToJSON ReleaseInfo where
  toJSON = object ∘ releaseInfoFields

releaseInfoFields ∷ ReleaseInfo → [(Text,Value)]
releaseInfoFields (ReleaseInfo a c r o s v t l d _) =
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
                               Nothing Nothing NotLive Nothing Nothing []

--------------------------------------------------------------------------------
--                                 test data                                  --
--------------------------------------------------------------------------------

_rinfo1 ∷ ReleaseInfo
_rinfo1 =
  ReleaseInfo { _artist           = "Depeche Mode"
              , _catno            = Nothing
              , _release          = Nothing
              , _original_release = Nothing
              , _source        = Just "World We Live in and Live in Hamburg,The"
              , _source_version   = Nothing
              , _live_type        = Live
              , _live_location    = Just "Hamburg"
              , _live_date        = Just [dateImpreciseRange|1984-12-14|]
              , _discnames        = []
              }

_rinfo2 ∷ ReleaseInfo
_rinfo2 = ReleaseInfo { _artist           = "Depeche Mode"
                      , _catno            = Just "DMDVD4"
                      , _release          = Nothing
                      , _original_release = Nothing
                      , _source           = Just "Devotional"
                      , _source_version   = Nothing
                      , _live_type        = NotLive
                      , _live_location    = Nothing
                      , _live_date        = Nothing
                      , _discnames        = []
                      }

_rinfo3 ∷ ReleaseInfo
_rinfo3 =
  ReleaseInfo { _artist           = "Depeche Mode"
              , _catno            = Just "12345"
              , _release          = Just [dateImprecise|1993|]
              , _original_release = Nothing
              , _source           = Just "Radio 1 in Concert"
              , _source_version   = Nothing
              , _live_type        = Live
              , _live_location    = Just "Crystal Palace"
              , _live_date        = Just [dateImpreciseRange|1993-07-31|]
              , _discnames        = []
              }

_rinfo4 ∷ ReleaseInfo
_rinfo4 = ReleaseInfo { _artist           = "Depeche Mode"
                      , _catno            = Just "BX Stumm 300"
                      , _release          = Just [dateImprecise|2009-04-17|]
                      , _original_release = Nothing
                      , _source           =
                          Just "Sounds of the Universe  (Deluxe Box Set)"
                      , _source_version   = Nothing
                      , _live_type        = NotLive
                      , _live_location    = Nothing
                      , _live_date        = Nothing
                      , _discnames        = []
                      }

_rinfo5 ∷ ReleaseInfo
_rinfo5 = ReleaseInfo { _artist           = "Depeche Mode"
                      , _catno            = Nothing
                      , _release          = Just [dateImprecise|2009-04-17|]
                      , _original_release = Just [dateImprecise|2009-01-01|]
                      , _source           = Just "Sounds of the Universe"
                      , _source_version   = Just "Deluxe Box Set"
                      , _live_type        = NotLive
                      , _live_location    = Nothing
                      , _live_date        = Nothing
                      , _discnames        = []
                      }

_rinfo6 ∷ ReleaseInfo
_rinfo6 = ReleaseInfo ("Depeche Mode") Nothing
                      (Just ([dateImprecise|2009-04-17|]))
                      Nothing (Just "Sounds of the Universe")
                      (Just "Deluxe Box Set") NotLive Nothing Nothing []

_rinfo7 ∷ ReleaseInfo
_rinfo7 = ReleaseInfo { _artist           = "Various Artists"
                      , _catno            = Nothing
                      , _release          = Nothing
                      , _original_release = Just [dateImprecise|2009-01-01|]
                      , _source           = Just "Compilation"
                      , _source_version   = Nothing
                      , _live_type        = NotLive
                      , _live_location    = Nothing
                      , _live_date        = Nothing
                      , _discnames        = []
                      }

_rinfo8 ∷ ReleaseInfo
_rinfo8 = ReleaseInfo ("Depeche Mode") Nothing
                      (Just ([dateImprecise|2009-04-17|]))
                      Nothing (Just "Sounds of the Universe")
                      (Just "Deluxe Box Set") NotLive Nothing Nothing
                      [Nothing, Just ("Remixen",Just"R")]

_rinfo9 ∷ ReleaseInfo
_rinfo9 = ReleaseInfo ("All About Eve") Nothing
                      Nothing 
                      Nothing (Just "Live Preston Guildhall 1991")
                      Nothing
                      Live (Just "Preston Guildhall")
                           (Just ([dateImpreciseRange|1991-11-09|]))
                      []

-- that's all, folks! ----------------------------------------------------------
