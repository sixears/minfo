{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

{- | Date, with varying precisions - day, month, year. -}

module MInfo.Types.Date.Error
  ( AsDateError( _DateError ), DateError_
  , badDateError, dateRangeError, dateRangeError_ )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( ($), id )
import Data.Typeable      ( Typeable )
import Text.Show          ( Show )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data Show ρ ⇒ DateError_ ρ = BadDateError ρ -- (Year,Month,DayOfM)
                           | DateRangeError (ρ,ρ)
  deriving (Eq, Show)

instance (Show ρ, Typeable ρ) ⇒ Exception (DateError_ ρ)

class AsDateError ρ ε where
  _DateError ∷ Prism' ε (DateError_ ρ)

instance AsDateError ρ (DateError_ ρ) where
  _DateError = id

badDateError ∷ (Show ρ, AsDateError ρ ε, MonadError ε η) ⇒ ρ → η α
badDateError x = throwError $ _DateError # BadDateError x

dateRangeError ∷ (Show ρ, AsDateError ρ ε, MonadError ε η) ⇒ ρ → ρ → η α
dateRangeError d0 d1 = throwError $ _DateError # DateRangeError (d0,d1)

dateRangeError_ ∷ (Show ρ, AsDateError ρ ε) ⇒ ρ → ρ → ε
dateRangeError_ d0 d1 = _DateError # DateRangeError (d0,d1)

-- that's all, folks! ----------------------------------------------------------
