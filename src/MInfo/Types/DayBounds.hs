{-# LANGUAGE UnicodeSyntax #-}

module MInfo.Types.DayBounds
  ( DayBounds( endDay, startDay ) ) 
where

-- time --------------------------------

import Data.Time  ( Day )

--------------------------------------------------------------------------------

{- | The first & last days in a "range" of imprecision -}
class DayBounds α where
  startDay ∷ α → Day
  endDay   ∷ α → Day

-- that's all, folks! ----------------------------------------------------------
