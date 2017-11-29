{-# LANGUAGE UnicodeSyntax #-}

module Dreamnet.CoordVector
( CoordVector(..)
, linCoord'
, linCoord
, coordLin'
, coordLin
) where

import Linear (V2(V2))

--------------------------------------------------------------------------------

class CoordVector c where
    width  ∷ c → Word


linCoord' ∷ Int → V2 Int → Int
linCoord' w (V2 x y) = y * w + x
{-# INLINE linCoord' #-}


linCoord ∷ (CoordVector c) ⇒ c → V2 Int → Int
linCoord c = linCoord' (fromIntegral $ width c)
{-# INLINE linCoord #-}



coordLin' ∷ Int → Int → V2 Int
coordLin' w i = let x = i `mod` w
                    y = i `div` w
                in  V2 x y 
{-# INLINE coordLin' #-}


coordLin ∷ (CoordVector c) ⇒ c → Int → V2 Int
coordLin c = coordLin' (fromIntegral $ width c)
{-# INLINE coordLin #-}


