{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}

module Dreamnet.Engine.CoordVector
( Width
, Height
, CoordVector(..)
, linCoord'
, linCoord
, coordLin'
, coordLin
, outOfBounds
) where

import Linear (V2(V2))

--------------------------------------------------------------------------------

type Width    = Word
type Height   = Word

class CoordVector c where
    width  ∷ c → Width
    height ∷ c → Height


linCoord' ∷ Width → V2 Int → Int
linCoord' (fromIntegral → w) (V2 x y) = y * w + x
{-# INLINE linCoord' #-}


linCoord ∷ (CoordVector c) ⇒ c → V2 Int → Int
linCoord c = linCoord' (width c)
{-# INLINE linCoord #-}



coordLin' ∷ Width → Int → V2 Int
coordLin' (fromIntegral → w) i =
    let x = i `mod` w
        y = i `div` w
    in  V2 x y 
{-# INLINE coordLin' #-}


coordLin ∷ (CoordVector c) ⇒ c → Int → V2 Int
coordLin c = coordLin' (width c)
{-# INLINE coordLin #-}


outOfBounds ∷ (CoordVector c) ⇒ c → V2 Int → Bool
outOfBounds c (V2 x y)
    | x < 0                        = True
    | y < 0                        = True
    | x >= fromIntegral (width c)  = True
    | y >= fromIntegral (height c) = True
    | otherwise                    = False
{-# INLINE outOfBounds #-}

