{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dreamnet.Engine.CoordVector
( Width(..), Height(..), Depth(..)
, CoordVector(..), squared, cubed

, linCoord', linCoord, coordLin', coordLin, outOfBounds
) where


import Control.Monad.Random (Random, randomR, random)
import Numeric.Natural      (Natural)
import Linear               (V2(V2))

--------------------------------------------------------------------------------

newtype Width = Width Natural
               deriving(Eq, Ord, Show, Read, Enum, Num, Real, Integral)
newtype Height = Height Natural
               deriving(Eq, Ord, Show, Read, Enum, Num, Real, Integral)
newtype Depth = Depth Natural
               deriving(Eq, Ord, Show, Read, Enum, Num, Real, Integral)

instance Random Width where
    randomR (wmin, wmax) g =
        let (x, g') = randomR (fromIntegral wmin ∷ Integer, fromIntegral wmax ∷ Integer) g
        in  (Width (fromIntegral x), g')

    random g = let (a ∷ Integer, g') = random g
               in  (Width (fromIntegral a), g')


instance Random Height where
    randomR (wmin, wmax) g =
        let (x, g') = randomR (fromIntegral wmin ∷ Integer, fromIntegral wmax ∷ Integer) g
        in  (Height (fromIntegral x), g')

    random g = let (a ∷ Integer, g') = random g
               in  (Height (fromIntegral a), g')


instance Random Depth where
    randomR (wmin, wmax) g =
        let (x, g') = randomR (fromIntegral wmin ∷ Integer, fromIntegral wmax ∷ Integer) g
        in  (Depth (fromIntegral x), g')

    random g = let (a ∷ Integer, g') = random g
               in  (Depth (fromIntegral a), g')
--------------------------------------------------------------------------------

class CoordVector c where
    width  ∷ c → Width
    height ∷ c → Height


squared ∷ Width → Height → Natural
squared (Width w) (Height h) = w * h


cubed ∷ Width → Height → Depth → Natural
cubed (Width w) (Height h) (Depth d) = w * h * d


linCoord' ∷ Width → V2 Int → Int
linCoord' (Width w) (V2 x y) = y * fromIntegral w + x
{-# INLINE linCoord' #-}


linCoord ∷ (CoordVector c) ⇒ c → V2 Int → Int
linCoord c = linCoord' (width c)
{-# INLINE linCoord #-}



coordLin' ∷ Width → Int → V2 Int
coordLin' (Width w) i =
    let x = i `mod` fromIntegral w
        y = i `div` fromIntegral w
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

