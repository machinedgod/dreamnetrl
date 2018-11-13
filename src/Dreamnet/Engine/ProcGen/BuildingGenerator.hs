{-# LANGUAGE UnicodeSyntax #-}

module Dreamnet.Engine.ProcGen.BuildingGenerator
( Building, rectBuilding

, buildingToShape

, rasterizeBuilding
)
where


import Control.Monad.Random (MonadRandom, getRandomR)
import Linear               (V2(V2))


import Dreamnet.Engine.CoordVector
import Dreamnet.Engine.ProcGen.Geometry

--------------------------------------------------------------------------------

data Building = Building {
      _b_width  ∷ Width
    , _b_height ∷ Height
    , _b_depth  ∷ Depth
    }
    deriving (Show)


rectBuilding ∷ (MonadRandom r) ⇒ Width → Height → Depth → Int → r Building
rectBuilding w h d r = Building
    <$> getRandomR (w - fromIntegral r, w + fromIntegral r)
    <*> getRandomR (h - fromIntegral r, h + fromIntegral r)
    <*> getRandomR (d - fromIntegral r, d + fromIntegral r)

--------------------------------------------------------------------------------

buildingToShape ∷ Building → Shape
buildingToShape b = let w = fromIntegral (_b_width b)
                        h = fromIntegral (_b_height b)
                    in  fromList [ V2 0 0
                                 , V2 w 0
                                 , V2 w h
                                 , V2 0 h
                                 ]

--------------------------------------------------------------------------------

rasterizeBuilding ∷ Building → Canvas → Canvas
rasterizeBuilding = undefined
