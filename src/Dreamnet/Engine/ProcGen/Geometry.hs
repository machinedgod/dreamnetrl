{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}


module Dreamnet.Engine.ProcGen.Geometry
( AffineTrans(..)

, Winding(..), Shape(..), fromList, shapeLines, ngonShape

, Canvas, newCanvas, rasterizePoint, rasterizeLine, drawCanvas
)
where


import Control.Lens           (makeLenses, (%~), view, views, _1, _2)
import Linear          hiding (rotate)
import Numeric.Natural

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

import Dreamnet.Engine.CoordVector
import Dreamnet.Engine.Utils

--------------------------------------------------------------------------------

class AffineTrans t where
    translate ∷ V2 Float → t → t
    rotate    ∷ Float → t → t
    scale     ∷ Float → t → t


instance AffineTrans (V2 Float) where
    translate v = (+v)

    rotate a v = let ur = V2 (cos a) (sin a)
                     l  = sqrt (quadrance v)
                 in  ur ^* l

    scale s = (^*s)

--------------------------------------------------------------------------------

data Winding = CW
             | CCW

--------------------------------------------------------------------------------

newtype Shape = Shape { points ∷ V.Vector (V2 Float) }
              deriving (Show)


fromList ∷ [V2 Float] → Shape
fromList = Shape . V.fromList


instance AffineTrans Shape where
    translate v = Shape . fmap (translate v) . points

    rotate a = Shape . fmap (rotate a) . points

    scale s = Shape . fmap (scale s) . points
    

shapeLines ∷ Shape → V.Vector (V2 Float, V2 Float)
shapeLines s = let cycled = V.snoc (V.drop 1 (points s)) (V.head (points s))
               in  V.zip (points s) cycled


ngonShape ∷ Winding → Natural → Shape
ngonShape w (fromIntegral → n) =
    let s = (2 * (case w of; CCW → -pi; CW → pi)) / n
        p = V2 1.0 0.0
    in  Shape $ (\i → rotate (i * s) p) <$> V.fromList [0..n - 1]
 

--------------------------------------------------------------------------------

data Canvas = Canvas {
      _c_size   ∷ (Width, Height)
    , _c_data   ∷ V.Vector Char
    }
makeLenses ''Canvas


instance CoordVector Canvas where
    width  = view (c_size._1)
    height = view (c_size._2)
 

newCanvas ∷ Width → Height → Char → Canvas
newCanvas w h ch = Canvas (w, h) (V.replicate (fromIntegral (squared w h)) ch)


-- Will break into another column if you go over the width
rasterizePoint ∷ Char → V2 Int → Canvas → Canvas
rasterizePoint ch v c = let i = linCoord c v
                        in  c_data %~ flip V.update (V.singleton (i, ch)) $ c


rasterizeLine ∷ Char → (V2 Int, V2 Int) → Canvas → Canvas
rasterizeLine ch (p1, p2) c =
    let coords = linCoord c <$> bla p1 p2
    in  c_data %~ V.modify (\mv → sequence_ ((\i → MV.write mv i ch) <$> coords)) $ c


drawCanvas ∷ Canvas → IO ()
drawCanvas c = mapM_ putStrLn toListOfStrings
    where
        toListOfStrings = let rows  = [0 .. views (c_size._2) (fromIntegral . subtract 1) c]
                              w     = views (c_size._1) fromIntegral c
                              v     = _c_data c
                              row r = V.toList (V.take w (V.drop (w * r) v))
                          in  row <$> rows

