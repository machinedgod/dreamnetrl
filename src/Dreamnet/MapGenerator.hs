{-# LANGUAGE UnicodeSyntax, TupleSections, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.MapGenerator
where


import Safe                   (headMay)
import Control.Lens           ((%=), (%~), (.=), (^.), uses, view, views)
import Control.Monad.Random   (MonadRandom, getRandomR)
import Control.Monad.State    (MonadState, execStateT)
import Control.Monad.IO.Class (MonadIO)
import Data.Semigroup         ((<>))
import Data.Maybe             (fromMaybe)
import Data.List              (genericReplicate)
import Linear                 (V2(V2), _x, _y, (^*), quadrance)

import qualified Data.Vector as V (fromList, toList, imap, (!?))
import qualified Data.Map    as M ((!), unions)


import Dreamnet.Utils        (bla)
import Dreamnet.CoordVector
import Dreamnet.TileMap

--------------------------------------------------------------------------------

generateMap ∷ (MonadIO m, MonadRandom m) ⇒ m TileMap
generateMap = do
    maps ← loadPredefined
    blocks ← randomBlocksLayout
    let (V2 mw mh) = cityMapSize blocks

    flip execStateT (newTileMap (fromIntegral mw) (fromIntegral mh) base) $ do
        prepareTileset (view m_tileset <$> maps)
        placeSpawns
        --stampBuilding 4 4 (head maps)
    where
        base = Tile '.' (V.fromList [ "Spawn", "True", "True" ])


cityMapSize ∷ [Block] → V2 Word
cityMapSize = foldr largest (V2 0 0)
    where
        largest (V2 x y, V2 w h) (V2 rw rh) =
            let w' = max (fromIntegral x + w) rw
                h' = max (fromIntegral y + h) rh
            in  V2 w' h'


-- TODO add 'isValid' that checks that all necessary files are present for
-- a filename in list
loadPredefined ∷ (MonadIO m) ⇒ m [TileMap]
loadPredefined = traverse (loadTileMap . ("res/" <>)) maps
    where
        maps = [ "apartment0"
               , "bar"
               , "job"
               ]
    

prepareTileset ∷ (MonadState TileMap s) ⇒ [Tileset] → s ()
prepareTileset ts = m_tileset .= M.unions ts


placeSpawns ∷ (MonadRandom s, MonadState TileMap s) ⇒ s ()
placeSpawns = do
    spawn ← uses m_tileset (M.! '╳')
    m_layers %= fmap ( changeTile (V2 1 1) spawn
                     . changeTile (V2 0 1) spawn
                     . changeTile (V2 1 0) spawn
                     . changeTile (V2 0 0) spawn
                     )


stampBuilding ∷ (MonadState TileMap s) ⇒ Word → Word → TileMap → s ()
stampBuilding x y tm = do
    let firstLayer = views m_layers (fromMaybe (error "Loaded map has no layers, so it can't be stamped!") . headMay . V.toList) tm
    m_layers %= fmap (copyData (width tm) (height tm) firstLayer x y)
    where
        copyData ∷ Width → Height → TileLayer → Word → Word → TileLayer → TileLayer
        copyData (fromIntegral → sw) (fromIntegral → sh) sl (fromIntegral → dx) (fromIntegral → dy) dl =
            l_data %~ V.imap (\i c → replaceTile (coordLin dl i) c) $ dl
            where
                replaceTile ∷ V2 Int → Char → Char
                replaceTile v c =
                    let (V2 x' y') = v - (V2 dx dy)
                    in  if x' < 0 || x' >= sw || y' < 0 || y' >= sh
                            then c
                            else views l_data (\slv → fromMaybe (error "Unreasonable coordinate!") $ slv V.!? (linCoord sl (V2 x' y'))) sl

--------------------------------------------------------------------------------

type Block = (V2 Int, V2 Word)


randomBlocksLayout ∷ (MonadRandom r) ⇒ r [Block]
randomBlocksLayout = do
    c1 ← rndQuad
    c2 ← rndQuad
    c3 ← rndQuad
    c4 ← rndQuad

    pure [ (V2 0 0, c1)
         , (V2 (fromIntegral $ c1^._x + 2) 0, c2)
         , (V2 0 (fromIntegral $ c1^._y + 2), c3)
         , (V2 (fromIntegral $ c1^._x + 2) (fromIntegral $ c1^._y + 2), c4)
         ]
    where
        rndQuad ∷ (MonadRandom r) ⇒ r (V2 Word)
        rndQuad = V2 <$> getRandomR (20, 30) <*> getRandomR (10, 15)
 
--------------------------------------------------------------------------------

type Canvas = [String]


newCanvas ∷ Width → Height → Canvas
newCanvas w h = genericReplicate h (genericReplicate w '.')


rasterizePoint ∷ V2 Int → Canvas → Canvas
rasterizePoint (V2 x y) c =
    let moddedColumn s = take x s ++ ['#'] ++ drop (x + 1) s
    in  take y c ++ [moddedColumn (c !! y)] ++ drop (y + 1) c


rasterizeLine ∷ (V2 Int, V2 Int) → Canvas → Canvas
rasterizeLine (p1, p2) c =
    let ps = bla p1 p2
    in  foldr rasterizePoint c ps


drawCanvas ∷ [String] → IO ()
drawCanvas = mapM_ putStrLn

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


newtype Shape = Shape { points ∷ [V2 Float] }


instance AffineTrans Shape where
    translate v = Shape . fmap (translate v) . points

    rotate a = Shape . fmap (rotate a) . points

    scale s = Shape . fmap (scale s) . points
    

shapeLines ∷ Shape → [(V2 Float, V2 Float)]
shapeLines s = zip (points s) (drop 1 $ cycle (points s))


randomShape ∷ (MonadRandom r) ⇒ Word → r Shape
randomShape = undefined


ngonShape ∷ Winding → Word → Shape
ngonShape w (fromIntegral → n) =
    let s = (2 * (case w of; CCW → -pi; CW → pi)) / n
        p = V2 1.0 0.0
    in  Shape $ (\i → rotate (i * s) p) <$> [0..n - 1]
 
