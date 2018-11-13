{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Dreamnet.Engine.ProcGen.MapGenerator
where


import Safe                   (fromJustNote)
import Control.Lens           ((%=), (.=), (^.), (^?), uses, view,
                               _1, _2, traversed, index, mapMOf_)
import Control.Monad.Random   (MonadRandom, getRandomR)
import Control.Monad.State    (MonadState, execStateT, get)
import Data.Semigroup         ((<>))
import Numeric.Natural        (Natural)
import Linear                 (V2(V2))

import qualified Data.Vector as V (fromList, replicate)
import qualified Data.Map    as M ((!), unions)


import Dreamnet.Engine.CoordVector
import Dreamnet.Engine.TileMap

import Dreamnet.Engine.ProcGen.Common

--------------------------------------------------------------------------------

generateMap ∷ IO TileMap
generateMap = do
    maps   ← loadPredefined
    blocks ← randomBlocksLayout
    let (mw, mh) = cityMapSize blocks

    flip execStateT (newTileMap mw mh base) $ do
        prepareTileset (view m_tileset <$> maps)
        placeBlocks blocks
        placeSpawns
        --stampBuilding 4 4 (head maps)
    where
        base = Tile '_' (V.fromList [ "Base", "True", "True" ])

        prepareTileset ts = m_tileset .= M.unions ts

        placeBlocks bs =
        --placeBlocks bs = uses m_tileset (M.! '.') >>= \base →
            get >>= mapMOf_ (m_layers.traversed.index 0.l_size) modf 
            where
                modf ls = m_layers.traversed.index 0.l_data %= flip (foldr (placeBlockTile ls)) bs

                placeBlockTile (lw, lh) (Block w h (V2 x y)) =
                    blit
                        w h (V.replicate (fromIntegral $ squared w h) '.')
                        x y
                        lw lh

        placeSpawns = uses m_tileset (M.! '╳') >>= \spawn → 
            m_layers.traversed.index 0 %= ( changeTile (V2 1 1) spawn
                                          . changeTile (V2 0 1) spawn
                                          . changeTile (V2 1 0) spawn
                                          . changeTile (V2 0 0) spawn
                                          )


cityMapSize ∷ [Block] → (Width, Height)
cityMapSize = foldr largest (0, 0)
    where
        largest (Block w h (V2 x y)) (rw, rh) =
            let w' = max (fromIntegral x + w) rw
                h' = max (fromIntegral y + h) rh
            in  (w', h')


-- TODO add 'isValid' that checks that all necessary files are present for
-- a filename in list
loadPredefined ∷ IO [TileMap]
loadPredefined = traverse (loadTileMap . ("res/" <>)) maps
    where
        maps = [ "apartment0"
               , "bar"
               , "job"
               ]


stampBuilding ∷ (MonadState TileMap s) ⇒ Natural → Natural → TileMap → s ()
stampBuilding x y tm =
    let dataOptic  = m_layers.traversed.index 0.l_data
        firstLayer = fromJustNote "Loaded map has no layers, so it can't be stamped!" (tm ^? dataOptic)
    in  m_layers.traversed.index 0.l_data %= blit (width tm) (height tm) firstLayer x y (width tm) (height tm)


--------------------------------------------------------------------------------

data Block = Block {
      _b_width    ∷ Width
    , _b_height   ∷ Height
    , _b_position ∷ V2 Natural
    }


randomBlocksLayout ∷ (MonadRandom r) ⇒ r [Block]
randomBlocksLayout = do
    c1 ← rndQuad
    c2 ← rndQuad
    c3 ← rndQuad
    c4 ← rndQuad

    pure [ uncurry Block c1 (V2 0 0)
         , uncurry Block c2 (V2 (fromIntegral $ c1^._1 + 2) 0)
         , uncurry Block c3 (V2 0 (fromIntegral $ c1^._2 + 2))
         , uncurry Block c4 (V2 (fromIntegral $ c1^._1 + 2) (fromIntegral $ c1^._2 + 2))
         ]
    where
        rndQuad ∷ (MonadRandom r) ⇒ r (Width, Height)
        rndQuad = let w = Width  . fromIntegral <$> getRandomR (20 ∷ Int, 30 ∷ Int)
                      h = Height . fromIntegral <$> getRandomR (20 ∷ Int, 30 ∷ Int)
                  in  (,) <$> w <*> h

